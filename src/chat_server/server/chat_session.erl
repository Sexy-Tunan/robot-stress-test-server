%% ==============================================================
%% 文件: chat_session.erl (聊天会话)
%% 每个客户端连接对应一个独立的业务处理进程
%% ==============================================================
-module(chat_session).
-export([start/1,new_loop/2,terminate/2]).

-include("../../../include/protocol/chat_protocol.hrl").
-include("../../../include/database/chat_database.hrl").

-define(WORLD_CHANNEL_NAME, "world").


start(Socket) ->
	process_flag(trap_exit, true),
	receive
		start ->
			io:format("[chat_session, ~p] 获取了新建socket通道控制权~n", [self()]),
			io:format("[chat_session, ~p] 正在等待登录消息包~n", [self()]),
			inet:setopts(Socket, [{active, once}]),
			new_loop(Socket,#{socket => Socket})
	end.



new_loop(Socket,State) ->
	receive
	%% ----  获取socket通道数据
		{tcp,WebSocket,<<ProtoId:16, JsonBin/binary>>} ->
			DataMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
			case ProtoId of
				%% 处理登录请求
				?LOGIN_REQUEST_PROTOCOL_NUMBER ->
					UserName = maps:get(userName, DataMap),
					{PayloadJsonBin,NewState} = case login_check(DataMap) of
						true ->
							io:format("用户[~ts]登录成功~p~n",[UserName,self()]),
							TempState = State#{login_state => true, user => UserName, last_world_send_time => 0},
							%% 查询数据库该用户加入了哪些频道，这些频道有哪些用户
							{ok,AllChannelInfoWithMembers} = database_queryer:query_all_channel_info_with_members(),
							PayloadJsonBin = jsx:encode(#{state => true, user => UserName, data => AllChannelInfoWithMembers}),
							%% 向已加入的频道注册自己的登录信息，以便频道信息广播能接收到
							{ok,JoinChannelList} = database_queryer:query_joined_channel_info(UserName),
							{ok,ChannelPidList} = channel_manager:query_channel_pid_batch(JoinChannelList),
							[Pid ! {user_login_register,UserName,self()} || Pid <- ChannelPidList],
							{PayloadJsonBin,TempState};
						false ->
							io:format("用户登录失败,密码错误~n"), put(login_state, false),
							PayloadJsonBin = jsx:encode(#{state => false, user => UserName, reason => unicode:characters_to_binary("密码错误",utf8,utf8)}),
							{PayloadJsonBin,State}
					end,
					PacketLength = 2 + byte_size(PayloadJsonBin),
					Packet = <<
%%						PacketLength:32/big-unsigned-integer,
						?Login_RESPONSE_PROTOCOL_NUMBER:16/big-unsigned-integer,
						PayloadJsonBin/binary
					>>,
					%% 构造数据包返回
					gen_tcp:send(Socket, Packet),
%%					case maps:get of
%%
%%					end

					inet:setopts(Socket, [{active, once}]),
					new_loop(Socket, NewState);

				%% 用户发送频道消息
				?MSG_REQUEST_PROTOCOL_NUMBER ->
					%% 向频道管理者得到所属频道PID
					Sender = maps:get(sender,DataMap),
					ChannelName = maps:get(channel,DataMap),
					Message = maps:get(message,DataMap),
					%% 增加世界频道发言时间限制
					case ChannelName =:= <<"world">> of
						true -> %%如果是世界频道需判断发言时间
							LastWorldSendTime = maps:get(last_world_send_time,State),
							io:format("系统当前时间:~p",[erlang:system_time()]),
							io:format("上次发言时间:~p",[LastWorldSendTime]),
							case (erlang:system_time(second) - LastWorldSendTime) > 10 of
								true ->
									io:format("用户[~ts]往[~ts]频道发送了消息[~ts]~n",[Sender,ChannelName,Message]),
									{ok, ChannelPid} = channel_manager:query_channel_pid(ChannelName),
									ChannelPid ! {msg, Sender, Message},

									inet:setopts(Socket, [{active, once}]),
									new_loop(Socket, State#{last_world_send_time => erlang:system_time(second)});
								false ->
									io:format("用户[~ts]在十秒内往[~ts]频道发送了多次消息~n",[Sender,ChannelName]),
									PayloadJsonBin = jsx:encode(#{state => false, reason => unicode:characters_to_binary("世界频道只允许十秒发言一次",utf8,utf8)}),
									Packet = <<
										?LIMIT_WORLD_SEND_PROTOCOL_NUMBER:16/big-unsigned-integer,
										PayloadJsonBin/binary
									>>,
									inet:setopts(Socket, [{active, once}]),
									gen_tcp:send(Socket, Packet),
									new_loop(Socket, State)
							end;
						false ->
							io:format("用户[~ts]往[~ts]频道发送了消息[~ts]~n",[Sender,ChannelName,Message]),
							{ok, ChannelPid} = channel_manager:query_channel_pid(ChannelName),
							ChannelPid ! {msg, Sender, Message},

							inet:setopts(Socket, [{active, once}]),
							new_loop(Socket, State)
					end;



				%% 用户创建频道
				?CHANNEL_CREATE_REQUEST_PROTOCOL_NUMBER ->
					%% 向频道管理者发送新增频道消息
					Creator = maps:get(user,DataMap),
					ChannelName = maps:get(channel,DataMap),
					io:format("用户[~p]创建了[~p]频道~n",[Creator,ChannelName]),
					{ok,ChannelPid} = channel_manager:register_channel(Creator,ChannelName),

					%% 创建者自动加入频道
					database_queryer:add_channel_user_record(Creator, ChannelName),
					%% 向频道进程注册创建者的信息
					ChannelPid ! {user_login_register, Creator, self()},

					inet:setopts(Socket, [{active, once}]),
					new_loop(Socket, State);

				%% 用户删除频道
				?CHANNEL_DELETE_REQUEST_PROTOCOL_NUMBER ->
					Deleter = maps:get(user,DataMap),
					ChannelName = maps:get(channel,DataMap),
					io:format("用户[~ts]尝试删除频道[~ts]~n",[Deleter,ChannelName]),
					case channel_manager:revoke_channel(Deleter,ChannelName) of
						ok ->
							io:format("频道[~ts]删除成功~n",[ChannelName]),
							{ok, State};
						{error, not_creator} ->
							io:format("用户[~ts]不是频道[~ts]的创建者，无法删除~n",[Deleter,ChannelName]),
							{ok, State};
						{error, Reason} ->
							io:format("删除频道失败: ~p~n", [Reason]),
							{ok, State}
					end,
				inet:setopts(Socket, [{active, once}]),
				new_loop(Socket, State);


				%% 用户加入频道
				?JOIN_CHANNEL_REQUEST_PROTOCOL_NUMBER ->
					Joiner = maps:get(user,DataMap),
					ChannelName = maps:get(channel,DataMap),
					io:format("用户[~p]加入了[~p]频道~n",[Joiner,ChannelName]),
					{ok,ChannelPid} = channel_manager:query_channel_pid(ChannelName),
					%% 数据库新增频道与用户关系记录（参数顺序：Member, ChannelName）
					database_queryer:add_channel_user_record(Joiner, ChannelName),
					%% 向频道进程注册自己的信息，其会向其他客户端广播说明自己的加入
					ChannelPid ! {user_join_register, Joiner, self()},
					%% 返回当前频道的信息,并构造数据包返回
					{ok, ChannelInfo} = database_queryer:query_channel_info_with_members_by_channel(ChannelName),

					PayloadJsonBin = jsx:encode(#{state => true, data => ChannelInfo}),
					PacketLength = 2 + byte_size(PayloadJsonBin),
					Packet = <<
%%						PacketLength:32/big-unsigned-integer,
						?JOIN_CHANNEL_RESPONSE_PROTOCOL_NUMBER:16/big-unsigned-integer,
						PayloadJsonBin/binary
					>>,
					gen_tcp:send(Socket,Packet),
					inet:setopts(Socket, [{active, once}]),
					new_loop(Socket, State);

				%% 用户退出频道
				?QUIT_CHANNEL_REQUEST_PROTOCOL_NUMBER ->
					Quitter = maps:get(user,DataMap),
					ChannelName = maps:get(channel,DataMap),
					io:format("用户[~ts]退出了[~ts]频道~n",[Quitter,ChannelName]),
					{ok,ChannelPid} = channel_manager:query_channel_pid(ChannelName),
					%% 数据库删除频道与用户关系记录（参数顺序：Member, ChannelName）
					database_queryer:remove_channel_user_record(Quitter, ChannelName),
					%% 向频道进程注销自己的信息，其会向其他客户端广播说明自己的退出
					ChannelPid ! {user_revoke, Quitter},
					{ok, State}
			end;


		%% ---- TCP 连接关闭 ----
		{tcp_closed, Socket} ->
			io:format("[~p] 客户端断开连接~n",[self()]),
			{error, disconnect};

		%% 接收到频道广播消息，发送给客户端
		{msg_broadcast, ChannelName, SenderName, Message} ->
			io:format("用户[~ts]接受到了频道[~ts]的广播消息[~ts],发消息者[~ts]~n",[maps:get(user,State),ChannelName,Message,SenderName]),
			PayloadJsonBin = jsx:encode(#{channel => ChannelName, sender => SenderName, message => Message}),
			PacketLength = 2 + byte_size(PayloadJsonBin),
			Packet =
				<<
%%					PacketLength:32/big-unsigned-integer,
					?MSG_BROADCAST_PROTOCOL_NUMBER:16/big-unsigned-integer,
					PayloadJsonBin/binary
				>>,
			gen_tcp:send(Socket,Packet),
			new_loop(Socket,State);

		%% 接受频道广播消息 告知客户端用户加入频道信息
		{user_join_channel, UserName, ChannelName} ->
			io:format("用户[~ts]接受到了[~ts]加入频道[~ts]的广播消息~n",[maps:get(user,State),UserName,ChannelName]),
			PayloadJsonBin = jsx:encode(#{user => UserName, channel => ChannelName}),
			PacketLength = 2 + byte_size(PayloadJsonBin),
			Packet =
				<<
%%					PacketLength:32/big-unsigned-integer,
					?JOIN_CHANNEL_BROADCAST_PROTOCOL_NUMBER:16/big-unsigned-integer,
					PayloadJsonBin/binary
				>>,
			gen_tcp:send(Socket,Packet),
			new_loop(Socket,State);

		%% 接受频道广播消息 告知客户端用户退出频道信息
		{user_quit_channel, UserName, ChannelName} ->
			io:format("用户[~ts]接受到了[~ts]退出频道[~ts]的广播消息~n",[maps:get(user,State),UserName,ChannelName]),
			PayloadJsonBin = jsx:encode(#{user => UserName, channel => ChannelName}),
			PacketLength = 2 + byte_size(PayloadJsonBin),
			Packet =
				<<
%%					PacketLength:32/big-unsigned-integer,
					?QUIT_CHANNEL_BROADCAST_PROTOCOL_NUMBER:16/big-unsigned-integer,
					PayloadJsonBin/binary
				>>,
			gen_tcp:send(Socket,Packet),
			new_loop(Socket,State);

		%% 接受频道广播消息 告知客户端新建频道信息
		{create_channel,Creator,CreatedChannelName} ->
			io:format("用户[~ts]接受到了新建频道[~ts]的广播消息，创建者[~ts]~n",[maps:get(user,State),CreatedChannelName,Creator]),
			PayloadJsonBin = jsx:encode(#{user => Creator, channel => CreatedChannelName}),
			io:format("发送频道创建广播，JSON: ~p~n", [PayloadJsonBin]),
			PacketLength = 2 + byte_size(PayloadJsonBin),
			Packet =
				<<
%%					PacketLength:32/big-unsigned-integer,
					?CREATE_CHANNEL_BROADCAST_PROTOCOL_NUMBER:16/big-unsigned-integer,
					PayloadJsonBin/binary>>,
			gen_tcp:send(Socket,Packet),
			new_loop(Socket,State);

		%% 接受频道广播消息 告知客户端删除频道信息
		{delete_channel,Deleter,DeletedChannelName} ->
			io:format("用户[~ts]接受到了删除频道[~ts]的广播消息，删除者[~ts]~n",[maps:get(user,State),DeletedChannelName,Deleter]),
			PayloadJsonBin = jsx:encode(#{user => Deleter, channel => DeletedChannelName}),
			PacketLength = 2 + byte_size(PayloadJsonBin),
			Packet =
				<<
	%%				PacketLength:32/big-unsigned-integer,
					?DELETE_CHANNEL_BROADCAST_PROTOCOL_NUMBER:16/big-unsigned-integer,
					PayloadJsonBin/binary
				>>,
			gen_tcp:send(Socket,Packet),
			new_loop(Socket,State)
	end.


terminate(_Reason, State) ->
%%	Socket = maps:get(socket,State),
%%	UserName = maps:get(user,State),


	ok.


%%  =============================================================================================

login_check(DataMap) ->
	%% 默认将键转为原子
	UserName = maps:get(userName, DataMap),
	Password = maps:get(password, DataMap),
	case database_queryer:query_user_message_by_user_name(UserName) of
		{ok, User} -> Password =:= User#user.password;
		not_found ->
			%% 用户不存在，为其注册
			io:format("用户[~s]不存在，为其进行注册~n",[unicode:characters_to_list(UserName,utf8)]),
			database_queryer:add_user_record(UserName,Password),
			database_queryer:add_channel_user_record(UserName, <<"world">>),
			{ok,WorldChannelPid} = channel_manager:query_channel_pid(<<"world">>),
			WorldChannelPid ! {user_join_register, UserName, self()},
			true
	end.