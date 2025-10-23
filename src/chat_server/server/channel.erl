%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc  只提供启动方法用于启动对应频道的进程，想要销毁只能通过channel_manager销毁，管理者会发送stop消息给对应的频道进程处理。
%%% @end
%%% Created : 16. 10月 2025 19:27
%%%-------------------------------------------------------------------
-module(channel).
-author("caiogu").

-behavior(gen_server).
%% API
-export([start/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-include("../../../include/database/chat_database.hrl").
-define(WORLD_CHANNEL_NAME, <<"world">>).

start(ChannelName) ->
	gen_server:start(?MODULE, [ChannelName], []).

init([ChannelName]) ->
	put(channelName, ChannelName),
	io:format("创建[~ts]频道进程,[~p]~n",[ChannelName,self()]),
	io:format("初始化[~ts]频道的地图为"),
	%% 不使用named_table选项，这样就不会全局注册导致冲突
	Ets = ets:new(user_pid_ets,[set, private, {keypos, #user_pid.user_name}]),
	{ok,Ets}.


%% ===================================================================================
%% 回调方法
%% 查询pid

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(stop, State) -> {stop, normal, State};

handle_info({msg,SenderName,Message},State) ->
	%% 聊天消息广播
	ChannelName = get(channelName),
	PidList = ets:match(State, {'_','_','$1'}),

	%% 得到的是[[pid1],[[pid2]]，需展开
	[Pid ! {msg_broadcast,ChannelName,SenderName,Message} || [Pid] <- PidList],
	{noreply, State};

%% 处理移动消息并广播
handle_info({move, User, FromX, FromY, ToX, ToY}, State) ->
	ChannelName = get(channelName),
	PidList = ets:match(State, {'_','_','$1'}),
	%% 向所有用户广播移动消息
	[Pid ! {move_broadcast, ChannelName, User, FromX, FromY, ToX, ToY} || [Pid] <- PidList],
	{noreply, State};

%% 用户向频道注册自己的信息
handle_info({user_join_register, UserName, From},State) ->
	%% 用户加入频道，或者用户连接了服务都需要来到频道注册自己的信息，不然无法进行广播
	io:format("用户[~ts]加入频道[~ts]后向注册了自己的信息",[UserName,get(channelName)]),
	ets:insert(State,#user_pid{user_name = UserName, pid = From}),
	%% 广播其他用户有新用户加入频道
	ChannelName = get(channelName),
	PidList = ets:match(State, {'_','_','$1'}),
	[Pid ! {user_join_channel,UserName,ChannelName} || [Pid] <- PidList],
	{noreply, State};

handle_info({user_login_register, UserName, From},State) ->
	%% 用户加入频道，或者用户连接了服务都需要来到频道注册自己的信息，不然无法进行广播
	io:format("用户[~ts]登录后向频道[~ts]注册了自己的信息~n",[UserName,get(channelName)]),
	ets:insert(State,#user_pid{user_name = UserName, pid = From}),
	{noreply, State};

%% 用户向频道注销自己的信息
handle_info({user_revoke, UserName},State) ->
	ChannelName = get(channelName),
	%% 用户websocket断联，或者退出频道都需要向频道注销自己的信息
	ets:delete(State,UserName),
	%% 广播其他用户有新用户加入频道
	PidList = ets:match(State, {'_','_','$1'}),
	[Pid ! {user_quit_channel,UserName,ChannelName} || [Pid] <- PidList],
	{noreply, State};

%% 通过世界频道进程向所有用户广播新频道创建信息
handle_info({create_channel, Creator, CreatedChannelName},State) ->
	PidList = ets:match(State, {'_','_','$1'}),
	[Pid ! {create_channel,Creator,CreatedChannelName} || [Pid] <- PidList],
	{noreply, State};

%% 通过世界频道进程向所有用户广播频道删除信息
handle_info({delete_channel, Deleter, DeletedChannelName},State) ->
	PidList = ets:match(State, {'_','_','$1'}),
	[Pid ! {delete_channel,Deleter,DeletedChannelName} || [Pid] <- PidList],
	{noreply, State};

handle_info(show_all,State) ->
	Records = ets:match(State, '$1'),
	io:format("进程[~ts]的用户已注册信息~n",[get(channelName)]),
	lists:foreach(fun([Record]) -> io:format(">>>> [~ts] == ~p~n",[Record#user_pid.user_name,Record#user_pid.pid]) end, Records),
	{noreply, State};

handle_info(broadcast_shutdown, State) ->
	%% 通知所有客户端断开连接，因为服务器要关闭了
	case get(channelName) =:= ?WORLD_CHANNEL_NAME of
		true ->
			%% 世界频道通知所有客户端断联
			io:format("世界频道开始通知所有客户端断联~n"),
			AllUserPidList = ets:match(State, {'_','_','$1'}),
			io:format("啦啦啦~p~n",[[Pid || [Pid] <- AllUserPidList]]),
			[Pid ! {broadcast_shutdown, everyone} || [Pid] <- AllUserPidList],
			{stop, normal, State};
		false -> {stop, normal, State}
	end;

handle_info(_Info, State) -> {noreply, State}.



handle_call(_Res, _From, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	io:format("频道[~ts]停止~n",[get(channelName)]),
	ok.