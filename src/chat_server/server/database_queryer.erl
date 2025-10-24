%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc 数据查询接口提供
%%%
%%% @end
%%% Created : 16. 10月 2025 09:22
%%%-------------------------------------------------------------------
-module(database_queryer).
-author("Administrator").
-behaviour(gen_server).

-include("../../../include/database/chat_database.hrl").


%% API
-export([start/1,stop/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,terminate/2]).
%% 用户api
-export([add_user_record/2,query_user_message_by_user_name/1, query_user_message_by_channel_name/1]).
%% 频道api
-export([add_channel_record/2,remove_channel_record/2,query_all_channel_name_alive/0, query_all_channel_info_with_members/0]).
%% 频道用户api
-export([add_channel_user_record/2, remove_channel_user_record/2, query_joined_channel_info/1, query_channel_info_with_members_by_channel/1]).
%% 频道地图api
-export([query_channel_map_info/1]).


-spec query_user_message_by_user_name(user_name()) -> user_message().
-type user_name() :: string().
-type user_message() :: {ok,#user{}} | {error, no_table} | user_not_found.

-define(WORLD_CHANNEL,"world").
-record(state, {
	tables % #{TableName => #{ets => Ets, dets => Dets}},其实就是一个map映射存储表信息
}).

%% 启停方法
start(IsNeedInitData) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [IsNeedInitData], []).

stop() ->
	gen_server:call(?MODULE, stop).

%% API 接口

%% @doc 根据用户名查询用户信息
%% @Return {ok,record} | {error,not_found}
query_user_message_by_user_name(UserName) -> gen_server:call(?MODULE,{query_user, user, user_name, UserName}).

%% @doc 根据用户名查询用户信息
%% @Return {ok,record} | {
add_user_record(UserName,Password) -> gen_server:call(?MODULE,{add_user, user, UserName,Password}).

%% @doc 根据频道名字其所属的用户信息
%% @Return {ok,UserNameList} | {error,not_found}
query_user_message_by_channel_name(ChannelName) -> gen_server:call(?MODULE,{query_user, channel_user, channel_name, ChannelName}).

%% @doc 查询所有未删除的频道名字
%% @Return {ok,ChannelNameList} | {error,not_found}
query_all_channel_name_alive() -> gen_server:call(?MODULE,{query_channel, alive, channel}).

%% @doc 新增频道记录
%% @Return ok | {error,exist}
add_channel_record(Creator, ChannelName) -> gen_server:call(?MODULE,{add_channel, channel, Creator, ChannelName}).

%% @doc 删除频道记录
%% @Return ok | {error, not_creator}
remove_channel_record(Owner, ChannelName) -> gen_server:call(?MODULE,{remove_channel, channel, Owner, ChannelName}).

%% @doc 新增频道用户关系记录
%% @Return ok
add_channel_user_record(Member, ChannelName) -> gen_server:call(?MODULE,{add_channel_user, channel_user, Member, ChannelName}).

%% @doc 删除频道用户关系记录
%% @Return ok
remove_channel_user_record(Member, ChannelName) -> gen_server:call(?MODULE,{remove_channel_user, channel_user, Member, ChannelName}).

%% @Return {ok, [{channel_name => ChannelName, members => Members},{channel_name => ChannelName, members => Members},......] }
query_all_channel_info_with_members() -> gen_server:call(?MODULE,{query_all_channel_info_with_members, channel_user, channel}).

%% 根据频道名字查询频道的信息，包括频道成员
%% @Return {ok, [{channel_name => ChannelName, members => Members},{channel_name => ChannelName, members => Members},......] }
query_channel_info_with_members_by_channel(ChannelName) -> gen_server:call(?MODULE,{query_channel_info_with_members_by_channel, channel_user, channel, ChannelName}).

%% 根据用户名字查询已加入频道的信息，包括频道成员
%% @Return {ok, [{channel_name => ChannelName, members => Members},{channel_name => ChannelName, members => Members},......] }
query_joined_channel_info(UserName) -> gen_server:call(?MODULE,{query_joined_channel_info, channel_user, UserName}).

%% 查询所有频道的地图
query_channel_map_info(ChannelName) -> gen_server:call(?MODULE,{query_channel_map_info, channel_map, ChannelName}).
%% ====================================================================
%% 初始化方法
init([IsNeedInitData]) ->

	%% 表示希望捕获退出信号，而不是被动退出
	process_flag(trap_exit, true),

	%% 从dets数据库中查出数据加载到ets
	%% 定义所有表的元信息
	TablesInfo = [
		{user,    "data/user.dets",    user_ets,    #user.name},
%%		{msg,     "data/msg.dets",     msg_ets,     #msg.user_name},
		{channel, "data/channel.dets", channel_ets, #channel.name},
		{channel_user, "data/channel_user.dets", channel_user_ets, #channel_user.channel_name},
		{channel_map, "data/channel_map.dets", channel_map_ets, #channel_map.channel_name}
	],
	Tables = load_all_dets_to_ets(TablesInfo),
	case IsNeedInitData of
		true -> init_data(Tables);
		false -> do_nothing
	end,
	io:format("数据查询处理者加载成功[~p]~n",[self()]),
	{ok, #state{tables = Tables}}.

load_all_dets_to_ets(TableInfos) ->
	lists:foldl(fun load_one_table/2, #{}, TableInfos).

%% 加载单张表
load_one_table({DetsName, DetsFile, EtsName, KeyPos}, Acc) ->
	{ok, _} = dets:open_file(DetsName,[{repair,true},{file,DetsFile},{type,bag}]),
	Ets = ets:new(EtsName, [bag, named_table, public, {keypos, KeyPos}]),
	load_from_dets(DetsName, Ets),
	maps:put(DetsName, #{ets => Ets, dets => DetsName}, Acc).

%% 将 dets 中的记录导入 ets
load_from_dets(Dets, Ets) ->
	dets:traverse(Dets,
		fun(Record) -> ets:insert(Ets, Record), continue end
	),
	ok.

init_data(Tables) ->
	io:format("往ets中插入一些数据~n"),
	%% 插入部分数据到ets
	case maps:get(user,Tables,undefined) of
		undefined -> nothing;
		#{ets := Ets1} ->
			ets:insert(Ets1,#user{name=unicode:characters_to_binary("Bruce",utf8,utf8),password=unicode:characters_to_binary("123456",utf8,utf8)}),
			ets:insert(Ets1,#user{name=unicode:characters_to_binary("Ben",utf8,utf8),password=unicode:characters_to_binary("123456",utf8,utf8)}),
			ets:insert(Ets1,#user{name=unicode:characters_to_binary("菜狗",utf8,utf8),password=unicode:characters_to_binary("123456",utf8,utf8)})
	end,

	case maps:get(channel,Tables,undefined) of
		undefined -> nothing;
		#{ets := Ets2} ->
%%			ets:insert(Ets2,#channel{name=unicode:characters_to_binary("channel of bruce"),creator = unicode:characters_to_binary("Bruce",utf8,utf8), alive = true}),
%%			ets:insert(Ets2,#channel{name=unicode:characters_to_binary("channel of ben"),creator = unicode:characters_to_binary("Ben",utf8,utf8), alive = true}),
			ets:insert(Ets2,#channel{name=unicode:characters_to_binary(?WORLD_CHANNEL),creator = unicode:characters_to_binary("admin",utf8,utf8), alive = true})
%%			ets:insert(Ets2,#channel{name=unicode:characters_to_binary("菜狗的频道"),creator = unicode:characters_to_binary("菜狗",utf8,utf8), alive = true})
	end,

	case maps:get(channel_user,Tables,undefined) of
		undefined -> nothing;
		#{ets := Ets3} ->
			ets:insert(Ets3,#channel_user{channel_name = unicode:characters_to_binary(?WORLD_CHANNEL,utf8,utf8), user_name = unicode:characters_to_binary("Bruce",utf8,utf8)}),
			ets:insert(Ets3,#channel_user{channel_name = unicode:characters_to_binary(?WORLD_CHANNEL,utf8,utf8), user_name = unicode:characters_to_binary("Ben",utf8,utf8)}),
			ets:insert(Ets3,#channel_user{channel_name = unicode:characters_to_binary(?WORLD_CHANNEL,utf8,utf8), user_name = unicode:characters_to_binary("菜狗",utf8,utf8)})
%%			ets:insert(Ets3,#channel_user{channel_name = unicode:characters_to_binary("channel of bruce",utf8,utf8), user_name = unicode:characters_to_binary("Bruce",utf8,utf8)}),
%%			ets:insert(Ets3,#channel_user{channel_name = unicode:characters_to_binary("channel of ben",utf8,utf8), user_name = unicode:characters_to_binary("Ben",utf8,utf8)}),
%%			ets:insert(Ets3,#channel_user{channel_name = unicode:characters_to_binary("菜狗的频道",utf8,utf8), user_name = unicode:characters_to_binary("菜狗",utf8,utf8)})
	end,

	case maps:get(channel_map, Tables, undefined) of
		undefined -> nothing;
		#{ets := Ets4} ->
			ets:insert(Ets4, #channel_map{channel_name = unicode:characters_to_binary(?WORLD_CHANNEL, utf8, utf8), width = 100, height = 100})
	end.



%% =======================================================================
%% 回调方法
%% 根据用户名查询用户信息
handle_call({query_user, TableName, user_name, UserName}, _From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName, Tables, undefined) of
		undefined -> {reply, {error, no_table}, State};
		#{ets := Ets} ->
			case ets:lookup(Ets, UserName) of
				[Record] -> {reply, {ok, Record}, State};
				[] -> {reply, not_found, State}
			end
	end;

%% 添加用户
handle_call({add_user, TableName, UserName, Password}, _From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName, Tables, undefined) of
		undefined -> {reply, {error, no_table}, State};
		#{ets := Ets} ->
			ets:insert(Ets, #user{name = UserName,password = Password}),
			{reply, ok, State}
	end;

%% 根据频道名字查询关联的用户信息
%% table -> channel_user
handle_call({query_user, TableName, channel_name, ChannelName}, _From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName, Tables, undefined) of
		undefined -> {reply, {error, no_table}, State};
		#{ets := Ets} ->
			UserNameList = ets:match(Ets, {'_',ChannelName,'$1'}),
			{reply, {ok, [X || [X] <- UserNameList]}, State}
	end;

%% 查询所有存活的频道名字
handle_call({query_channel, alive, TableName},_From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			ChannelNameList = ets:match(Ets, {'_', '$1', '_', true}),
			{reply, {ok, [X || [X] <- ChannelNameList]}, State}
	end;

%% 新增频道记录
%% 特别说明：不允许存在两个相同名字的频道
handle_call({add_channel, TableName, Creator, ChannelName},_From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			%% 查询是否存在相同名字的频道
			case ets:lookup(Ets, ChannelName) of
				[_ExistsRecord|_] -> {reply, {error, exists}, State};
				[] ->
					ets:insert(Ets, #channel{name = ChannelName, creator = Creator, alive = true}),
					{reply, ok, State}
			end
	end;

%% 删除频道记录（将alive标记为false，而不是真正删除）
handle_call({remove_channel, TableName, Owner, ChannelName},_From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			%% 判断是否是频道创建者要删除频道
			case ets:lookup(Ets, ChannelName) of
				[#channel{creator = Creator}] ->
					case Creator =:= Owner of
						true -> 
							%% 将频道标记为已删除，而不是真正删除记录
							ets:insert(Ets, #channel{name = ChannelName, creator = Creator, alive = false}),
							{reply, ok, State};
						false -> {reply, {error, not_creator}, State}
					end;
				[] -> {reply, {error, not_found}, State}
			end
	end;

%% 新增频道用户信息记录
handle_call({add_channel_user, TableName, Member, ChannelName},_From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			ets:insert(Ets, #channel_user{channel_name = ChannelName, user_name = Member}),
			{reply, ok, State}
	end;

%% 删除频道用户信息记录
handle_call({remove_channel_user, TableName, Member, ChannelName},_From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			ets:match_delete(Ets, #channel_user{channel_name = ChannelName, user_name = Member}),
			{reply, ok, State}
	end;


handle_call({query_all_channel_info_with_members, ChannelUserChannelName, ChannelTableName}, _From, State) ->
	#state{tables = Tables} = State,

	#{ets := ChannelEts} = maps:get(ChannelTableName,Tables),
	AllChannelNameListWithCreator = ets:match(ChannelEts, {'_','$1','$2','_'}),
	case maps:get(ChannelUserChannelName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			%% 已加入的频道[[<<"channel of bruce">>],[<<"world">>]]  ,外层是一个列表，列表里的每一个元素还是列表，元素列表里面只有一个元素，就是字符串
			ChannelInfoList = lists:map(
				fun([ChannelName, Creator]) ->
					Members = ets:match(Ets, {'_',ChannelName,'$1'}),
					#{channel_name => ChannelName, members => [Member || [Member] <- Members], creator => Creator} end
				, AllChannelNameListWithCreator),
			{reply, {ok, ChannelInfoList}, State}
	end;


handle_call({query_channel_info_with_members_by_channel, ChannelUserChannelName, ChannelTableName, ChannelName}, _From, State) ->
	#state{tables = Tables} = State,

	#{ets := ChannelEts} = maps:get(ChannelTableName,Tables),
	[ChannelInfo] = ets:lookup(ChannelEts, ChannelName),
	Creator = ChannelInfo#channel.creator,
	case maps:get(ChannelUserChannelName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			%% 已加入的频道[[<<"channel of bruce">>],[<<"world">>]]  ,外层是一个列表，列表里的每一个元素还是列表，元素列表里面只有一个元素，就是字符串
			Members = ets:match(Ets, {'_',ChannelName,'$1'}),
			ChannelInfoWithMembers = #{channel_name => ChannelName, members => [Member || [Member] <- Members], creator => Creator},
			{reply, {ok, ChannelInfoWithMembers}, State}
	end;

handle_call({query_joined_channel_info, TableName, User}, _From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			%% 查询用户加入了哪些频道
			JoinedChannelList = ets:match(Ets, {'_','$1',User}),
			{reply, {ok, [X || [X] <- JoinedChannelList]}, State}
	end;

handle_call({query_channel_map_info, TableName, ChannelName}, _From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName,Tables, undefined) of
		undefined -> {reply, {error, no_such_table}, State};
		#{ets := Ets} ->
			%% 查询指定频道的地图
			case ets:lookup(Ets, ChannelName) of
				[ChannelMap] -> {reply, {ok, ChannelMap}, State};
				[] -> {reply, not_found, State}
			end
	end;


handle_call(stop, _From, State) ->
	#state{tables = Tables} = State,
	io:format("接受到停止请求，开始将ets数据写回dets~n"),
	write_ets_back_to_dets(Tables),
	{stop, normal, stopped, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) ->
	%% 将数据写回Dets
	#state{tables = Tables} = State,
	io:format("接受到停止请求，开始将ets数据写回dets~n"),
	write_ets_back_to_dets(Tables),
	ok.

%% 将ets中的数据写回dets
write_ets_back_to_dets(Tables) ->
	%% 便利Tables映射组将ets数据写回dets
	maps:foreach(
		fun(_TableName, #{ets := Ets, dets := Dets}) ->
			dets:from_ets(Dets,Ets),
			io:format("将~p数据写回dets~n",[_TableName])
		end,
		Tables
	).


