%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 10月 2025 17:55
%%%-------------------------------------------------------------------
-module(channel_manager).
-author("Administrator").
-behavior(gen_server).

-define(WORLD_CHANNEL_NAME, <<"world">>).

%% API
-export([start/0,broadcast_stop/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([query_channel_pid/1, query_channel_pid_batch/1, register_channel/2, revoke_channel/2, query_all/0]).


-include("../../../include/database/chat_database.hrl").

start() ->
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	io:format("创建频道管理者进程[~p]~n",[Pid]),
	{ok,Pid}.
broadcast_stop() ->
	gen_server:call(?MODULE, broadcast_stop).


init([]) ->
	%% 预设此管理进程会挂，那么监控树会拉起，但是并非每次启动都需要去启动频道进程，只有第一次需要
	case ets:whereis(channel_pid_ets) of
		undefined ->
			%% 查询数据库，得到所有的未删除的频道，创建对应的所有频道进程
			{ok, ChannelNameList} = database_queryer:query_all_channel_name_alive(),
			io:format("频道管理者查询得到的未删除的频道：~n"),
			lists:foreach(fun(Channel) -> io:format("--------~ts~n",[Channel]) end, ChannelNameList),
			%% 循环创建频道进程，并将对应关系插入
			ChannelEts = ets:new(channel_pid_ets, [set, named_table, private, {keypos, #channel_pid.channel_name}]),
			lists:foreach(fun(ChannelName) ->
				{ok, Pid} = channel:start(ChannelName),
				ets:insert(ChannelEts, #channel_pid{channel_name = ChannelName, pid = Pid}) end
				, ChannelNameList
			),
			{ok, ChannelEts};
		ChannelEts ->
			{ok, ChannelEts}
	end.


%% ==================================================================================
%% API
query_channel_pid(ChannelName) -> gen_server:call(?MODULE, {query_pid, ChannelName}).
query_channel_pid_batch(ChannelNameList) -> gen_server:call(?MODULE, {query_pid_batch, ChannelNameList}).
register_channel(Creator,ChannelName) -> gen_server:call(?MODULE, {register, Creator, ChannelName}).
revoke_channel(Deleter,ChannelName) -> gen_server:call(?MODULE, {revoke, Deleter, ChannelName}).
query_all() -> gen_server:call(?MODULE, all).





%% ===================================================================================
%% 回调方法
%% 查询pid
handle_call({query_pid,ChannelName}, _From, State) ->
	case ets:lookup(State,ChannelName) of
		[Record] -> {reply, {ok, Record#channel_pid.pid}, State};
		[] -> {reply, {error, not_found}, State}
	end;

%% 批量查询pid
handle_call({query_pid_batch,ChannelNameList}, _From, State) ->

	ChannelPidList = [
		Pid || Name <- ChannelNameList, [#channel_pid{pid = Pid}] <- [ets:lookup(State, Name)]  %% 只匹配查到的
	],
	{reply, {ok, ChannelPidList}, State};

%% 创建频道回调方法
handle_call({register,Creator, ChannelName}, _From, State) ->
	%% 创建频道进程并将关系写入ets内存
	{ok, NewChannelPid} = channel:start(ChannelName),
	ets:insert(State, #channel_pid{channel_name = ChannelName, pid = NewChannelPid}),
	%% 数据库表新增记录
	database_queryer:add_channel_record(Creator,ChannelName),
	%% 通过世界频道进程将新频道信息广播给所有用户进程并通知客户端
	case ets:lookup(State,?WORLD_CHANNEL_NAME) of
		[#channel_pid{pid = WorldPid}] -> WorldPid ! {create_channel, Creator, ChannelName};
		[] -> io:format("警告：世界频道不存在~n")
	end,
	{reply, {ok, NewChannelPid}, State};

%% 管理删除频道的回调方法
handle_call({revoke,Deleter, ChannelName}, _From, State) ->
	%% 先检查数据库权限，只有创建者才能删除
	case database_queryer:remove_channel_record(Deleter, ChannelName) of
		ok ->
			%% 删除频道进程，停止频道
			case ets:lookup(State, ChannelName) of
				[#channel_pid{pid = ChannelPid}] -> 
					ChannelPid ! stop,
					ets:delete(State, ChannelName);
				[] -> io:format("警告：频道进程不存在~n")
			end,
			%% 通过世界频道进程将删除的频道信息广播给所有用户进程并通知客户端
			case ets:lookup(State,?WORLD_CHANNEL_NAME) of
				[#channel_pid{pid = WorldPid}] -> WorldPid ! {delete_channel, Deleter, ChannelName};
				[] -> io:format("警告：世界频道不存在~n")
			end,
			{reply, ok, State};
		{error, Reason} ->
			io:format("删除频道失败: ~p~n", [Reason]),
			{reply, {error, Reason}, State}
	end;

handle_call(all, _From, State) ->
	Records = ets:match(State,'$1'),
	{reply, {ok, [Record || [Record] <- Records]}, State};

handle_call(broadcast_stop, _From, State) ->
	%% 通知所有频道服务关闭消息
	io:format("频道管理者发送服务停止消息给所有频道~n"),
	AllChannelPidList = ets:match(State, {'_','_','$1'}),
%%	spawn(fun() -> async_broadcast_stop(AllChannelPidList) end),
	[Pid ! broadcast_shutdown || [Pid] <- AllChannelPidList],
	{reply,ok,State}.

async_broadcast_stop(PidList) ->
	[Pid ! broadcast_shutdown || [Pid] <- PidList].

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
	io:format("管理者接受到停止请求~n"),
	ok.