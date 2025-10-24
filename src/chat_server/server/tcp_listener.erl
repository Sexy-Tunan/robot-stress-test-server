%% ==============================================================
%% 文件: tcp_listener.erl (聊天会话)
%% 聊天服务器tcp连接监听模块
%% ==============================================================
%%
-module(tcp_listener).
-behaviour(gen_server).

%% 服务启停接口
-export([start/0, stop/0]).
%% gen_server 回调接口
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).

-define(LISTEN_PORT, 10088).


start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).


%% =============================
%% gen_server 回调模块
%% 初始化
init([]) ->
	{ok, ListenSocket} = gen_tcp:listen(?LISTEN_PORT, [
		binary,
		{packet, 4},
		{active, once},
		{reuseaddr, true},
		{backlog, 128}
	]),
	io:format("~n[tcp_listemer]进程 正在监听端口~p ...~n", [?LISTEN_PORT]),
	%% 给自己发消息，异步等待连接
	self() ! accept,

	{ok, ListenSocket}.

handle_info(accept, ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("[chat_session] 新连接被接受创建新的chat_session进程处理.~n"),
	%% 为每个连接创建一个独立进程去处理
	Pid = spawn_link(chat_session, start, [Socket]),
	case gen_tcp:controlling_process(Socket, Pid) of
		%% 移交控制权后再通知其开始接受消息
		ok -> Pid ! start;
		{error, Reason} ->
			io:format("控制权转移失败: ~p~n", [Reason]),
			gen_tcp:close(Socket)
	end,

	%% 再次监听下一个连接
	self() ! accept,
	{noreply, ListenSocket};

handle_info(_Msg, State) ->
	{noreply, State}.


handle_call(_Req, _From, State) ->
	{reply, ok, State}.


handle_cast(stop, State) ->
	io:format("[chat_tcp_server] stopped.~n"),
	{stop, normal, State};

handle_cast(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.
%%
%%code_change(_OldVsn, State, _Extra) ->
%%	{ok, State}.