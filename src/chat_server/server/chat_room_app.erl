%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%		application进程
%%% @end
%%% Created : 2025 14:21
%%%-------------------------------------------------------------------
-module(chat_room_app).
-author("caigou").
-behavior(application).
%% API
-export([start/2]).
-export([stop/1,prep_stop/1]).

start(_Type, _Args) ->
	chat_room_sup:start_link().


prep_stop(State) ->
	channel_manager:broadcast_stop(),
	State.

stop(_State) ->
	ok.

