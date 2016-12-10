%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone_launcher).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(Action :: mfa()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Action) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Action, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Action) ->
    gen_server:cast(self(), {apply, Action}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {stop, unexpected, State}.

handle_cast({apply, {M, F, A}}, State) ->
    apply(M, F, A),
    {stop, normal, State}.

handle_info(_Info, State) ->
    {stop, unexpected, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
