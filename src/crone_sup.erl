%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy  => rest_for_one,
                 intensity => 1,
                 period    => 5},

    TaskSup = #{id       => crone_task_sup,
                start    => {crone_task_sup, start_link, []},
                restart  => permanent,
                shutdown => infinity,
                type     => supervisor},

    Server  = #{id       => crone_server,
                start    => {crone_server, start_link, []},
                restart  => permanent,
                shutdown => 5000,
                type     => worker},

    Timer   = #{id       => crone_timer,
                start    => {crone_timer, start_link, []},
                restart  => permanent,
                shutdown => 5000,
                type     => worker},

    {ok, {SupFlags, [TaskSup, Server, Timer]}}.
