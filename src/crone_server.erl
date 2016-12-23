%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone_server).

-behaviour(gen_server).

-ifdef(TEST).
-compile(export_all).
-endif.

%% API
-export([start_link/0,
         update/1,
         check_time/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { crontab :: crone:crontab() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Update the crontab configuration. Will crash if the supplied
%% configuration is invalid.
%% @end
%%--------------------------------------------------------------------
update(CronTab) ->
    true = is_valid(CronTab),
    gen_server:call(?SERVER, {update, CronTab}).


%%--------------------------------------------------------------------
%% @doc
%% Launch any tasks at the specified time.
%% @end
%%--------------------------------------------------------------------
-spec check_time(Time :: crone:time()) -> ok.
check_time(Time) ->
    gen_server:cast(?SERVER, {check_time, Time}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{crontab = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({update, CronTab}, _From, _State) ->
    {reply, ok, #state{crontab = CronTab}};
handle_call(_Request, _From, State) ->
    {reply, unexpected, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({check_time, Time}, #state{crontab = CronTab} = State) ->
    check(Time, CronTab),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the current time against the CronTab entries and start any
%% matching entries.
%% @end
%%--------------------------------------------------------------------
-spec check(Time :: crone:time(), CronTab :: crone:crontab()) -> ok.
check(Time, CronTab) ->
    Tasks = [T || {P, T} <- CronTab, is_match(P, Time)],
    lists:foreach(fun launch/1, Tasks),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the pattern matches the time, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_match(crone:pattern(), crone:time()) -> boolean().
is_match({Pmi, Pho, Pdotm, Pdotw, Pmo}, {Mi, Ho, Dotm, Dotw, Mo, Yr}) ->
    is_part_match(minute, Pmi, Mi) andalso
    is_part_match(hour, Pho, Ho) andalso
    is_part_match({day, Mo, Yr}, Pdotm, Dotm) andalso
    is_part_match({dayofweek, Dotm, Mo, Yr}, Pdotw, Dotw) andalso
    is_part_match(month, Pmo, Mo).

%%--------------------------------------------------------------------
%% @doc
%% Check whether an individual pattern element matches its corresponding
%% value in the time.
%% @end
%%--------------------------------------------------------------------
is_part_match(Part, [Pattern|Rest], Val) ->
    is_part_match(Part, Pattern, Val) orelse is_part_match(Part, Rest, Val);
is_part_match({day, Mo, Yr}, Pattern, Val)
  when is_integer(Pattern) andalso Pattern < 0 ->
    LastDay = calendar:last_day_of_the_month(Yr, Mo),
    Pattern + LastDay + 1 =:= Val;
is_part_match({dayofweek, Day, Mo, Yr}, Pattern, Val)
  when is_integer(Pattern) andalso Pattern < 0 ->
    LastDay = calendar:last_day_of_the_month(Yr, Mo),
    (LastDay - Day) < 7 andalso abs(Pattern) =:= Val;
is_part_match(_, Pattern, Val) ->
    Pattern =:= any orelse Pattern =:= Val.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Launch the action. A temporary process is used to launch the task.
%% @end
%%--------------------------------------------------------------------
-spec launch(Mfa :: crone:task()) -> ok.
launch(Mfa) ->
    supervisor:start_child(crone_task_sup, [Mfa]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check that the crontab is valid.
%% @end
%%--------------------------------------------------------------------
-spec is_valid(CronTab :: crone:crontab()) -> boolean().
is_valid(CronTab) ->
    lists:all(fun is_valid_entry/1, CronTab).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check that the crontab entry is valid.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_entry(Entry :: crone:entry()) -> boolean().
is_valid_entry({{Mi, Ho, Dm, Dw, Mn}, _}) ->
    is_valid_elem(mins, Mi) andalso
    is_valid_elem(hour, Ho) andalso
    is_valid_elem(dotm, Dm) andalso
    is_valid_elem(dotw, Dw) andalso
    is_valid_elem(month, Mn).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check that the element is either a valid value or a list of valid
%% of valid values.
%% @end
%%--------------------------------------------------------------------
is_valid_elem(P, L) when is_list(L) -> lists:all(fun (V) -> is_valid_value(P,V) end, L);
is_valid_elem(P, V) -> is_valid_value(P, V).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check that the value is within a valid range.
%% @end
%%--------------------------------------------------------------------
is_valid_value(_, any) -> true;
is_valid_value(mins, V)  -> V >= 0   andalso V =< 59;
is_valid_value(hour, V)  -> V >= 0   andalso V =< 23;
is_valid_value(dotm, V)  -> V >= -31 andalso V =< 31 andalso V =/= 0;
is_valid_value(dotw, V)  -> V >= -7  andalso V =< 7  andalso V =/= 0;
is_valid_value(month, V) -> V >= 1   andalso V =< 12.
