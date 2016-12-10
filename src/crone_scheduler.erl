%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone_scheduler).

-behaviour(gen_server).

%% API
-export([start_link/0,
         update/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(INTERVAL, 60000).

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
%% Update the crontab configuration.
%% @end
%%--------------------------------------------------------------------
-spec update(CronTab :: crone:crontab()) -> ok.
update(CronTab) ->
    case is_valid_crontab(CronTab) of
        true  -> gen_server:call(?SERVER, {update, CronTab});
        false -> {error, invalid}
    end.


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
    timer:send_interval(?INTERVAL, timeout),
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
handle_info(timeout, #state{crontab = CronTab} = State) ->
    check(CronTab),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State, ?INTERVAL}.

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
-spec check(CronTab :: crone:crontab()) -> ok.
check(CronTab) ->
    Now = erlang:timestamp(),
    {{_, Mo, Day}, {Ho, Mi, _}} = calendar:now_to_local_time(Now),
    check(CronTab, {Mi, Ho, Day, Mo}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tail-recursive implementation of check/1.
%% @end
%%--------------------------------------------------------------------
-spec check(CronTab :: crone:crontab(), Time :: crone:time()) -> ok.
check([], _) ->
    ok;
check([{Pattern, Mfa}|Rest], Time) ->
    case is_match(Pattern, Time) of
        true  -> on_match(Pattern, Mfa);
        false -> ok
    end,
    check(Rest, Time).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the pattern matches the time, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_match(crone:time_pattern(), crone:time()) -> boolean().
is_match({Pmi, Pho, Pday, Pmo}, {Mi, Ho, Day, Mo}) ->
    is_part_match(Pmi, Mi)   andalso
    is_part_match(Pho, Ho)   andalso
    is_part_match(Pday, Day) andalso
    is_part_match(Pmo, Mo).
is_part_match([Pattern|Rest], Val) ->
    is_part_match(Pattern, Val) orelse is_part_match(Rest, Val);
is_part_match(Pattern, Val) ->
    Pattern =:= any orelse Pattern =:= Val.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Launch the action.
%% @end
%%--------------------------------------------------------------------
-spec on_match(crone:time_pattern(), mfa()) -> ok.
on_match(_Pattern, Action) ->
    {ok, _} = supervisor:start_child(crone_launcher_sup, [Action]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% True if the whole list of crontab entries is valud, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_crontab(crone:crontab()) -> boolean().
is_valid_crontab(CronTab) ->
    lists:all(fun is_valid_entry/1, CronTab).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% True of the crontab entry is valid, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_entry({crone:time_pattern(), mfa()}) -> boolean().
is_valid_entry({Pattern, Action}) ->
    is_valid_pattern(Pattern) andalso is_valid_action(Action);
is_valid_entry(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% True if the time pattern is valid, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_pattern(crone:time_pattern()) -> boolean().
is_valid_pattern({Mi,Hr,Day,Mo}) ->
    is_valid_part(Mi, 0, 59) andalso
    is_valid_part(Hr, 0, 23) andalso
    is_valid_part(Day, 1, 7) andalso
    is_valid_part(Mo, 1, 12);
is_valid_pattern(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% True if the pattern part is valid, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_part(any | integer(), integer(), integer()) -> boolean().
is_valid_part(Val, Min, Max)
  when is_integer(Min) andalso is_integer(Max) ->
    Val =:= any orelse
   (is_integer(Val) andalso Val >= Min andalso Val =< Max);
is_valid_part(_, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% True if the action is a valid MFA, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_valid_action(mfa()) -> boolean().
is_valid_action({M, F, A}) ->
  is_atom(M) andalso is_atom(F) andalso is_list(A);
is_valid_action(_) ->
    false.
