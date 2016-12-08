%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone_timer).

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

-define(INTERVAL, 1000).

-type second()  :: calendar:minute() | 'any'.
-type minute()  :: calendar:minute() | 'any'.
-type hour()    :: calendar:hour()   | 'any'.
-type day()     :: calendar:day()    | 'any'.
-type month()   :: calendar:month()  | 'any'.

-type time_pattern() :: {second(), minute(), hour(), day(), month()}.
-type cron_entry()   :: {time_pattern(), mfa()}.

-record(state, { crontab :: [cron_entry()] }).

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
start_link(CronTab) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, CronTab, []).

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
init(CronTab) ->
    process_flag(trap_exit, true),
    case is_valid(CronTab) of
        true  -> {ok, #state{ crontab = CronTab }, ?INTERVAL};
        false -> {stop, invalid}
    end.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
    {noreply, State, ?INTERVAL};
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
check(CronTab) ->
    Now = erlang:timestamp(),
    {{_, Mo, Day}, {Ho, Mi, Se}} = calendar:now_to_local_time(Now),
    check(CronTab, {Se, Mi, Ho, Day, Mo}).

check([], _) ->
    ok;
check([{Pattern, Mfa}|Rest], Time) ->
    case is_match(Pattern, Time) of
        true  -> on_match(Pattern, Mfa);
        false -> ok
    end,
    check(Rest, Time).

is_match({Pse, Pmi, Pho, Pday, Pmo}, {Se, Mi, Ho, Day, Mo}) ->
    is_match(Pse, Se)   andalso
    is_match(Pmi, Mi)   andalso
    is_match(Pho, Ho)   andalso
    is_match(Pday, Day) andalso
    is_match(Pmo, Mo);
is_match([Pattern|Rest], Val) ->
    is_match(Pattern, Val) orelse is_match(Rest, Val);
is_match(Pattern, Val) ->
    Pattern =:= any orelse Pattern =:= Val.

on_match(_Pattern, Action) ->
    supervisor:start_child(crone_launcher_sup, [Action]).

is_valid([]) ->
    true;
is_valid([{{Se,Mi,Hr,Day,Mo},_}|Rest]) ->
    is_valid_second(Se) andalso
    is_valid_minute(Mi) andalso
    is_valid_hour(Hr)   andalso
    is_valid_day(Day)   andalso
    is_valid_month(Mo)  andalso
    is_valid(Rest).

is_valid_second(V) -> is_valid_part(V, 0, 59).
is_valid_minute(V) -> is_valid_part(V, 0, 59).
is_valid_hour(V)   -> is_valid_part(V, 1, 12).
is_valid_day(V)    -> is_valid_part(V, 1, 31).
is_valid_month(V)  -> is_valid_part(V, 1, 12).

is_valid_part([Part|Rest], Min, Max) ->
    is_valid_part(Part, Min, Max) orelse is_valid_part(Rest, Min, Max);
is_valid_part([], _, _) ->
    false;
is_valid_part(Part, Min, Max) ->
    Part =:= any orelse (Part >= Min andalso Part =< Max).
