%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone).

-type time()    :: {calender:minute(),
                    calendar:hour(),
                    calendar:day(),
                    calendar:month()}.
-type minute()  :: calendar:minute() | 'any'.
-type hour()    :: calendar:hour()   | 'any'.
-type day()     :: calendar:day()    | 'any'.
-type month()   :: calendar:month()  | 'any'.
-type time_pattern() :: {minute(), hour(), day(), month()}.
-type cron_entry()   :: {time_pattern(), mfa()}.
-type crontab()      :: [cron_entry()].

%% API
-export_type([crontab/0, time/0]).
-export([get_crontab/0,
         update/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Return the configuration held in the crontab environment variable
%% within the crone application.
%% @end
%%--------------------------------------------------------------------
-spec get_crontab() -> {ok, term()}.
get_crontab() ->
    CronTab = application:get_env(crone, crontab, []),
    {ok, CronTab}.

%%--------------------------------------------------------------------
%% @doc
%% Update the active crontab configuration.
%% @end
%%--------------------------------------------------------------------
-spec update(CronTab :: crontab()) -> ok.
update(CronTab) ->
    crone_scheduler:update(CronTab).
