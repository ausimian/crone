%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%% This module provides the (limited) public API of the application.
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
%% Return the value held in the crontab environment variable
%% of the crone application configuration.
%% If no such value is specified, it defaults to the empty list.
%% @end
%%--------------------------------------------------------------------
-spec get_crontab() -> {ok, term()}.
get_crontab() ->
    CronTab = application:get_env(crone, crontab, []),
    {ok, CronTab}.

%%--------------------------------------------------------------------
%% @doc
%% Update the active crontab configuration. The new configuration
%% replaces the old one entirely.
%% @end
%%--------------------------------------------------------------------
-spec update(CronTab :: crontab()) -> ok.
update(CronTab) ->
    crone_scheduler:update(CronTab).
