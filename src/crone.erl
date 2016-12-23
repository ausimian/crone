%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%% This module provides the (limited) public API of the application.
%%% @end
%%% Created : 10 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(crone).

-type minute()       :: 0..59.
-type hour()         :: 0..23.
-type day()          :: -31..-1 | 1..31.
-type dayofweek()    :: -7..-1  | 1..7.
-type month()        :: 1..12.
-type year()         :: 1970..10000.

-type pattern() :: {minute()    | list(minute())    | any,
                    hour()      | list(hour())      | any,
                    day()       | list(day())       | any,
                    dayofweek() | list(dayofweek()) | any,
                    month()     | list(month())     | any}.
-type time()    :: {minute(),
                    hour(),
                    day(),
                    dayofweek(),
                    month(),
                    year()}.

-type task()    :: {atom(), atom(), [any()]}.
-type entry()   :: {pattern(), task()}.
-type crontab() :: [entry()].

%% API
-export_type([minute/0,
              hour/0,
              day/0,
              dayofweek/0,
              month/0,
              pattern/0,
              time/0,
              entry/0,
              crontab/0]).

-export([get_crontab/0,
         update/1,
         check_time/1]).

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
    crone_server:update(CronTab).


%%--------------------------------------------------------------------
%% @doc
%% Check the time and launch any pending tasks.
%% @end
%%--------------------------------------------------------------------
-spec check_time(Time :: time()) -> ok.
check_time(Time) ->
    crone_server:check_time(Time).
