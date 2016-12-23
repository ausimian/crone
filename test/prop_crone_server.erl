%%%-------------------------------------------------------------------
%%% @author Nick Gunn <nick@ausimian.net>
%%% @copyright (C) 2016, Nick Gunn
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2016 by Nick Gunn <nick@ausimian.net>
%%%-------------------------------------------------------------------
-module(prop_crone_server).

-include_lib("proper/include/proper.hrl").

prop_valid_crontab() ->
    ?FORALL(CronTab, crone:crontab(),
            begin
                crone_server:is_valid(CronTab)
            end).

prop_exact_match() ->
    ?FORALL({Min,Hour,Dotm,Month,Year}, {crone:minute(), crone:hour(), range(1,7), crone:month(), range(1970, 10000)},
            begin
                crone_server:is_match({Min,Hour,Dotm,any,Month}, {Min,Hour,Dotm,any,Month,Year})
            end).

prop_wildcard_match() ->
    ?FORALL({Min,Hour,Dotm,Month,Year}, {crone:minute(), crone:hour(), range(1,7), crone:month(), range(1970, 10000)},
            begin
                crone_server:is_match({any,Hour,Dotm,any,Month}, {Min,Hour,Dotm,any,Month,Year}) andalso
                crone_server:is_match({Min,any,Dotm,any,Month}, {Min,Hour,Dotm,any,Month,Year}) andalso
                crone_server:is_match({Min,Hour,any,any,Month}, {Min,Hour,Dotm,any,Month,Year}) andalso
                crone_server:is_match({Min,Hour,Dotm,any,any}, {Min,Hour,Dotm,any,Month,Year})
            end).
