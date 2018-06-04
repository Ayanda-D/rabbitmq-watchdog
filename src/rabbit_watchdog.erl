%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Watchdog.
%%
%% The Developer of this component is Erlang Solutions, Ltd.
%% Copyright (c) 2017-2018 Erlang Solutions, Ltd.  All rights reserved.
%%

-module(rabbit_watchdog).

-include_lib("rabbit_watchdog.hrl").

-export([info/1, stop/1]).

-export([delay/1, get_env/2, restart_app/1,  restart_app/2,
         restart_app_with_unload/1, restart_app_with_unload/2]).

-callback init(term())      -> {ok, term()}.
-callback validate(term())  -> {action, term()} | {noaction, term()}.
-callback action(term())    -> {ok, term()}.
-callback terminate(term()) ->  ok.

info(Name) ->
    rabbit_watchdog_proc:info(Name).

stop(Name) ->
    rabbit_watchdog_proc:info(Name).

delay(T) when is_integer(T) ->
    timer:sleep(T).

get_env(Env, Default) ->
    rabbit_misc:get_env(rabbitmq_watchdog, Env, Default).

restart_app(App) ->
    restart_app(App, ?DEFAULT_DELAY).

restart_app(App, Delay) ->
    application:stop(App),
    timer:sleep(Delay),
    application:start(App).

restart_app_with_unload(App) ->
    restart_app_with_unload(App, ?DEFAULT_DELAY).

restart_app_with_unload(App, Delay) ->
    application:stop(App),
    timer:sleep(Delay),
    application:unload(App),
    application:start(App).
