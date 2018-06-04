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

-module(rabbit_watchdog_management).

-behaviour(rabbit_watchdog).

-include_lib("rabbit_watchdog.hrl").

-export([init/1, validate/1, action/1, terminate/1]).

-record(mgmt_wd_state,
            {application = rabbitmq_management,
             udata       = [] }).

% ----------
% Callbacks
% ----------
init(UData) ->
    {ok, #mgmt_wd_state{udata = UData}}.

validate(State) ->
    {case rabbit_misc:pget(alarms, rabbit:status()) of
         []     -> noaction;
         Alarms when is_list(Alarms) ->
             IsResLimit = is_resource_limit(Alarms),
             if IsResLimit ->
                    ?ERR_MSG("resource limit alarm detected"),
                    action;
                true -> noaction
             end
     end, State}.

action(State = #mgmt_wd_state{application = Application}) ->
    rabbit_watchdog:restart_app_with_unload(Application),
    {ok, State}.

terminate(_State) ->
    ok.

% --------
% Private
% --------
is_resource_limit([]) -> false;
is_resource_limit([{{resource_limit, _Source, _Node}, _Info} | _Rem]) -> true;
is_resource_limit([_|Rem]) -> is_resource_limit(Rem).
