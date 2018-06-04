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

-module(rabbit_watchdog_shovel).

-behaviour(rabbit_watchdog).

-include_lib("rabbit_watchdog.hrl").

-export([init/1, validate/1, action/1, terminate/1]).

-record(shovel_wd_state,
            {application = rabbitmq_shovel,
             udata       = [] }).

% ----------
% Callbacks
% ----------
init(UData) ->
    {ok, #shovel_wd_state{application = verify_shovel(), udata = UData}}.

validate(State = #shovel_wd_state{application = undefined}) -> {noaction, State};
validate(State = #shovel_wd_state{application = Application}) ->
    case verify_shovel() of
        rabbitmq_shovel ->
            StaticConfiguredShovels  = rabbit_misc:get_env(Application, shovels, []),
            DynamicConfiguredShovels =
                rabbit_runtime_parameters:list_component(<<"shovel">>),
            NConfiguredShovels = length(StaticConfiguredShovels) +
                                 length(DynamicConfiguredShovels),
            ActiveShovels = rabbit_shovel_status:status(),

            %% TODO: Match shovel name as well
            RunningActiveShovels =
                [ Name || {Name, _Type, {running, _Info}, _TS} <- ActiveShovels],
            {if length(RunningActiveShovels) =:= NConfiguredShovels -> action;
                true -> action
            end, State};
        undefined ->
            %% Shovel application could've terminated during runtime.
            %% Apply action regardless
            {action, State}
    end.

action(State = #shovel_wd_state{application = Application, udata = UData}) ->
    rabbit_watchdog:restart_app(Application,
        rabbit_misc:pget(delay, UData, ?DEFAULT_DELAY)),
    {ok, State}.

terminate(_State) ->
    ok.

% --------
% Private
% --------
verify_shovel() ->
    case [A || {A = rabbitmq_shovel, _, _} <- application:which_applications()] of
        [_] -> rabbitmq_shovel;
        _   -> undefined
    end.
