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

-module(rabbit_watchdog_rabbit).

-behaviour(rabbit_watchdog).

-include_lib("rabbit_watchdog.hrl").

-export([init/1, validate/1, action/1, terminate/1]).

-record(rbbt_wd_state,
            {application = rabbit,
             udata       = [] }).

% ----------
% Callbacks
% ----------
init(UData) ->
    {ok, #rbbt_wd_state{application = verify_rabbit(), udata = UData}}.

validate(State = #rbbt_wd_state{application = undefined}) ->
    %% Always attempt to apply action if rabbit application is found to be down!
    {noaction, State};
validate(State) ->
    case verify_rabbit() of
        rabbit ->
            {case rabbit_health_check:node(node()) of
                 ok                    -> noaction;
                 {error_string, Error} ->
                     ?WARN_MSG("health check failed, " ++ Error),
                     %% Consider applying an action
                     noaction
             end, State};
        undefined ->
            %% Rabbit may have terminated during runtime..
            %% Strongly consider applying action
            {noaction, State}
    end.

 action(State = #rbbt_wd_state{application = Application, udata = UData}) ->
     rabbit_watchdog:restart_app(Application,
         rabbit_misc:pget(delay, UData, ?DEFAULT_DELAY)),
    {ok, State}.

terminate(_State) ->
    ok.

% --------
% Private
% --------
verify_rabbit() ->
    case [A || {A = rabbit, _, _} <- application:which_applications()] of
        [_] -> rabbit;
        _   -> undefined
    end.
