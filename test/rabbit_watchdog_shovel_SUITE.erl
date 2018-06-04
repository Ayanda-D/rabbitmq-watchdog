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
%% Copyright (c) 2017-2018 Erlang Solutions, Ltd. All rights reserved.
%%

-module(rabbit_watchdog_shovel_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("rabbit_watchdog.hrl").

-compile(export_all).

-define(SHOVEL,      test_shovel).
-define(EXCHANGE,    <<"test_exchange">>).
-define(TO_SHOVEL,   <<"to_the_shovel">>).
-define(FROM_SHOVEL, <<"from_the_shovel">>).
-define(UNSHOVELLED, <<"unshovelled">>).
-define(SHOVELLED,   <<"shovelled">>).
-define(TIMEOUT,     1000).

all() ->
    [
      {group, non_parallel_tests}
    ].

groups() ->
    [
      {non_parallel_tests, [], [
          watchdog_with_action_application_terminated,
          watchdog_with_action_link_failure,
          watchdog_with_no_action
        ]}
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
        {rmq_nodename_suffix, ?MODULE}
      ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
      rabbit_ct_broker_helpers:setup_steps() ++
      rabbit_ct_client_helpers:setup_steps() ++
      [fun disable_shovel_plugin/1]).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
      rabbit_ct_client_helpers:teardown_steps() ++
      rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase),
    _ = enable_shovel_plugin(Config),
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      application, stop, [rabbitmq_watchdog]),
    Config.

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -----------
%% Test Cases
%% -----------
watchdog_with_action_application_terminated(Config) ->
    ok = setup_watchdog(Config, 0, 200, 40),
    setup_shovels(Config),
    ok = drop_shovel(Config),
    ok = verify_terminated_shovel(Config, ?SHOVEL),
    rabbit_watchdog:delay(500),
    ok = verify_running_shovel(Config),
    passed.

watchdog_with_action_link_failure(Config) ->
    ok = setup_watchdog(Config, 0, 200, 40),
    setup_shovels(Config),
    ok = drop_shovel_link_status(Config, ?SHOVEL),
    ok = verify_dropped_shovel_link(Config),
    rabbit_watchdog:delay(300),
    ok = verify_running_shovel(Config),
    ok = await_running_shovel(Config, ?SHOVEL),
    passed.

watchdog_with_no_action(Config) ->
    ok = setup_watchdog(Config, 0, 200, 40),
    setup_shovels(Config),
    rabbit_watchdog:delay(300),
    ok = await_running_shovel(Config, ?SHOVEL),
    passed.

%% ---------
%% Internal
%% ---------
setup_watchdog(Config, Node, WD_Interval, Delay) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, Node,
            ?MODULE, init_shovel_watchdog_remote, [WD_Interval, Delay]).

init_shovel_watchdog_remote(WD_Interval, Delay) ->
    application:set_env(rabbitmq_watchdog, watchdogs,
        [{"Shovel WD", rabbit_watchdog_shovel, WD_Interval, [{delay, Delay}]}]),
    ok = application:start(rabbitmq_watchdog).

setup_shovels(Config) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, setup_shovels1, [Config]).

setup_shovels1(Config) ->
    _ = application:stop(rabbitmq_shovel),
    Hostname = ?config(rmq_hostname, Config),
    TcpPort = rabbit_ct_broker_helpers:get_node_config(Config, 0,
        tcp_port_amqp),
    application:set_env(
      rabbitmq_shovel,
      shovels,
      [{test_shovel,
        [{sources,
          [{broker, rabbit_misc:format("amqp://~s:~b/%2f?heartbeat=5",
                                       [Hostname, TcpPort])},
           {declarations,
            [{'queue.declare',    [exclusive, auto_delete]},
             {'exchange.declare', [{exchange, ?EXCHANGE}, auto_delete]},
             {'queue.bind',       [{queue, <<>>}, {exchange, ?EXCHANGE},
                                   {routing_key, ?TO_SHOVEL}]}
            ]}]},
         {destinations,
          [{broker, rabbit_misc:format("amqp://~s:~b/%2f",
                                       [Hostname, TcpPort])}]},
         {queue, <<>>},
         {ack_mode, on_confirm},
         {publish_fields, [{exchange, ?EXCHANGE}, {routing_key, ?FROM_SHOVEL}]},
         {publish_properties, [{delivery_mode, 2},
                               {cluster_id,    <<"my-cluster">>},
                               {content_type,  ?SHOVELLED}]},
         {add_forward_headers, true}
        ]}],
      infinity),

    ok = application:start(rabbitmq_shovel),
    await_running_shovel1(?SHOVEL).

await_running_shovel(Config, Name) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, await_running_shovel1, [Name]).

await_running_shovel1(Name) ->
    case [N || {N, _, {running, _}, _}
                      <- rabbit_shovel_status:status(),
                         N =:= Name] of
        [_] -> ok;
        _   -> timer:sleep(100),
             await_running_shovel1(Name)
    end.

verify_terminated_shovel(Config, Name) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, verify_terminated_shovel1, [Name]).

verify_terminated_shovel1(Name) ->
    undefined = whereis(Name),
    undefined = whereis(rabbit_shovel_sup),
    undefined = whereis(rabbit_shovel_status),
    ok.

verify_running_shovel(Config) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, verify_running_shovel1, []).

verify_running_shovel1() ->
    case [A || {A = rabbitmq_shovel, _, _} <- application:which_applications()] of
        [_] -> ok;
        _   -> throw({error, {not_running, rabbitmq_shovel}})
    end.

verify_running_shovel_link(Config, Name) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, verify_running_shovel1, [Name]).

verify_running_shovel_link1(Name) ->
    case [N || {N, _, {running, _}, _}
                      <- rabbit_shovel_status:status(),
                         N =:= Name] of
        [_] -> ok;
        _   ->
            throw(?WATCHDOG_ERR(?MODULE, verify_running_shovel_link1,
                {Name, not_running}))
    end.

drop_shovel(Config) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      application, stop, [rabbitmq_shovel]).

drop_shovel_link_status(Config, Name) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      rabbit_shovel_status, remove, [Name]).

verify_dropped_shovel_link(Config) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, verify_dropped_shovel_link1, []).

verify_dropped_shovel_link1() ->
    [] = rabbit_shovel_status:status(),
    ok.

enable_shovel_plugin(Config) ->
    _ = rabbit_ct_broker_helpers:enable_plugin(Config, 0, rabbitmq_shovel),
    Config.

disable_shovel_plugin(Config) ->
    _ = rabbit_ct_broker_helpers:disable_plugin(Config, 0, rabbitmq_shovel),
    Config.
