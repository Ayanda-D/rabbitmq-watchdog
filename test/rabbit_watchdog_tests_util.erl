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

-module(rabbit_watchdog_tests_util).

-include_lib("common_test/include/ct.hrl").
-include_lib("rabbit_watchdog.hrl").

-define(WATCHDOG_DEFAULT_INTERVAL, 200).
-define(LOW_LIMIT_WATERMARK, 0.0001).

-compile(export_all).

trigger_memory_alarm(Config) ->
    trigger_memory_alarm(Config, ?WATCHDOG_DEFAULT_INTERVAL).

trigger_memory_alarm(Config, Delay) ->
    OriginalVmMemHighWatermark = get_vm_memory_high_watermark(Config),
    set_vm_memory_high_watermark(Config, ?LOW_LIMIT_WATERMARK),
    rabbit_watchdog:delay(Delay * 2),
    set_vm_memory_high_watermark(Config, OriginalVmMemHighWatermark),
    rabbit_watchdog:delay(Delay * 2),
    ok.

get_vm_memory_high_watermark(Config) ->
    rabbit_ct_broker_helpers:rpc(Config, 0,
      vm_memory_monitor, get_vm_memory_high_watermark, []).

set_vm_memory_high_watermark(Config, Percentage) ->
    rabbit_ct_broker_helpers:rpc(Config, 0,
      vm_memory_monitor, set_vm_memory_high_watermark, [Percentage]).

trigger_disk_free_alarm(Config) ->
    trigger_disk_free_alarm(Config, ?WATCHDOG_DEFAULT_INTERVAL).

trigger_disk_free_alarm(Config, Delay) ->
    OrigDiskFreeLimit = get_disk_free_limit(Config),
    DiskFree = disk_free(Config),

    ok = set_disk_free_limit(Config, io_lib:format("~w", [100 * DiskFree])),
    rabbit_watchdog:delay(Delay * 2),
    set_disk_free_limit(Config, OrigDiskFreeLimit).

set_disk_free_limit(Config, Limit) ->
    rabbit_ct_broker_helpers:rpc(
      Config, 0, rabbit_disk_monitor, set_disk_free_limit, [Limit]).

get_disk_free_limit(Config) ->
  rabbit_ct_broker_helpers:rpc(
    Config, 0, rabbit_disk_monitor, get_disk_free_limit, []).

disk_free(Config) ->
    RabbitStatus = rabbit_status(Config),
    rabbit_misc:pget(disk_free, RabbitStatus).

rabbit_status(Config) ->
    rabbit_ct_broker_helpers:rpc(Config, 0, rabbit, status, []).
