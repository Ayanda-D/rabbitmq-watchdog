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

-define(DEFAULT_DELAY,      5000).

-define(WATCHDOG_ERR(M, F, Reason), {error, {M, F, Reason}}).

-define(MSG_PREFIX,         rabbit_data_coercion:to_list(?MODULE)).
-define(ERR_MSG(MSG, A),    error_logger:error_msg(?MSG_PREFIX ++ ": " ++ MSG, A)).
-define(INFO_MSG(MSG, A),   error_logger:info_msg(?MSG_PREFIX ++ ": " ++ MSG, A)).
-define(WARN_MSG(MSG, A),   error_logger:warning_msg(?MSG_PREFIX ++ ": " ++ MSG, A)).
-define(ERR_MSG(MSG),       ?ERR_MSG(MSG,  [])).
-define(INFO_MSG(MSG),      ?INFO_MSG(MSG, [])).
-define(WARN_MSG(MSG),      ?WARN_MSG(MSG, [])).
