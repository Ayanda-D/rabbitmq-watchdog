PROJECT = rabbitmq_watchdog
PROJECT_DESCRIPTION = RabbitMQ Watchdog
PROJECT_MOD = rabbit_watchdog_app

define PROJECT_ENV
[
  {watchdogs,
     [  %% {Title, Implementation/Callback, WatchdogInterval}
	    {"Rabbit Watchdog", rabbit_watchdog_rabbit, 5000, [] },
	    {"Shovel Watchdog", rabbit_watchdog_shovel, 5000, [{delay, 100}] },
        {"Management Watchdog", rabbit_watchdog_management, 60000, []}
	  ]
	}]
endef

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, []}
endef

DEPS = rabbit_common rabbit amqp_client amqp10_client
dep_amqp10_client = git https://github.com/rabbitmq/rabbitmq-amqp1.0-client.git master

LOCAL_DEPS = crypto

TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers rabbitmq_amqp1_0 rabbitmq_shovel

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
