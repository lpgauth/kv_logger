-module(ms_logger_test).
-include_lib("eunit/include/eunit.hrl").

-define(LOG_FILE, "../.eunit/log/2015-09-03-09.log").
-define(TIMESTAMP, {1441, 271599, 757400}).

-spec test() -> ok.

%% runners
-spec ms_logger_test_() -> ok.

ms_logger_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
        fun log/0
    }.

%% tests
log() ->
    ok = ms_logger:log(?TIMESTAMP, <<"test">>),
    ok = ms_logger:log(?TIMESTAMP, <<"test">>),

    fast_disk_log:sync(<<"2015-09-03-09">>),
    timer:sleep(500),

    {ok, Log} = file:read_file(?LOG_FILE),
    ?assertEqual(<<"test\ntest\n">>, Log).

%% utils
cleanup() ->
    ms_logger_app:stop().

setup() ->
    file:delete(?LOG_FILE),
    error_logger:tty(false),
    application:load(lager),
    application:set_env(lager, error_logger_redirect, false),
    application:load(fast_disk_log),
    application:set_env(fast_disk_log, max_size, 1),
    ms_logger_app:start().
