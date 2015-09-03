-module(ms_logger).
-include("ms_logger.hrl").

-export([
    log/2
]).

%% public
-spec log(erlang:timestamp(), binary()) -> ok.

log(Timestamp, Bin) ->
    Name = name(Timestamp),
    Bin2 = <<Bin/binary, "\n">>,

    case fast_disk_log:log(Name, Bin2) of
        ok ->
            lager:error("DEBUG: ok", []),
            ok;
        {error, no_such_log} ->
            case open(Name) of
                ok ->
                    fast_disk_log:log(Name, Bin2),
                    ok;
                {error, Reason} ->
                    lager:error("kv_logger open error: ~p~n", [Reason]),
                    ok
            end
    end.

%% private
name(Timestamp) ->
    {{Y, M, D}, {H, _M, _S}} = calendar:now_to_universal_time(Timestamp),
    <<(zero_pad(Y))/binary, "-", (zero_pad(M))/binary, "-",
        (zero_pad(D))/binary, "-", (zero_pad(H))/binary>>.

open(Logger) ->
    Filename = path() ++ binary_to_list(<<"/", Logger/binary, ".log">>),
    fast_disk_log:open(Logger, Filename, [{auto_close, true}]).

path() ->
    application:get_env(?APP, log_path, ?DEFAULT_LOG_PATH).

zero_pad(Integer) when Integer < 10 ->
    <<"0", (integer_to_binary(Integer))/binary>>;
zero_pad(Integer) ->
    integer_to_binary(Integer).
