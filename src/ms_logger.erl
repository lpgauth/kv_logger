-module(ms_logger).
-include("ms_logger.hrl").

-export([
    log/2
]).

-define(SAMPLE_RATE, 1).

%% public
-spec log(erlang:timestamp() | binary(), binary()) -> ok.

log({_, _, _} = Timestamp, Bin) ->
    Name = name(Timestamp),
    log(Name, Bin);
log(Name, Bin) ->
    Bin2 = <<Bin/binary, "\n">>,
    case fast_disk_log:log(Name, Bin2) of
        ok ->
            ms_base_metric:increment(<<"ms_logger.ok">>, 1, ?SAMPLE_RATE),
            ok;
        {error, no_such_log} ->
            case open(Name) of
                ok ->
                    ms_base_metric:increment(<<"ms_logger.ok">>, 1, ?SAMPLE_RATE),
                    fast_disk_log:log(Name, Bin2);
                {error, Reason} ->
                    ms_base_metric:increment(<<"ms_logger.error">>, 1, ?SAMPLE_RATE),
                    lager:error("ms_logger open error: ~p~n", [Reason]),
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
