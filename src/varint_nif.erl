-module(varint_nif).

-export([
    int_decode/1,
    int_encode/1,
    fcap_encode/1,
    fcap_decode/1,
    get_offset/0,
    perf/1, perf/0
]).

-nifs([int_encode/1, int_decode/1, fcap_decode/1, fcap_encode/1, get_offset/0]).

-on_load(init/0).

-define(APPNAME, varint_nif).
-define(LIBNAME, varint_nif).

init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).


int_encode(_A) ->
    not_loaded(?LINE).

int_decode(_A) ->
    not_loaded(?LINE).

fcap_encode(_A) ->
    not_loaded(?LINE).

fcap_decode(_A) ->
    not_loaded(?LINE).

get_offset() ->
    not_loaded(?LINE).

perf() -> perf(10000000).
perf(N) ->
    T1 = os:system_time(microsecond),
    decode_fcap_times(N),
    Total = os:system_time(microsecond) - T1,
    io:format("decode time: ~p ~n", [Total]),
    io:format("single call: ~p micros ~n", [Total / N]).

decode_fcap_times(0) -> ok;
decode_fcap_times(N) ->
    varint_nif:fcap_decode(<<218,199,125,1,177,9,178,20,144,78>>),
    decode_fcap_times(N-1).
