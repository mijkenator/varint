-module(varint_nif).

-export([
    int_decode/1,
    int_encode/1
]).

-nifs([int_encode/1, int_decode/1]).

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

