-module(varint_test).

-include_lib("eunit/include/eunit.hrl").


integer_test() ->
    % Re1 = erlav_nif:int_decode(<<180,141,172,122,2,226,18,228,40>>),
    %Re1 = varint_nif:int_decode(<<218,134,150,61,1,177,9,178,20>>),
    Re1 = varint_nif:fcap_decode(<<218,134,150,61,1,177,9,178,20>>),
    ?debugFmt("decode result: ~p ~n", [Re1]),
    % expected [128287578,1,1201,2610]
    
    %Re2 = varint_nif:int_encode([128287578,1,1201,2610]),
    Re2 = varint_nif:fcap_encode([1706124378, 1706124379, 1706125580, 1706128190]),
    ?debugFmt("encode result: ~p ~n", [Re2]),

    Re3 = varint_nif:fcap_decode(Re2),
    ?debugFmt("decode encoded result: ~p ~n", [Re3]),
    ok.

perf_test() ->
    T1 = os:system_time(microsecond),
    decode_fcap_times(1000000),
    Total = os:system_time(microsecond) - T1,
    ?debugFmt("decode time: ~p ~n", [Total]).

decode_fcap_times(0) -> ok;
decode_fcap_times(N) ->
    varint_nif:fcap_decode(<<218,199,125,1,177,9,178,20,144,78>>),
    decode_fcap_times(N-1).

random_fcap_enc_dec_test() ->
    Fun = fun() ->
        {_, FcapLst} = lists:foldl(fun(_, {A, Ret}) -> 
            A1 = rand:uniform(1000) + A,
            {A1, [A1 | Ret]}
        end, {1706124378, []}, lists:seq(1,15)),
        FcapLstSorted = lists:sort(FcapLst),
        %?debugFmt("test list: ~p ~n", [FcapLstSorted]),
        Enc = varint_nif:fcap_encode(FcapLstSorted),
        %?debugFmt("Enc: ~p ~n", [Enc]),
        Dec = varint_nif:fcap_decode(Enc),
        %?debugFmt("Dec: ~p ~n", [Dec]),
        ?assert(Dec =:= FcapLstSorted),
        ?debugFmt("Size: ~p,  lst->enc->dec =:= lst OK ", [size(Enc)])
    end,
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, 10000)).
