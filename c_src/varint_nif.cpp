#include <cstring>
#include <fstream>
#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>

#include <erl_nif.h>



int64_t decodeVarint(uint8_t*& it) {
    uint64_t encoded = 0;
    int shift = 0;
    uint8_t u;

    do {
        if (shift >= 64) {
            throw std::invalid_argument("Invalid Avro varint");
        }
        u = *it;
        ++it;
        encoded |= static_cast<uint64_t>(u & 0x7f) << shift;
        shift += 7;
    } while (u & 0x80);

    return encoded;
}

size_t
encodeVarint(int64_t val, std::vector<uint8_t>& ret) noexcept {
    const int mask = 0x7F;
    auto v = val & mask;
    while (val >>= 7) {
        ret.push_back( (v| 0x80) );
        v = val & mask;
    }

    ret.push_back(v);
    return 1;
}

ERL_NIF_TERM
int_encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary retbin;
    std::vector<uint8_t> retv;
    unsigned int len;
    long i64;
    ERL_NIF_TERM elem, list;
    retv.reserve(100);

    list = argv[0];
    if (!enif_is_list(env, list)) {
        return enif_make_badarg(env);
    }
    enif_get_list_length(env, list, &len);
    for (uint32_t i = 0; i < len; i++) {
        if (enif_get_list_cell(env, list, &elem, &list)) {
            enif_get_int64(env, elem, &i64);
            encodeVarint(i64, retv);
        }
    }


    auto retlen = retv.size();
    enif_alloc_binary(retlen, &retbin);
    memcpy(retbin.data, retv.data(), retlen);
    return enif_make_binary(env, &retbin);
}

ERL_NIF_TERM
int_decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary sbin;
    std::vector<ERL_NIF_TERM> retv;

    if (!enif_inspect_binary(env, argv[0], &sbin)) {
        return enif_make_badarg(env);
    }
    uint8_t* p = sbin.data;
    uint8_t* end = sbin.data + sbin.size;
    while ( p < end ){
        uint64_t ri = decodeVarint(p);
        retv.push_back(enif_make_int64(env, ri));
    }

    return enif_make_list_from_array(env, retv.data(), retv.size());
}

ErlNifFunc nif_funcs[] = {{"int_encode", 1, int_encode_nif},
                          {"int_decode", 1, int_decode_nif}};

ERL_NIF_INIT(varint_nif, nif_funcs, nullptr, nullptr, nullptr, nullptr);
