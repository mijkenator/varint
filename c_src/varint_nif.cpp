#include <cstring>
#include <fstream>
#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>

#include <chrono>
#include <ctime>

#include <erl_nif.h>

typedef std::chrono::system_clock Clock;
const uint32_t TAYear = 2024 - 1900;
const uint8_t TAMonth = 0;
const uint64_t TA = 1704067200;
ErlNifBinary sbin_glob;

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
    std::vector<ERL_NIF_TERM> retv;
    ErlNifBinary sbin;
    retv.reserve(20);

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

//
// --------------------------- fcap funcs -------------------------------------
//

struct ts_offset {uint8_t mon; uint32_t sec; uint64_t ta;};

struct ts_offset get_ts_offset(uint64_t * ts) {
    std::tm *secTodate = localtime((const time_t*)ts);
	uint8_t monthOffset = (secTodate->tm_year - TAYear)*12 + secTodate->tm_mon;

    std::tm TAstruct = {.tm_sec = 0, .tm_min = 0, .tm_hour = 0, .tm_mday = 1, .tm_mon = secTodate->tm_mon, .tm_year = secTodate->tm_year};
    std::time_t newTA = mktime(&TAstruct);
    uint32_t secs = *ts - newTA; 

    struct ts_offset ret_tso = {.mon = monthOffset, .sec = secs, .ta = (uint64_t)newTA};
    return ret_tso;
}

uint64_t ts_offset_to_ts(struct ts_offset ts){
    int tsmon, tsyear;
    if(ts.mon > 11){
        tsyear = TAYear + ts.mon / 12;
        tsmon = ts.mon - (ts.mon / 12) * 12;
    }else{
        tsyear = TAYear;
        tsmon = ts.mon;
    }
    std::tm TAstruct = {.tm_sec = 0, .tm_min = 0, .tm_hour = 0, .tm_mday = 1, .tm_mon = tsmon, .tm_year = tsyear};
    std::time_t newTA = mktime(&TAstruct);

    return newTA + ts.sec;
}

uint64_t get_ta(int tsmon){
    int tsyear;
    if(tsmon > 11){
        tsyear = TAYear + tsmon / 12;
        tsmon = tsmon - (tsmon / 12) * 12;
    }else{
        tsyear = TAYear;
    }
    std::tm TAstruct = {.tm_sec = 0, .tm_min = 0, .tm_hour = 0, .tm_mday = 1, .tm_mon = tsmon, .tm_year = tsyear};
    std::time_t newTA = mktime(&TAstruct);

    return (uint64_t)newTA;
}

ERL_NIF_TERM
get_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    std::cout << "GET CURRENT OFFSET" << "\r\n";


    uint64_t ss = 1707943436;
    struct ts_offset tso = get_ts_offset(&ss);
    
    std::cout << ss << "\r\n";
    std::cout << std::to_string(tso.mon) << " - " << tso.sec << "\r\n";

    uint64_t ret = ts_offset_to_ts(tso);
    std::cout << "RET:" << ret << "\r\n";

    return enif_make_int(env, 777); 
}

ERL_NIF_TERM
fcap_encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary retbin;
    std::vector<uint8_t> retv;
    unsigned int len;
    long i64;
    ERL_NIF_TERM elem, list;
    retv.reserve(100);
    uint64_t prev_value = 0;

    list = argv[0];
    if (!enif_is_list(env, list)) {
        return enif_make_badarg(env);
    }
    enif_get_list_length(env, list, &len);
    struct ts_offset tso; 
    for (uint32_t i = 0; i < len; i++) {
        if (enif_get_list_cell(env, list, &elem, &list)) {
            enif_get_int64(env, elem, &i64);
            if(i == 0){
                tso = get_ts_offset((uint64_t*)&i64);
                encodeVarint(tso.mon, retv);
                i64 = tso.sec;
            }else{
                i64 = i64 - tso.ta;
            }
            encodeVarint(i64 - prev_value, retv);
            prev_value = i64;
        }
    }


    auto retlen = retv.size();
    enif_alloc_binary(retlen, &retbin);
    memcpy(retbin.data, retv.data(), retlen);
    return enif_make_binary(env, &retbin);
}

ERL_NIF_TERM
fcap_decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    std::vector<ERL_NIF_TERM> retv;
    retv.reserve(20);
    uint64_t prev_value = 0;

    if (!enif_inspect_binary(env, argv[0], &sbin_glob)) {
        return enif_make_badarg(env);
    }
    uint8_t* p = sbin_glob.data;
    uint8_t* end = sbin_glob.data + sbin_glob.size;
    uint64_t newTA = get_ta((int)decodeVarint(p));
    while ( p < end ){
        uint64_t ri = decodeVarint(p) + prev_value;
        retv.push_back(enif_make_int64(env, ri + newTA));
        prev_value = ri;
    }

    return enif_make_list_from_array(env, retv.data(), retv.size());
}

ERL_NIF_TERM
fcap_encode_no_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary retbin;
    std::vector<uint8_t> retv;
    unsigned int len;
    long i64;
    ERL_NIF_TERM elem, list;
    retv.reserve(100);
    uint64_t prev_value = 0;

    list = argv[0];
    if (!enif_is_list(env, list)) {
        return enif_make_badarg(env);
    }
    enif_get_list_length(env, list, &len);
    for (uint32_t i = 0; i < len; i++) {
        if (enif_get_list_cell(env, list, &elem, &list)) {
            enif_get_int64(env, elem, &i64);
            encodeVarint(i64 - prev_value, retv);
            prev_value = i64;
        }
    }


    auto retlen = retv.size();
    enif_alloc_binary(retlen, &retbin);
    memcpy(retbin.data, retv.data(), retlen);
    return enif_make_binary(env, &retbin);
}

ERL_NIF_TERM
fcap_decode_no_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    std::vector<ERL_NIF_TERM> retv;
    retv.reserve(20);
    uint64_t prev_value = 0;

    if (!enif_inspect_binary(env, argv[0], &sbin_glob)) {
        return enif_make_badarg(env);
    }
    uint8_t* p = sbin_glob.data;
    uint8_t* end = sbin_glob.data + sbin_glob.size;
    while ( p < end ){
        uint64_t ri = decodeVarint(p) + prev_value;
        retv.push_back(enif_make_int64(env, ri));
        prev_value = ri;
    }

    return enif_make_list_from_array(env, retv.data(), retv.size());
}

ErlNifFunc nif_funcs[] = {{"int_encode", 1, int_encode_nif},
                          {"int_decode", 1, int_decode_nif},
                          {"fcap_decode", 1, fcap_decode_nif},
                          {"fcap_decode_no", 1, fcap_decode_no_nif},
                          {"fcap_encode_no", 1, fcap_encode_no_nif},
                          {"get_offset", 0, get_offset},
                          {"fcap_encode", 1, fcap_encode_nif}};

ERL_NIF_INIT(varint_nif, nif_funcs, nullptr, nullptr, nullptr, nullptr);
