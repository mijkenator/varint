#!/bin/bash

rm c_src/varint_nif.o
rm priv/varint_nif.so

rebar3 compile
