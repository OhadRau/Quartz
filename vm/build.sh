#!/usr/bin/env bash
g++ -g -std=c++17 -lpthread src/qvm_types.cpp src/qvm_interpreter.cpp src/qvm_instrs.cpp src/main.cpp |& less
./a.out |& less
