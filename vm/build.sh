#!/usr/bin/env bash
g++ -std=c++17 -lpthread src/qvm_types.cpp src/main.cpp 2>&1 | less
