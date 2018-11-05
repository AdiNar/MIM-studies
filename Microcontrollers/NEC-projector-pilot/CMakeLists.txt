cmake_minimum_required(VERSION 3.8)
project(ZALB)

set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES
        main.c
        nec_pilot.h
        timers.h
        dma.h)

add_executable(ZALB ${SOURCE_FILES})