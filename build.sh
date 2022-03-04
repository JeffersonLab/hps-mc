#!/bin/bash

(
    rm -rf build && mkdir -p build && cd build
    cmake .. \
        -DPython3_EXECUTABLE=$(which python3) \
        -DCMAKE_BUILD_TYPE=RelWithDbInfo \
        -DCMAKE_CXX_COMPILER=$(which g++) -DCMAKE_C_COMPILER=$(which gcc) \
        -DHPSMC_ENABLE_EGS5=ON \
        -DHPSMC_ENABLE_MADGRAPH=ON \
        -DHPSMC_ENABLE_STDHEP=ON \
        -DHPSMC_ENABLE_FIELDMAPS=ON \
        -DHPSMC_ENABLE_LCIO=ON \
        -DHPSMC_ENABLE_HPSJAVA=ON \
        -DHPSMC_ENABLE_CONDITIONS=OFF
    make install
)
