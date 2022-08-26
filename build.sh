#!/bin/bash

(
    rm -rf build && mkdir -p build && cd build
    /sdf/group/hps/users/sgaiser/src/cmake/3.22/bin/cmake .. \
        -DPython3_EXECUTABLE=$(which python3) \
        -DCMAKE_BUILD_TYPE=RelWithDbInfo \
        -DCMAKE_CXX_COMPILER=$(which g++) -DCMAKE_C_COMPILER=$(which gcc) \
        -DHPSMC_ENABLE_EGS5=OFF \
        -DHPSMC_ENABLE_MADGRAPH=OFF \
        -DHPSMC_ENABLE_STDHEP=ON \
        -DHPSMC_ENABLE_FIELDMAPS=OFF \
        -DHPSMC_ENABLE_LCIO=OFF \
        -DHPSMC_ENABLE_HPSJAVA=OFF \
        -DHPSMC_ENABLE_CONDITIONS=OFF
    make install
)
