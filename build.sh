#!/bin/bash

(
    if [ -z "$GSL_ROOT_DIR" ]; then
        echo "ERROR: Missing GSL_ROOT_DIR in environment"
        exit 1
    fi
    rm -rf build &> /dev/null
    mkdir -p build && cd build
    cmake .. \
        -DCMAKE_INSTALL_PREFIX=$(realpath ../install) \
        -DPython3_EXECUTABLE=`which python3` \
        -DGSL_ROOT_DIR=${GSL_ROOT_DIR} \
        -DCMAKE_BUILD_TYPE=RelWithDbInfo \
        -DCMAKE_CXX_COMPILER=$(which g++) -DCMAKE_C_COMPILER=$(which gcc) \
        -DHPSJAVA_TAG=master \
        -DHPSMC_ENABLE_EGS5=OFF \
        -DHPSMC_ENABLE_MADGRAPH=OFF \
        -DHPSMC_ENABLE_STDHEP=OFF \
        -DHPSMC_ENABLE_FIELDMAPS=OFF \
        -DHPSMC_ENABLE_LCIO=OFF \
        -DHPSMC_ENABLE_HPSJAVA=OFF \
        -DHPSMC_ENABLE_CONDITIONS=OFF
    make install
)
