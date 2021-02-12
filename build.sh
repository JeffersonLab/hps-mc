(
    if [ -z "$GSL_ROOT_DIR" ]; then
        echo "ERROR: Missing GSL_ROOT_DIR in environment"
        exit 1
    fi
    mkdir -p build && cd build && \
    cmake .. \
        -DCMAKE_INSTALL_PREFIX=$(realpath ../install) \
        -DPython3_EXECUTABLE=`which python3` \
        -DGSL_ROOT_DIR=${GSL_ROOT_DIR} \
        -DCMAKE_BUILD_TYPE=RelWithDbInfo \
        -DCMAKE_CXX_COMPILER=$(which g++) -DCMAKE_C_COMPILER=$(which gcc) \
        -DENABLE_INSTALL_GENERATORS=ON \
        -DENABLE_INSTALL_FIELDMAPS=ON \
        -DENABLE_INSTALL_LCIO=ON \
        -DENABLE_INSTALL_HPSJAVA=ON \
        -DENABLE_INSTALL_CONDITIONS=OFF \
        -DHPSJAVA_TAG=master && \
    make install
)
