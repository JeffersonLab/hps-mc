set(LCIO "LCIO")
set(LCIO_BUILD_DIR   ${CMAKE_BINARY_DIR}/${LCIO})
set(LCIO_INSTALL_DIR ${CMAKE_INSTALL_PREFIX})

# copied from LCIO pom.xml
set(LCIO_JAR_NAME "lcio-2.7.4-SNAPSHOT-bin.jar")

externalproject_add(
    ${LCIO}

    GIT_REPOSITORY   "https://github.com/JeffersonLab/hps-lcio"
    GIT_TAG          master
    GIT_SHALLOW      ON

    INSTALL_DIR      ${LCIO_INSTALL_DIR}
    SOURCE_DIR       ${LCIO_BUILD_DIR}

    UPDATE_COMMAND   git pull
    PATCH_COMMAND    ""

    CMAKE_ARGS       -DINSTALL_DOC=OFF -DBUILD_ROOTDICT=OFF
                     -DBUILD_F77_TESTJOBS=OFF -DBUILD_LCIO_EXAMPLES=OFF -DBUILD_TESTING=OFF
                     -DINSTALL_JAR=OFF -DLCIO_JAVA_USE_MAVEN=OFF
                     -DCMAKE_INSTALL_PREFIX=${LCIO_INSTALL_DIR}

    BUILD_COMMAND    ${CMAKE_MAKE_PROGRAM} -j4 && cd ${LCIO_BUILD_DIR} && ${MAVEN} clean install -DskipTests

    INSTALL_COMMAND  ${CMAKE_MAKE_PROGRAM} install && cp ${LCIO_BUILD_DIR}/target/${LCIO_JAR_NAME} ${LCIO_INSTALL_DIR}/lib
)

add_dependencies(external LCIO)

#externalproject_get_property(LCIO SOURCE_DIR)
externalproject_get_property(LCIO INSTALL_DIR)

message(STATUS "LCIO will be installed to: ${INSTALL_DIR}")
