set(HPSJAVA              "HpsJava")
set(HPSJAVA_TAG          master)
set(HPSJAVA_VERSION      4.5-SNAPSHOT)
set(HPSJAVA_JAR_NAME     hps-distribution-${HPSJAVA_VERSION}-bin.jar)
set(HPSJAVA_INSTALL_DIR  ${CMAKE_INSTALL_PREFIX})
set(HPSJAVA_BUILD_DIR    ${CMAKE_BINARY_DIR}/${HPSJAVA})
set(HPSJAVA_BIN_JAR      ${HPSJAVA_INSTALL_DIR}/lib/${HPSJAVA_JAR_NAME})

externalproject_add(
    ${HPSJAVA}
    INSTALL_DIR       ${CMAKE_INSTALL_PREFIX}
    GIT_REPOSITORY    "https://github.com/JeffersonLab/hps-java"
    GIT_TAG           ${HPSJAVA_TAG}
    GIT_SHALLOW       ON
    SOURCE_DIR        ${HPSJAVA_BUILD_DIR}
    BUILD_COMMAND     cd ${HPSJAVA_BUILD_DIR} && ${MAVEN_BUILD_COMMAND}
    INSTALL_COMMAND   cp ${HPSJAVA_BUILD_DIR}/distribution/target/${HPSJAVA_JAR_NAME} ${HPSJAVA_BIN_JAR}
    CONFIGURE_COMMAND ""
)

add_dependencies(external ${HPSJAVA})

#externalproject_get_property(${HPSJAVA} SOURCE_DIR)
#externalproject_get_property(${HPSJAVA} INSTALL_DIR)

message(STATUS "${HPSJAVA} bin jar will be installed to: ${HPSJAVA_BIN_JAR}")
