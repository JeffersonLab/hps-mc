set(HPSJAVA              "HpsJava")
if(NOT DEFINED HPSJAVA_TAG)
    set(HPSJAVA_TAG      "master")
endif()
set(HPSJAVA_INSTALL_DIR  ${CMAKE_INSTALL_PREFIX}/lib)
set(HPSJAVA_BUILD_DIR    ${CMAKE_BINARY_DIR}/${HPSJAVA})
set(HPSJAVA_POM_URL      https://raw.githubusercontent.com/JeffersonLab/hps-java/${HPSJAVA_TAG}/pom.xml)
set(HPSJAVA_POM_LOCAL    ${CMAKE_BINARY_DIR}/${HPSJAVA}_pom.xml)

file(DOWNLOAD ${HPSJAVA_POM_URL} ${HPSJAVA_POM_LOCAL})
message(STATUS "Downloading ${HPSJAVA} POM file from: ${HPSJAVA_POM_URL}")
execute_process(COMMAND ${CMAKE_SOURCE_DIR}/scripts/print_hps_java_version.sh ${HPSJAVA_POM_LOCAL} OUTPUT_VARIABLE HPSJAVA_VERSION)

set(HPSJAVA_JAR_NAME     hps-distribution-${HPSJAVA_VERSION}-bin.jar)
set(HPSJAVA_BIN_JAR      ${HPSJAVA_INSTALL_DIR}/${HPSJAVA_JAR_NAME})

message(STATUS "Using HPS Java tag: ${HPSJAVA_TAG}")
message(STATUS "Using HPS Java version: ${HPSJAVA_VERSION}")

if(NOT EXISTS "${HPSJAVA_BIN_JAR}")

    externalproject_add(

        ${HPSJAVA}

        INSTALL_DIR       ${HPSJAVA_INSTALL_DIR}
        SOURCE_DIR        ${HPSJAVA_BUILD_DIR}

        GIT_REPOSITORY    "https://github.com/JeffersonLab/hps-java"
        GIT_TAG           ${HPSJAVA_TAG}
        GIT_SHALLOW       ON
        GIT_REMOTE_UPDATE_STRATEGY CHECKOUT

        UPDATE_COMMAND    cd ${HPSJAVA_BUILD_DIR} && git checkout ${HPSJAVA_TAG} && git pull
        CONFIGURE_COMMAND ""
        BUILD_COMMAND     cd ${HPSJAVA_BUILD_DIR} && ${MAVEN_BUILD_COMMAND}
        INSTALL_COMMAND   mkdir -p ${HPSJAVA_INSTALL_DIR} && cp ${HPSJAVA_BUILD_DIR}/distribution/target/${HPSJAVA_JAR_NAME} ${HPSJAVA_INSTALL_DIR}
    )

    add_dependencies(external ${HPSJAVA})

    message(STATUS "${HPSJAVA} bin jar will be installed to: ${HPSJAVA_BIN_JAR}")

else()
    message(STATUS "${HPSJAVA} will not be rebuilt. The bin jar is already installed at: ${HPSJAVA_BIN_JAR}")
endif()

install(DIRECTORY ${HPSJAVA_BUILD_DIR}/detector-data/detectors DESTINATION ${CMAKE_INSTALL_PREFIX}/share
        FILES_MATCHING PATTERN "*.lcdd"
        PATTERN "SamplingFractions" EXCLUDE)
