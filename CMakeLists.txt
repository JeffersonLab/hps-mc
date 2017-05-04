cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

# build stdhep tools
add_subdirectory(tools/stdhep-lib)
add_subdirectory(tools/stdhep-tools)

# build generations
add_subdirectory(generators/egs5)
add_subdirectory(generators/madgraph4)

# set default hps-java version if not set from arg
if (NOT HPSJAVA_VERSION)
    set(HPSJAVA_VERSION "master")
endif()

# make hps-java work dir
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/hps-java)

# download and untar hps-java
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/hps-java/${HPSJAVA_VERSION}.tar.gz ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}
    COMMAND wget ARGS -q --no-check-certificate https://github.com/JeffersonLab/hps-java/archive/${HPSJAVA_VERSION}.tar.gz
    COMMAND tar ARGS -zxvf ${HPSJAVA_VERSION}.tar.gz
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/hps-java)
add_custom_target(hps_java_download ALL DEPENDS ${CMAKE_BINARY_DIR}/hps-java/${HPSJAVA_VERSION}.tar.gz ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION})

# figure out hps-java project version for jar name
execute_process(COMMAND wget -q https://raw.githubusercontent.com/JeffersonLab/hps-java/${HPSJAVA_VERSION}/pom.xml -O pom-${HPSJAVA_VERSION}.xml
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/hps-java)
execute_process(COMMAND ${CMAKE_SOURCE_DIR}/scripts/print_mvn_project_version.sh pom-${HPSJAVA_VERSION}.xml
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/hps-java
    OUTPUT_VARIABLE HPSJAVA_PROJECT_VERSION) 
string(STRIP "${HPSJAVA_PROJECT_VERSION}" HPSJAVA_PROJECT_VERSION)
#message("HPSJAVA_PROJECT_VERSION='${HPSJAVA_PROJECT_VERSION}'")

# build hps-java
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/distribution/target/hps-distribution-${HPSJAVA_PROJECT_VERSION}-bin.jar 
    COMMAND mvn ARGS -DskipTests
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION})
add_custom_target(hps_java_build ALL DEPENDS hps_java_download ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/distribution/target/hps-distribution-${HPSJAVA_PROJECT_VERSION}-bin.jar)

# install hps-java jar to lib dir
install(FILES ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/distribution/target/hps-distribution-${HPSJAVA_PROJECT_VERSION}-bin.jar DESTINATION lib)

# config and install env script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin)

