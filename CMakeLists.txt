cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

# build stdhep tools
add_subdirectory(tools/stdhep-lib)
add_subdirectory(tools/stdhep-tools)

# build generations
add_subdirectory(generators/egs5)
add_subdirectory(generators/madgraph4)
add_subdirectory(generators/madgraph5)

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

# build hps-java
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/distribution/target/hps-distribution-${HPSJAVA_PROJECT_VERSION}-bin.jar 
    COMMAND mvn ARGS -DskipTests -q
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION})
add_custom_target(hps_java_build ALL DEPENDS hps_java_download ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/distribution/target/hps-distribution-${HPSJAVA_PROJECT_VERSION}-bin.jar)

# install hps-java jar to lib dir
install(FILES ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/distribution/target/hps-distribution-${HPSJAVA_PROJECT_VERSION}-bin.jar DESTINATION lib)

# install detectors to share dir
install(DIRECTORY ${CMAKE_BINARY_DIR}/hps-java/hps-java-${HPSJAVA_VERSION}/detector-data/detectors/ DESTINATION share/detectors/ 
    PATTERN "*.properties" EXCLUDE
    PATTERN "*.xml" EXCLUDE
    PATTERN "SampleFractions" EXCLUDE)

# create fieldmap install dir
file(MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/fieldmaps/)

# config fieldmap install script
configure_file(${PROJECT_SOURCE_DIR}/scripts/install_fieldmaps.sh.in ${CMAKE_BINARY_DIR}/install_fieldmaps.sh)

# clone the fieldmap project from github
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/hps-fieldmaps/
    COMMAND git clone https://github.com/JeffersonLab/hps-fieldmaps.git
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_custom_target(hps_fieldmaps_download ALL DEPENDS ${CMAKE_BINARY_DIR}/hps-fieldmaps/)

# copy fieldmaps to install dir
add_custom_command(OUTPUT ${CMAKE_INSTALL_PREFIX}/share/fieldmaps/BmapCorrected3D_13k_unfolded_scaled_1.15384615385.dat
    COMMAND "${CMAKE_BINARY_DIR}/install_fieldmaps.sh"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/hps-fieldmaps/)
add_custom_target(hps_fieldmaps_install ALL DEPENDS hps_fieldmaps_download ${CMAKE_INSTALL_PREFIX}/share/fieldmaps/BmapCorrected3D_13k_unfolded_scaled_1.15384615385.dat)

# clone LCIO from github
set(LCIO_TAG v02-07-04)
set(LCIO_VERSION 2.7.4)
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/LCIO
    COMMAND git clone https://github.com/iLCSoft/LCIO.git
    COMMAND cd ${CMAKE_BINARY_DIR}/LCIO && git checkout ${LCIO_TAG}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_custom_target(lcio_download ALL DEPENDS ${CMAKE_BINARY_DIR}/LCIO)

# build LCIO java lib
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/LCIO/target/lcio-${LCIO_VERSION}-SNAPSHOT-bin.jar
    COMMAND mvn ARGS -DskipTests -q
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/LCIO)
add_custom_target(lcio_java_build ALL DEPENDS lcio_download ${CMAKE_BINARY_DIR}/LCIO/target/lcio-${LCIO_VERSION}-SNAPSHOT-bin.jar)

# build LCIO C++ lib
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/lcio-build)
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/LCIO/lib/liblcio.so.${LCIO_VERSION}
    COMMAND cmake -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/LCIO ${CMAKE_BINARY_DIR}/LCIO && make install
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/lcio-build) 
add_custom_target(lcio_cpp_build ALL DEPENDS lcio_download ${CMAKE_BINARY_DIR}/LCIO/lib/liblcio.so.${LCIO_VERSION})

# install LCIO C++ tools
install(FILES ${CMAKE_BINARY_DIR}/lcio-build/bin/dumpevent DESTINATION bin RENAME lcio_dumpevent
          PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE WORLD_READ WORLD_WRITE)

# install LCIO jar to lib dir
install(FILES ${CMAKE_BINARY_DIR}/LCIO/target/lcio-${LCIO_VERSION}-SNAPSHOT-bin.jar DESTINATION lib)
set(LCIO_JAR ${CMAKE_INSTALL_PREFIX}/lib/lcio-${LCIO_VERSION}-SNAPSHOT-bin.jar)
message( STATUS "LCIO jar will be installed to: ${LCIO_JAR}")

# config and install env script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin)

# install run params data
install(FILES ${CMAKE_SOURCE_DIR}/data/run_params.json DESTINATION share)
