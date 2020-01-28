cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

# Check for Fortran compiler
find_program(GFORTRAN gfortran)
if(GFORTRAN-NOTFOUND)
    message(FATAL_ERROR "gfortran was not found")
else()
    message(STATUS "gfortran was found at: ${GFORTRAN}")
endif()

# Check for python
find_program(PYTHON python)
if(PYTHON-NOTFOUND)
    message(FATAL_ERROR "python was not found")
else()
    message(STATUS "python was found at: ${PYTHON}")
endif()

# Check for Java
find_package(Java)
if(JAVA-NOTFOUND)
    message(FATAL_ERROR "java was not found")
else()
    message(STATUS "java was found at: ${Java_JAVA_EXECUTABLE}")
    message(STATUS "java version: ${Java_VERSION}")
endif()

# Check for gunzip
find_program(GUNZIP gunzip)
if(GUNZIP-NOTFOUND)
    message(FATAL_ERROR "gunzip was not found")
else()
    message(STATUS "gunzip found at: ${GUNZIP}")
endif()

# Check for GSL
find_package(GSL REQUIRED)
message(STATUS "GSL found with version: ${GSL_VERSION}")

# Build stdhep tools
add_subdirectory(tools/stdhep-lib)
add_subdirectory(tools/stdhep-tools)

# Build generators
add_subdirectory(generators/egs5)
add_subdirectory(generators/madgraph4)
add_subdirectory(generators/madgraph5)

# Config and install bash env script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Config and install csh env script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.csh.in ${CMAKE_BINARY_DIR}/hps-mc-env.csh)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.csh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Config and install workflow script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-workflow.in ${CMAKE_BINARY_DIR}/hps-mc-workflow)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-workflow DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Config and install bsub script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-bsub.in ${CMAKE_BINARY_DIR}/hps-mc-bsub)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-bsub DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Config and install jsub script
configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-jsub.in ${CMAKE_BINARY_DIR}/hps-mc-jsub)
install(FILES ${CMAKE_BINARY_DIR}/hps-mc-jsub DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Install job script
install(FILES ${PROJECT_SOURCE_DIR}/scripts/hps-mc-job DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Install stdhep script
install(FILES scripts/stdhep_count.sh DESTINATION bin
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# Install python scripts
install(DIRECTORY python DESTINATION lib)

# Print install dir
message(STATUS "HPSMC will be installed at: ${CMAKE_INSTALL_PREFIX}")
