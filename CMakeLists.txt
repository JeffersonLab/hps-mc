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

# Install configured scripts
file(GLOB SCRIPTS_IN scripts/*.in)
foreach(SCRIPT_IN ${SCRIPTS_IN})
    get_filename_component(SCRIPT_IN_FILE ${SCRIPT_IN} NAME)
    string(REPLACE ".in" "" SCRIPT_OUT ${SCRIPT_IN_FILE})
    configure_file(${SCRIPT_IN} ${CMAKE_BINARY_DIR}/${SCRIPT_OUT} @ONLY)
    install(FILES ${CMAKE_BINARY_DIR}/${SCRIPT_OUT} DESTINATION bin
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)   
endforeach()

# Install python scripts
install(DIRECTORY python DESTINATION lib)

# Print install dir
message(STATUS "HPSMC will be installed at: ${CMAKE_INSTALL_PREFIX}")
