cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 2.0.0)

# Find Python 3 executable
find_package(PythonInterp)
if(${PYTHON_VERSION_MAJOR} LESS 3)
    message(FATAL_ERROR "Python 3 is required.")
endif()
set(PYTHON ${PYTHON_EXECUTABLE})
message(STATUS "Python executable found at: ${PYTHON_EXECUTABLE}")

# Check for Fortran compiler
find_program(GFORTRAN gfortran)
if(GFORTRAN-NOTFOUND)
    message(FATAL_ERROR "gfortran was not found")
else()
    message(STATUS "gfortran was found at: ${GFORTRAN}")
endif()

# Check for Java
find_package(Java)
if(JAVA-NOTFOUND)
    message(FATAL_ERROR "java was not found")
else()
    message(STATUS "java was found at: ${Java_JAVA_EXECUTABLE}")
    message(STATUS "java version: ${Java_VERSION}")
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

# Recursively install all generator files.
install(DIRECTORY generators DESTINATION share)

# Install python scripts
install(DIRECTORY python DESTINATION lib)

# Print install dir
message(STATUS "HPSMC will be installed at: ${CMAKE_INSTALL_PREFIX}")
