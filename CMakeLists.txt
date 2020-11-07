cmake_minimum_required(VERSION 3.12)

project(hps-mc VERSION 2.0.0)

# cmake macros
include(ExternalProject)
include(FindPackageHandleStandardArgs)

# cmake macros for this project
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# Find Python 3
find_package(Python3 COMPONENTS Interpreter REQUIRED)
message(STATUS "Python3 executable found at: ${Python3_EXECUTABLE}")
set(PYTHON ${Python3_EXECUTABLE})

# Find Python 2
#find_package(Python2 COMPONENTS Interpreter REQUIRED)
#message(STATUS "Python2 executable found at: ${Python2_EXECUTABLE}")

# Check for Fortran compiler
find_program(GFORTRAN gfortran)
if(GFORTRAN-NOTFOUND)
    message(FATAL_ERROR "gfortran was not found")
else()
    message(STATUS "gfortran was found at: ${GFORTRAN}")
endif()

# Check for Java
find_package(Java REQUIRED)
message(STATUS "java was found at: ${Java_JAVA_EXECUTABLE}")
message(STATUS "java version: ${Java_VERSION}")

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

# Recursively install all generator files
option(ENABLE_INSTALL_GENERATORS "Install physics generators (EGS5, Madgraph)" ON)
if(ENABLE_INSTALL_GENERATORS)
    message(STATUS "Physics generators will be installed automatically.")
    install(DIRECTORY generators DESTINATION share USE_SOURCE_PERMISSIONS)
else()
    message(STATUS "Installation of physics generators is disabled.")
endif()

# External projects will be added to this target
add_custom_target(external)

# Install magnetic field maps
option(ENABLE_INSTALL_FIELDMAPS "Install fieldmaps" ON)
if(ENABLE_INSTALL_FIELDMAPS)
    include(InstallFieldmaps)
endif()

# Install python scripts
install(DIRECTORY python DESTINATION lib)

# Print install dir
message(STATUS "HPSMC will be installed at: ${CMAKE_INSTALL_PREFIX}")
