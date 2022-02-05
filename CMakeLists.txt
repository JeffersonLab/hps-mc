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
find_program(GFORTRAN gfortran REQUIRED)

# Check for Java
find_package(Java COMPONENTS Runtime REQUIRED)
message(STATUS "java was found at: ${Java_JAVA_EXECUTABLE}")
message(STATUS "java version: ${Java_VERSION}")

# Check for Maven
find_program(MAVEN mvn REQUIRED)

set(MAVEN_BUILD_COMMAND ${MAVEN} clean install -DskipTests -T 4 -q)

# Check for GSL
find_package(GSL REQUIRED)
message(STATUS "GSL found with version: ${GSL_VERSION}")

# Build and install stdhep tools
option(HPSMC_ENABLE_STDHEP "Install StdHep tools" OFF)
if (HPSMC_ENABLE_STDHEP)
    add_subdirectory(tools/stdhep-lib)
    add_subdirectory(tools/stdhep-tools)
else()
    message(STATUS "StdHep tools are disabled")
endif()

# Build and install EGS5
option(HPSMC_ENABLE_EGS5 "Install EGS5" OFF)
if (HPSMC_ENABLE_EGS5)
    if (NOT HPSMC_ENABLE_STDHEP)
        message(FATAL_ERROR "StdHep must be enabled to use EGS5")
    endif()
    add_subdirectory(generators/egs5)
    install(DIRECTORY generators/egs5 DESTINATION share/generators USE_SOURCE_PERMISSIONS)
else()
    message(STATUS "EGS5 is disabled")
endif()

# Build and install Madgraph
option(HPSMC_ENABLE_MADGRAPH "Install Madgraph" OFF)
if (HPSMC_ENABLE_MADGRAPH)
    add_subdirectory(generators/madgraph4)
    add_subdirectory(generators/madgraph5)
    install(DIRECTORY generators/madgraph4 DESTINATION share/generators USE_SOURCE_PERMISSIONS)
    install(DIRECTORY generators/madgraph5 DESTINATION share/generators USE_SOURCE_PERMISSIONS)
else()
    message(STATUS "Madgraph is disabled")
endif()

option(HPSMC_ENABLE_ROOT "Enable ROOT (requires ROOTSYS set in the environment)" OFF)
if (HPSMC_ENABLE_ROOT)
    if(DEFINED ENV{ROOTSYS})
        set(ROOTSYS $ENV{ROOTSYS})
        message(STATUS "ROOTSYS set to: ${ROOTSYS}")
    else()
        message(FATAL_ERROR "ROOT was enabled but ROOTSYS is not set in the environment")
    endif()
else()
    message(STATUS "ROOT is disabled")
endif()

# External projects will be added to this target
add_custom_target(external)

# Enable specific external software packages and data files
option(HPSMC_ENABLE_FIELDMAPS  "Install fieldmaps" OFF)
option(HPSMC_ENABLE_LCIO       "Install LCIO" OFF)
option(HPSMC_ENABLE_HPSJAVA    "Install HPS Java" OFF)
option(HPSMC_ENABLE_CONDITIONS "Install local conditions database" OFF)

# Install magnetic field maps
if(HPSMC_ENABLE_FIELDMAPS)
    include(InstallFieldmaps)
else()
    message(STATUS "Fieldmaps are disabled")
endif()

# Install LCIO
if(HPSMC_ENABLE_LCIO)
    include(InstallLCIO)
else()
    message(STATUS "LCIO is disabled")
endif()

# Install HPS Java
if(HPSMC_ENABLE_HPSJAVA)
    include(InstallHpsJava)
else()
    message(STATUS "HpsJava is disabled")
endif()

# Install local conditions database
if(HPSMC_ENABLE_CONDITIONS)
    include(InstallConditions)
else()
    message(STATUS "Local conditions db is disabled")
endif()

# Install python scripts
install(DIRECTORY python DESTINATION lib)

# Custom target to only install the python scripts (must be executed manually)
add_custom_target(InstallPythonScripts
                  COMMAND mkdir -p ${CMAKE_INSTALL_PREFIX}/lib && cp -R ${PROJECT_SOURCE_DIR}/python ${CMAKE_INSTALL_PREFIX}/lib)

# Install configured scripts
file(GLOB SCRIPTS_IN scripts/*.in)
foreach(SCRIPT_IN ${SCRIPTS_IN})
    get_filename_component(SCRIPT_IN_FILE ${SCRIPT_IN} NAME)
    string(REPLACE ".in" "" SCRIPT_OUT ${SCRIPT_IN_FILE})
    configure_file(${SCRIPT_IN} ${CMAKE_BINARY_DIR}/${SCRIPT_OUT} @ONLY)
    install(FILES ${CMAKE_BINARY_DIR}/${SCRIPT_OUT} DESTINATION bin
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endforeach()

# Print install dir
message(STATUS "HPSMC will be installed at: ${CMAKE_INSTALL_PREFIX}")
