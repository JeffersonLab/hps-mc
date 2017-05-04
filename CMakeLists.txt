cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

# FIXME: these should go in EGS5 cmake file
set(EGS5_DATA_DIR ${PROJECT_SOURCE_DIR}/generators/egs5/data)
set(EGS5_CONFIG_DIR ${PROJECT_SOURCE_DIR}/generators/egs5/config)
set(EGS5_BIN_DIR ${CMAKE_INSTALL_PREFIX}/bin)

# FIXME: these should go in MG4 cmake file
set(MG4_DIR ${PROJECT_SOURCE_DIR}/generators/madgraph4/src)

# FIXME: hard-coded to install prefix 
set(STDHEP_BIN_DIR ${CMAKE_INSTALL_PREFIX}/bin)

configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)

install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin)

#set(STDHEP_DIR ${PROJECT_SOURCE_DIR}/tools/stdhep-lib/src)

#add_subdirectory(generators)
#add_subdirectory(tools)
