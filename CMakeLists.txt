cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)

install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin)

# FIXME: Should this be set by stdhep-lib prj and exported???
#set(STDHEP_DIR ${PROJECT_SOURCE_DIR}/tools/stdhep-lib/src)

#add_subdirectory(generators)
#add_subdirectory(tools)
