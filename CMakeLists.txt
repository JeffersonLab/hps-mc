cmake_minimum_required(VERSION 3.0)

project(hps-mc VERSION 0.0.1)

add_subdirectory(tools/stdhep-lib)
add_subdirectory(tools/stdhep-tools)

add_subdirectory(generators/egs5)
add_subdirectory(generators/madgraph4)

configure_file(${PROJECT_SOURCE_DIR}/scripts/hps-mc-env.sh.in ${CMAKE_BINARY_DIR}/hps-mc-env.sh)

install(FILES ${CMAKE_BINARY_DIR}/hps-mc-env.sh DESTINATION bin)
