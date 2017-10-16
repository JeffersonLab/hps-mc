cmake_minimum_required(VERSION 3.0)

project(stdhep-tools)

file(COPY ${PROJECT_SOURCE_DIR}/src/ DESTINATION ${CMAKE_BINARY_DIR}/stdhep-tools-src/)

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/stdhep-tools-src/bin)

set(STDHEP_TOOLS_BIN_DIR ${CMAKE_BINARY_DIR}/stdhep-tools-src/bin)

file(GLOB stdhep_program_sources ${PROJECT_SOURCE_DIR}/src/*.cc)
foreach (stdhep_program_source ${stdhep_program_sources})
    get_filename_component(stdhep_program ${stdhep_program_source} NAME_WE)
    message(STATUS "Configuring StdHep tool: ${stdhep_program}")
    list(APPEND stdhep_programs ${STDHEP_TOOLS_BIN_DIR}/${stdhep_program})
    install(FILES ${CMAKE_BINARY_DIR}/stdhep-tools-src/bin/${stdhep_program} DESTINATION bin RENAME stdhep_${stdhep_program}
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endforeach()

add_custom_command(OUTPUT ${stdhep_programs}
    COMMAND make BIN=${STDHEP_TOOLS_BIN_DIR} SRC_DIR=${CMAKE_BINARY_DIR}/stdhep-tools-src/ STDHEP_DIR=${STDHEP_DIR}
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/stdhep-tools-src"
    DEPENDS "${CMAKE_BINARY_DIR}/stdhep-tools-src")

add_custom_target(stdhep_tools_build ALL DEPENDS ${stdhep_programs})
