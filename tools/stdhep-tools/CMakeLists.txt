cmake_minimum_required(VERSION 3.0)

project(stdhep-tools)

add_custom_target(stdhep-tools ALL)
#add_dependencies(stdhep-tools stdhep-lib)

if (NOT STDHEP_DIR)
  message(FATAL_ERROR "The variable STDHEP_DIR pointing to a valid stdhep installation is not defined.")
endif()

set(STDHEP_TOOLS_SOURCE_DIR ${PROJECT_SOURCE_DIR}/src)
set(STDHEP_TOOLS_BUILD_DIR ${CMAKE_BINARY_DIR}/stdhep-tools-src)

configure_file(${STDHEP_TOOLS_SOURCE_DIR}/Makefile.in ${STDHEP_TOOLS_BUILD_DIR}/Makefile)

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_custom_command(TARGET stdhep-tools PRE_BUILD COMMAND cp ARGS -R ${STDHEP_TOOLS_SOURCE_DIR}/* . WORKING_DIRECTORY ${STDHEP_TOOLS_BUILD_DIR})
add_custom_command(TARGET stdhep-tools PRE_BUILD COMMAND make WORKING_DIRECTORY ${STDHEP_TOOLS_BUILD_DIR})

file(GLOB stdhep_program_sources ${PROJECT_SOURCE_DIR}/src/*.cc)
foreach (stdhep_program_source ${stdhep_program_sources})
  get_filename_component(stdhep_program ${stdhep_program_source} NAME_WE)
  install(FILES ${CMAKE_BINARY_DIR}/bin/${stdhep_program} DESTINATION bin RENAME stdhep_${stdhep_program}
          PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE WORLD_READ WORLD_WRITE)
endforeach()

#install(DIRECTORY ${CMAKE_BINARY_DIR}/bin/ DESTINATION bin PATTERN "*"
#        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
