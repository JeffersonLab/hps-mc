cmake_minimum_required(VERSION 3.0)

project(egs5)

add_custom_target(egs5 ALL)

set(EGS5_DIR ${PROJECT_SOURCE_DIR}/src)

if (NOT STDHEP_DIR)
  message(FATAL_ERROR "The variable STDHEP_DIR pointing to a valid stdhep installation is not defined.")
endif()

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/config)
add_custom_command(TARGET egs5 PRE_BUILD COMMAND cp ARGS -R ${PROJECT_SOURCE_DIR}/config/* . WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/config)

configure_file(${PROJECT_SOURCE_DIR}/config/src/egs5run.in ${CMAKE_BINARY_DIR}/config/src/egs5run)

add_custom_command(TARGET egs5 PRE_BUILD COMMAND ln ARGS -f -s ${PROJECT_SOURCE_DIR}/data . WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/config)

file(GLOB egs_program_sources ${CMAKE_BINARY_DIR}/config/src/*.f)
list(REMOVE_ITEM egs_program_sources "${CMAKE_BINARY_DIR}/config/src/singlePrecision_randomset.f")
foreach (egs_program_source ${egs_program_sources})
  get_filename_component(egs_program ${egs_program_source} NAME_WE)
  message("configuring EGS5 program: ${egs_program}")
  add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/config/${egs_program}.exe PRE_BUILD 
          COMMAND src/build.sh ARGS ${egs_program} WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/config)
  add_custom_target(${egs_program} DEPENDS ${CMAKE_BINARY_DIR}/config/${egs_program}.exe)
  add_dependencies(egs5 ${egs_program})
  install(FILES ${CMAKE_BINARY_DIR}/config/${egs_program}.exe DESTINATION bin RENAME egs5_${egs_program}
          PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE WORLD_READ WORLD_WRITE)
endforeach()
