project(egs5)

add_custom_target(egs5 ALL)
add_dependencies(egs5 stdhep-lib)

set(EGS5_DIR ${PROJECT_SOURCE_DIR}/src)

configure_file(${PROJECT_SOURCE_DIR}/config/src/egs5run.in ${CMAKE_BINARY_DIR}/config/src/egs5run)

install(FILES ${CMAKE_BINARY_DIR}/config/src/egs5run DESTINATION ${PROJECT_SOURCE_DIR}/config/src/
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
