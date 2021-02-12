externalproject_add(
    Fieldmaps
    INSTALL_DIR       "${CMAKE_INSTALL_PREFIX}/share/fieldmap"
    GIT_REPOSITORY    "https://github.com/JeffersonLab/hps-fieldmaps"
    GIT_SHALLOW       ON
    SOURCE_DIR        ${CMAKE_BINARY_DIR}/Fieldmaps
    BUILD_COMMAND     cd ${CMAKE_BINARY_DIR}/Fieldmaps && ./unzip.sh
    UPDATE_COMMAND    git pull
    INSTALL_COMMAND   ""
    CONFIGURE_COMMAND ""
)

add_dependencies(external Fieldmaps)

externalproject_get_property(Fieldmaps SOURCE_DIR)
externalproject_get_property(Fieldmaps INSTALL_DIR)

file(GLOB_RECURSE FIELDMAP_FILES "${SOURCE_DIR}/*.dat")
install(FILES ${FIELDMAP_FILES} DESTINATION ${INSTALL_DIR})

message(STATUS "Fieldmaps will be installed to: ${INSTALL_DIR}")