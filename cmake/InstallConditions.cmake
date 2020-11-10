set(CONDITIONS              "Conditions")
set(CONDITIONS_BUILD_DIR    ${CMAKE_BINARY_DIR}/${CONDITIONS})
set(CONDITIONS_VERSION      2020_11_06)
set(CONDITIONS_FILE         hps_conditions_${CONDITIONS_VERSION}.db)
set(CONDITIONS_INSTALL_DIR  ${CMAKE_INSTALL_PREFIX}/share/conditions)
set(CONDITIONS_URL          jdbc:sqlite:${CONDITIONS_INSTALL_DIR}/${CONDITIONS_FILE})

externalproject_add(

    ${CONDITIONS}

    GIT_REPOSITORY    "git@github.com:JeffersonLab/hps-conditions-backup.git"
    GIT_SHALLOW       ON

    INSTALL_DIR       ${CONDITIONS_INSTALL_DIR}
    SOURCE_DIR        ${CONDITIONS_BUILD_DIR}
    BUILD_IN_SOURCE   ON

    BUILD_COMMAND     cd ${CONDITIONS_BUILD_DIR} && tar -zxvf ${CONDITIONS_FILE}.tar.gz
    INSTALL_COMMAND   cp ${CONDITIONS_FILE} ${CONDITIONS_INSTALL_DIR}
    CONFIGURE_COMMAND ""
)

add_dependencies(external ${CONDITIONS})
externalproject_get_property(${CONDITIONS} INSTALL_DIR)
message(STATUS "${CONDITIONS} will be installed to: ${INSTALL_DIR}")