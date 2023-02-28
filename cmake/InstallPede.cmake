externalproject_add("pede"
    PREFIX            "pede"
    # download MillepedeII from its source on DESY's GitLab
    GIT_REPOSITORY    "https://gitlab.desy.de/claus.kleinwort/millepede-ii.git"
    GIT_TAG           "V04-12-03"
    GIT_SHALLOW       ON
    GIT_REMOTE_UPDATE_STRATEGY CHECKOUT
    # have the build commands run from inside source directory
    BUILD_IN_SOURCE   ON
    # update install prefix during configure command by editing the Makefile
    CONFIGURE_COMMAND sed -i "s|^PREFIX.*$|PREFIX = ${CMAKE_INSTALL_PREFIX}|g" Makefile
    # simply run make to build it
    BUILD_COMMAND     make
    # simply run make to install it
    INSTALL_COMMAND   make install
)

add_dependencies(external "pede")
message(STATUS "pede will be installed to: ${CMAKE_INSTALL_PREFIX}")
