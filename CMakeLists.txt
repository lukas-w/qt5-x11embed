cmake_minimum_required(VERSION 2.8.0 FATAL_ERROR)

find_package(Qt5Core COMPONENTS Private REQUIRED)
find_package(Qt5Widgets COMPONENTS Private REQUIRED)
find_package(Qt5X11Extras REQUIRED)

set(CMAKE_AUTOMOC ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11")

add_library(qx11embedcontainer
	src/X11EmbedContainer.cpp
	src/X11EmbedContainer.h
)
target_include_directories(qx11embedcontainer
	PRIVATE ${Qt5Core_PRIVATE_INCLUDE_DIRS} ${Qt5Widgets_PRIVATE_INCLUDE_DIRS}
	INTERFACE src
)
target_link_Libraries(qx11embedcontainer
	Qt5::Core Qt5::Widgets
	xcb xcb-util X11-xcb Qt5::X11Extras X11
)

export(TARGETS qx11embedcontainer FILE QX11EmbedConfig.cmake)

include(GNUInstallDirs)
install(TARGETS qx11embedcontainer
	ARCHIVE  DESTINATION ${CMAKE_INSTALL_LIBDIR}
	LIBRARY  DESTINATION ${CMAKE_INSTALL_LIBDIR}
	RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR}
)
