cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)
project(qt5-x11embed)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/ECM/find-modules")

find_package(Qt5Core COMPONENTS Private REQUIRED)
find_package(Qt5Widgets COMPONENTS Private REQUIRED)
find_package(Qt5X11Extras REQUIRED)
find_package(XCB REQUIRED COMPONENTS XCB ATOM KEYSYMS UTIL)
find_package(X11 REQUIRED)
find_package(X11_XCB REQUIRED)


set(CMAKE_AUTOMOC ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11")

add_library(qx11embedcontainer
	src/X11EmbedContainer.cpp
	src/X11EmbedContainer.h
)
target_include_directories(qx11embedcontainer
	PRIVATE ${Qt5Core_PRIVATE_INCLUDE_DIRS} ${Qt5Widgets_PRIVATE_INCLUDE_DIRS} ${X11_INCLUDE_DIR} ${X11_Xutil_INCLUDE_PATH}
	INTERFACE src
)
target_link_Libraries(qx11embedcontainer
	Qt5::Core Qt5::Widgets Qt5::X11Extras
	XCB::ATOM XCB::KEYSYMS XCB::UTIL
	${X11_LIBRARIES} X11::XCB
)

export(TARGETS qx11embedcontainer FILE QX11EmbedConfig.cmake)

include(GNUInstallDirs)
install(TARGETS qx11embedcontainer
	ARCHIVE  DESTINATION ${CMAKE_INSTALL_LIBDIR}
	LIBRARY  DESTINATION ${CMAKE_INSTALL_LIBDIR}
	RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR}
)
