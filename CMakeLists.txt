cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

FIND_PACKAGE(Qt5Core COMPONENTS Private REQUIRED)
FIND_PACKAGE(Qt5Widgets COMPONENTS Private REQUIRED)
FIND_PACKAGE(Qt5X11Extras REQUIRED)

include_directories(${Qt5Core_PRIVATE_INCLUDE_DIRS})
include_directories(${Qt5Widgets_PRIVATE_INCLUDE_DIRS})

add_library(qx11embedcontainer
	src/X11EmbedContainer.cpp
	src/X11EmbedContainer.h
)
target_link_Libraries(qx11embedcontainer
	Qt5::Core Qt5::Widgets
	xcb xcb-util X11-xcb Qt5::X11Extras X11
)
