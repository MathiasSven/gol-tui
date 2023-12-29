cmake_minimum_required(VERSION 3.27)

project(gol-tui)

set(CMAKE_SKIP_INSTALL_RULES True)

find_package(NumCpp REQUIRED)

configure_file(../gol-tui.buildinfo.in gol-tui.buildinfo)
