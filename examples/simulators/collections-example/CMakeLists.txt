#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#
# SPDX-License-Identifier: LGPL-2.1-or-later
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

#
# Release: This file is part of the kim-api.git repository.
#


cmake_minimum_required(VERSION 3.10)

project(collections-example VERSION 1.0.0 LANGUAGES CXX Fortran)

find_package(KIM-API 2.2 REQUIRED CONFIG)

add_executable(${PROJECT_NAME} ${PROJECT_NAME}.cpp)
target_link_libraries(${PROJECT_NAME} KIM-API::kim-api)

add_executable(${PROJECT_NAME}-c ${PROJECT_NAME}-c.cpp)
target_link_libraries(${PROJECT_NAME}-c KIM-API::kim-api)

add_executable(${PROJECT_NAME}-fortran ${PROJECT_NAME}-fortran.f90)
target_link_libraries(${PROJECT_NAME}-fortran KIM-API::kim-api)
