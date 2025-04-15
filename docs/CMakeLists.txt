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
# Release: This file is part of the kim-api-2.4.1 package.
#

cmake_minimum_required(VERSION 3.10)

find_package(Doxygen QUIET)
if(Doxygen_FOUND)
  include(FindPackageMessage)
  find_package_message(Doxygen "Found Doxygen: (${DOXYGEN_EXECUTABLE})" "found")
endif()

set(DOXYGEN_PROJECT_NAME         "${PROJECT_NAME}")
set(DOXYGEN_PROJECT_NUMBER       "${PROJECT_VERSION_STRING}")
set(DOXYGEN_STRIP_FROM_PATH      "../../")
set(DOXYGEN_CASE_SENSE_NAMES     NO)
set(DOXYGEN_TAB_SIZE             2)
set(DOXYGEN_EXCLUDE_SYMBOLS      "*_DEFINED_" "*_HPP_" "*_H_")
set(DOXYGEN_ENABLE_PREPROCESSING YES)
set(DOXYGEN_SKIP_FUNCTION_MACROS NO)
set(DOXYGEN_EXTRACT_ALL          YES)
set(DOXYGEN_EXTRACT_STATIC       YES)
set(DOXYGEN_EXTRACT_ANON_NSPACES YES)
set(DOXYGEN_FILE_PATTERNS
  "*.c" "*.cpp" "*.h" "*.h.in" "*.hpp" "*.hpp.in" "*.inc" "*.f90"
  )
# additional patterns for examples
set(DOXYGEN_FILE_PATTERNS ${DOXYGEN_FILE_PATTERNS}
  "*.LGPL" "*.txt" "*.sh" "README" "*.params" "*.in" "*.bib" "*.edn"
  )
set(DOXYGEN_EXTENSION_MAPPING    "in=C++")

set(DOXYGEN_RECURSIVE            YES)
set(DOXYGEN_EXAMPLE_PATH         "../examples")
set(DOXYGEN_IMAGE_PATH           "src/asymptote-figures")
set(DOXYGEN_SOURCE_BROWSER       YES)
set(DOXYGEN_USE_MATHJAX          YES)
set(DOXYGEN_MATHJAX_RELPATH      "https://cdn.jsdelivr.net/npm/mathjax@2")
set(DOXYGEN_EXTRA_PACKAGES       "amsmath")
set(DOXYGEN_GENERATE_LATEX       YES)

if(READTHEDOCS)
  set(DOXYGEN_HTML_FOOTER "readthedocs/footer.html")
endif(READTHEDOCS)

set(SOURCES
  "src/cmake-files.txt"
  "src/table-of-contents.txt"
  "src/features.txt"
  "src/theory.txt"
  "src/implementation.txt"
  "src/version2-differences.txt"
  "../cpp/include/"
  "../c/include/"
  "../fortran/include/"
  "../INSTALL"
  "../NEWS"
  "../examples/model-drivers/"
  "../examples/portable-models/"
  "../examples/simulator-models/"
  "../examples/simulators/"
  )

if(DOXYGEN_FOUND)
  doxygen_add_docs(
    docs
    ${SOURCES}
    COMMENT "Generate documentation"
    )

  add_custom_command(OUTPUT refman.pdf
    COMMAND make
    COMMAND ${CMAKE_COMMAND} -E copy refman.pdf ${CMAKE_CURRENT_BINARY_DIR}/refman.pdf
    WORKING_DIRECTORY latex
    DEPENDS docs
    )

  # TODO fix dependencies
  add_custom_target(pdf DEPENDS docs refman.pdf SOURCES ${SOURCES})
endif(DOXYGEN_FOUND)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_DOCDIR} OPTIONAL)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/refman.pdf
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_DOCDIR} OPTIONAL)
