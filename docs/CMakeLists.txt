#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013--2020, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
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
  "*.CDDL" "*.txt" "*.sh" "README" "*.params" "*.in" "*.bib" "*.edn"
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
