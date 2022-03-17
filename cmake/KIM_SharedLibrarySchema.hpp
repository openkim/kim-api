//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api-2.3.0 package.
//


#ifndef KIM_SHARED_LIBRARY_SCHEMA_HPP_
#define KIM_SHARED_LIBRARY_SCHEMA_HPP_

#define KIM_SHARED_LIBRARY_SCHEMA_VERSION 2


#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"
#endif

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif

namespace KIM
{
// Forward declarations
class LanguageName;


namespace SHARED_LIBRARY_SCHEMA
{
struct SharedLibrarySchemaV2
{
  struct EmbeddedFile
  {
    char const * fileName;
    unsigned int const fileLength;
    unsigned char const * const filePointer;
  };  // struct EmbeddedFile

  CollectionItemType const itemType;
  char const * const itemName;
  LanguageName const createLanguageName;
  Function * createRoutine;
  char const * const driverName;
  EmbeddedFile const * const simulatorModelSpecificationFile;
  int const numberOfParameterFiles;
  EmbeddedFile const * const parameterFiles;
  int const numberOfMetadataFiles;
  EmbeddedFile const * const metadataFiles;
};  // struct SharedLibrarySchemaV2

struct SharedLibrarySchemaV1
{
  enum ITEM_TYPE {
    STAND_ALONE_MODEL,
    PARAMETERIZED_MODEL,
    SIMULATOR_MODEL,
    MODEL_DRIVER
  };

  struct EmbeddedFile
  {
    char const * fileName;
    unsigned int const fileLength;
    unsigned char const * const filePointer;
  };  // struct EmbeddedFile

  char const * const compiledWithVersion;
  ITEM_TYPE const itemType;
  char const * const itemName;
  LanguageName const createLanguageName;
  Function * createRoutine;
  char const * const driverName;
  int const numberOfParameterFiles;
  EmbeddedFile const * const parameterFiles;
  EmbeddedFile const * const metadataFile;
};  // struct SharedLibrarySchemaV1
}  // namespace SHARED_LIBRARY_SCHEMA
}  // namespace KIM

#endif  // KIM_SHARED_LIBRARY_SCHEMA_HPP_
