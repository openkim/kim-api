//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.1.0 package.
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
