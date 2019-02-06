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
// Release: This file is part of the kim-api-v2-2.0.0 package.
//


#ifndef KIM_SHARED_LIBRARY_HPP_
#define KIM_SHARED_LIBRARY_HPP_

#include <string>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"
#endif

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

namespace KIM
{
// Forward declarations
class LanguageName;
namespace SHARED_LIBRARY_SCHEMA
{
struct SharedLibrarySchemaV1;
}  // namespace SHARED_LIBRARY_SCHEMA


class SharedLibrary
{
 public:
  SharedLibrary(Log * const log);
  ~SharedLibrary();

  enum ITEM_TYPE {
    STAND_ALONE_MODEL,
    PARAMETERIZED_MODEL,
    SIMULATOR_MODEL,
    MODEL_DRIVER
  };

  int Open(std::string const & sharedLibraryName);
  int Close();
  int GetName(std::string * const name) const;
  int GetType(ITEM_TYPE * const type) const;
  int GetCreateFunctionPointer(LanguageName * const languageName,
                               Function ** const functionPointer) const;
  int GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;
  int GetParameterFile(int const index,
                       std::string * const parameterFileName,
                       unsigned int * const parameterFileLength,
                       unsigned char const ** const parameterFileData) const;
  int GetMetadataFile(std::string * const metadataFileName,
                      unsigned int * const metadataFileLength,
                      unsigned char const ** const metadataFileData) const;
  int GetDriverName(std::string * const driverName) const;
  int GetCompiledWithVersion(std::string * const versionString) const;

  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;

 private:
  std::string sharedLibraryName_;
  void * sharedLibraryHandle_;
  int const * sharedLibrarySchemaVersion_;
  SHARED_LIBRARY_SCHEMA::SharedLibrarySchemaV1 const * sharedLibrarySchema_;
  Log * log_;
};  // class SharedLibrary
}  // namespace KIM
#endif  // KIM_SHARED_LIBRARY_HPP_
