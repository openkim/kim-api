//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Alexander Stukowski
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
// Release: This file is part of the kim-api-2.4.1 package.
//


#ifndef KIM_SHARED_LIBRARY_HPP_
#define KIM_SHARED_LIBRARY_HPP_

#include <string>
#include <vector>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif

#ifndef KIM_FILESYSTEM_PATH_HPP_
#include "KIM_FilesystemPath.hpp"
#endif

namespace KIM
{
// Forward declarations
class Log;


class SharedLibrary
{
 public:
  SharedLibrary(Log * const log);
  ~SharedLibrary();

  int Open(FILESYSTEM::Path const & sharedLibraryName);
  int IsOpen() const;
  int IsOpen(FILESYSTEM::Path const & sharedLibraryName) const;
  int Close();
  int GetType(CollectionItemType * const type) const;
  int GetCreateFunctionPointer(LanguageName * const languageName,
                               Function ** const functionPointer) const;
  int GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;
  int GetParameterFile(int const index,
                       std::string * const parameterFileName,
                       unsigned int * const parameterFileLength,
                       unsigned char const ** const parameterFileData) const;
  int GetSimulatorModelSpecificationFile(
      std::string * const specFileName,
      unsigned int * const specFileLength,
      unsigned char const ** const specFileData) const;

  int WriteParameterFileDirectory();
  int GetParameterFileDirectoryName(
      FILESYSTEM::Path * const directoryName) const;
  int RemoveParameterFileDirectory();

  int GetDriverName(std::string * const driverName) const;
  int GetNumberOfMetadataFiles(int * const numberOfMetadataFiles) const;
  int GetMetadataFile(int const index,
                      std::string * const metadataFileName,
                      unsigned int * const metadataFileLength,
                      unsigned char const ** const metadataFileData) const;

  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;

  static FILESYSTEM::Path GetORIGIN();

 private:
  // do not allow copy constructor or operator=
  SharedLibrary(SharedLibrary const &);
  void operator=(SharedLibrary const &);
  // do not allow default constructor
  SharedLibrary();

  struct EmbeddedFile
  {
    char const * fileName;
    unsigned int fileLength;
    unsigned char const * filePointer;

    EmbeddedFile();
  };  // struct EmbeddedFile

  static FILESYSTEM::Path const ORIGIN;

  FILESYSTEM::Path sharedLibraryName_;
  void * sharedLibraryHandle_;
  int const * sharedLibrarySchemaVersion_;

  CollectionItemType itemType_;
  LanguageName createLanguageName_;
  Function * createRoutine_;
  std::string driverName_;
  EmbeddedFile simulatorModelSpecificationFile_;
  int numberOfParameterFiles_;
  std::vector<EmbeddedFile> parameterFiles_;
  int numberOfMetadataFiles_;
  std::vector<EmbeddedFile> metadataFiles_;

  FILESYSTEM::Path parameterFileDirectoryName_;

  Log * log_;
};  // class SharedLibrary
}  // namespace KIM
#endif  // KIM_SHARED_LIBRARY_HPP_
