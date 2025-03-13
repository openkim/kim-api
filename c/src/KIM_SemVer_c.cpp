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
// Release: This file is part of the kim-api.git repository.
//


#include <cstring>
#include <string>

#ifndef KIM_SEM_VER_HPP_
#include "KIM_SemVer.hpp"
#endif
extern "C" {
#ifndef KIM_SEM_VER_H_
#include "KIM_SemVer.h"
#endif
}  // extern "C"

extern "C" {
char const * KIM_SEM_VER_GetSemVer()
{
  return KIM::SEM_VER::GetSemVer().c_str();
}

int KIM_SEM_VER_IsLessThan(char const * const lhs,
                           char const * const rhs,
                           int * const isLessThan)
{
  return KIM::SEM_VER::IsLessThan(
      std::string(lhs), std::string(rhs), isLessThan);
}

int KIM_SEM_VER_ParseSemVer(char const * const version,
                            int const prereleaseLength,
                            int const buildMetadataLength,
                            int * const major,
                            int * const minor,
                            int * const patch,
                            char * const prerelease,
                            char * const buildMetadata)
{
  std::string prereleaseLocal;
  std::string buildMetadataLocal;
  std::string * prerel;
  std::string * build;
  if (prerelease == NULL)
    prerel = NULL;
  else
    prerel = &prereleaseLocal;
  if (buildMetadata == NULL)
    build = NULL;
  else
    build = &buildMetadataLocal;

  int error = KIM::SEM_VER::ParseSemVer(
      std::string(version), major, minor, patch, prerel, build);

  if (!error)
  {
    if (prerelease != NULL)
    {
      char * copyReturn
          = strncpy(prerelease, prereleaseLocal.c_str(), prereleaseLength);
      if (copyReturn != prerelease) error = 1;
    }
    if (buildMetadata != NULL)
    {
      char * copyReturn = strncpy(
          buildMetadata, buildMetadataLocal.c_str(), buildMetadataLength);
      if (copyReturn != buildMetadata) error = 1;
    }
  }

  return error;
}
}  // extern "C"
