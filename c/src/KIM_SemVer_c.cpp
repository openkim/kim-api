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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2-2.0.0-beta.3 package.
//

#include "string.h"

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

int KIM_SEM_VER_IsLessThan(char const * const versionA,
                           char const * const versionB,
                           int * const isLessThan)
{
  return KIM::SEM_VER::IsLessThan(
      std::string(versionA), std::string(versionB), isLessThan);
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
