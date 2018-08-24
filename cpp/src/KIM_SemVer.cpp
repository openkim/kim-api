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
// Release: This file is part of the kim-api.git repository.
//


#include "stdlib.h"
#include "vector"

#ifndef KIM_VERSION_HPP_
#include "KIM_Version.hpp"
#endif

#ifndef KIM_SEM_VER_HPP_
#include "KIM_SemVer.hpp"
#endif

namespace KIM
{
namespace SEM_VER
{
namespace
{
std::string const version(KIM_VERSION_STRING);

enum SERIES_TYPE {PRERELEASE, BUILD_METADATA};
enum IDENTIFIER_TYPE {NUMERIC, ALPHANUMERIC, INVALID};
char const numeric[] = "0123456789";
char const alphaNumeric[] = "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789-";

IDENTIFIER_TYPE IdentifierType(std::string const & identifier)
{
  size_t found;
  found = identifier.find_first_not_of(numeric);
  if (found == std::string::npos)
  {
    return NUMERIC;
  }

  found = identifier.find_first_not_of(alphaNumeric);
  if (found == std::string::npos)
  {
    return ALPHANUMERIC;
  }

  return INVALID;
}

int ParseIdentifiers(SERIES_TYPE type, std::string const & series,
                     std::vector<std::string> * const identifiers)
{
  if (identifiers != NULL)
  {
    identifiers->clear();
  }

  size_t found;
  std::string subStr;
  std::string remainderStr = series;

  while (remainderStr.length() != 0)
  {
    found = remainderStr.find_first_of('.');
    if (found == std::string::npos)
    {
      switch (IdentifierType(remainderStr))
      {
        case INVALID:
          return true;  // Invalid identifier
          break;
        case NUMERIC:
          if (type == PRERELEASE)
          {
            if ((remainderStr[0] == '0') && (remainderStr.length() > 1))
            {
              return true;  // Has leading zero
            }
          }
          break;
        case ALPHANUMERIC:
          if (identifiers != NULL)
          {
            identifiers->push_back(remainderStr);
          }
          break;
      }
      remainderStr = "";
    }
    else
    {
      subStr = remainderStr.substr(0, found);
      remainderStr = remainderStr.substr(found+1);

      if (remainderStr.length() == 0)
      {
        return true;  // Empty final identifier
      }

      if (subStr.length() == 0)
      {
        return true;  // Identifier is empty
      }

      switch (IdentifierType(subStr))
      {
        case INVALID:
          return true;  // Invalid identifier
          break;
        case NUMERIC:
          if (type == PRERELEASE)
          {
            if ((subStr[0] == '0') && (subStr.length() > 1))
            {
              return true;  // Has leading zero
            }
          }
          break;
        case ALPHANUMERIC:
          if (identifiers != NULL)
          {
            identifiers->push_back(subStr);
          }
          break;
      }
    }
  }

  return false;
}

bool IsPrereleaseLessThan(std::string const & prereleaseA,
                          std::string const & prereleaseB)
{
  std::vector<std::string> identifiersA;
  std::vector<std::string> identifiersB;

  // Assume already checked for validity
  ParseIdentifiers(PRERELEASE, prereleaseA, &identifiersA);
  ParseIdentifiers(PRERELEASE, prereleaseB, &identifiersB);

  // use smaller size
  int size = ((identifiersA.size() > identifiersB.size()) ?
              identifiersB.size() : identifiersA.size());

  for (int i=0; i<size; ++i)
  {
    std::string const & idA = identifiersA[i];
    std::string const & idB = identifiersB[i];
    IDENTIFIER_TYPE typeA = IdentifierType(idA);
    IDENTIFIER_TYPE typeB = IdentifierType(idB);

    if ((typeA == ALPHANUMERIC) && (typeB == NUMERIC))
    {
      return false;
    }
    else if ((typeA == NUMERIC) && (typeB == ALPHANUMERIC))
    {
      return true;
    }
    else if ((typeA == NUMERIC) && (typeB == NUMERIC))
    {
      char * end;
      int numberA = strtol(idA.c_str(), &end, 10);
      int numberB = strtol(idB.c_str(), &end, 10);

      if (numberA > numberB)
      {
        return false;
      }

      if (numberA < numberB)
      {
        return true;
      }

      // numeric identifers are equal
    }
    else if ((typeA == ALPHANUMERIC) && (typeB == ALPHANUMERIC))
    {
      if (idA > idB)
      {
        return false;
      }

      if (idA < idB)
      {
        return true;
      }

      // alphanumeric identifiers are equal
    }

    // move on to the next identifier
  }

  if (identifiersA.size() > identifiersB.size())
  {
    return false;
  }

  if (identifiersA.size() < identifiersB.size())
  {
    return true;
  }

  return false;  // Prerelease A and B are identical
}

enum HAS_ANCILLARY
{HAS_NONE, HAS_PRERELEASE_ONLY, HAS_BUILD_METADATA_ONLY, HAS_BOTH};
}  // namespace


void GetSemVer(std::string const ** const version)
{
  *version = &(SEM_VER::version);
}

int IsLessThan(std::string const & versionA, std::string const & versionB,
               int * const isLessThan)
{
  int majorA;
  int minorA;
  int patchA;
  std::string prereleaseA;
  std::string buildMetadataA;
  int majorB;
  int minorB;
  int patchB;
  std::string prereleaseB;
  std::string buildMetadataB;

  if ((ParseSemVer(versionA, &majorA, &minorA, &patchA, &prereleaseA,
                   &buildMetadataA)
       || ParseSemVer(versionB, &majorB, &minorB, &patchB, &prereleaseB,
                      &buildMetadataB)))
  {
    return true;
  }

  if (majorA > majorB)
  {
    *isLessThan = false;
    return false;
  }
  else if (majorA < majorB)
  {
    *isLessThan = true;
    return false;
  }
  // Major values are equal

  if (minorA > minorB)
  {
    *isLessThan = false;
    return false;
  }
  else if (minorA < minorB)
  {
    *isLessThan = true;
    return false;
  }
  // Minor values are equal

  if (patchA > patchB)
  {
    *isLessThan = false;
    return false;
  }
  else if (patchA < patchB)
  {
    *isLessThan = true;
    return false;
  }
  // Patch values are equal

  if ((prereleaseA == "") && (prereleaseB != ""))
  {
    *isLessThan = false;
    return false;
  }
  else if ((prereleaseA != "") && (prereleaseB == ""))
  {
    *isLessThan = true;
    return false;
  }
  else if ((prereleaseA == "") && (prereleaseB == ""))
  {
    *isLessThan = false;  // A and B are equal
    return false;
  }
  // Both are prereleases of the same version

  if (IsPrereleaseLessThan(prereleaseA, prereleaseB))
  {
    *isLessThan = true;
    return false;
  }
  else
  {
    *isLessThan = false;
    return false;
  }
  return false;  // should not get here
}

int ParseSemVer(std::string const & version, int * const major,
                int * const minor, int * const patch,
                std::string * const prerelease,
                std::string * const buildMetadata)
{
  HAS_ANCILLARY hasAncillary = HAS_NONE;
  std::string majorStr;
  std::string minorStr;
  std::string patchStr;
  std::string prereleaseStr;
  std::string buildMetadataStr;

  size_t found;
  found = version.find_first_of('+');
  if (found != std::string::npos)
  {
    hasAncillary = HAS_BUILD_METADATA_ONLY;
  }
  size_t pre = version.find_first_of('-');
  if ((pre != std::string::npos) && (pre < found))
  {
    if (hasAncillary == HAS_BUILD_METADATA_ONLY)
      hasAncillary = HAS_BOTH;
    else
      hasAncillary = HAS_PRERELEASE_ONLY;
  }

  std::string subStr;
  std::string remainderStr;
  // Find Major
  found = version.find_first_of('.');
  if (found == std::string::npos)
  {
    return true;  // Missing Minor and Patch
  }
  majorStr = version.substr(0, found);
  remainderStr = version.substr(found+1);
  // Find Minor
  found = remainderStr.find_first_of('.');
  if (found == std::string::npos)
  {
    return true;  // Missing Patch
  }
  minorStr = remainderStr.substr(0, found);
  remainderStr = remainderStr.substr(found+1);
  // Find Patch
  switch (hasAncillary)
  {
    case HAS_NONE:
      patchStr = remainderStr;
      break;
    case HAS_PRERELEASE_ONLY:
      found = remainderStr.find_first_of('-');  // Will succeed
      patchStr = remainderStr.substr(0, found);
      prereleaseStr = remainderStr.substr(found+1);
      break;
    case HAS_BUILD_METADATA_ONLY:
      found = remainderStr.find_first_of('+');  // Will succeed
      patchStr = remainderStr.substr(0, found);
      buildMetadataStr = remainderStr.substr(found+1);
      break;
    case HAS_BOTH:
      found = remainderStr.find_first_of('-');  // Will succeed
      patchStr = remainderStr.substr(0, found);
      remainderStr = remainderStr.substr(found+1);
      found = remainderStr.find_first_of('+');  // Will succeed
      prereleaseStr = remainderStr.substr(0, found);
      buildMetadataStr = remainderStr.substr(found+1);
      break;
  }
  remainderStr = "";

  // validate major
  if ((majorStr[0] == '0') && (majorStr.length() > 1))
  {
    return true;  // Has leading zeros
  }
  char * end;
  int verNumber = strtol(majorStr.c_str(), &end, 10);
  if ('\0' != *end)
  {
    return true;  // Not a valid integer
  }
  if (major != NULL)
  {
    *major = verNumber;
  }

  // validate minor
  if ((minorStr[0] == '0') && (minorStr.length() > 1))
  {
    return true;  // Has leading zeros
  }
  verNumber = strtol(minorStr.c_str(), &end, 10);
  if ('\0' != *end)
  {
    return true;  // Not a valid integer
  }
  if (minor != NULL)
  {
    *minor = verNumber;
  }

  // validate patch
  if ((patchStr[0] == '0') && (patchStr.length() > 1))
  {
    return true;  // Has leading zeros
  }
  verNumber = strtol(patchStr.c_str(), &end, 10);
  if ('\0' != *end)
  {
    return true;  // Not a valid integer
  }
  if (patch != NULL)
  {
    *patch = verNumber;
  }

  // validate prerelease
  if ((hasAncillary == HAS_PRERELEASE_ONLY) ||
      (hasAncillary == HAS_BOTH))
  {
    if (ParseIdentifiers(PRERELEASE, prereleaseStr, NULL))
    {
      return true;  // Prerelease is invalid
    }
  }
  if (prerelease != NULL)
  {
    *prerelease = prereleaseStr;
  }

  // validate buildMetadata
  if ((hasAncillary == HAS_BUILD_METADATA_ONLY) ||
      (hasAncillary == HAS_BOTH))
  {
    if (ParseIdentifiers(BUILD_METADATA, buildMetadataStr, NULL))
    {
      return true;  // Build Metadata is invalid
    }
  }
  if (buildMetadata != NULL)
  {
    *buildMetadata = buildMetadataStr;
  }

  return false;
}
}  // namespace SEM_VER
}  // namespace KIM
