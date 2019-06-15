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
// Release: This file is part of the kim-api.git repository.
//

#include <map>

#ifndef KIM_COLLECTION_HPP_
#include "KIM_Collection.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace COLLECTION
{
#include "KIM_Collection.inc"
Collection const system(ID_system);
Collection const user(ID_user);
Collection const environmentVariable(ID_environmentVariable);
Collection const currentWorkingDirectory(ID_currentWorkingDirectory);

namespace
{
typedef std::map<Collection const, std::string, COLLECTION::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[system] = "system";
  m[user] = "user";
  m[environmentVariable] = "environmentVariable";
  m[currentWorkingDirectory] = "currentWorkingDirectory";
  return m;
}

StringMap const collectionToString = GetStringMap();
std::string const collectionUnknown("unknown");
}  // namespace


void GetNumberOfCollections(int * const numberOfCollections)
{
  *numberOfCollections = collectionToString.size();
}

int GetCollection(int const index, Collection * const collection)
{
  int numberOfCollections;
  GetNumberOfCollections(&numberOfCollections);
  if ((index < 0) || (index >= numberOfCollections)) return true;

  StringMap::const_iterator iter = collectionToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *collection = iter->first;
  return false;  // no error
}
}  // namespace COLLECTION

// implementation of Collection
Collection::Collection() {}
Collection::Collection(int const id) : collectionID(id) {}
Collection::Collection(std::string const & str)
{
  collectionID = -1;
  for (COLLECTION::StringMap::const_iterator iter
       = COLLECTION::collectionToString.begin();
       iter != COLLECTION::collectionToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      collectionID = (iter->first).collectionID;
      break;
    }
  }
}

bool Collection::Known() const
{
  int numberOfCollections;
  COLLECTION::GetNumberOfCollections(&numberOfCollections);

  for (int i = 0; i < numberOfCollections; ++i)
  {
    Collection col;
    COLLECTION::GetCollection(i, &col);

    if (*this == col) { return true; }
  }

  return false;
}

bool Collection::operator==(Collection const & rhs) const
{
  return collectionID == rhs.collectionID;
}
bool Collection::operator!=(Collection const & rhs) const
{
  return collectionID != rhs.collectionID;
}

std::string const & Collection::ToString() const
{
  COLLECTION::StringMap::const_iterator iter
      = COLLECTION::collectionToString.find(*this);
  if (iter == COLLECTION::collectionToString.end())
    return COLLECTION::collectionUnknown;
  else
    return iter->second;
}
}  // namespace KIM
