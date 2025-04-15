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
// Release: This file is part of the kim-api-2.4.1 package.
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
