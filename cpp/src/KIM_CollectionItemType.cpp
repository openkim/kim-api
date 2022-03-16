//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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

#include <map>

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace COLLECTION_ITEM_TYPE
{
#include "KIM_CollectionItemType.inc"
CollectionItemType const modelDriver(ID_modelDriver);
CollectionItemType const portableModel(ID_portableModel);
CollectionItemType const simulatorModel(ID_simulatorModel);

namespace
{
typedef std::
    map<CollectionItemType const, std::string, COLLECTION_ITEM_TYPE::Comparator>
        StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[modelDriver] = "modelDriver";
  m[portableModel] = "portableModel";
  m[simulatorModel] = "simulatorModel";
  return m;
}

StringMap const collectionItemTypeToString = GetStringMap();
std::string const collectionItemTypeUnknown("unknown");
}  // namespace


void GetNumberOfCollectionItemTypes(int * const numberOfCollectionItemTypes)
{
  *numberOfCollectionItemTypes = collectionItemTypeToString.size();
}

int GetCollectionItemType(int const index,
                          CollectionItemType * const collectionItemType)
{
  int numberOfCollectionItemTypes;
  GetNumberOfCollectionItemTypes(&numberOfCollectionItemTypes);
  if ((index < 0) || (index >= numberOfCollectionItemTypes)) return true;

  StringMap::const_iterator iter = collectionItemTypeToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *collectionItemType = iter->first;
  return false;  // no error
}
}  // namespace COLLECTION_ITEM_TYPE

// implementation of CollectionItemType
CollectionItemType::CollectionItemType() {}
CollectionItemType::CollectionItemType(int const id) : collectionItemTypeID(id)
{
}
CollectionItemType::CollectionItemType(std::string const & str)
{
  collectionItemTypeID = -1;
  for (COLLECTION_ITEM_TYPE::StringMap::const_iterator iter
       = COLLECTION_ITEM_TYPE::collectionItemTypeToString.begin();
       iter != COLLECTION_ITEM_TYPE::collectionItemTypeToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      collectionItemTypeID = (iter->first).collectionItemTypeID;
      break;
    }
  }
}

bool CollectionItemType::Known() const
{
  int numberOfCollectionItemTypes;
  COLLECTION_ITEM_TYPE::GetNumberOfCollectionItemTypes(
      &numberOfCollectionItemTypes);

  for (int i = 0; i < numberOfCollectionItemTypes; ++i)
  {
    CollectionItemType colItemType;
    COLLECTION_ITEM_TYPE::GetCollectionItemType(i, &colItemType);

    if (*this == colItemType) { return true; }
  }

  return false;
}

bool CollectionItemType::operator==(CollectionItemType const & rhs) const
{
  return collectionItemTypeID == rhs.collectionItemTypeID;
}
bool CollectionItemType::operator!=(CollectionItemType const & rhs) const
{
  return collectionItemTypeID != rhs.collectionItemTypeID;
}

std::string const & CollectionItemType::ToString() const
{
  COLLECTION_ITEM_TYPE::StringMap::const_iterator iter
      = COLLECTION_ITEM_TYPE::collectionItemTypeToString.find(*this);
  if (iter == COLLECTION_ITEM_TYPE::collectionItemTypeToString.end())
    return COLLECTION_ITEM_TYPE::collectionItemTypeUnknown;
  else
    return iter->second;
}
}  // namespace KIM
