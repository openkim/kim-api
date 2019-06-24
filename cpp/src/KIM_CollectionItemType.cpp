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
