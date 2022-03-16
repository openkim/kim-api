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


#include <string>

#ifndef KIM_COLLECTION_ITEM_TYPE_HPP_
#include "KIM_CollectionItemType.hpp"
#endif
extern "C" {
#ifndef KIM_COLLECTION_ITEM_TYPE_H_
#include "KIM_CollectionItemType.h"
#endif
}  // extern "C"


namespace
{
KIM::CollectionItemType
makeCollectionItemTypeCpp(KIM_CollectionItemType const collectionItemType)
{
  KIM::CollectionItemType const * const collectionItemTypeCpp
      = reinterpret_cast<KIM::CollectionItemType const *>(&collectionItemType);
  return *collectionItemTypeCpp;
}

KIM_CollectionItemType
makeCollectionItemTypeC(KIM::CollectionItemType const collectionItemType)
{
  KIM_CollectionItemType const * const collectionItemTypeC
      = reinterpret_cast<KIM_CollectionItemType const *>(&collectionItemType);
  return *collectionItemTypeC;
}
}  // namespace

extern "C" {
KIM_CollectionItemType KIM_CollectionItemType_FromString(char const * const str)
{
  return makeCollectionItemTypeC(KIM::CollectionItemType(std::string(str)));
}

int KIM_CollectionItemType_Known(
    KIM_CollectionItemType const collectionItemType)
{
  return makeCollectionItemTypeCpp(collectionItemType).Known();
}

int KIM_CollectionItemType_Equal(KIM_CollectionItemType const lhs,
                                 KIM_CollectionItemType const rhs)
{
  return (lhs.collectionItemTypeID == rhs.collectionItemTypeID);
}

int KIM_CollectionItemType_NotEqual(KIM_CollectionItemType const lhs,
                                    KIM_CollectionItemType const rhs)
{
  return (!KIM_CollectionItemType_Equal(lhs, rhs));
}

char const *
KIM_CollectionItemType_ToString(KIM_CollectionItemType const collectionItemType)
{
  return makeCollectionItemTypeCpp(collectionItemType).ToString().c_str();
}

#include "KIM_CollectionItemType.inc"
KIM_CollectionItemType const KIM_COLLECTION_ITEM_TYPE_modelDriver
    = {ID_modelDriver};
KIM_CollectionItemType const KIM_COLLECTION_ITEM_TYPE_portableModel
    = {ID_portableModel};
KIM_CollectionItemType const KIM_COLLECTION_ITEM_TYPE_simulatorModel
    = {ID_simulatorModel};

void KIM_COLLECTION_ITEM_TYPE_GetNumberOfCollectionItemTypes(
    int * const numberOfCollectionItemTypes)
{
  KIM::COLLECTION_ITEM_TYPE::GetNumberOfCollectionItemTypes(
      numberOfCollectionItemTypes);
}

int KIM_COLLECTION_ITEM_TYPE_GetCollectionItemType(
    int const index, KIM_CollectionItemType * const collectionItemType)
{
  KIM::CollectionItemType collectionItemTypeCpp;
  int error = KIM::COLLECTION_ITEM_TYPE::GetCollectionItemType(
      index, &collectionItemTypeCpp);
  if (error) return error;
  *collectionItemType = makeCollectionItemTypeC(collectionItemTypeCpp);
  return false;
}

}  // extern "C"
