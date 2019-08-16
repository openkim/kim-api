//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common
// Development and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name
// LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner].
// All rights reserved.
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
// Release: This file is part of the kim-api-2.1.3 package.
//

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
