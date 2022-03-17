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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <string>

#ifndef KIM_COLLECTION_HPP_
#include "KIM_Collection.hpp"
#endif
extern "C" {
#ifndef KIM_COLLECTION_H_
#include "KIM_Collection.h"
#endif
}  // extern "C"


namespace
{
KIM::Collection makeCollectionCpp(KIM_Collection const collection)
{
  KIM::Collection const * const collectionCpp
      = reinterpret_cast<KIM::Collection const *>(&collection);
  return *collectionCpp;
}

KIM_Collection makeCollectionC(KIM::Collection const collection)
{
  KIM_Collection const * const collectionC
      = reinterpret_cast<KIM_Collection const *>(&collection);
  return *collectionC;
}
}  // namespace

extern "C" {
KIM_Collection KIM_Collection_FromString(char const * const str)
{
  return makeCollectionC(KIM::Collection(std::string(str)));
}

int KIM_Collection_Known(KIM_Collection const collection)
{
  return makeCollectionCpp(collection).Known();
}

int KIM_Collection_Equal(KIM_Collection const lhs, KIM_Collection const rhs)
{
  return (lhs.collectionID == rhs.collectionID);
}

int KIM_Collection_NotEqual(KIM_Collection const lhs, KIM_Collection const rhs)
{
  return (!KIM_Collection_Equal(lhs, rhs));
}

char const * KIM_Collection_ToString(KIM_Collection const collection)
{
  return makeCollectionCpp(collection).ToString().c_str();
}

#include "KIM_Collection.inc"
KIM_Collection const KIM_COLLECTION_system = {ID_system};
KIM_Collection const KIM_COLLECTION_user = {ID_user};
KIM_Collection const KIM_COLLECTION_environmentVariable
    = {ID_environmentVariable};
KIM_Collection const KIM_COLLECTION_currentWorkingDirectory
    = {ID_currentWorkingDirectory};

void KIM_COLLECTION_GetNumberOfCollections(int * const numberOfCollections)
{
  KIM::COLLECTION::GetNumberOfCollections(numberOfCollections);
}

int KIM_COLLECTION_GetCollection(int const index,
                                 KIM_Collection * const collection)
{
  KIM::Collection collectionCpp;
  int error = KIM::COLLECTION::GetCollection(index, &collectionCpp);
  if (error) return error;
  *collection = makeCollectionC(collectionCpp);
  return false;
}

}  // extern "C"
