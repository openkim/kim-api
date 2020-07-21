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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
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
