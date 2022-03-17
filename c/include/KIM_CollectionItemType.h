/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2022, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */
/* SPDX-License-Identifier: LGPL-2.1-or-later                                 */
/*                                                                            */
/* This library is free software; you can redistribute it and/or              */
/* modify it under the terms of the GNU Lesser General Public                 */
/* License as published by the Free Software Foundation; either               */
/* version 2.1 of the License, or (at your option) any later version.         */
/*                                                                            */
/* This library is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          */
/* Lesser General Public License for more details.                            */
/*                                                                            */
/* You should have received a copy of the GNU Lesser General Public License   */
/* along with this library; if not, write to the Free Software Foundation,    */
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_COLLECTION_ITEM_TYPE_H_
#define KIM_COLLECTION_ITEM_TYPE_H_

/**
 ** \brief \copybrief KIM::CollectionItemType
 **
 ** \sa KIM::CollectionItemType,
 ** kim_collection_module::kim_collection_item_type_type
 **
 ** \since 2.1
 **/
struct KIM_CollectionItemType
{
  /**
   ** \brief \copybrief KIM::CollectionItemType::collectionItemTypeID
   **
   ** \sa KIM::CollectionItemType::collectionItemTypeID,
   ** kim_collection_module::kim_collection_type::collectionItemType_id
   **
   ** \since 2.1
   **/
  int collectionItemTypeID;
};
#ifndef KIM_COLLECTION_ITEM_TYPE_DEFINED_
#define KIM_COLLECTION_ITEM_TYPE_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.1
 **/
typedef struct KIM_CollectionItemType KIM_CollectionItemType;
#endif

/**
 ** \brief \copybrief KIM::CollectionItemType::CollectionItemType(<!---------
 ** -->std::string const &)
 **
 ** \sa KIM::CollectionItemType::CollectionItemType(std::string const &),
 ** kim_collection_item_type_module::kim_from_string
 **
 ** \since 2.1
 **/
KIM_CollectionItemType
KIM_CollectionItemType_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::CollectionItemType::Known
 **
 ** \sa KIM::CollectionItemType::Known,
 ** kim_collection_item_type_module::kim_known
 **
 ** \since 2.1
 **/
int KIM_CollectionItemType_Known(
    KIM_CollectionItemType const collectionItemType);

/**
 ** \brief \copybrief KIM::CollectionItemType::operator==()
 **
 ** \sa KIM::CollectionItemType::operator==(),
 ** kim_collection_item_type_module::operator(.eq.)
 **
 ** \since 2.1
 **/
int KIM_CollectionItemType_Equal(KIM_CollectionItemType const lhs,
                                 KIM_CollectionItemType const rhs);

/**
 ** \brief \copybrief KIM::CollectionItemType::operator!=()
 **
 ** \sa KIM::CollectionItemType::operator!=(),
 ** kim_collection_item_type_module::operator(.ne.)
 **
 ** \since 2.1
 **/
int KIM_CollectionItemType_NotEqual(KIM_CollectionItemType const lhs,
                                    KIM_CollectionItemType const rhs);

/**
 ** \brief \copybrief KIM::CollectionItemType::ToString
 **
 ** \sa KIM::CollectionItemType::ToString,
 ** kim_collection_item_type_module::kim_to_string
 **
 ** \since 2.1
 **/
char const * KIM_CollectionItemType_ToString(
    KIM_CollectionItemType const collectionItemType);

/**
 ** \brief \copybrief KIM::COLLECTION_ITEM_TYPE::modelDriver
 **
 ** \sa KIM::COLLECTION_ITEM_TYPE::modelDriver,
 ** kim_collection_item_type_module::kim_collection_item_type_model_driver
 **
 ** \since 2.1
 **/
extern KIM_CollectionItemType const KIM_COLLECTION_ITEM_TYPE_modelDriver;

/**
 ** \brief \copybrief KIM::COLLECTION_ITEM_TYPE::portableModel
 **
 ** \sa KIM::COLLECTION_ITEM_TYPE::portableModel,
 ** kim_collection_item_type_module::kim_collection_item_type_portable_model
 **
 ** \since 2.1
 **/
extern KIM_CollectionItemType const KIM_COLLECTION_ITEM_TYPE_portableModel;

/**
 ** \brief \copybrief KIM::COLLECTION_ITEM_TYPE::simulatorModel
 **
 ** \sa KIM::COLLECTION_ITEM_TYPE::simulatorModel,
 ** kim_collection_item_type_module::kim_collection_item_type_simulator_model
 **
 ** \since 2.1
 **/
extern KIM_CollectionItemType const KIM_COLLECTION_ITEM_TYPE_simulatorModel;

/**
 ** \brief \copybrief KIM::COLLECTION_ITEM_TYPE::GetNumberOfCollectionItemTypes
 **
 ** \sa KIM::COLLECTION_ITEM_TYPE::GetNumberOfCollectionItemTypes,
 ** kim_collection_item_type_module::kim_get_number_of_collection_item_types
 **
 ** \since 2.1
 **/
void KIM_COLLECTION_ITEM_TYPE_GetNumberOfCollectionItemTypes(
    int * const numberOfCollectionItemTypes);

/**
 ** \brief \brief \copybrief KIM::COLLECTION_ITEM_TYPE::GetCollectionItemType
 **
 ** \sa KIM::COLLECTION_ITEM_TYPE::GetCollectionItemType,
 ** kim_collection_item_type_module::kim_get_collection_item_type
 **
 ** \since 2.1
 **/
int KIM_COLLECTION_ITEM_TYPE_GetCollectionItemType(
    int const index, KIM_CollectionItemType * const collectionItemType);

#endif /* KIM_COLLECTION_ITEM_TYPE_H_ */
