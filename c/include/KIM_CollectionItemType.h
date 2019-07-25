/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api-2.1.1 package.                   */
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
