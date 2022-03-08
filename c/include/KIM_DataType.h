/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2021, Regents of the University of Minnesota.          */
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


#ifndef KIM_DATA_TYPE_H_
#define KIM_DATA_TYPE_H_

/**
 ** \brief \copybrief KIM::DataType
 **
 ** \sa KIM::DataType, kim_data_type_module::kim_data_type_type
 **
 ** \since 2.0
 **/
struct KIM_DataType
{
  /**
   ** \brief \copybrief KIM::DataType::dataTypeID
   **
   ** \sa KIM::DataType::dataTypeID,
   ** kim_data_type_module::kim_data_type_type::data_type_id
   **
   ** \since 2.0
   **/
  int dataTypeID;
};
#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_DataType KIM_DataType;
#endif

/**
 ** \brief \copybrief KIM::DataType::DataType(std::string const &)
 **
 ** \sa KIM::DataType::DataType(std::string const &),
 ** kim_data_type_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_DataType KIM_DataType_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::DataType::Known
 **
 ** \sa KIM::DataType::Known, kim_data_type_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_DataType_Known(KIM_DataType const dataType);

/**
 ** \brief \copybrief KIM::DataType::operator==()
 **
 ** \sa KIM::DataType::operator==(), kim_data_type_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_DataType_Equal(KIM_DataType const lhs, KIM_DataType const rhs);

/**
 ** \brief \copybrief KIM::DataType::operator!=()
 **
 ** \sa KIM::DataType::operator!=(), kim_data_type_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_DataType_NotEqual(KIM_DataType const lhs, KIM_DataType const rhs);

/**
 ** \brief \copybrief KIM::DataType::ToString
 **
 ** \sa KIM::DataType::ToString, kim_data_type_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_DataType_ToString(KIM_DataType const dataType);


/**
 ** \brief \copybrief KIM::DATA_TYPE::Integer
 **
 ** \sa KIM::DATA_TYPE::Integer, kim_data_type_module::kim_data_type_integer
 **
 ** \since 2.0
 **/
extern KIM_DataType const KIM_DATA_TYPE_Integer;

/**
 ** \brief \copybrief KIM::DATA_TYPE::Double
 **
 ** \sa KIM::DATA_TYPE::Double, kim_data_type_module::kim_data_type_double
 **
 ** \since 2.0
 **/
extern KIM_DataType const KIM_DATA_TYPE_Double;

/**
 ** \brief \copybrief KIM::DATA_TYPE::GetNumberOfDataTypes
 **
 ** \sa KIM::DATA_TYPE::GetNumberOfDataTypes,
 ** kim_data_type_module::kim_get_number_of_data_types
 **
 ** \since 2.0
 **/
void KIM_DATA_TYPE_GetNumberOfDataTypes(int * const numberOfDataTypes);

/**
 ** \brief \copybrief KIM::DATA_TYPE::GetDataType
 **
 ** \sa KIM::DATA_TYPE::GetDataType, kim_data_type_module::kim_get_data_type
 **
 ** \since 2.0
 **/
int KIM_DATA_TYPE_GetDataType(int const index, KIM_DataType * const dataType);

#endif /* KIM_DATA_TYPE_H_ */
