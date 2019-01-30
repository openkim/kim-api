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
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_DATA_TYPE_H_
#define KIM_DATA_TYPE_H_

/**
 ** \brief \copybrief KIM::DataType
 **
 ** \sa KIM::DataType
 **
 ** \since 2.0
 **/
struct KIM_DataType
{
  /**
   ** \brief \copybrief KIM::DataType::dataTypeID
   **
   ** \sa KIM::DataType::dataTypeID
   **
   ** \since 2.0
   **/
  int dataTypeID;
};
#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
typedef struct KIM_DataType KIM_DataType;
#endif

/**
 ** \brief \copybrief KIM::DataType::DataType(std::string const &)
 **
 ** \sa KIM::DataType::DataType(std::string const &)
 **
 ** \since 2.0
 **/
KIM_DataType KIM_DataType_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::DataType::Known
 **
 ** \sa KIM::DataType::Known
 **
 ** \since 2.0
 **/
int KIM_DataType_Known(KIM_DataType const dataType);

/**
 ** \brief \copybrief KIM::DataType::operator==()
 **
 ** \sa KIM::DataType::operator==()
 **
 ** \since 2.0
 **/
int KIM_DataType_Equal(KIM_DataType const lhs, KIM_DataType const rhs);

/**
 ** \brief \copybrief KIM::DataType::operator!=()
 **
 ** \sa KIM::DataType::operator!=()
 **
 ** \since 2.0
 **/
int KIM_DataType_NotEqual(KIM_DataType const lhs, KIM_DataType const rhs);

/**
 ** \brief \copybrief KIM::DataType::ToString
 **
 ** \sa KIM::DataType::ToString
 **
 ** \since 2.0
 **/
char const * KIM_DataType_ToString(KIM_DataType const dataType);


/**
 ** \brief \copybrief KIM::DATA_TYPE::Integer
 **
 ** \sa KIM::DATA_TYPE::Integer
 **
 ** \since 2.0
 **/
extern KIM_DataType const KIM_DATA_TYPE_Integer;

/**
 ** \brief \copybrief KIM::DATA_TYPE::Double
 **
 ** \sa KIM::DATA_TYPE::Double
 **
 ** \since 2.0
 **/
extern KIM_DataType const KIM_DATA_TYPE_Double;

/**
 ** \brief \copybrief KIM::DATA_TYPE::GetNumberOfDataTypes
 **
 ** \sa KIM::DATA_TYPE::GetNumberOfDataTypes
 **
 ** \since 2.0
 **/
void KIM_DATA_TYPE_GetNumberOfDataTypes(int * const numberOfDataTypes);

/**
 ** \brief \copybrief KIM::DATA_TYPE::GetDataType
 **
 ** \sa KIM::DATA_TYPE::GetDataType
 **
 ** \since 2.0
 **/
int KIM_DATA_TYPE_GetDataType(int const index, KIM_DataType * const dataType);

#endif /* KIM_DATA_TYPE_H_ */
