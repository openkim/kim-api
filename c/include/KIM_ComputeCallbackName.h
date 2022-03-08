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


#ifndef KIM_COMPUTE_CALLBACK_NAME_H_
#define KIM_COMPUTE_CALLBACK_NAME_H_

/**
 ** \brief \copybrief KIM::ComputeCallbackName
 **
 ** \sa KIM::ComputeCallbackName,
 ** kim_compute_callback_name_module::kim_compute_callback_name_type
 **
 ** \since 2.0
 **/
struct KIM_ComputeCallbackName
{
  /**
   ** \brief \copybrief KIM::ComputeCallbackName::computeCallbackNameID
   **
   ** \sa KIM::ComputeCallbackName::computeCallbackNameID,
   ** kim_compute_callback_name_module::kim_compute_callback_name_type::<!--
   ** -->compute_callback_name_id
   **
   ** \since 2.0
   **/
  int computeCallbackNameID;
};
#ifndef KIM_COMPUTE_CALLBACK_NAME_DEFINED_
#define KIM_COMPUTE_CALLBACK_NAME_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_ComputeCallbackName KIM_ComputeCallbackName;
#endif

/**
 ** \brief \copybrief <!--
 ** --> KIM::ComputeCallbackName::ComputeCallbackName(std::string const &)
 **
 ** \sa KIM::ComputeCallbackName::ComputeCallbackName(std::string const &),
 ** kim_compute_callback_name_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_ComputeCallbackName
KIM_ComputeCallbackName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::ComputeCallbackName::Known
 **
 ** \sa KIM::ComputeCallbackName::Known,
 ** kim_compute_callback_name_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_ComputeCallbackName_Known(
    KIM_ComputeCallbackName const computeCallbackName);

/**
 ** \brief \copybrief KIM::ComputeCallbackName::operator==()
 **
 ** \sa KIM::ComputeCallbackName::operator==(),
 ** kim_compute_callback_name_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_ComputeCallbackName_Equal(KIM_ComputeCallbackName const lhs,
                                  KIM_ComputeCallbackName const rhs);

/**
 ** \brief \copybrief KIM::ComputeCallbackName::operator!=()
 **
 ** \sa KIM::ComputeCallbackName::operator!=(),
 ** kim_compute_callback_name_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_ComputeCallbackName_NotEqual(KIM_ComputeCallbackName const lhs,
                                     KIM_ComputeCallbackName const rhs);

/**
 ** \brief \copybrief KIM::ComputeCallbackName::ToString
 **
 ** \sa KIM::ComputeCallbackName::ToString,
 ** kim_compute_callback_name_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_ComputeCallbackName_ToString(
    KIM_ComputeCallbackName const computeCallbackName);

/**
 ** \brief \copybrief KIM::COMPUTE_CALLBACK_NAME::GetNeighborList
 **
 ** \sa KIM::COMPUTE_CALLBACK_NAME::GetNeighborList,
 ** kim_compute_callback_name_module::<!--
 ** -->kim_compute_callback_name_get_neighbor_list
 **
 ** \since 2.0
 **/
extern KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_GetNeighborList;

/**
 ** \brief \copybrief KIM::COMPUTE_CALLBACK_NAME::ProcessDEDrTerm
 **
 ** \sa KIM::COMPUTE_CALLBACK_NAME::ProcessDEDrTerm,
 ** kim_compute_callback_name_module::<!--
 ** -->kim_compute_callback_name_process_dedr_term
 **
 ** \since 2.0
 **/
extern KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm;

/**
 ** \brief \copybrief KIM::COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term
 **
 ** \sa KIM::COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term,
 ** kim_compute_callback_name_module::<!--
 ** -->kim_compute_callback_name_process_d2edr2_term
 **
 ** \since 2.0
 **/
extern KIM_ComputeCallbackName const
    KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term;

/**
 ** \brief \copybrief <!--
 ** -> KIM::COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbackNames
 **
 ** \sa KIM::COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbackNames,
 ** kim_compute_callback_name_module::kim_get_number_of_compute_callback_names
 **
 ** \since 2.0
 **/
void KIM_COMPUTE_CALLBACK_NAME_GetNumberOfComputeCallbackNames(
    int * const numberOfComputeCallbackNames);

/**
 ** \brief \copybrief KIM::COMPUTE_CALLBACK_NAME::GetComputeCallbackName
 **
 ** \sa KIM::COMPUTE_CALLBACK_NAME::GetComputeCallbackName,
 ** kim_compute_callback_name_module::kim_get_compute_callback_name
 **
 ** \since 2.0
 **/
int KIM_COMPUTE_CALLBACK_NAME_GetComputeCallbackName(
    int const index, KIM_ComputeCallbackName * const computeCallbackName);

#endif /* KIM_COMPUTE_CALLBACK_NAME_H_ */
