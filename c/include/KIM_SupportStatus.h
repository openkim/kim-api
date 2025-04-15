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
/* Release: This file is part of the kim-api-2.4.1 package.                   */
/*                                                                            */


#ifndef KIM_SUPPORT_STATUS_H_
#define KIM_SUPPORT_STATUS_H_

/**
 ** \brief \copybrief KIM::SupportStatus
 **
 ** \sa KIM::SupportStatus, kim_support_status_module::kim_support_status_type
 **
 ** \since 2.0
 **/
struct KIM_SupportStatus
{
  /**
   ** \brief \copybrief KIM::SupportStatus::supportStatusID
   **
   ** \sa KIM::SupportStatus::supportStatusID,
   ** kim_support_status_module::kim_support_status_type::support_status_id
   **
   ** \since 2.0
   **/
  int supportStatusID;
};
#ifndef KIM_SUPPORT_STATUS_DEFINED_
#define KIM_SUPPORT_STATUS_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_SupportStatus KIM_SupportStatus;
#endif

/**
 ** \brief \copybrief KIM::SupportStatus::SupportStatus(std::string const &)
 **
 ** \sa KIM::SupportStatus::SupportStatus(std::string const &),
 ** kim_support_status_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_SupportStatus KIM_SupportStatus_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::SupportStatus::Known
 **
 ** \sa KIM::SupportStatus::Known, kim_support_status_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_SupportStatus_Known(KIM_SupportStatus const supportStatus);

/**
 ** \brief \copybrief KIM::SupportStatus::operator==()
 **
 ** \sa KIM::SupportStatus::operator==(),
 ** kim_support_status_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_SupportStatus_Equal(KIM_SupportStatus const lhs,
                            KIM_SupportStatus const rhs);

/**
 ** \brief \copybrief KIM::SupportStatus::operator!=()
 **
 ** \sa KIM::SupportStatus::operator!=(),
 ** kim_support_status_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_SupportStatus_NotEqual(KIM_SupportStatus const lhs,
                               KIM_SupportStatus const rhs);

/**
 ** \brief \copybrief KIM::SupportStatus::ToString
 **
 ** \sa KIM::SupportStatus::ToString, kim_support_status_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_SupportStatus_ToString(KIM_SupportStatus const supportStatus);

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::requiredByAPI
 **
 ** \sa KIM::SUPPORT_STATUS::requiredByAPI,
 ** kim_support_status_module::kim_support_status_required_by_api
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_requiredByAPI;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::notSupported
 **
 ** \sa KIM::SUPPORT_STATUS::notSupported,
 ** kim_support_status_module::kim_support_status_not_supported
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_notSupported;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::required
 **
 ** \sa KIM::SUPPORT_STATUS::required,
 ** kim_support_status_module::kim_support_status_required
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_required;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::optional
 **
 ** \sa KIM::SUPPORT_STATUS::optional,
 ** kim_support_status_module::kim_support_status_optional
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_optional;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses
 **
 ** \sa KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses,
 ** kim_support_status_module::kim_get_number_of_support_statuses
 **
 ** \since 2.0
 **/
void KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses(
    int * const numberOfSupportStatuses);

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::GetSupportStatus
 **
 ** \sa KIM::SUPPORT_STATUS::GetSupportStatus,
 ** kim_support_status_module::kim_get_support_status
 **
 ** \since 2.0
 **/
int KIM_SUPPORT_STATUS_GetSupportStatus(
    int const index, KIM_SupportStatus * const supportStatus);

#endif /* KIM_SUPPORT_STATUS_H_ */
