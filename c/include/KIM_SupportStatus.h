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


#ifndef KIM_SUPPORT_STATUS_H_
#define KIM_SUPPORT_STATUS_H_

/**
 ** \brief \copybrief KIM::SupportStatus
 **
 ** \sa KIM::SupportStatus
 **
 ** \since 2.0
 **/
struct KIM_SupportStatus
{
  /**
   ** \brief \copybrief KIM::SupportStatus::supportStatusID
   **
   ** \sa KIM::SupportStatus::supportStatusID
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
 ** \sa KIM::SupportStatus::SupportStatus(std::string const &)
 **
 ** \since 2.0
 **/
KIM_SupportStatus KIM_SupportStatus_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::SupportStatus::Known
 **
 ** \sa KIM::SupportStatus::Known
 **
 ** \since 2.0
 **/
int KIM_SupportStatus_Known(KIM_SupportStatus const supportStatus);

/**
 ** \brief \copybrief KIM::SupportStatus::operator==()
 **
 ** \sa KIM::SupportStatus::operator==()
 **
 ** \since 2.0
 **/
int KIM_SupportStatus_Equal(KIM_SupportStatus const lhs,
                            KIM_SupportStatus const rhs);

/**
 ** \brief \copybrief KIM::SupportStatus::operator!=()
 **
 ** \sa KIM::SupportStatus::operator!=()
 **
 ** \since 2.0
 **/
int KIM_SupportStatus_NotEqual(KIM_SupportStatus const lhs,
                               KIM_SupportStatus const rhs);

/**
 ** \brief \copybrief KIM::SupportStatus::ToString
 **
 ** \sa KIM::SupportStatus::ToString
 **
 ** \since 2.0
 **/
char const * KIM_SupportStatus_ToString(KIM_SupportStatus const supportStatus);

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::requiredByAPI
 **
 ** \sa KIM::SUPPORT_STATUS::requiredByAPI
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_requiredByAPI;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::notSupported
 **
 ** \sa KIM::SUPPORT_STATUS::notSupported
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_notSupported;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::required
 **
 ** \sa KIM::SUPPORT_STATUS::required
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_required;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::optional
 **
 ** \sa KIM::SUPPORT_STATUS::optional
 **
 ** \since 2.0
 **/
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_optional;

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses
 **
 ** \sa KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses
 **
 ** \since 2.0
 **/
void KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses(
    int * const numberOfSupportStatuses);

/**
 ** \brief \copybrief KIM::SUPPORT_STATUS::GetSupportStatus
 **
 ** \sa KIM::SUPPORT_STATUS::GetSupportStatus
 **
 ** \since 2.0
 **/
int KIM_SUPPORT_STATUS_GetSupportStatus(
    int const index, KIM_SupportStatus * const supportStatus);

#endif /* KIM_SUPPORT_STATUS_H_ */
