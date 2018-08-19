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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
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

struct KIM_SupportStatus
{
  int supportStatusID;
};
#ifndef KIM_SUPPORT_STATUS_DEFINED_
#define KIM_SUPPORT_STATUS_DEFINED_
typedef struct KIM_SupportStatus KIM_SupportStatus;
#endif

KIM_SupportStatus KIM_SupportStatus_FromString(char const * const str);

int KIM_SupportStatus_Equal(KIM_SupportStatus const left,
                            KIM_SupportStatus const right);
int KIM_SupportStatus_NotEqual(KIM_SupportStatus const left,
                               KIM_SupportStatus const right);
char const * KIM_SupportStatus_String(KIM_SupportStatus const supportStatus);

extern KIM_SupportStatus const KIM_SUPPORT_STATUS_requiredByAPI;
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_notSupported;
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_required;
extern KIM_SupportStatus const KIM_SUPPORT_STATUS_optional;

void KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses(
    int * const numberOfSupportStatuses);
int KIM_SUPPORT_STATUS_GetSupportStatus(
    int const index,
    KIM_SupportStatus * const supportStatus);

#endif  /* KIM_SUPPORT_STATUS_H_ */
