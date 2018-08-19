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


#ifndef KIM_COMPUTE_CALLBACK_NAME_H_
#define KIM_COMPUTE_CALLBACK_NAME_H_

struct KIM_ComputeCallbackName
{
  int computeCallbackNameID;
};
#ifndef KIM_COMPUTE_CALLBACK_NAME_DEFINED_
#define KIM_COMPUTE_CALLBACK_NAME_DEFINED_
typedef struct KIM_ComputeCallbackName KIM_ComputeCallbackName;
#endif

KIM_ComputeCallbackName KIM_ComputeCallbackName_FromString(
    char const * const str);

int KIM_ComputeCallbackName_Equal(KIM_ComputeCallbackName const left,
                                  KIM_ComputeCallbackName const right);
int KIM_ComputeCallbackName_NotEqual(KIM_ComputeCallbackName const left,
                                     KIM_ComputeCallbackName const right);

char const * KIM_ComputeCallbackName_String(
    KIM_ComputeCallbackName const computeCallbackName);

extern KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_GetNeighborList;
extern KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm;
extern
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term;

void KIM_COMPUTE_CALLBACK_NAME_GetNumberOfComputeCallbackNames(
    int * const numberOfComputeCallbackNames);
int KIM_COMPUTE_CALLBACK_NAME_GetComputeCallbackName(
    int const index, KIM_ComputeCallbackName * const computeCallbackName);

#endif  /* KIM_COMPUTE_CALLBACK_NAME_H_ */
