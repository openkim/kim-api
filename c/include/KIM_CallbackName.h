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
/* Copyright (c) 2016--2017, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_CALLBACK_NAME_H_
#define KIM_CALLBACK_NAME_H_

struct KIM_CallbackName
{
  int callbackNameID;
};
#ifndef KIM_CALLBACK_NAME_DEFINED_
#define KIM_CALLBACK_NAME_DEFINED_
typedef struct KIM_CallbackName KIM_CallbackName;
#endif

KIM_CallbackName KIM_CallbackNameFromString(char const * const str);

int KIM_CallbackNameEqual(KIM_CallbackName const left,
                          KIM_CallbackName const right);
int KIM_CallbackNameNotEqual(KIM_CallbackName const left,
                             KIM_CallbackName const right);

char const * const KIM_CallbackNameString(KIM_CallbackName const callbackName);

extern KIM_CallbackName const KIM_CALLBACK_NAME_GetNeighborList;
extern KIM_CallbackName const KIM_CALLBACK_NAME_ProcessDEDrTerm;
extern KIM_CallbackName const KIM_CALLBACK_NAME_ProcessD2EDr2Term;

void KIM_CALLBACK_NAME_GetNumberOfCallbacks(int * const numberOfCallbacks);
int KIM_CALLBACK_NAME_GetCallbackName(
    int const index, KIM_CallbackName * const callbackName);

#endif  /* KIM_CALLBACK_NAME_H_ */
