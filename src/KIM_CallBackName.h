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


#ifndef KIM_CALL_BACK_NAME_H_
#define KIM_CALL_BACK_NAME_H_

struct KIM_CallBackName
{
  int callBackNameID;
};
#ifndef KIM_CALL_BACK_NAME_DEFINED_
#define KIM_CALL_BACK_NAME_DEFINED_
typedef struct KIM_CallBackName KIM_CallBackName;
#endif

char const * const KIM_CallBackNameString(KIM_CallBackName const callBackName);

extern KIM_CallBackName const KIM_CALL_BACK_NAME_get_neigh;
extern KIM_CallBackName const KIM_CALL_BACK_NAME_process_dEdr;
extern KIM_CallBackName const KIM_CALL_BACK_NAME_process_d2Edr2;

void KIM_CALL_BACK_NAME_get_number_of_call_backs(int * const numberOfCallBacks);
int KIM_CALL_BACK_NAME_get_call_back_name(
    int const index, KIM_CallBackName * const callBackName);

#endif  /* KIM_CALL_BACK_NAME_H_ */
