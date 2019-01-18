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


#ifndef KIM_MODEL_ROUTINE_NAME_H_
#define KIM_MODEL_ROUTINE_NAME_H_

struct KIM_ModelRoutineName
{
  int modelRoutineNameID;
};
#ifndef KIM_MODEL_ROUTINE_NAME_DEFINED_
#define KIM_MODEL_ROUTINE_NAME_DEFINED_
typedef struct KIM_ModelRoutineName KIM_ModelRoutineName;
#endif

KIM_ModelRoutineName KIM_ModelRoutineName_FromString(char const * const str);
int KIM_ModelRoutineName_Known(KIM_ModelRoutineName const modelRoutineName);
int KIM_ModelRoutineName_Equal(KIM_ModelRoutineName const lhs,
                               KIM_ModelRoutineName const rhs);
int KIM_ModelRoutineName_NotEqual(KIM_ModelRoutineName const lhs,
                                  KIM_ModelRoutineName const rhs);
char const *
KIM_ModelRoutineName_ToString(KIM_ModelRoutineName const modelRoutineName);

extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Create;
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate;
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Compute;
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Extension;
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Refresh;
extern KIM_ModelRoutineName const
    KIM_MODEL_ROUTINE_NAME_WriteParameterizedModel;
extern KIM_ModelRoutineName const
    KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy;
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Destroy;

void KIM_MODEL_ROUTINE_NAME_GetNumberOfModelRoutineNames(
    int * const numberOfModelRoutineNames);
int KIM_MODEL_ROUTINE_NAME_GetModelRoutineName(
    int const index, KIM_ModelRoutineName * const modelRoutineName);

#endif /* KIM_MODEL_ROUTINE_NAME_H_ */
