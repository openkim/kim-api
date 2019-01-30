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

/**
 ** \brief \copybrief KIM::ModelRoutineName
 **
 ** \sa KIM::ModelRoutineName
 **
 ** \since 2.0
 **/
struct KIM_ModelRoutineName
{
  /**
   ** \brief \copybrief KIM::ModelRoutineName::modelRoutineNameID
   **
   ** \sa KIM::ModelRoutineName::modelRoutineNameID
   **
   ** \since 2.0
   **/
  int modelRoutineNameID;
};
#ifndef KIM_MODEL_ROUTINE_NAME_DEFINED_
#define KIM_MODEL_ROUTINE_NAME_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelRoutineName KIM_ModelRoutineName;
#endif

/**
 ** \brief \copybrief <!--
 ** --> KIM::ModelRoutineName::ModelRoutineName(std::string const &)
 **
 ** \sa KIM::ModelRoutineName::ModelRoutineName(std::string const &)
 **
 ** \since 2.0
 **/
KIM_ModelRoutineName KIM_ModelRoutineName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::ModelRoutineName::Known
 **
 ** \sa KIM::ModelRoutineName::Known
 **
 ** \since 2.0
 **/
int KIM_ModelRoutineName_Known(KIM_ModelRoutineName const modelRoutineName);

/**
 ** \brief \copybrief KIM::ModelRoutineName::operator==()
 **
 ** \sa KIM::ModelRoutineName::operator==()
 **
 ** \since 2.0
 **/
int KIM_ModelRoutineName_Equal(KIM_ModelRoutineName const lhs,
                               KIM_ModelRoutineName const rhs);

/**
 ** \brief \copybrief KIM::ModelRoutineName::operator!=()
 **
 ** \sa KIM::ModelRoutineName::operator!=()
 **
 ** \since 2.0
 **/
int KIM_ModelRoutineName_NotEqual(KIM_ModelRoutineName const lhs,
                                  KIM_ModelRoutineName const rhs);

/**
 ** \brief \copybrief KIM::ModelRoutineName::ToString
 **
 ** \sa KIM::ModelRoutineName::ToString
 **
 ** \since 2.0
 **/
char const *
KIM_ModelRoutineName_ToString(KIM_ModelRoutineName const modelRoutineName);

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Create
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Create
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Create;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::ComputeArgumentsCreate
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::ComputeArgumentsCreate
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Compute
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Compute
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Compute;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Extension
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Extension
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Extension;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Refresh
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Refresh
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Refresh;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::WriteParameterizedModel
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::WriteParameterizedModel
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const
    KIM_MODEL_ROUTINE_NAME_WriteParameterizedModel;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::ComputeArgumentsDestroy
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::ComputeArgumentsDestroy
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const
    KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Destroy
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Destroy
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Destroy;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames
 **
 ** \since 2.0
 **/
void KIM_MODEL_ROUTINE_NAME_GetNumberOfModelRoutineNames(
    int * const numberOfModelRoutineNames);

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::GetModelRoutineName
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::GetModelRoutineName
 **
 ** \since 2.0
 **/
int KIM_MODEL_ROUTINE_NAME_GetModelRoutineName(
    int const index, KIM_ModelRoutineName * const modelRoutineName);

#endif /* KIM_MODEL_ROUTINE_NAME_H_ */
