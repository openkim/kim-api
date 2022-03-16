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


#ifndef KIM_MODEL_ROUTINE_NAME_H_
#define KIM_MODEL_ROUTINE_NAME_H_

/**
 ** \brief \copybrief KIM::ModelRoutineName
 **
 ** \sa KIM::ModelRoutineName,
 ** kim_model_routine_name_module::kim_model_routine_name_type
 **
 ** \since 2.0
 **/
struct KIM_ModelRoutineName
{
  /**
   ** \brief \copybrief KIM::ModelRoutineName::modelRoutineNameID
   **
   ** \sa KIM::ModelRoutineName::modelRoutineNameID,
   ** kim_model_routine_name_module::kim_model_routine_name_type::<!--
   ** -->model_routine_name_id
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
 ** \sa KIM::ModelRoutineName::ModelRoutineName(std::string const &),
 ** kim_model_routine_name_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_ModelRoutineName KIM_ModelRoutineName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::ModelRoutineName::Known
 **
 ** \sa KIM::ModelRoutineName::Known, kim_model_routine_name_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_ModelRoutineName_Known(KIM_ModelRoutineName const modelRoutineName);

/**
 ** \brief \copybrief KIM::ModelRoutineName::operator==()
 **
 ** \sa KIM::ModelRoutineName::operator==(),
 ** kim_model_routine_name_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_ModelRoutineName_Equal(KIM_ModelRoutineName const lhs,
                               KIM_ModelRoutineName const rhs);

/**
 ** \brief \copybrief KIM::ModelRoutineName::operator!=()
 **
 ** \sa KIM::ModelRoutineName::operator!=(),
 ** kim_model_routine_name_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_ModelRoutineName_NotEqual(KIM_ModelRoutineName const lhs,
                                  KIM_ModelRoutineName const rhs);

/**
 ** \brief \copybrief KIM::ModelRoutineName::ToString
 **
 ** \sa KIM::ModelRoutineName::ToString,
 ** kim_model_routine_name_module::kim_to_string
 **
 ** \since 2.0
 **/
char const *
KIM_ModelRoutineName_ToString(KIM_ModelRoutineName const modelRoutineName);

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Create
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Create,
 ** kim_model_routine_name_module::kim_model_routine_name_create
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Create;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::ComputeArgumentsCreate
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::ComputeArgumentsCreate,
 ** kim_model_routine_name_module::<!--
 ** -->kim_model_routine_name_compute_arguments_create
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Compute
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Compute,
 ** kim_model_routine_name_module::kim_model_routine_name_compute
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Compute;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Extension
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Extension,
 ** kim_model_routine_name_module::kim_model_routine_name_extension
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Extension;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Refresh
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Refresh,
 ** kim_model_routine_name_module::kim_model_routine_name_refresh
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Refresh;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::WriteParameterizedModel
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::WriteParameterizedModel,
 ** kim_model_routine_name_module::<!--
 ** -->kim_model_routine_name_write_parameterized_model
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const
    KIM_MODEL_ROUTINE_NAME_WriteParameterizedModel;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::ComputeArgumentsDestroy
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::ComputeArgumentsDestroy,
 ** kim_model_routine_name_module::<!--
 ** -->kim_model_routine_name_compute_arguments_destroy
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const
    KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::Destroy
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::Destroy,
 ** kim_model_routine_name_module::kim_model_routine_name_destroy
 **
 ** \since 2.0
 **/
extern KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Destroy;

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames,
 ** kim_model_routine_name_module::kim_get_number_of_model_routine_names
 **
 ** \since 2.0
 **/
void KIM_MODEL_ROUTINE_NAME_GetNumberOfModelRoutineNames(
    int * const numberOfModelRoutineNames);

/**
 ** \brief \copybrief KIM::MODEL_ROUTINE_NAME::GetModelRoutineName
 **
 ** \sa KIM::MODEL_ROUTINE_NAME::GetModelRoutineName,
 ** kim_model_routine_name_module::kim_get_model_routine_name
 **
 ** \since 2.0
 **/
int KIM_MODEL_ROUTINE_NAME_GetModelRoutineName(
    int const index, KIM_ModelRoutineName * const modelRoutineName);

#endif /* KIM_MODEL_ROUTINE_NAME_H_ */
