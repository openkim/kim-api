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


#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif


struct KIM_ModelWriteParameterizedModel;

#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel
 **
 ** \sa KIM::ModelWriteParameterizedModel
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelWriteParameterizedModel
    KIM_ModelWriteParameterizedModel;
#endif

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::GetPath
 **
 ** \sa KIM::ModelWriteParameterizedModel::GetPath
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_GetPath(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const ** const path);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::GetModelName
 **
 ** \sa KIM::ModelWriteParameterizedModel::GetModelName
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_GetModelName(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const ** const modelName);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::SetParameterFileName
 **
 ** \sa KIM::ModelWriteParameterizedModel::SetParameterFileName
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_SetParameterFileName(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::GetModelBufferPointer
 **
 ** \sa KIM::ModelWriteParameterizedModel::GetModelBufferPointer
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_GetModelBufferPointer(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::LogEntry
 **
 ** \sa KIM::ModelWriteParameterizedModel::LogEntry
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_LogEntry(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::ToString
 **
 ** \sa KIM::ModelWriteParameterizedModel::ToString
 **
 ** \since 2.0
 **/
char const * KIM_ModelWriteParameterizedModel_ToString(
    KIM_ModelWriteParameterizedModel const * const
        modelWriteParameterizedModel);

#endif /* KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_ */
