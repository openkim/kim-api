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
/* Release: This file is part of the kim-api-v2-2.0.0 package.                */
/*                                                                            */


#ifndef KIM_MODEL_REFRESH_H_
#define KIM_MODEL_REFRESH_H_

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif


#ifndef KIM_MODEL_REFRESH_DEFINED_
#define KIM_MODEL_REFRESH_DEFINED_
/**
 ** \brief \copybrief KIM::ModelRefresh
 **
 ** \sa KIM::ModelRefresh,
 ** kim_model_refresh_module::kim_model_refresh_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelRefresh KIM_ModelRefresh;
#endif

/**
 ** \brief \copybrief KIM::ModelRefresh::SetInfluenceDistancePointer
 **
 ** \sa KIM::ModelRefresh::SetInfluenceDistancePointer,
 ** kim_model_refresh_module::kim_set_influence_distance_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelRefresh_SetInfluenceDistancePointer(
    KIM_ModelRefresh * const modelRefresh,
    double const * const influenceDistance);

/**
 ** \brief \copybrief KIM::ModelRefresh::SetNeighborListPointers
 **
 ** \sa KIM::ModelRefresh::SetNeighborListPointers,
 ** kim_model_refresh_module::kim_set_neighbor_list_pointers
 **
 ** \since 2.0
 **/
void KIM_ModelRefresh_SetNeighborListPointers(
    KIM_ModelRefresh * const modelRefresh,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

/**
 ** \brief \copybrief KIM::ModelRefresh::GetModelBufferPointer
 **
 ** \sa KIM::ModelRefresh::GetModelBufferPointer,
 ** kim_model_refresh_module::kim_get_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelRefresh_GetModelBufferPointer(
    KIM_ModelRefresh const * const modelRefresh, void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelRefresh::LogEntry
 **
 ** \sa KIM::ModelRefresh::LogEntry, kim_model_refresh_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelRefresh_LogEntry(KIM_ModelRefresh const * const modelRefresh,
                               KIM_LogVerbosity const logVerbosity,
                               char const * const message,
                               int const lineNumber,
                               char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelRefresh::ToString
 **
 ** \sa KIM::ModelRefresh::ToString, kim_model_refresh_module::kim_to_string
 **
 ** \since 2.0
 **/
char const *
KIM_ModelRefresh_ToString(KIM_ModelRefresh const * const modelRefresh);

#endif /* KIM_MODEL_REFRESH_H_ */
