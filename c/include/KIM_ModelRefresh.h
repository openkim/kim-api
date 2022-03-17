/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2022, Regents of the University of Minnesota.          */
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
/* Release: This file is part of the kim-api-2.3.0 package.                   */
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
