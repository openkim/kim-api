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


#ifndef KIM_LOG_H_
#define KIM_LOG_H_

#ifndef KIM_FUNCTION_TYPES_H_
#include "KIM_FunctionTypes.h" /* IWYU pragma: export */
#endif

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

#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.2
 **/
typedef struct KIM_LanguageName KIM_LanguageName;
#endif


#ifndef KIM_LOG_DEFINED_
#define KIM_LOG_DEFINED_
/**
 ** \brief \copybrief KIM::Log
 **
 ** \sa KIM::Log, kim_log_module::kim_log_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_Log KIM_Log;
#endif

/**
 ** \brief \copybrief KIM::Log::Create
 **
 ** \sa KIM::Log::Create, kim_log_module::kim_log_create
 **
 ** \since 2.0
 **/
int KIM_Log_Create(KIM_Log ** const log);

/**
 ** \brief \copybrief KIM::Log::Destroy
 **
 ** \sa KIM::Log::Destroy, kim_log_module::kim_log_destroy
 **
 ** \since 2.0
 **/
void KIM_Log_Destroy(KIM_Log ** const log);

/**
 ** \brief \copybrief KIM::Log::PushDefaultVerbosity
 **
 ** \sa KIM::Log::PushDefaultVerbosity,
 ** kim_log_module::kim_push_default_verbosity
 **
 ** \since 2.0
 **/
void KIM_Log_PushDefaultVerbosity(KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::Log::PopDefaultVerbosity
 **
 ** \sa KIM::Log::PopDefaultVerbosity,
 ** kim_log_module::kim_pop_default_verbosity
 **
 ** \since 2.0
 **/
void KIM_Log_PopDefaultVerbosity();

/**
 ** \brief \copybrief KIM::Log::PushDefaultPrintFunction
 **
 ** \sa KIM::Log::PushDefaultPrintFunction,
 ** kim_log_module::kim_push_default_print_function
 **
 ** \since 2.2
 **/
void KIM_Log_PushDefaultPrintFunction(KIM_LanguageName const languageName,
                                      KIM_Function * const fptr);

/**
 ** \brief \copybrief KIM::Log::PopDefaultPrintFunction
 **
 ** \sa KIM::Log::PopDefaultPrintFunction,
 ** kim_log_module::kim_pop_default_print_function
 **
 ** \since 2.2
 **/
void KIM_Log_PopDefaultPrintFunction();

/**
 ** \brief \copybrief KIM::Log::GetID
 **
 ** \sa KIM::Log::GetID, kim_log_module::kim_get_id
 **
 ** \since 2.0
 **/
char const * KIM_Log_GetID(KIM_Log const * const log);

/**
 ** \brief \copybrief KIM::Log::SetID
 **
 ** \sa KIM::Log::SetID, kim_log_module::kim_set_id
 **
 ** \since 2.0
 **/
void KIM_Log_SetID(KIM_Log * const log, char const * const id);

/**
 ** \brief \copybrief KIM::Log::PushVerbosity
 **
 ** \sa KIM::Log::PushVerbosity, kim_log_module::kim_push_verbosity
 **
 ** \since 2.0
 **/
void KIM_Log_PushVerbosity(KIM_Log * const log,
                           KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::Log::PopVerbosity
 **
 ** \sa KIM::Log::PopVerbosity, kim_log_module::kim_pop_verbosity
 **
 ** \since 2.0
 **/
void KIM_Log_PopVerbosity(KIM_Log * const log);

/**
 ** \brief \copybrief KIM::Log::LogEntry
 **
 ** \sa KIM::Log::LogEntry, kim_log_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_Log_LogEntry(KIM_Log const * const log,
                      KIM_LogVerbosity const logVerbosity,
                      char const * const message,
                      int const lineNumber,
                      char const * const fileName);

#endif /* KIM_LOG_H_ */
