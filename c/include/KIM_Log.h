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
/* Release: This file is part of the kim-api-v2-2.0.1 package.                */
/*                                                                            */


#ifndef KIM_LOG_H_
#define KIM_LOG_H_

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
