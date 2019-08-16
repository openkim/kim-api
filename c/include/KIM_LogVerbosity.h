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
/* Release: This file is part of the kim-api-2.1.3 package.                   */
/*                                                                            */


#ifndef KIM_LOG_VERBOSITY_H_
#define KIM_LOG_VERBOSITY_H_

#ifndef KIM_LOG_DEFINES_INC_
#include "KIM_LOG_DEFINES.inc"
#endif

/**
 ** \brief \copybrief KIM::LogVerbosity
 **
 ** \sa KIM::LogVerbosity, kim_log_verbosity_module::kim_log_verbosity_type
 **
 ** \since 2.0
 **/
struct KIM_LogVerbosity
{
  /**
   ** \brief \copybrief KIM::LogVerbosity::logVerbosityID
   **
   ** \sa KIM::LogVerbosity::logVerbosityID,
   ** kim_log_verbosity_module::kim_log_verbosity_type::log_verbosity_id
   **
   ** \since 2.0
   **/
  int logVerbosityID;
};
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

/**
 ** \brief \copybrief KIM::LogVerbosity::LogVerbosity(std::string const &)
 **
 ** \sa KIM::LogVerbosity::LogVerbosity(std::string const &),
 ** kim_log_verbosity_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_LogVerbosity KIM_LogVerbosity_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::LogVerbosity::Known
 **
 ** \sa KIM::LogVerbosity::Known, kim_log_verbosity_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_Known(KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator<()
 **
 ** \sa KIM::LogVerbosity::operator<(),
 ** kim_log_verbosity_module::operator(.lt.)
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_LessThan(KIM_LogVerbosity const lhs,
                              KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator>()
 **
 ** \sa KIM::LogVerbosity::operator>(),
 ** kim_log_verbosity_module::operator(.gt.)
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_GreaterThan(KIM_LogVerbosity const lhs,
                                 KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator<=()
 **
 ** \sa KIM::LogVerbosity::operator<=(),
 ** kim_log_verbosity_module::operator(.le.)
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_LessThanEqual(KIM_LogVerbosity const lhs,
                                   KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator>=()
 **
 ** \sa KIM::LogVerbosity::operator>=(),
 ** kim_log_verbosity_module::operator(.ge.)
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_GreaterThanEqual(KIM_LogVerbosity const lhs,
                                      KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator==()
 **
 ** \sa KIM::LogVerbosity::operator==(),
 ** kim_log_verbosity_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_Equal(KIM_LogVerbosity const lhs,
                           KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator!=()
 **
 ** \sa KIM::LogVerbosity::operator!=(),
 ** kim_log_verbosity_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_NotEqual(KIM_LogVerbosity const lhs,
                              KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::ToString
 **
 ** \sa KIM::LogVerbosity::ToString, kim_log_verbosity_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_LogVerbosity_ToString(KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::silent
 **
 ** \sa KIM::LOG_VERBOSITY::silent,
 ** kim_log_verbosity_module::kim_log_verbosity_silent
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_silent;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::fatal
 **
 ** \sa KIM::LOG_VERBOSITY::fatal,
 ** kim_log_verbosity_module::kim_log_verbosity_fatal
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_fatal;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::error
 **
 ** \sa KIM::LOG_VERBOSITY::error,
 ** kim_log_verbosity_module::kim_log_verbosity_error
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_error;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::warning
 **
 ** \sa KIM::LOG_VERBOSITY::warning,
 ** kim_log_verbosity_module::kim_log_verbosity_warning
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_warning;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::information
 **
 ** \sa KIM::LOG_VERBOSITY::information,
 ** kim_log_verbosity_module::kim_log_verbosity_information
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_information;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::debug
 **
 ** \sa KIM::LOG_VERBOSITY::debug,
 ** kim_log_verbosity_module::kim_log_verbosity_debug
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_debug;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::GetNumberOfLogVerbosities
 **
 ** \sa KIM::LOG_VERBOSITY::GetNumberOfLogVerbosities,
 ** kim_log_verbosity_module::kim_get_number_of_log_verbosities
 **
 ** \since 2.0
 **/
void KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities(
    int * const numberOfLogVerbosities);

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::GetLogVerbosity
 **
 ** \sa KIM::LOG_VERBOSITY::GetLogVerbosity,
 ** kim_log_verbosity_module::kim_get_log_verbosity
 **
 ** \since 2.0
 **/
int KIM_LOG_VERBOSITY_GetLogVerbosity(int const index,
                                      KIM_LogVerbosity * const logVerbosity);

#endif /* KIM_LOG_VERBOSITY_H_ */
