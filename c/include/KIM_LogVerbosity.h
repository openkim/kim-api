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
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_LOG_VERBOSITY_H_
#define KIM_LOG_VERBOSITY_H_


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
