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


#ifndef KIM_LOG_VERBOSITY_H_
#define KIM_LOG_VERBOSITY_H_

#ifndef KIM_LOG_DEFINES_INC_
#include "KIM_LOG_DEFINES.inc"
#endif

/**
 ** \brief \copybrief KIM::LogVerbosity
 **
 ** \sa KIM::LogVerbosity
 **
 ** \since 2.0
 **/
struct KIM_LogVerbosity
{
  /**
   ** \brief \copybrief KIM::LogVerbosity::logVerbosityID
   **
   ** \sa KIM::LogVerbosity::logVerbosityID
   **
   ** \since 2.0
   **/
  int logVerbosityID;
};
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

/**
 ** \brief \copybrief KIM::LogVerbosity::LogVerbosity(std::string const &)
 **
 ** \sa KIM::LogVerbosity::LogVerbosity(std::string const &)
 **
 ** \since 2.0
 **/
KIM_LogVerbosity KIM_LogVerbosity_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::LogVerbosity::Known
 **
 ** \sa KIM::LogVerbosity::Known
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_Known(KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator<()
 **
 ** \sa KIM::LogVerbosity::operator<()
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_LessThan(KIM_LogVerbosity const lhs,
                              KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator>()
 **
 ** \sa KIM::LogVerbosity::operator>()
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_GreaterThan(KIM_LogVerbosity const lhs,
                                 KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator<=()
 **
 ** \sa KIM::LogVerbosity::operator<=()
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_LessThanEqual(KIM_LogVerbosity const lhs,
                                   KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator>=()
 **
 ** \sa KIM::LogVerbosity::operator>=()
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_GreaterThanEqual(KIM_LogVerbosity const lhs,
                                      KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator==()
 **
 ** \sa KIM::LogVerbosity::operator==()
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_Equal(KIM_LogVerbosity const lhs,
                           KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::operator!=()
 **
 ** \sa KIM::LogVerbosity::operator!=()
 **
 ** \since 2.0
 **/
int KIM_LogVerbosity_NotEqual(KIM_LogVerbosity const lhs,
                              KIM_LogVerbosity const rhs);

/**
 ** \brief \copybrief KIM::LogVerbosity::ToString
 **
 ** \sa KIM::LogVerbosity::ToString
 **
 ** \since 2.0
 **/
char const * KIM_LogVerbosity_ToString(KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::silent
 **
 ** \sa KIM::LOG_VERBOSITY::silent
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_silent;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::fatal
 **
 ** \sa KIM::LOG_VERBOSITY::fatal
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_fatal;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::error
 **
 ** \sa KIM::LOG_VERBOSITY::error
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_error;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::warning
 **
 ** \sa KIM::LOG_VERBOSITY::warning
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_warning;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::information
 **
 ** \sa KIM::LOG_VERBOSITY::information
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_information;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::debug
 **
 ** \sa KIM::LOG_VERBOSITY::debug
 **
 ** \since 2.0
 **/
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_debug;

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::GetNumberOfLogVerbosities
 **
 ** \sa KIM::LOG_VERBOSITY::GetNumberOfLogVerbosities
 **
 ** \since 2.0
 **/
void KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities(
    int * const numberOfLogVerbosities);

/**
 ** \brief \copybrief KIM::LOG_VERBOSITY::GetLogVerbosity
 **
 ** \sa KIM::LOG_VERBOSITY::GetLogVerbosity
 **
 ** \since 2.0
 **/
int KIM_LOG_VERBOSITY_GetLogVerbosity(int const index,
                                      KIM_LogVerbosity * const logVerbosity);

#endif /* KIM_LOG_VERBOSITY_H_ */
