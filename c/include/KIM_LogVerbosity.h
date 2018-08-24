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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
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

#include "KIM_LOG_DEFINES.inc"


struct KIM_LogVerbosity
{
  int logVerbosityID;
};
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

KIM_LogVerbosity KIM_LogVerbosity_FromString(char const * const str);

int KIM_LogVerbosity_LessThan(KIM_LogVerbosity const left,
                              KIM_LogVerbosity const right);
int KIM_LogVerbosity_GreaterThan(KIM_LogVerbosity const left,
                                 KIM_LogVerbosity const right);
int KIM_LogVerbosity_LessThanEqual(KIM_LogVerbosity const left,
                                   KIM_LogVerbosity const right);
int KIM_LogVerbosity_GreaterThanEqual(KIM_LogVerbosity const left,
                                      KIM_LogVerbosity const right);
int KIM_LogVerbosity_Equal(KIM_LogVerbosity const left,
                           KIM_LogVerbosity const right);
int KIM_LogVerbosity_NotEqual(KIM_LogVerbosity const left,
                              KIM_LogVerbosity const right);
char const * KIM_LogVerbosity_String(KIM_LogVerbosity const logVerbosity);

extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_silent;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_fatal;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_error;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_warning;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_information;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_debug;

void KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities(
    int * const numberOfLogVerbosities);
int KIM_LOG_VERBOSITY_GetLogVerbosity(int const index,
                                      KIM_LogVerbosity * const logVerbosity);

#endif  /* KIM_LOG_VERBOSITY_H_ */
