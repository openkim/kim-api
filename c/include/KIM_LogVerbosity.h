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
/* Copyright (c) 2016--2017, Regents of the University of Minnesota.          */
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

KIM_LogVerbosity KIM_LogVerbosityFromString(char const * const str);

int KIM_LogVerbosityLessThan(KIM_LogVerbosity const left,
                             KIM_LogVerbosity const right);
int KIM_LogVerbosityGreaterThan(KIM_LogVerbosity const left,
                                KIM_LogVerbosity const right);
int KIM_LogVerbosityLessThanEqual(KIM_LogVerbosity const left,
                              KIM_LogVerbosity const right);
int KIM_LogVerbosityGreaterThanEqual(KIM_LogVerbosity const left,
                                 KIM_LogVerbosity const right);
int KIM_LogVerbosityEqual(KIM_LogVerbosity const left,
                          KIM_LogVerbosity const right);
int KIM_LogVerbosityNotEqual(KIM_LogVerbosity const left,
                             KIM_LogVerbosity const right);
char const * const KIM_LogVerbosityString(KIM_LogVerbosity const logVerbosity);

extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_silent;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_fatal;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_error;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_warning;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_information;
extern KIM_LogVerbosity const KIM_LOG_VERBOSITY_debug;

#endif  /* KIM_LOG_VERBOSITY_H_ */
