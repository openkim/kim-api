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


#ifndef KIM_TIME_UNIT_H_
#define KIM_TIME_UNIT_H_

/**
 ** \brief \copybrief KIM::TimeUnit
 **
 ** \sa KIM::TimeUnit
 **
 ** \since 2.0
 **/
struct KIM_TimeUnit
{
  /**
   ** \brief \copybrief KIM::TimeUnit::timeUnitID
   **
   ** \sa KIM::TimeUnit::timeUnitID
   **
   ** \since 2.0
   **/
  int timeUnitID;
};
#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif

/**
 ** \brief \copybrief KIM::TimeUnit::TimeUnit(std::string const &)
 **
 ** \sa KIM::TimeUnit::TimeUnit(std::string const &)
 **
 ** \since 2.0
 **/
KIM_TimeUnit KIM_TimeUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::TimeUnit::Known
 **
 ** \sa KIM::TimeUnit::Known
 **
 ** \since 2.0
 **/
int KIM_TimeUnit_Known(KIM_TimeUnit const timeUnit);

/**
 ** \brief \copybrief KIM::TimeUnit::operator==()
 **
 ** \sa KIM::TimeUnit::operator==()
 **
 ** \since 2.0
 **/
int KIM_TimeUnit_Equal(KIM_TimeUnit const lhs, KIM_TimeUnit const rhs);

/**
 ** \brief \copybrief KIM::TimeUnit::operator!=()
 **
 ** \sa KIM::TimeUnit::operator!=()
 **
 ** \since 2.0
 **/
int KIM_TimeUnit_NotEqual(KIM_TimeUnit const lhs, KIM_TimeUnit const rhs);

/**
 ** \brief \copybrief KIM::TimeUnit::ToString
 **
 ** \sa KIM::TimeUnit::ToString
 **
 ** \since 2.0
 **/
char const * KIM_TimeUnit_ToString(KIM_TimeUnit const timeUnit);

/**
 ** \brief \copybrief KIM::TIME_UNIT::unused
 **
 ** \sa KIM::TIME_UNIT::unused
 **
 ** \since 2.0
 **/
extern KIM_TimeUnit const KIM_TIME_UNIT_unused;

/**
 ** \brief \copybrief KIM::TIME_UNIT::fs
 **
 ** \sa KIM::TIME_UNIT::fs
 **
 ** \since 2.0
 **/
extern KIM_TimeUnit const KIM_TIME_UNIT_fs;

/**
 ** \brief \copybrief KIM::TIME_UNIT::ps
 **
 ** \sa KIM::TIME_UNIT::ps
 **
 ** \since 2.0
 **/
extern KIM_TimeUnit const KIM_TIME_UNIT_ps;

/**
 ** \brief \copybrief KIM::TIME_UNIT::ns
 **
 ** \sa KIM::TIME_UNIT::ns
 **
 ** \since 2.0
 **/
extern KIM_TimeUnit const KIM_TIME_UNIT_ns;

/**
 ** \brief \copybrief KIM::TIME_UNIT::s
 **
 ** \sa KIM::TIME_UNIT::s
 **
 ** \since 2.0
 **/
extern KIM_TimeUnit const KIM_TIME_UNIT_s;

/**
 ** \brief \copybrief KIM::TIME_UNIT::GetNumberOfTimeUnits
 **
 ** \sa KIM::TIME_UNIT::GetNumberOfTimeUnits
 **
 ** \since 2.0
 **/
void KIM_TIME_UNIT_GetNumberOfTimeUnits(int * const numberOfTimeUnits);

/**
 ** \brief \copybrief KIM::TIME_UNIT::GetTimeUnit
 **
 ** \sa KIM::TIME_UNIT::GetTimeUnit
 **
 ** \since 2.0
 **/
int KIM_TIME_UNIT_GetTimeUnit(int const index, KIM_TimeUnit * const timeUnit);

#endif /* KIM_TIME_UNIT_H_ */
