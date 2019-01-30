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


#ifndef KIM_LENGTH_UNIT_H_
#define KIM_LENGTH_UNIT_H_

/**
 ** \brief \copybrief KIM::LengthUnit
 **
 ** \sa KIM::LengthUnit
 **
 ** \since 2.0
 **/
struct KIM_LengthUnit
{
  /**
   ** \brief \copybrief KIM::LengthUnit::lengthUnitID
   **
   ** \sa KIM::LengthUnit::lengthUnitID
   **
   ** \since 2.0
   **/
  int lengthUnitID;
};
#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

/**
 ** \brief \copybrief KIM::LengthUnit::LengthUnit(std::string const &)
 **
 ** \sa KIM::LengthUnit::LengthUnit(std::string const &)
 **
 ** \since 2.0
 **/
KIM_LengthUnit KIM_LengthUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::LengthUnit::Known
 **
 ** \sa KIM::LengthUnit::Known
 **
 ** \since 2.0
 **/
int KIM_LengthUnit_Known(KIM_LengthUnit const lengthUnit);

/**
 ** \brief \copybrief KIM::LengthUnit::operator==()
 **
 ** \sa KIM::LengthUnit::operator==()
 **
 ** \since 2.0
 **/
int KIM_LengthUnit_Equal(KIM_LengthUnit const lhs, KIM_LengthUnit const rhs);

/**
 ** \brief \copybrief KIM::LengthUnit::operator!=()
 **
 ** \sa KIM::LengthUnit::operator!=()
 **
 ** \since 2.0
 **/
int KIM_LengthUnit_NotEqual(KIM_LengthUnit const lhs, KIM_LengthUnit const rhs);

/**
 ** \brief \copybrief KIM::LengthUnit::ToString
 **
 ** \sa KIM::LengthUnit::ToString
 **
 ** \since 2.0
 **/
char const * KIM_LengthUnit_ToString(KIM_LengthUnit const lengthUnit);

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::unused
 **
 ** \sa KIM::LENGTH_UNIT::unused
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_unused;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::A
 **
 ** \sa KIM::LENGTH_UNIT::A
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_A;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::Bohr
 **
 ** \sa KIM::LENGTH_UNIT::Bohr
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_Bohr;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::cm
 **
 ** \sa KIM::LENGTH_UNIT::cm
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_cm;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::m
 **
 ** \sa KIM::LENGTH_UNIT::m
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_m;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::nm
 **
 ** \sa KIM::LENGTH_UNIT::nm
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_nm;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::GetNumberOfLengthUnits
 **
 ** \sa KIM::LENGTH_UNIT::GetNumberOfLengthUnits
 **
 ** \since 2.0
 **/
void KIM_LENGTH_UNIT_GetNumberOfLengthUnits(int * const numberOfLengthUnits);

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::GetLengthUnit
 **
 ** \sa KIM::LENGTH_UNIT::GetLengthUnit
 **
 ** \since 2.0
 **/
int KIM_LENGTH_UNIT_GetLengthUnit(int const index,
                                  KIM_LengthUnit * const lengthUnit);

#endif /* KIM_LENGTH_UNIT_H_ */
