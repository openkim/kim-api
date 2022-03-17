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


#ifndef KIM_LENGTH_UNIT_H_
#define KIM_LENGTH_UNIT_H_

/**
 ** \brief \copybrief KIM::LengthUnit
 **
 ** \sa KIM::LengthUnit, kim_length_unit_module::kim_length_unit_type
 **
 ** \since 2.0
 **/
struct KIM_LengthUnit
{
  /**
   ** \brief \copybrief KIM::LengthUnit::lengthUnitID
   **
   ** \sa KIM::LengthUnit::lengthUnitID,
   ** kim_length_unit_module::kim_length_unit_type::length_unit_id
   **
   ** \since 2.0
   **/
  int lengthUnitID;
};
#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

/**
 ** \brief \copybrief KIM::LengthUnit::LengthUnit(std::string const &)
 **
 ** \sa KIM::LengthUnit::LengthUnit(std::string const &),
 ** kim_length_unit_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_LengthUnit KIM_LengthUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::LengthUnit::Known
 **
 ** \sa KIM::LengthUnit::Known, kim_length_unit_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_LengthUnit_Known(KIM_LengthUnit const lengthUnit);

/**
 ** \brief \copybrief KIM::LengthUnit::operator==()
 **
 ** \sa KIM::LengthUnit::operator==(), kim_length_unit_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_LengthUnit_Equal(KIM_LengthUnit const lhs, KIM_LengthUnit const rhs);

/**
 ** \brief \copybrief KIM::LengthUnit::operator!=()
 **
 ** \sa KIM::LengthUnit::operator!=(), kim_length_unit_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_LengthUnit_NotEqual(KIM_LengthUnit const lhs, KIM_LengthUnit const rhs);

/**
 ** \brief \copybrief KIM::LengthUnit::ToString
 **
 ** \sa KIM::LengthUnit::ToString, kim_length_unit_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_LengthUnit_ToString(KIM_LengthUnit const lengthUnit);

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::unused
 **
 ** \sa KIM::LENGTH_UNIT::unused,
 ** kim_length_unit_module::kim_length_unit_unused
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_unused;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::A
 **
 ** \sa KIM::LENGTH_UNIT::A, kim_length_unit_module::kim_length_unit_a
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_A;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::Bohr
 **
 ** \sa KIM::LENGTH_UNIT::Bohr, kim_length_unit_module::kim_length_unit_bohr
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_Bohr;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::cm
 **
 ** \sa KIM::LENGTH_UNIT::cm, kim_length_unit_module::kim_length_unit_cm
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_cm;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::m
 **
 ** \sa KIM::LENGTH_UNIT::m, kim_length_unit_module::kim_length_unit_m
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_m;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::nm
 **
 ** \sa KIM::LENGTH_UNIT::nm, kim_length_unit_module::kim_length_unit_nm
 **
 ** \since 2.0
 **/
extern KIM_LengthUnit const KIM_LENGTH_UNIT_nm;

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::GetNumberOfLengthUnits
 **
 ** \sa KIM::LENGTH_UNIT::GetNumberOfLengthUnits,
 ** kim_length_unit_module::kim_get_number_of_length_units
 **
 ** \since 2.0
 **/
void KIM_LENGTH_UNIT_GetNumberOfLengthUnits(int * const numberOfLengthUnits);

/**
 ** \brief \copybrief KIM::LENGTH_UNIT::GetLengthUnit
 **
 ** \sa KIM::LENGTH_UNIT::GetLengthUnit,
 ** kim_length_unit_module::kim_get_length_unit
 **
 ** \since 2.0
 **/
int KIM_LENGTH_UNIT_GetLengthUnit(int const index,
                                  KIM_LengthUnit * const lengthUnit);

#endif /* KIM_LENGTH_UNIT_H_ */
