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


#ifndef KIM_CHARGE_UNIT_H_
#define KIM_CHARGE_UNIT_H_

/**
 ** \brief \copybrief KIM::ChargeUnit
 **
 ** \sa KIM::ChargeUnit, kim_charge_unit_module::kim_charge_unit_type
 **
 ** \since 2.0
 **/
struct KIM_ChargeUnit
{
  /**
   ** \brief \copybrief KIM::ChargeUnit::chargeUnitID
   **
   ** \sa KIM::ChargeUnit::chargeUnitID,
   ** kim_charge_unit_module::kim_charge_unit_type::charge_unit_id
   **
   ** \since 2.0
   **/
  int chargeUnitID;
};
#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

/**
 ** \brief \copybrief KIM::ChargeUnit::ChargeUnit(std::string const &)
 **
 ** \sa KIM::ChargeUnit::ChargeUnit(std::string const &),
 ** kim_charge_unit_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_ChargeUnit KIM_ChargeUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::ChargeUnit::Known
 **
 ** \sa KIM::ChargeUnit::Known, kim_charge_unit_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_ChargeUnit_Known(KIM_ChargeUnit const chargeUnit);

/**
 ** \brief \copybrief KIM::ChargeUnit::operator==()
 **
 ** \sa KIM::ChargeUnit::operator==(), kim_charge_unit_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_ChargeUnit_Equal(KIM_ChargeUnit const lhs, KIM_ChargeUnit const rhs);

/**
 ** \brief \copybrief KIM::ChargeUnit::operator!=()
 **
 ** \sa KIM::ChargeUnit::operator!=(), kim_charge_unit_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_ChargeUnit_NotEqual(KIM_ChargeUnit const lhs, KIM_ChargeUnit const rhs);

/**
 ** \brief \copybrief KIM::ChargeUnit::ToString
 **
 ** \sa KIM::ChargeUnit::ToString, kim_charge_unit_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_ChargeUnit_ToString(KIM_ChargeUnit const chargeUnit);

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::unused
 **
 ** \sa KIM::CHARGE_UNIT::unused,
 ** kim_charge_unit_module::kim_charge_unit_unused
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_unused;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::C
 **
 ** \sa KIM::CHARGE_UNIT::C, kim_charge_unit_module::kim_charge_unit_c
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_C;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::e
 **
 ** \sa KIM::CHARGE_UNIT::e, kim_charge_unit_module::kim_charge_unit_e
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_e;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::statC
 **
 ** \sa KIM::CHARGE_UNIT::StatC, kim_charge_unit_module::kim_charge_unit_statc
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_statC;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::GetNumberOfChargeUnits
 **
 ** \sa KIM::CHARGE_UNIT::GetNumberOfChargeUnits,
 ** kim_charge_unit_module::kim_get_number_of_charge_units
 **
 ** \since 2.0
 **/
void KIM_CHARGE_UNIT_GetNumberOfChargeUnits(int * const numberOfChargeUnits);

/**
 ** \brief \brief \copybrief KIM::CHARGE_UNIT::GetChargeUnit
 **
 ** \sa KIM::CHARGE_UNIT::GetChargeUnit,
 ** kim_charge_unit_module::kim_get_charge_unit
 **
 ** \since 2.0
 **/
int KIM_CHARGE_UNIT_GetChargeUnit(int const index,
                                  KIM_ChargeUnit * const chargeUnit);

#endif /* KIM_CHARGE_UNIT_H_ */
