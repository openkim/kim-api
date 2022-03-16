/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2021, Regents of the University of Minnesota.          */
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


#ifndef KIM_ENERGY_UNIT_H_
#define KIM_ENERGY_UNIT_H_

/**
 ** \brief \copybrief KIM::EnergyUnit
 **
 ** \sa KIM::EnergyUnit, kim_energy_unit_module::kim_energy_unit_type
 **
 ** \since 2.0
 **/
struct KIM_EnergyUnit
{
  /**
   ** \brief \copybrief KIM::EnergyUnit::energyUnitID
   **
   ** \sa KIM::EnergyUnit::energyUnitID,
   ** kim_energy_unit_module::kim_energy_unit_type::energy_unit_id
   **
   ** \since 2.0
   **/
  int energyUnitID;
};
#ifndef KIM_ENERGY_UNIT_DEFINED_
#define KIM_ENERGY_UNIT_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_EnergyUnit KIM_EnergyUnit;
#endif

/**
 ** \brief \copybrief KIM::EnergyUnit::EnergyUnit(std::string const &)
 **
 ** \sa KIM::EnergyUnit::EnergyUnit(std::string const &),
 ** kim_energy_unit_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_EnergyUnit KIM_EnergyUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::EnergyUnit::Known
 **
 ** \sa KIM::EnergyUnit::Known, kim_energy_unit_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_EnergyUnit_Known(KIM_EnergyUnit const energyUnit);

/**
 ** \brief \copybrief KIM::EnergyUnit::operator==()
 **
 ** \sa KIM::EnergyUnit::operator==(), kim_energy_unit_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_EnergyUnit_Equal(KIM_EnergyUnit const lhs, KIM_EnergyUnit const rhs);

/**
 ** \brief \copybrief KIM::EnergyUnit::operator!=()
 **
 ** \sa KIM::EnergyUnit::operator!=(), kim_energy_unit_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_EnergyUnit_NotEqual(KIM_EnergyUnit const lhs, KIM_EnergyUnit const rhs);

/**
 ** \brief \copybrief KIM::EnergyUnit::ToString
 **
 ** \sa KIM::EnergyUnit::ToString, kim_energy_unit_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_EnergyUnit_ToString(KIM_EnergyUnit const energyUnit);

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::unused
 **
 ** \sa KIM::ENERGY_UNIT::unused,
 ** kim_energy_unit_module::kim_energy_unit_unused
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_unused;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::amu_A2_per_ps2
 **
 ** \sa KIM::ENERGY_UNIT::amu_A2_per_ps2,
 ** kim_energy_unit_module::kim_energy_unit_amu_a2_per_ps2
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_amu_A2_per_ps2;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::erg
 **
 ** \sa KIM::ENERGY_UNIT::erg, kim_energy_unit_module::kim_energy_unit_erg
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_erg;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::eV
 **
 ** \sa KIM::ENERGY_UNIT::eV, kim_energy_unit_module::kim_energy_unit_ev
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_eV;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::Hartree
 **
 ** \sa KIM::ENERGY_UNIT::Hartree,
 ** kim_energy_unit_module::kim_energy_unit_hartree
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_Hartree;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::J
 **
 ** \sa KIM::ENERGY_UNIT::J, kim_energy_unit_module::kim_energy_unit_j
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_J;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::kcal_mol
 **
 ** \sa KIM::ENERGY_UNIT::kcal_mol,
 ** kim_energy_unit_module::kim_energy_unit_kcal_mol
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_kcal_mol;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::GetNumberOfEnergyUnits
 **
 ** \sa KIM::ENERGY_UNIT::GetNumberOfEnergyUnits,
 ** kim_energy_unit_module::kim_get_number_of_energy_units
 **
 ** \since 2.0
 **/
void KIM_ENERGY_UNIT_GetNumberOfEnergyUnits(int * const numberOfEnergyUnits);

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::GetEnergyUnit
 **
 ** \sa KIM::ENERGY_UNIT::GetEnergyUnit,
 ** kim_energy_unit_module::kim_get_energy_unit
 **
 ** \since 2.0
 **/
int KIM_ENERGY_UNIT_GetEnergyUnit(int const index,
                                  KIM_EnergyUnit * const energyUnit);

#endif /* KIM_ENERGY_UNIT_H_ */
