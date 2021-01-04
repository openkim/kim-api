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
/* Copyright (c) 2016--2021, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
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
