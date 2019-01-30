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


#ifndef KIM_ENERGY_UNIT_H_
#define KIM_ENERGY_UNIT_H_

/**
 ** \brief \copybrief KIM::EnergyUnit
 **
 ** \sa KIM::EnergyUnit
 **
 ** \since 2.0
 **/
struct KIM_EnergyUnit
{
  /**
   ** \brief \copybrief KIM::EnergyUnit::energyUnitID
   **
   ** \sa KIM::EnergyUnit::energyUnitID
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
 ** \sa KIM::EnergyUnit::EnergyUnit(std::string const &)
 **
 ** \since 2.0
 **/
KIM_EnergyUnit KIM_EnergyUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::EnergyUnit::Known
 **
 ** \sa KIM::EnergyUnit::Known
 **
 ** \since 2.0
 **/
int KIM_EnergyUnit_Known(KIM_EnergyUnit const energyUnit);

/**
 ** \brief \copybrief KIM::EnergyUnit::operator==()
 **
 ** \sa KIM::EnergyUnit::operator==()
 **
 ** \since 2.0
 **/
int KIM_EnergyUnit_Equal(KIM_EnergyUnit const lhs, KIM_EnergyUnit const rhs);

/**
 ** \brief \copybrief KIM::EnergyUnit::operator!=()
 **
 ** \sa KIM::EnergyUnit::operator!=()
 **
 ** \since 2.0
 **/
int KIM_EnergyUnit_NotEqual(KIM_EnergyUnit const lhs, KIM_EnergyUnit const rhs);

/**
 ** \brief \copybrief KIM::EnergyUnit::ToString
 **
 ** \sa KIM::EnergyUnit::ToString
 **
 ** \since 2.0
 **/
char const * KIM_EnergyUnit_ToString(KIM_EnergyUnit const energyUnit);

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::unused
 **
 ** \sa KIM::ENERGY_UNIT::unused
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_unused;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::amu_A2_per_ps2
 **
 ** \sa KIM::ENERGY_UNIT::amu_A2_per_ps2
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_amu_A2_per_ps2;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::erg
 **
 ** \sa KIM::ENERGY_UNIT::erg
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_erg;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::eV
 **
 ** \sa KIM::ENERGY_UNIT::eV
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_eV;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::Hartree
 **
 ** \sa KIM::ENERGY_UNIT::Hartree
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_Hartree;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::J
 **
 ** \sa KIM::ENERGY_UNIT::J
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_J;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::kcal_mol
 **
 ** \sa KIM::ENERGY_UNIT::kcal_mol
 **
 ** \since 2.0
 **/
extern KIM_EnergyUnit const KIM_ENERGY_UNIT_kcal_mol;

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::GetNumberOfEnergyUnits
 **
 ** \sa KIM::ENERGY_UNIT::GetNumberOfEnergyUnits
 **
 ** \since 2.0
 **/
void KIM_ENERGY_UNIT_GetNumberOfEnergyUnits(int * const numberOfEnergyUnits);

/**
 ** \brief \copybrief KIM::ENERGY_UNIT::GetEnergyUnit
 **
 ** \sa KIM::ENERGY_UNIT::GetEnergyUnit
 **
 ** \since 2.0
 **/
int KIM_ENERGY_UNIT_GetEnergyUnit(int const index,
                                  KIM_EnergyUnit * const energyUnit);

#endif /* KIM_ENERGY_UNIT_H_ */
