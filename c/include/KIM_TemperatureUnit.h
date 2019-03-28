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
/* Release: This file is part of the kim-api-2.0.2 package.                   */
/*                                                                            */


#ifndef KIM_TEMPERATURE_UNIT_H_
#define KIM_TEMPERATURE_UNIT_H_

/**
 ** \brief \copybrief KIM::TemperatureUnit
 **
 ** \sa KIM::TemperatureUnit,
 *kim_temperature_unit_module::kim_temperature_unit_type
 **
 ** \since 2.0
 **/
struct KIM_TemperatureUnit
{
  /**
   ** \brief \copybrief KIM::TemperatureUnit::temperatureUnitID
   **
   ** \sa KIM::TemperatureUnit::temperatureUnitID,
   ** kim_temperature_unit_module::kim_temperature_unit_type::<!--
   ** -->temperature_unit_id
   **
   ** \since 2.0
   **/
  int temperatureUnitID;
};
#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

/**
 ** \brief \copybrief KIM::TemperatureUnit::TemperatureUnit(std::string const &)
 **
 ** \sa KIM::TemperatureUnit::TemperatureUnit(std::string const &),
 ** kim_temperature_unit_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_TemperatureUnit KIM_TemperatureUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::TemperatureUnit::Known
 **
 ** \sa KIM::TemperatureUnit::Known, kim_temperature_unit_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_TemperatureUnit_Known(KIM_TemperatureUnit const temperatureUnit);

/**
 ** \brief \copybrief KIM::TemperatureUnit::operator==()
 **
 ** \sa KIM::TemperatureUnit::operator==(),
 ** kim_temperature_unit_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_TemperatureUnit_Equal(KIM_TemperatureUnit const lhs,
                              KIM_TemperatureUnit const rhs);

/**
 ** \brief \copybrief KIM::TemperatureUnit::operator!=()
 **
 ** \sa KIM::TemperatureUnit::operator!=(),
 ** kim_temperature_unit_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_TemperatureUnit_NotEqual(KIM_TemperatureUnit const lhs,
                                 KIM_TemperatureUnit const rhs);

/**
 ** \brief \copybrief KIM::TemperatureUnit::ToString
 **
 ** \sa KIM::TemperatureUnit::ToString,
 ** kim_temperature_unit_module::kim_to_string
 **
 ** \since 2.0
 **/
char const *
KIM_TemperatureUnit_ToString(KIM_TemperatureUnit const temperatureUnit);

/**
 ** \brief \copybrief KIM::TEMPERATURE_UNIT::unused
 **
 ** \sa KIM::TEMPERATURE_UNIT::unused,
 ** kim_temperature_unit_module::kim_temperature_unit_unused
 **
 ** \since 2.0
 **/
extern KIM_TemperatureUnit const KIM_TEMPERATURE_UNIT_unused;

/**
 ** \brief \copybrief KIM::TEMPERATURE_UNIT::K
 **
 ** \sa KIM::TEMPERATURE_UNIT::K,
 ** kim_temperature_unit_module::kim_temperature_unit_k
 **
 ** \since 2.0
 **/
extern KIM_TemperatureUnit const KIM_TEMPERATURE_UNIT_K;

/**
 ** \brief \copybrief KIM::TEMPERATURE_UNIT::GetNumberOfTemperatureUnits
 **
 ** \sa KIM::TEMPERATURE_UNIT::GetNumberOfTemperatureUnits,
 ** kim_temperature_unit_module::kim_get_number_of_temperature_units
 **
 ** \since 2.0
 **/
void KIM_TEMPERATURE_UNIT_GetNumberOfTemperatureUnits(
    int * const numberOfTemperatureUnits);

/**
 ** \brief \copybrief KIM::TEMPERATURE_UNIT::GetTemperatureUnit
 **
 ** \sa KIM::TEMPERATURE_UNIT::GetTemperatureUnit,
 ** kim_temperature_unit_module::kim_get_temperature_unit
 **
 ** \since 2.0
 **/
int KIM_TEMPERATURE_UNIT_GetTemperatureUnit(
    int const index, KIM_TemperatureUnit * const temperatureUnit);

#endif /* KIM_TEMPERATURE_UNIT_H_ */
