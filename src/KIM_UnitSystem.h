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


#ifndef KIM_UNIT_SYSTEM_H_
#define KIM_UNIT_SYSTEM_H_

struct KIM_LengthUnit
{
  int lengthUnitID;
};
#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

struct KIM_EnergyUnit
{
  int energyUnitID;
};
#ifndef KIM_ENERGY_UNIT_DEFINED_
#define KIM_ENERGY_UNIT_DEFINED_
typedef struct KIM_EnergyUnit KIM_EnergyUnit;
#endif

struct KIM_ChargeUnit
{
  int chargeUnitID;
};
#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

struct KIM_TemperatureUnit
{
  int temperatureUnitID;
};
#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

struct KIM_TimeUnit
{
  int timeUnitID;
};
#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif

extern KIM_LengthUnit const KIM_UNITS_A;
extern KIM_LengthUnit const KIM_UNITS_Bohr;
extern KIM_LengthUnit const KIM_UNITS_cm;
extern KIM_LengthUnit const KIM_UNITS_m;
extern KIM_LengthUnit const KIM_UNITS_nm;

extern KIM_EnergyUnit const KIM_UNITS_amu_A2_per_ps2;
extern KIM_EnergyUnit const KIM_UNITS_erg;
extern KIM_EnergyUnit const KIM_UNITS_eV;
extern KIM_EnergyUnit const KIM_UNITS_Hartree;
extern KIM_EnergyUnit const KIM_UNITS_J;
extern KIM_EnergyUnit const KIM_UNITS_kcal_mol;

extern KIM_ChargeUnit const KIM_UNITS_C;
extern KIM_ChargeUnit const KIM_UNITS_e;
extern KIM_ChargeUnit const KIM_UNITS_statC;

extern KIM_TemperatureUnit const KIM_UNITS_K;

extern KIM_TimeUnit const KIM_UNITS_fs;
extern KIM_TimeUnit const KIM_UNITS_ps;
extern KIM_TimeUnit const KIM_UNITS_ns;
extern KIM_TimeUnit const KIM_UNITS_s;

#endif  /* KIM_UNIT_SYSTEM_H_ */
