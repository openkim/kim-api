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
#include "KIM_UnitSystem.h"
#endif

KIM_LengthUnit const KIM_UNITS_A = {1};
KIM_LengthUnit const KIM_UNITS_Bohr = {2};
KIM_LengthUnit const KIM_UNITS_cm = {3};
KIM_LengthUnit const KIM_UNITS_m = {4};
KIM_LengthUnit const KIM_UNITS_nm = {5};

KIM_EnergyUnit const KIM_UNITS_amu_A2_per_ps2 = {1};
KIM_EnergyUnit const KIM_UNITS_erg = {2};
KIM_EnergyUnit const KIM_UNITS_eV = {3};
KIM_EnergyUnit const KIM_UNITS_Hartree = {4};
KIM_EnergyUnit const KIM_UNITS_J = {5};
KIM_EnergyUnit const KIM_UNITS_kcal_mol = {6};

KIM_ChargeUnit const KIM_UNITS_C = {1};
KIM_ChargeUnit const KIM_UNITS_e = {2};
KIM_ChargeUnit const KIM_UNITS_statC = {3};

KIM_TemperatureUnit const KIM_UNITS_K = {1};

KIM_TimeUnit const KIM_UNITS_fs = {1};
KIM_TimeUnit const KIM_UNITS_ps = {2};
KIM_TimeUnit const KIM_UNITS_ns = {3};
KIM_TimeUnit const KIM_UNITS_s = {4};
