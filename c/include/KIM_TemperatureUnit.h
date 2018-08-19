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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_TEMPERATURE_UNIT_H_
#define KIM_TEMPERATURE_UNIT_H_

struct KIM_TemperatureUnit
{
  int temperatureUnitID;
};
#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

KIM_TemperatureUnit KIM_TemperatureUnit_FromString(char const * const str);

int KIM_TemperatureUnit_Equal(KIM_TemperatureUnit const left,
                              KIM_TemperatureUnit const right);
int KIM_TemperatureUnit_NotEqual(KIM_TemperatureUnit const left,
                                 KIM_TemperatureUnit const right);
char const * KIM_TemperatureUnit_String(
    KIM_TemperatureUnit const temperatureUnit);

extern KIM_TemperatureUnit const KIM_TEMPERATURE_UNIT_unused;
extern KIM_TemperatureUnit const KIM_TEMPERATURE_UNIT_K;

void KIM_TEMPERATURE_UNIT_GetNumberOfTemperatureUnits(
    int * const numberOfTemperatureUnits);
int KIM_TEMPERATURE_UNIT_GetTemperatureUnit(
    int const index,
    KIM_TemperatureUnit * const temperatureUnit);

#endif  /* KIM_TEMPERATURE_UNIT_H_ */
