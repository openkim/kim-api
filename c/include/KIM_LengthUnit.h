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


#ifndef KIM_LENGTH_UNIT_H_
#define KIM_LENGTH_UNIT_H_

struct KIM_LengthUnit
{
  int lengthUnitID;
};
#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

KIM_LengthUnit KIM_LengthUnit_FromString(char const * const str);

int KIM_LengthUnit_Equal(KIM_LengthUnit left, KIM_LengthUnit right);
int KIM_LengthUnit_NotEqual(KIM_LengthUnit left, KIM_LengthUnit right);
char const * KIM_LengthUnit_String(KIM_LengthUnit const lengthUnit);

extern KIM_LengthUnit const KIM_LENGTH_UNIT_unused;
extern KIM_LengthUnit const KIM_LENGTH_UNIT_A;
extern KIM_LengthUnit const KIM_LENGTH_UNIT_Bohr;
extern KIM_LengthUnit const KIM_LENGTH_UNIT_cm;
extern KIM_LengthUnit const KIM_LENGTH_UNIT_m;
extern KIM_LengthUnit const KIM_LENGTH_UNIT_nm;

void KIM_LENGTH_UNIT_GetNumberOfLengthUnits(int * const numberOfLengthUnits);
int KIM_LENGTH_UNIT_GetLengthUnit(int const index,
                                  KIM_LengthUnit * const lengthUnit);

#endif  /* KIM_LENGTH_UNIT_H_ */
