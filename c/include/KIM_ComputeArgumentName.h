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


#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#define KIM_COMPUTE_ARGUMENT_NAME_H_

/* Forward declarations */
#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
typedef struct KIM_DataType KIM_DataType;
#endif

struct KIM_ComputeArgumentName
{
  int computeArgumentNameID;
};
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ComputeArgumentName KIM_ComputeArgumentName;
#endif

KIM_ComputeArgumentName KIM_ComputeArgumentName_FromString(
    char const * const str);

int KIM_ComputeArgumentName_Equal(KIM_ComputeArgumentName const left,
                                  KIM_ComputeArgumentName const right);
int KIM_ComputeArgumentName_NotEqual(KIM_ComputeArgumentName const left,
                                     KIM_ComputeArgumentName const right);
char const * KIM_ComputeArgumentName_String(
    KIM_ComputeArgumentName const computeArgumentName);

extern
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles;
extern
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes;
extern
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleContributing;
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates;
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialEnergy;
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialForces;
extern
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy;
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialVirial;
extern
KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial;

void KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames(
    int * const numberOfComputeArgumentNames);
int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName(
    int const index, KIM_ComputeArgumentName * const computeArgumentName);

int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType(
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_DataType * const dataType);

#endif  /* KIM_COMPUTE_ARGUMENT_NAME_H_ */
