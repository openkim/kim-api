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


#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#define KIM_COMPUTE_ARGUMENT_NAME_H_

struct KIM_COMPUTE_ArgumentName
{
  int argumentID;
};
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_COMPUTE_ArgumentName KIM_COMPUTE_ArgumentName;
#endif

extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfSpecies;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleSpecies;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleContributing;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_energy;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_forces;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleEnergy;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_virial;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleVirial;
extern KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_hessian;

#endif  /* KIM_COMPUTE_ARGUMENT_NAME_H_ */
