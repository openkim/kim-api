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

#ifndef KIM_COMPUTE_H_
#include "KIM_Compute.h"
#endif


/* Order doesn't matter as long as all values are unique */
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles = {0};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_numberOfSpecies = {1};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleSpecies = {2};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleContributing = {3};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates = {4};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_get_neigh = {5};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_process_dEdr = {6};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_process_d2Edr2 = {7};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_neighObject = {8};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_compute = {9};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_reinit = {10};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_destroy = {11};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_energy = {12};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_forces = {13};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleEnergy = {14};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_virial = {15};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_particleVirial = {16};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_hessian = {17};
KIM_COMPUTE_ArgumentName const KIM_COMPUTE_ARGUMENT_NAME_End = {-32000};
