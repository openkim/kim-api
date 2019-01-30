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


#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#define KIM_COMPUTE_ARGUMENT_NAME_H_

/* Forward declarations */
#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
typedef struct KIM_DataType KIM_DataType;
#endif


/**
 ** \brief \copybrief KIM::ComputeArgumentName
 **
 ** \sa KIM::ComputeArgumentName
 **
 ** \since 2.0
 **/
struct KIM_ComputeArgumentName
{
  /**
   ** \brief \copybrief KIM::ComputeArgumentName::computeArgumentNameID
   **
   ** \sa KIM::ComputeArgumentName::computeArgumentNameID
   **
   ** \since 2.0
   **/
  int computeArgumentNameID;
};
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ComputeArgumentName KIM_ComputeArgumentName;
#endif

/**
 ** \brief \copybrief <!--
 ** --> KIM::ComputeArgumentName::ComputeArgumentName(std::string const &)
 **
 ** \sa KIM::ComputeArgumentName::ComputeArgumentName(std::string const &)
 **
 ** \since 2.0
 **/
KIM_ComputeArgumentName
KIM_ComputeArgumentName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::ComputeArgumentName::Known
 **
 ** \sa KIM::ComputeArgumentName::Known
 **
 ** \since 2.0
 **/
int KIM_ComputeArgumentName_Known(
    KIM_ComputeArgumentName const computeArgumentName);

/**
 ** \brief \copybrief KIM::ComputeArgumentName::operator==()
 **
 ** \sa KIM::ComputeArgumentName::operator==()
 **
 ** \since 2.0
 **/
int KIM_ComputeArgumentName_Equal(KIM_ComputeArgumentName const lhs,
                                  KIM_ComputeArgumentName const rhs);

/**
 ** \brief \copybrief KIM::ComputeArgumentName::operator!=()
 **
 ** \sa KIM::ComputeArgumentName::operator!=()
 **
 ** \since 2.0
 **/
int KIM_ComputeArgumentName_NotEqual(KIM_ComputeArgumentName const lhs,
                                     KIM_ComputeArgumentName const rhs);

/**
 ** \brief \copybrief KIM::ComputeArgumentName::ToString
 **
 ** \sa KIM::ComputeArgumentName::ToString
 **
 ** \since 2.0
 **/
char const * KIM_ComputeArgumentName_ToString(
    KIM_ComputeArgumentName const computeArgumentName);

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const
    KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const
    KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::particleContributing
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::particleContributing
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const
    KIM_COMPUTE_ARGUMENT_NAME_particleContributing;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::coordinates
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::coordinates
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_coordinates;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialEnergy
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::partialEnergy
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialEnergy;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialForces
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::partialForces
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialForces;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const
    KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialVirial
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::partialVirial
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const KIM_COMPUTE_ARGUMENT_NAME_partialVirial;

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial
 **
 ** \since 2.0
 **/
extern KIM_ComputeArgumentName const
    KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial;

/**
 ** \brief \copybrief <!--
 ** --> KIM::COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames
 **
 ** \since 2.0
 **/
void KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames(
    int * const numberOfComputeArgumentNames);

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName
 **
 ** \since 2.0
 **/
int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName(
    int const index, KIM_ComputeArgumentName * const computeArgumentName);

/**
 ** \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentDataType
 **
 ** \sa KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentDataType
 **
 ** \since 2.0
 **/
int KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType(
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_DataType * const dataType);

#endif /* KIM_COMPUTE_ARGUMENT_NAME_H_ */
