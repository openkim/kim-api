//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2.0.0-beta.2 package.
//


#ifndef KIM_FUNCTION_TYPES_HPP_
#define KIM_FUNCTION_TYPES_HPP_

namespace KIM
{
// Forward declarations
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ModelCreate;
class ModelDriverCreate;
class ModelCompute;
class ModelComputeArgumentsCreate;
class ModelComputeArguments;
class ModelRefresh;
class ModelComputeArgumentsDestroy;
class ModelDestroy;


typedef void(Function)(void);  // Generic funciton pointer

typedef int ModelCreateFunction(ModelCreate * const modelCreate,
                                LengthUnit const requestedLengthUnit,
                                EnergyUnit const requestedEnergyUnit,
                                ChargeUnit const requestedChargeUnit,
                                TemperatureUnit const requestedTemperatureUnit,
                                TimeUnit const requestedTimeUnit);

typedef int
ModelDriverCreateFunction(ModelDriverCreate * const modelDriverCreate,
                          LengthUnit const requestedLengthUnit,
                          EnergyUnit const requestedEnergyUnit,
                          ChargeUnit const requestedChargeUnit,
                          TemperatureUnit const requestedTemperatureUnit,
                          TimeUnit const requestedTimeUnit);

typedef int ModelComputeArgumentsCreateFunction(
    ModelCompute const * const modelCompute,
    ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);

typedef int ModelComputeFunction(
    ModelCompute const * const modelCompute,
    ModelComputeArguments const * const modelComputeArgumentsCreate);

typedef int GetNeighborListFunction(void * const dataObject,
                                    int const numberOfNeighborLists,
                                    double const * const cutoffs,
                                    int const neighborListIndex,
                                    int const particleNumber,
                                    int * const numberOfNeighbors,
                                    int const ** const neighborsOfParticle);

typedef int ProcessDEDrTermFunction(void * const dataObject,
                                    double const de,
                                    double const r,
                                    double const * const dx,
                                    int const i,
                                    int const j);

typedef int ProcessD2EDr2TermFunction(void * const dataObject,
                                      double const de,
                                      double const * const r,
                                      double const * const dx,
                                      int const * const i,
                                      int const * const j);

typedef int ModelRefreshFunction(ModelRefresh * const modelRefresh);

typedef int ModelComputeArgumentsDestroyFunction(
    ModelCompute const * const modelCompute,
    ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

typedef int ModelDestroyFunction(ModelDestroy * const);
}  // namespace KIM

#endif  // KIM_FUNCTION_TYPES_HPP_
