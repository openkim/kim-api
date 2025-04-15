//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//


#include "KIM_LogMacros.hpp"
#include "KIM_ModelHeaders.hpp"
#include <cstddef>
#include <math.h>

#define DIMENSION 3


namespace
{
class LennardJones_Ar
{
 public:
  //****************************************************************************
  LennardJones_Ar(KIM::ModelCreate * const modelCreate,
                  KIM::LengthUnit const requestedLengthUnit,
                  KIM::EnergyUnit const requestedEnergyUnit,
                  KIM::ChargeUnit const requestedChargeUnit,
                  KIM::TemperatureUnit const requestedTemperatureUnit,
                  KIM::TimeUnit const requestedTimeUnit,
                  int * const error) :
      epsilon_(0.0104),
      sigma_(3.4000),
      influenceDistance_(8.1500),
      cutoff_(influenceDistance_),
      cutoffSq_(cutoff_ * cutoff_),
      modelWillNotRequestNeighborsOfNoncontributingParticles_(1)
  {
    *error = ConvertUnits(modelCreate,
                          requestedLengthUnit,
                          requestedEnergyUnit,
                          requestedChargeUnit,
                          requestedTemperatureUnit,
                          requestedTimeUnit);
    if (*error) return;

    modelCreate->SetModelNumbering(KIM::NUMBERING::zeroBased);

    modelCreate->SetInfluenceDistancePointer(&influenceDistance_);
    modelCreate->SetNeighborListPointers(
        1, &cutoff_, &modelWillNotRequestNeighborsOfNoncontributingParticles_);

    modelCreate->SetSpeciesCode(KIM::SPECIES_NAME::Ar, 0);

    // use function pointer declarations to verify prototypes
    KIM::ModelComputeArgumentsCreateFunction * CACreate
        = LennardJones_Ar::ComputeArgumentsCreate;
    KIM::ModelComputeFunction * compute = LennardJones_Ar::Compute;
    KIM::ModelComputeArgumentsDestroyFunction * CADestroy
        = LennardJones_Ar::ComputeArgumentsDestroy;
    KIM::ModelDestroyFunction * destroy = LennardJones_Ar::Destroy;

    *error = modelCreate->SetRoutinePointer(
                 KIM::MODEL_ROUTINE_NAME::ComputeArgumentsCreate,
                 KIM::LANGUAGE_NAME::cpp,
                 true,
                 reinterpret_cast<KIM::Function *>(CACreate))
             || modelCreate->SetRoutinePointer(
                 KIM::MODEL_ROUTINE_NAME::Compute,
                 KIM::LANGUAGE_NAME::cpp,
                 true,
                 reinterpret_cast<KIM::Function *>(compute))
             || modelCreate->SetRoutinePointer(
                 KIM::MODEL_ROUTINE_NAME::ComputeArgumentsDestroy,
                 KIM::LANGUAGE_NAME::cpp,
                 true,
                 reinterpret_cast<KIM::Function *>(CADestroy))
             || modelCreate->SetRoutinePointer(
                 KIM::MODEL_ROUTINE_NAME::Destroy,
                 KIM::LANGUAGE_NAME::cpp,
                 true,
                 reinterpret_cast<KIM::Function *>(destroy));
    if (*error) return;

    // everything is good
    *error = false;
    return;
  };

  //****************************************************************************
  ~LennardJones_Ar() {};

  //****************************************************************************
  // no need to make these "extern" since KIM will only access them
  // via function pointers.  "static" is required so that there is not
  // an implicit this pointer added to the prototype by the C++ compiler
  static int Destroy(KIM::ModelDestroy * const modelDestroy)
  {
    LennardJones_Ar * model;
    modelDestroy->GetModelBufferPointer(reinterpret_cast<void **>(&model));

    if (model != NULL)
    {
      // delete object itself
      delete model;
    }

    // everything is good
    return false;
  }

  //****************************************************************************
#undef KIM_LOGGER_OBJECT_NAME
#define KIM_LOGGER_OBJECT_NAME modelCompute
  //
  static int
  Compute(KIM::ModelCompute const * const modelCompute,
          KIM::ModelComputeArguments const * const modelComputeArguments)
  {
    int const * numberOfParticlesPointer;
    int const * particleSpeciesCodes;
    int const * particleContributing;
    double const * coordinates;
    double * partialEnergy;
    double * partialForces;

    LennardJones_Ar * lj;
    modelCompute->GetModelBufferPointer(reinterpret_cast<void **>(&lj));
    double const epsilon = lj->epsilon_;
    double const sigma = lj->sigma_;
    double const cutoffSq = lj->cutoffSq_;

    int error = modelComputeArguments->GetArgumentPointer(
                    KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles,
                    &numberOfParticlesPointer)
                || modelComputeArguments->GetArgumentPointer(
                    KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes,
                    &particleSpeciesCodes)
                || modelComputeArguments->GetArgumentPointer(
                    KIM::COMPUTE_ARGUMENT_NAME::particleContributing,
                    &particleContributing)
                || modelComputeArguments->GetArgumentPointer(
                    KIM::COMPUTE_ARGUMENT_NAME::coordinates,
                    (double const **) &coordinates)
                || modelComputeArguments->GetArgumentPointer(
                    KIM::COMPUTE_ARGUMENT_NAME::partialEnergy, &partialEnergy)
                || modelComputeArguments->GetArgumentPointer(
                    KIM::COMPUTE_ARGUMENT_NAME::partialForces,
                    (double const **) &partialForces);
    if (error)
    {
      LOG_ERROR("Unable to get argument pointers");
      return error;
    }

    int const numberOfParticles = *numberOfParticlesPointer;

    // initialize energy and forces
    *partialEnergy = 0.0;
    int const extent = numberOfParticles * DIMENSION;
    for (int i = 0; i < extent; ++i) { partialForces[i] = 0.0; }

    int jContributing;
    int i, j, jj, numberOfNeighbors;
    int const * neighbors;
    double phi;
    double xcoord, ycoord, zcoord;
    double xrij, yrij, zrij;
    double rij2;
    double r2inv, r6inv, dphiByR, dEidrByR;
    double xdf, ydf, zdf;
    double const fortyEight = 48.0 * epsilon * pow(sigma, 12.0);
    double const twentyFour = 24.0 * epsilon * pow(sigma, 6.0);
    double const four12 = 4.0 * epsilon * pow(sigma, 12.0);
    double const four6 = 4.0 * epsilon * pow(sigma, 6.0);
    for (i = 0; i < numberOfParticles; ++i)
    {
      if (particleContributing[i])
      {
        xcoord = coordinates[i * DIMENSION + 0];
        ycoord = coordinates[i * DIMENSION + 1];
        zcoord = coordinates[i * DIMENSION + 2];

        modelComputeArguments->GetNeighborList(
            0, i, &numberOfNeighbors, &neighbors);

        for (jj = 0; jj < numberOfNeighbors; ++jj)
        {
          j = neighbors[jj];
          jContributing = particleContributing[j];

          if (!(jContributing && (j < i)))
          {
            xrij = coordinates[j * DIMENSION + 0] - xcoord;
            yrij = coordinates[j * DIMENSION + 1] - ycoord;
            zrij = coordinates[j * DIMENSION + 2] - zcoord;

            rij2 = xrij * xrij + yrij * yrij + zrij * zrij;

            if (rij2 < cutoffSq)
            {
              r2inv = 1.0 / rij2;
              r6inv = r2inv * r2inv * r2inv;
              phi = 0.5 * r6inv * (four12 * r6inv - four6);
              dphiByR = r6inv * (twentyFour - fortyEight * r6inv) * r2inv;

              *partialEnergy += phi;
              if (jContributing)
              {
                *partialEnergy += phi;
                dEidrByR = dphiByR;
              }
              else { dEidrByR = 0.5 * dphiByR; }

              xdf = dEidrByR * xrij;
              ydf = dEidrByR * yrij;
              zdf = dEidrByR * zrij;
              partialForces[i * DIMENSION + 0] += xdf;
              partialForces[i * DIMENSION + 1] += ydf;
              partialForces[i * DIMENSION + 2] += zdf;
              partialForces[j * DIMENSION + 0] -= xdf;
              partialForces[j * DIMENSION + 1] -= ydf;
              partialForces[j * DIMENSION + 2] -= zdf;
            }  // if (rij2 < cutoffSq_)
          }  // if (i < j)
        }  // for jj
      }  // if (particleContributing[i])
    }  // for i

    // everything is good
    return false;
  };

  //****************************************************************************
  static int ComputeArgumentsCreate(
      KIM::ModelCompute const * const /* modelCompute */,
      KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
  {
    // register arguments
    int error = modelComputeArgumentsCreate->SetArgumentSupportStatus(
                    KIM::COMPUTE_ARGUMENT_NAME::partialEnergy,
                    KIM::SUPPORT_STATUS::required)
                || modelComputeArgumentsCreate->SetArgumentSupportStatus(
                    KIM::COMPUTE_ARGUMENT_NAME::partialForces,
                    KIM::SUPPORT_STATUS::required);

    // register callbacks
    //
    // none

    return error;
  }

  //****************************************************************************
  static int
  ComputeArgumentsDestroy(KIM::ModelCompute const * const /* modelCompute */,
                          KIM::ModelComputeArgumentsDestroy * const
                          /* modelComputeArgumentsDestroy */)
  {
    // nothing further to do

    return false;
  }

 private:
  //****************************************************************************
  // Member variables
  double epsilon_;
  double sigma_;
  double influenceDistance_;
  double cutoff_;
  double cutoffSq_;
  int const modelWillNotRequestNeighborsOfNoncontributingParticles_;

  //****************************************************************************
#undef KIM_LOGGER_OBJECT_NAME
#define KIM_LOGGER_OBJECT_NAME modelCreate
  //
  int ConvertUnits(KIM::ModelCreate * const modelCreate,
                   KIM::LengthUnit const requestedLengthUnit,
                   KIM::EnergyUnit const requestedEnergyUnit,
                   KIM::ChargeUnit const requestedChargeUnit,
                   KIM::TemperatureUnit const requestedTemperatureUnit,
                   KIM::TimeUnit const requestedTimeUnit)
  {
    int ier;

    // define default base units
    KIM::LengthUnit fromLength = KIM::LENGTH_UNIT::A;
    KIM::EnergyUnit fromEnergy = KIM::ENERGY_UNIT::eV;
    KIM::ChargeUnit fromCharge = KIM::CHARGE_UNIT::unused;
    KIM::TemperatureUnit fromTemperature = KIM::TEMPERATURE_UNIT::unused;
    KIM::TimeUnit fromTime = KIM::TIME_UNIT::unused;

    // changing units of cutoffs and sigmas
    double convertLength = 1.0;
    ier = KIM::ModelCreate::ConvertUnit(fromLength,
                                        fromEnergy,
                                        fromCharge,
                                        fromTemperature,
                                        fromTime,
                                        requestedLengthUnit,
                                        requestedEnergyUnit,
                                        requestedChargeUnit,
                                        requestedTemperatureUnit,
                                        requestedTimeUnit,
                                        1.0,
                                        0.0,
                                        0.0,
                                        0.0,
                                        0.0,
                                        &convertLength);
    if (ier)
    {
      LOG_ERROR("Unable to convert length unit");
      return ier;
    }
    influenceDistance_ *= convertLength;  // convert to active units
    cutoff_ = influenceDistance_;
    cutoffSq_ = cutoff_ * cutoff_;
    sigma_ *= convertLength;  // convert to active units

    // changing units of epsilons
    double convertEnergy = 1.0;
    ier = KIM::ModelCreate::ConvertUnit(fromLength,
                                        fromEnergy,
                                        fromCharge,
                                        fromTemperature,
                                        fromTime,
                                        requestedLengthUnit,
                                        requestedEnergyUnit,
                                        requestedChargeUnit,
                                        requestedTemperatureUnit,
                                        requestedTimeUnit,
                                        0.0,
                                        1.0,
                                        0.0,
                                        0.0,
                                        0.0,
                                        &convertEnergy);
    if (ier)
    {
      LOG_ERROR("Unable to convert energy unit");
      return ier;
    }
    epsilon_ *= convertEnergy;  // convert to active units

    // register units
    ier = modelCreate->SetUnits(requestedLengthUnit,
                                requestedEnergyUnit,
                                KIM::CHARGE_UNIT::unused,
                                KIM::TEMPERATURE_UNIT::unused,
                                KIM::TIME_UNIT::unused);
    if (ier)
    {
      LOG_ERROR("Unable to set units to requested values");
      return ier;
    }

    // everything is good
    ier = false;
    return ier;
  }
};

}  // namespace

extern "C" {
//******************************************************************************
int model_create(KIM::ModelCreate * const modelCreate,
                 KIM::LengthUnit const requestedLengthUnit,
                 KIM::EnergyUnit const requestedEnergyUnit,
                 KIM::ChargeUnit const requestedChargeUnit,
                 KIM::TemperatureUnit const requestedTemperatureUnit,
                 KIM::TimeUnit const requestedTimeUnit)
{
  int error;

  LennardJones_Ar * const model = new LennardJones_Ar(modelCreate,
                                                      requestedLengthUnit,
                                                      requestedEnergyUnit,
                                                      requestedChargeUnit,
                                                      requestedTemperatureUnit,
                                                      requestedTimeUnit,
                                                      &error);
  if (error)
  {
    // constructor already reported the error
    delete model;
    return error;
  }

  // register pointer to LennardJones_Ar object in moedelCreate object
  modelCreate->SetModelBufferPointer(reinterpret_cast<void *>(model));

  // everything is good
  return false;
}
}  // extern "C"
