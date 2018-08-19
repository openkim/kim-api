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
// Copyright (c) 2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

#include <math.h>
#include "KIM_ModelHeaders.hpp"

#define DIMENSION 3


namespace
{

class LennardJones_Ar
{
 public:
  //****************************************************************************
  LennardJones_Ar(
      KIM::ModelCreate * const modelCreate,
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
      cutoffSq_(cutoff_*cutoff_),
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
        1,
        &cutoff_,
        &modelWillNotRequestNeighborsOfNoncontributingParticles_);

    modelCreate->SetSpeciesCode(KIM::SPECIES_NAME::Ar, 0);

    *error = *error || modelCreate->SetDestroyPointer(
        KIM::LANGUAGE_NAME::cpp,
        reinterpret_cast<KIM::func *>(&(LennardJones_Ar::Destroy)));
    *error = *error || modelCreate->SetRefreshPointer(
        KIM::LANGUAGE_NAME::cpp,
        reinterpret_cast<KIM::func *>(&(LennardJones_Ar::Refresh)));
    *error = *error || modelCreate->SetComputePointer(
        KIM::LANGUAGE_NAME::cpp,
        reinterpret_cast<KIM::func *>(&(LennardJones_Ar::Compute)));
    *error = *error || modelCreate->SetComputeArgumentsCreatePointer(
        KIM::LANGUAGE_NAME::cpp,
        reinterpret_cast<KIM::func *>
        (&(LennardJones_Ar::ComputeArgumentsCreate)));
    *error = *error || modelCreate->SetComputeArgumentsDestroyPointer(
        KIM::LANGUAGE_NAME::cpp,
        reinterpret_cast<KIM::func *>
        (&(LennardJones_Ar::ComputeArgumentsDestroy)));
    if (*error) return;

    // everything is good
    *error = false;
    return;
  };

  //****************************************************************************
  ~LennardJones_Ar(){};

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
  static int Refresh(KIM::ModelRefresh * const modelRefresh)
  {
    LennardJones_Ar * model;
    modelRefresh->GetModelBufferPointer(reinterpret_cast<void **>(&model));

    // nothing to do

    modelRefresh->SetInfluenceDistancePointer(&(model->influenceDistance_));
    modelRefresh->SetNeighborListPointers(
        1,
        &(model->cutoff_),
        &(model->modelWillNotRequestNeighborsOfNoncontributingParticles_));

    // everything is good
    return false;
  };

  //****************************************************************************
#include "KIM_ModelComputeLogMacros.hpp"
  static int Compute(
      KIM::ModelCompute const * const modelCompute,
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

    int error =
        modelComputeArguments->GetArgumentPointer(
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
            KIM::COMPUTE_ARGUMENT_NAME::partialEnergy,
            &partialEnergy)
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
    int const extent = numberOfParticles*DIMENSION;
    for (int i = 0; i < extent; ++i)
    {
      partialForces[i] = 0.0;
    }

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
        xcoord = coordinates[i*DIMENSION + 0];
        ycoord = coordinates[i*DIMENSION + 1];
        zcoord = coordinates[i*DIMENSION + 2];

        modelComputeArguments->GetNeighborList(
            0, i, &numberOfNeighbors, &neighbors);

        for (jj = 0; jj < numberOfNeighbors; ++jj)
        {
          j = neighbors[jj];
          jContributing = particleContributing[j];

          if (! (jContributing && (j < i)))
          {
            xrij = coordinates[j*DIMENSION + 0] - xcoord;
            yrij = coordinates[j*DIMENSION + 1] - ycoord;
            zrij = coordinates[j*DIMENSION + 2] - zcoord;

            rij2 = xrij*xrij + yrij*yrij + zrij*zrij;

            if (rij2 < cutoffSq)
            {
              r2inv = 1.0/rij2;
              r6inv = r2inv*r2inv*r2inv;
              phi = 0.5 * r6inv * (four12*r6inv - four6);
              dphiByR = r6inv * (twentyFour - fortyEight*r6inv) * r2inv;

              *partialEnergy += phi;
              if (jContributing)
              {
                *partialEnergy += phi;
                dEidrByR = dphiByR;
              }
              else
              {
                dEidrByR = 0.5*dphiByR;
              }

              xdf = dEidrByR * xrij;
              ydf = dEidrByR * yrij;
              zdf = dEidrByR * zrij;
              partialForces[i*DIMENSION + 0] += xdf;
              partialForces[i*DIMENSION + 1] += ydf;
              partialForces[i*DIMENSION + 2] += zdf;
              partialForces[j*DIMENSION + 0] -= xdf;
              partialForces[j*DIMENSION + 1] -= ydf;
              partialForces[j*DIMENSION + 2] -= zdf;
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
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
  {
    // register arguments
    int error =
        modelComputeArgumentsCreate->SetArgumentSupportStatus(
            KIM::COMPUTE_ARGUMENT_NAME::partialEnergy,
            KIM::SUPPORT_STATUS::required)
        || modelComputeArgumentsCreate->SetArgumentSupportStatus(
            KIM::COMPUTE_ARGUMENT_NAME::partialForces,
            KIM::SUPPORT_STATUS::required);

    (void)modelCompute;  // avoid unused parameter warning

    // register callbacks
    //
    // none

    return error;
  }
  //****************************************************************************
  static int ComputeArgumentsDestroy(
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
  {
    // noting to do

    (void)modelCompute;  // avoid unused parameter warnings
    (void)modelComputeArgumentsDestroy;

    // everything is good
    return false;
  };

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
#include "KIM_ModelCreateLogMacros.hpp"
  int ConvertUnits(
      KIM::ModelCreate * const modelCreate,
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
    ier = modelCreate->ConvertUnit(
        fromLength, fromEnergy, fromCharge, fromTemperature, fromTime,
        requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
        requestedTemperatureUnit, requestedTimeUnit,
        1.0, 0.0, 0.0, 0.0, 0.0,
        &convertLength);
    if (ier)
    {
      LOG_ERROR("Unable to convert length unit");
      return ier;
    }
    influenceDistance_ *= convertLength;  // convert to active units
    cutoff_ = influenceDistance_;
    cutoffSq_ = cutoff_*cutoff_;
    sigma_ *= convertLength;  // convert to active units

    // changing units of epsilons
    double convertEnergy = 1.0;
    ier = modelCreate->ConvertUnit(
        fromLength, fromEnergy, fromCharge, fromTemperature, fromTime,
        requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
        requestedTemperatureUnit, requestedTimeUnit,
        0.0, 1.0, 0.0, 0.0, 0.0,
        &convertEnergy);
    if (ier)
    {
      LOG_ERROR("Unable to convert energy unit");
      return ier;
    }
    epsilon_ *= convertEnergy;  // convert to active units

    // register units
    ier = modelCreate->SetUnits(
        requestedLengthUnit,
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

extern "C"
{
//******************************************************************************
int model_create(
    KIM::ModelCreate * const modelCreate,
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
