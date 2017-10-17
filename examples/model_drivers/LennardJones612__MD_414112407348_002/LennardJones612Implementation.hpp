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
// Copyright (c) 2015, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Andrew Akerson
//


#ifndef LENNARD_JONES_612_IMPLEMENTATION_HPP_
#define LENNARD_JONES_612_IMPLEMENTATION_HPP_

#include <vector>
#include <map>
#include "KIM_LogVerbosity.hpp"
#include "LennardJones612.hpp"

#define DIMENSION 3
#define ONE 1.0
#define HALF 0.5

#define MAX_PARAMETER_FILES 1

#define PARAM_SHIFT_INDEX 0
#define PARAM_CUTOFFS_INDEX 1
#define PARAM_EPSILONS_INDEX 2
#define PARAM_SIGMAS_INDEX 3


//==============================================================================
//
// Type definitions, enumerations, and helper function prototypes
//
//==============================================================================

// type declaration for get neighbor functions
typedef int (GetNeighborFunction)(void const * const, int const,
                                  int * const, int const ** const);
// type declaration for vector of constant dimension
typedef double VectorOfSizeDIM[DIMENSION];

// helper routine declarations
void AllocateAndInitialize2DArray(double**& arrayPtr, int const extentZero,
                                  int const extentOne);
void Deallocate2DArray(double**& arrayPtr);

//==============================================================================
//
// Declaration of LennardJones612Implementation class
//
//==============================================================================

//******************************************************************************
class LennardJones612Implementation
{
 public:
  LennardJones612Implementation(
      KIM::ModelDriverCreate * const modelDriverCreate,
      KIM::LengthUnit const requestedLengthUnit,
      KIM::EnergyUnit const requestedEnergyUnit,
      KIM::ChargeUnit const requestedChargeUnit,
      KIM::TemperatureUnit const requestedTemperatureUnit,
      KIM::TimeUnit const requestedTimeUnit,
      int * const ier);
  ~LennardJones612Implementation();  // no explicit Destroy() needed here

  int Refresh(KIM::ModelRefresh * const modelRefresh);
  int Compute(KIM::ModelCompute const * const modelCompute);

 private:
  // Constant values that never change
  //   Set in constructor (via SetConstantValues)
  //
  //
  // LennardJones612Implementation: constants
  int numberModelSpecies_;
  std::vector<int> modelSpeciesCodeList_;
  int numberUniqueSpeciesPairs_;


  // Constant values that are read from the input files and never change
  //   Set in constructor (via functions listed below)
  //
  //
  // KIM API: Model Fixed Parameters
  //   Memory allocated in   AllocateFixedParameterMemory()
  //   Memory deallocated in destructor
  //   Data set in ReadParameterFile routines
  // none
  //
  // KIM API: Model Free Parameters whose (pointer) values never change
  //   Memory allocated in   AllocateFreeParameterMemory() (from constructor)
  //   Memory deallocated in destructor
  //   Data set in ReadParameterFile routines OR by KIM Simulator
  int shift_;
  double* cutoffs_;
  double* epsilons_;
  double* sigmas_;

  // Mutable values that only change when reinit() executes
  //   Set in Reinit (via SetReinitMutableValues)
  //
  //
  // KIM API: Model Fixed Parameters
  // none
  //
  // KIM API: Model Free Parameters
  // none
  //
  // LennardJones612Implementation: values
  double influenceDistance_;
  double** cutoffsSq2D_;
  double** fourEpsilonSigma6_2D_;
  double** fourEpsilonSigma12_2D_;
  double** twentyFourEpsilonSigma6_2D_;
  double** fortyEightEpsilonSigma12_2D_;
  double** oneSixtyEightEpsilonSigma6_2D_;
  double** sixTwentyFourEpsilonSigma12_2D_;
  double** shifts2D_;


  // Mutable values that can change with each call to Reinit() and Compute()
  //   Memory may be reallocated on each call
  //
  //
  // LennardJones612Implementation: values that change
  int cachedNumberOfParticles_;


  // Helper methods
  //
  //
  // Related to constructor
  void AllocateFreeParameterMemory();
  static int OpenParameterFiles(
      KIM::ModelDriverCreate * const modelDriverCreate,
      int const numberParameterFiles,
      FILE* parameterFilePointers[MAX_PARAMETER_FILES]);
  static void CloseParameterFiles(
      int const numberParameterFiles,
      FILE* const parameterFilePointers[MAX_PARAMETER_FILES]);
  int ProcessParameterFiles(
      KIM::ModelDriverCreate * const modelDriverCreate,
      int const numberParameterFiles,
      FILE* const parameterFilePointers[MAX_PARAMETER_FILES]);
  void getNextDataLine(FILE* const filePtr, char* const nextLine,
                       int const maxSize, int* endOfFileFlag);
  int ConvertUnits(
      KIM::ModelDriverCreate * const modelDriverCreate,
      KIM::LengthUnit const requestedLengthUnit,
      KIM::EnergyUnit const requestedEnergyUnit,
      KIM::ChargeUnit const requestedChargeUnit,
      KIM::TemperatureUnit const requestedTemperatureUnit,
      KIM::TimeUnit const requestedTimeUnit);
  int RegisterKIMModelSettings(
      KIM::ModelDriverCreate * const modelDriverCreate);
  int RegisterKIMParameters(
      KIM::ModelDriverCreate * const modelDriverCreate);
  int RegisterKIMFunctions(
      KIM::ModelDriverCreate * const modelDriverCreate) const;
  //
  // Related to Reinit()
  template<class ModelObj>
  int SetReinitMutableValues(ModelObj * const modelObj);

  //
  // Related to Compute()
  int SetComputeMutableValues(KIM::ModelCompute const * const modelCompute,
                              bool& isComputeProcess_dEdr,
                              bool& isComputeProcess_d2Edr2,
                              bool& isComputeEnergy,
                              bool& isComputeForces,
                              bool& isComputeParticleEnergy,
                              int const*& particleSpeciesCodes,
                              int const*& particleContributing,
                              VectorOfSizeDIM const*& coordinates,
                              double*& energy,
                              double*& particleEnergy,
                              VectorOfSizeDIM*& forces);
  int CheckParticleSpeciesCodes(KIM::ModelCompute const * const modelCompute,
                                int const* const particleSpeciesCodes) const;
  int GetComputeIndex(const bool& isComputeProcess_dEdr,
                      const bool& isComputeProcess_d2Edr2,
                      const bool& isComputeEnergy,
                      const bool& isComputeForces,
                      const bool& isComputeParticleEnergy,
                      const bool& isShift) const;

  // compute functions
  template< bool isComputeProcess_dEdr, bool isComputeProcess_d2Edr2,
            bool isComputeEnergy, bool isComputeForces,
            bool isComputeParticleEnergy, bool isShift >
  int Compute(KIM::ModelCompute const * const modelCompute,
              const int* const particleSpeciesCodes,
              const int* const particleContributing,
              const VectorOfSizeDIM* const coordinates,
              double* const energy,
              VectorOfSizeDIM* const forces,
              double* const particleEnergy);
};

//==============================================================================
//
// Definition of LennardJones612Implementation::Compute functions
//
// NOTE: Here we rely on the compiler optimizations to prune dead code
//       after the template expansions.  This provides high efficiency
//       and easy maintenance.
//
//==============================================================================

//******************************************************************************
// MACRO to compute Lennard-Jones phi
// (used for efficiency)
//
// exshift - expression to be added to the end of the phi value
#define LENNARD_JONES_PHI(exshift)                                      \
  phi = r6iv * (constFourEpsSig12_2D[iSpecies][jSpecies]*r6iv -         \
                constFourEpsSig6_2D[iSpecies][jSpecies]) exshift;

//******************************************************************************
#include "KIM_ModelComputeLogMacros.hpp"
template< bool isComputeProcess_dEdr, bool isComputeProcess_d2Edr2,
          bool isComputeEnergy, bool isComputeForces,
          bool isComputeParticleEnergy, bool isShift >
int LennardJones612Implementation::Compute(
    KIM::ModelCompute const * const modelCompute,
    const int* const particleSpeciesCodes,
    const int* const particleContributing,
    const VectorOfSizeDIM* const coordinates,
    double* const energy,
    VectorOfSizeDIM* const forces,
    double* const particleEnergy)
{
  int ier = false;

  if ((isComputeEnergy == false) &&
      (isComputeParticleEnergy == false) &&
      (isComputeForces == false) &&
      (isComputeProcess_dEdr == false) &&
      (isComputeProcess_d2Edr2 == false))
    return ier;

  // initialize energy and forces
  if (isComputeEnergy == true)
  {
    *energy = 0.0;
  }
  if (isComputeParticleEnergy == true)
  {
    int const cachedNumParticles = cachedNumberOfParticles_;
    for (int i = 0; i < cachedNumParticles; ++i)
    {
      particleEnergy[i] = 0.0;
    }
  }
  if (isComputeForces == true)
  {
    int const cachedNumParticles = cachedNumberOfParticles_;
    for (int i = 0; i < cachedNumParticles; ++i)
    {
      for (int j = 0; j < DIMENSION; ++j)
        forces[i][j] = 0.0;
    }
  }

  // calculate contribution from pair function
  //
  // Setup loop over contributing particles
  int ii = 0;
  int numnei = 0;
  int const * n1atom = 0;
  double const* const* const  constCutoffsSq2D = cutoffsSq2D_;
  double const* const* const  constFourEpsSig6_2D = fourEpsilonSigma6_2D_;
  double const* const* const  constFourEpsSig12_2D = fourEpsilonSigma12_2D_;
  double const* const* const  constTwentyFourEpsSig6_2D
      = twentyFourEpsilonSigma6_2D_;
  double const* const* const  constFortyEightEpsSig12_2D
      = fortyEightEpsilonSigma12_2D_;
  double const* const* const  constOneSixtyEightEpsSig6_2D
      = oneSixtyEightEpsilonSigma6_2D_;
  double const* const* const  constSixTwentyFourEpsSig12_2D
      = sixTwentyFourEpsilonSigma12_2D_;
  double const* const* const  constShifts2D = shifts2D_;
  for (ii = 0; ii < cachedNumberOfParticles_; ++ii)
  {
    if (particleContributing[ii])
    {
      modelCompute->GetNeighborList(0, ii, &numnei, &n1atom);
      int const numNei = numnei;
      int const * const n1Atom = n1atom;
      int const i = ii;
      int const iSpecies = particleSpeciesCodes[i];

      // Setup loop over neighbors of current particle
      for (int jj = 0; jj < numNei; ++jj)
      {
        int const j = n1Atom[jj];
        int const jSpecies = particleSpeciesCodes[j];
        double* r_ij;
        double r_ijValue[DIMENSION];
        // Compute r_ij
        r_ij = r_ijValue;
        for (int k = 0; k < DIMENSION; ++k)
          r_ij[k] = coordinates[j][k] - coordinates[i][k];
        double const* const r_ij_const = const_cast<double*>(r_ij);

        // compute distance squared
        double const rij2 =
            r_ij_const[0] * r_ij_const[0] +
            r_ij_const[1] * r_ij_const[1] +
            r_ij_const[2] * r_ij_const[2];

        if (rij2 <= constCutoffsSq2D[iSpecies][jSpecies])
        { // compute contribution to energy, force, etc.
          double phi = 0.0;
          double dphiByR = 0.0;
          double d2phi = 0.0;
          double dEidrByR = 0.0;
          double d2Eidr2 = 0.0;
          double const r2iv = 1.0/rij2;
          double const r6iv = r2iv*r2iv*r2iv;
          // Compute pair potential and its derivatives
          if (isComputeProcess_d2Edr2 == true)
          { // Compute d2phi
            d2phi =
                r6iv * (constSixTwentyFourEpsSig12_2D[iSpecies][jSpecies]*r6iv -
                        constOneSixtyEightEpsSig6_2D[iSpecies][jSpecies])
                * r2iv;
            d2Eidr2 = 0.5*d2phi;
          }

          if ((isComputeProcess_dEdr == true) || (isComputeForces == true))
          { // Compute dphi
            dphiByR =
                r6iv * (constTwentyFourEpsSig6_2D[iSpecies][jSpecies] -
                        constFortyEightEpsSig12_2D[iSpecies][jSpecies]*r6iv)
                * r2iv;
            dEidrByR = 0.5*dphiByR;
          }

          if ((isComputeEnergy == true) || (isComputeParticleEnergy == true))
          { // Compute phi
            if (isShift == true)
            {
              LENNARD_JONES_PHI(- constShifts2D[iSpecies][jSpecies]);
            }
            else
            {
              LENNARD_JONES_PHI(;);
            }
          }

          // Contribution to energy
          if (isComputeEnergy == true)
          {
            *energy += 0.5*phi;
          }

          // Contribution to particleEnergy
          if (isComputeParticleEnergy == true)
          {
            double const halfPhi = 0.5*phi;
            particleEnergy[i] += halfPhi;
          }

          // Contribution to forces
          if (isComputeForces == true)
          {
            for (int k = 0; k < DIMENSION; ++k)
            {
              double const contrib = dEidrByR * r_ij_const[k];
              forces[i][k] += contrib;
              forces[j][k] -= contrib;
            }
          }

          // Call process_dEdr
          if (isComputeProcess_dEdr == true)
          {
            double const rij = sqrt(rij2);
            double const dEidr = dEidrByR*rij;
            ier = modelCompute->ProcessDEDrTerm(dEidr, rij, r_ij_const, i, j);
            if (ier)
            {
              LOG_ERROR("process_dEdr");
              return ier;
            }
          }

          // Call process_d2Edr2
          if (isComputeProcess_d2Edr2 == true)
          {
            double const rij = sqrt(rij2);
            double const R_pairs[2] = {rij, rij};
            double const* const pRs = &R_pairs[0];
            double const Rij_pairs[6]
                = {r_ij_const[0], r_ij_const[1], r_ij_const[2],
                   r_ij_const[0], r_ij_const[1], r_ij_const[2]};
            double const* const pRijConsts = &Rij_pairs[0];
            int const i_pairs[2] = {i, i};
            int const j_pairs[2] = {j, j};
            int const* const pis = &i_pairs[0];
            int const* const pjs = &j_pairs[0];

            ier = modelCompute->ProcessD2EDr2Term(d2Eidr2, pRs, pRijConsts, pis,
                                                  pjs);
            if (ier)
            {
              LOG_ERROR("process_d2Edr2");
              return ier;
            }
          }
        }  // if particleContributing
      }  // if particles i and j interact
    }  // end of first neighbor loop
  }  // end of loop over contributing particles

  // everything is good
  ier = false;
  return ier;
}

#endif  // LENNARD_JONES_612_IMPLEMENTATION_HPP_
