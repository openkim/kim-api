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

#include <map>
#include "LennardJones612.hpp"
#include "KIM_API_status.h"

#define DIMENSION 3
#define ONE 1.0
#define HALF 0.5

#define MAX_PARAMETER_FILES 1


//==============================================================================
//
// Type definitions, enumerations, and helper function prototypes
//
//==============================================================================

// type declaration for get neighbor functions
typedef int (GetNeighborFunction)(void**, int*, int*, int*, int*, int**,
                                  double**);
// type declaration for vector of constant dimension
typedef double VectorOfSizeDIM[DIMENSION];

// enumeration for the different types of NBC's
enum NBCTypeEnum {Neigh_Rvec, Neigh_Pure, Mi_Opbc, Cluster};
// enumeration for the different methods of computing rij
enum RijEnum {Coordinates, RVec, MI_OPBC};

//==============================================================================
//
// Helper class definitions
//
//==============================================================================

// Iterator object for CLUSTER NBC
class ClusterIterator
{
 private:
  int* list_;
  int baseconvert_;
  int const cachedNumberContributingParticles_;
  int request_;
 public:
  ClusterIterator(KIM_API_model* const pkim,
                  GetNeighborFunction* const get_neigh,
                  int const baseconvert,
                  int const cachedNumberContributingParticles,
                  int* const i,
                  int* const numnei,
                  int** const n1atom,
                  double** const pRij)
      : baseconvert_(baseconvert),
        cachedNumberContributingParticles_(cachedNumberContributingParticles),
        request_(0)
  {
    // allocate memory for neighbor list
    list_ = new int[cachedNumberContributingParticles_];
    for (int k = 0; k < cachedNumberContributingParticles_; ++k)
      list_[k] = k - baseconvert_;

    *i = request_;
    // CLUSTER always uses half-list behavior
    *numnei = cachedNumberContributingParticles_ - request_ - 1;
    *n1atom = &(list_[request_ + 1]);
    *pRij = NULL;
  }
  ~ClusterIterator()
  {
    delete [] list_;
  }
  bool done() const
  {
    return !(request_ < cachedNumberContributingParticles_);
  }
  int next(int* const i, int* const numnei, int** const n1atom,
           double** const pRij)
  {
    ++request_;

    *i = request_;
    *numnei = cachedNumberContributingParticles_ - request_ - 1;
    *n1atom = &(list_[request_ + 1]);
    *pRij = NULL;
    return KIM_STATUS_OK;
  }
};

// Iterator object for Locator mode access to neighbor list
class LocatorIterator
{
 private:
  KIM_API_model* const pkim_;
  GetNeighborFunction* const get_neigh_;
  int const baseconvert_;
  int const cachedNumberContributingParticles_;
  int request_;
  int const mode_;
 public:
  LocatorIterator(KIM_API_model* const pkim,
                  GetNeighborFunction* const get_neigh,
                  int const baseconvert,
                  int const cachedNumberContributingParticles,
                  int* const i,
                  int* const numnei,
                  int** const n1atom,
                  double** const pRij)
      : pkim_(pkim),
        get_neigh_(get_neigh),
        baseconvert_(baseconvert),
        cachedNumberContributingParticles_(cachedNumberContributingParticles),
        request_(-baseconvert_),  // set to first value (test-based indexing)
    mode_(1)  // locator mode
  {
    next(i, numnei, n1atom, pRij);
  }
  bool done() const
  {
    return !(request_ + baseconvert_ <= cachedNumberContributingParticles_);
  }
  int next(int* const i, int* const numnei, int** const n1atom,
           double** const pRij)
  {
    int ier;
    // Allow for request_ to be incremented to one more than contributing
    // without causing an error/warning from the openkim-api
    int req = std::min(request_,
                       cachedNumberContributingParticles_-baseconvert_-1);
    ier = (*get_neigh_)(
        reinterpret_cast<void**>(const_cast<KIM_API_model**>(&pkim_)),
        (int*) &mode_,
        &req,
        (int*) i,
        (int*) numnei,
        (int**) n1atom,
        (double**) pRij);
    *i += baseconvert_;  // adjust index of current particle

    ++request_;
    return ier;
  }
};

// Iterator object for Iterator mode access to neighbor list
class IteratorIterator
{
 private:
  KIM_API_model* const pkim_;
  GetNeighborFunction* const get_neigh_;
  int const baseconvert_;
  int request_;
  int const mode_;
  int ier_;
 public:
  IteratorIterator(KIM_API_model* const pkim,
                   GetNeighborFunction* const get_neigh,
                   int const baseconvert,
                   int const cachedNumberContributingParticles,
                   int* const i,
                   int* const numnei,
                   int** const n1atom,
                   double** const pRij)
      : pkim_(pkim),
        get_neigh_(get_neigh),
        baseconvert_(baseconvert),
        request_(0),  // reset iterator
        mode_(0),     // iterator mode
        ier_(KIM_STATUS_FAIL)
  {
    ier_ = (*get_neigh_)(
        reinterpret_cast<void**>(const_cast<KIM_API_model**>(&pkim_)),
        (int*) &mode_, &request_, i, numnei, n1atom, pRij);
    if (ier_ != KIM_STATUS_NEIGH_ITER_INIT_OK)
    {
      pkim->report_error(__LINE__, __FILE__, "iterator init failed", ier_);
      ier_ = KIM_STATUS_FAIL;
    }
    request_ = 1;  // increment iterator
    // return initial set of neighbors
    next(i, numnei, n1atom, pRij);
  }
  bool done() const
  {
    return ((ier_ == KIM_STATUS_FAIL) ||
            (ier_ == KIM_STATUS_NEIGH_ITER_PAST_END));
  }
  int next(int* const i, int* const numnei, int** const n1atom,
           double** const pRij)
  {
    ier_ = (*get_neigh_)(
        reinterpret_cast<void**>(const_cast<KIM_API_model**>(&pkim_)),
        (int*) &mode_,
        &request_,
        (int*) i,
        (int*) numnei,
        (int**) n1atom,
        (double**) pRij);
    *i += baseconvert_;  // adjust index of current particle

    return ier_;
  }
};

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
      KIM_API_model* const pkim,
      char const* const parameterFileNames,
      int const parameterFileNameLength,
      int const numberParameterFiles,
      int* const ier);
  ~LennardJones612Implementation();  // no explicit Destroy() needed here

  int Reinit(KIM_API_model* pkim);
  int Compute(KIM_API_model* pkim);

 private:
  // Constant values that never change
  //   Set in constructor (via SetConstantValues)
  //
  //
  // KIM API: Conventions
  int baseconvert_;
  NBCTypeEnum NBCType_;
  bool isHalf_;
  bool isLocatorMode_;
  //
  // KIM API: Model Input indices
  int numberOfSpeciesIndex_;
  int numberOfParticlesIndex_;
  int numberContributingParticlesIndex_;
  int particleSpeciesIndex_;
  int coordinatesIndex_;
  int boxSideLengthsIndex_;
  int get_neighIndex_;
  int process_dEdrIndex_;
  int process_d2Edr2Index_;
  //
  // KIM API: Model Output indices
  int cutoffIndex_;
  int energyIndex_;
  int forcesIndex_;
  int particleEnergyIndex_;
  //
  // LennardJones612Implementation: constants
  int numberModelSpecies_;
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
  int cachedNumberContributingParticles_;


  // Helper methods
  //
  //
  // Related to constructor
  int SetConstantValues(KIM_API_model* const pkim);
  int DetermineNBCTypeAndHalf(KIM_API_model* const pkim);
  void AllocateFreeParameterMemory();
  static int OpenParameterFiles(
      KIM_API_model* const pkim,
      char const* const parameterFileNames,
      int const parameterFileNameLength,
      int const numberParameterFiles,
      FILE* parameterFilePointers[MAX_PARAMETER_FILES]);
  static void CloseParameterFiles(
      FILE* const parameterFilePointers[MAX_PARAMETER_FILES],
      int const numberParameterFiles);
  int ProcessParameterFiles(
      KIM_API_model* const pkim,
      FILE* const parameterFilePointers[MAX_PARAMETER_FILES],
      int const numberParameterFiles);
  void getNextDataLine(FILE* const filePtr, char* const nextLine,
                       int const maxSize, int* endOfFileFlag);
  int ConvertUnits(KIM_API_model* const pkim);
  int RegisterKIMParameters(KIM_API_model* const pkim) const;
  int RegisterKIMFunctions(KIM_API_model* const pkim) const;
  //
  // Related to Reinit()
  int SetReinitMutableValues(KIM_API_model* const pkim);
  //
  // Related to Compute()
  int SetComputeMutableValues(KIM_API_model* const pkim,
                              bool& isComputeProcess_dEdr,
                              bool& isComputeProcess_d2Edr2,
                              bool& isComputeEnergy,
                              bool& isComputeForces,
                              bool& isComputeParticleEnergy,
                              int const*& particleSpecies,
                              GetNeighborFunction *& get_neigh,
                              double const*& boxSideLengths,
                              VectorOfSizeDIM const*& coordinates,
                              double*& energy,
                              double*& particleEnergy,
                              VectorOfSizeDIM*& forces);
  int CheckParticleSpecies(KIM_API_model* const pkim,
                           int const* const particleSpecies) const;
  int GetComputeIndex(const bool& isComputeProcess_dEdr,
                      const bool& isComputeProcess_d2Edr2,
                      const bool& isComputeEnergy,
                      const bool& isComputeForces,
                      const bool& isComputeParticleEnergy,
                      const bool& isShift) const;
  static void ApplyMIOPBC(double const* const boxSideLengths, double* const dx);

  // compute functions
  template< class Iter, bool isHalf, RijEnum rijMethod,
            bool isComputeProcess_dEdr, bool isComputeProcess_d2Edr2,
            bool isComputeEnergy, bool isComputeForces,
            bool isComputeParticleEnergy, bool isShift >
  int Compute(KIM_API_model* const pkim,
              const int* const particleSpecies,
              GetNeighborFunction* const get_neigh,
              const double* const boxSideLengths,
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
template< class Iter, bool isHalf, RijEnum rijMethod,
          bool isComputeProcess_dEdr, bool isComputeProcess_d2Edr2,
          bool isComputeEnergy, bool isComputeForces,
          bool isComputeParticleEnergy, bool isShift >
int LennardJones612Implementation::Compute(
    KIM_API_model* const pkim,
    const int* const particleSpecies,
    GetNeighborFunction* const get_neigh,
    const double* const boxSideLengths,
    const VectorOfSizeDIM* const coordinates,
    double* const energy,
    VectorOfSizeDIM* const forces,
    double* const particleEnergy)
{
  int ier = KIM_STATUS_OK;

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
  int* n1atom = 0;
  double* pRij = 0;
  int const baseConvert = baseconvert_;
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
  int const cachedNumContribParticles = cachedNumberContributingParticles_;
  for (Iter iterator(pkim, get_neigh, baseConvert,
                     cachedNumberContributingParticles_, &ii, &numnei, &n1atom,
                     &pRij);
       iterator.done() == false;
       iterator.next(&ii, &numnei, &n1atom, &pRij))
  {
    int const numNei = numnei;
    int const * const n1Atom = n1atom;
    double const * const pRijConst = pRij;
    int const i = ii;
    int const iSpecies = particleSpecies[i];

    // Setup loop over neighbors of current particle
    for (int jj = 0; jj < numNei; ++jj)
    { // adjust index of particle neighbor
      int const j = n1Atom[jj] + baseConvert;
      int const jSpecies = particleSpecies[j];
      double* r_ij;
      double r_ijValue[DIMENSION];
      // Compute r_ij appropriately
      switch (rijMethod)
      {
        case Coordinates:
        {
          r_ij = r_ijValue;
          for (int k = 0; k < DIMENSION; ++k)
            r_ij[k] = coordinates[j][k] - coordinates[i][k];
          break;
        }
        case MI_OPBC:
        {
          r_ij = r_ijValue;
          for (int k = 0; k < DIMENSION; ++k)
            r_ij[k] = coordinates[j][k] - coordinates[i][k];
          // apply minimum image convention
          ApplyMIOPBC(boxSideLengths, r_ij);
          break;
        }
        case RVec:
        {
          r_ij = const_cast<double*>(&pRijConst[jj * DIMENSION]);
          break;
        }
      }
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
                      constOneSixtyEightEpsSig6_2D[iSpecies][jSpecies]) * r2iv;
          if ((isHalf == true) && (j < cachedNumContribParticles))
          {
            d2Eidr2 = d2phi;
          }
          else
          {
            d2Eidr2 = 0.5*d2phi;
          }
        }

        if ((isComputeProcess_dEdr == true) || (isComputeForces == true))
        { // Compute dphi
          dphiByR =
              r6iv * (constTwentyFourEpsSig6_2D[iSpecies][jSpecies] -
                      constFortyEightEpsSig12_2D[iSpecies][jSpecies]*r6iv)
              * r2iv;
          if ((isHalf == true) && (j < cachedNumContribParticles))
          {
            dEidrByR = dphiByR;
          }
          else
          {
            dEidrByR = 0.5*dphiByR;
          }
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
          if ((isHalf == true) && (j < cachedNumContribParticles))
          {
            *energy += phi;
          }
          else
          {
            *energy += 0.5*phi;
          }
        }

        // Contribution to particleEnergy
        if (isComputeParticleEnergy == true)
        {
          double const halfPhi = 0.5*phi;
          particleEnergy[i] += halfPhi;
          if ((isHalf == true) && (j < cachedNumContribParticles))
          {
            particleEnergy[j] += halfPhi;
          }
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
          ier = pkim->process_dEdr(const_cast<KIM_API_model**>(&pkim),
                                   const_cast<double*>(&dEidr),
                                   const_cast<double*>(&rij),
                                   const_cast<double**>(&r_ij_const),
                                   const_cast<int*>(&i),
                                   const_cast<int*>(&j));
          if (ier < KIM_STATUS_OK)
          {
            pkim->report_error(__LINE__, __FILE__, "process_dEdr", ier);
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

          ier = pkim->process_d2Edr2(const_cast<KIM_API_model**>(&pkim),
                                     &d2Eidr2,
                                     const_cast<double**>(&pRs),
                                     const_cast<double**>(&pRijConsts),
                                     const_cast<int**>(&pis),
                                     const_cast<int**>(&pjs)
                                     );
          if (ier < KIM_STATUS_OK)
          {
            pkim->report_error(__LINE__, __FILE__, "process_d2Edr2", ier);
            return ier;
          }
        }
      }  // if particles i and j interact
    }  // end of first neighbor loop
  }  // end of loop over contributing particles

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

#endif  // LENNARD_JONES_612_IMPLEMENTATION_HPP_
