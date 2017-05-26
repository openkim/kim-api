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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_HPP_
#define KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_HPP_

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif


// A macro to disallow the copy constructor and operator= functions.
// This should be used in the private: declarations for a class
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

namespace KIM
{
// Forward declarations
class Simulator;

namespace COMPUTE
{
// Forward declarations
class ArgumentName;


class SimulatorComputeArguments
{
 public:
  void get_neighObject(void ** const ptr) const;
  int get_neigh(int const neighborListIndex, int const particleNumber,
                int * const numberOfNeighbors,
                int const ** const neighborsOfParticle) const;

  int process_dEdr(Simulator const * const simulator,
                   double const de, double const r, double const * const dx,
                   int const i, int const j) const;

  int process_d2Edr2(Simulator const * const simulator,
                     double const de, double const * const r,
                     double const * const dx, int const * const i,
                     int const * const j) const;

  // data functions
  int get_data(ArgumentName const argumentName, void ** const ptr)
      const;

  // compute functions
  int get_compute(ArgumentName const argumentName, int * const flag)
      const;

  void get_process_dEdr_compute(int * const flag) const;
  void get_process_d2Edr2_compute(int * const flag) const;

  // who uses this?
  int get_size(ArgumentName const argumentName, int * const size)
      const;

 private:
  // do not allow copy constructor or operator=
  DISALLOW_COPY_AND_ASSIGN(SimulatorComputeArguments);

  SimulatorComputeArguments();
  ~SimulatorComputeArguments();

  class SimulatorComputeArgumentsImplementation;
  SimulatorComputeArgumentsImplementation * pimpl;
};  // class SimulatorComputeArguments

}  // namespace COMPUTE
}  // namespace KIM
#endif  // KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_HPP_
