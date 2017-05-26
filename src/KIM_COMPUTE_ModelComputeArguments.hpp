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


#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_HPP_
#define KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_HPP_

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
namespace COMPUTE
{
// Forward declarations
class ArgumentName;
class ArgumentAttribute;

class ModelComputeArguments
{
 public:
  void get_argument_attribute(ArgumentName const argumentName,
                              ArgumentAttribute * const argumentAttribute)
      const;

  // method functions
  void set_neigh(LanguageName const languageName, func * const fptr,
                 void const * const dataObject);

  void get_process_dEdr_attribute(ArgumentAttribute * const argumentAttribute)
      const;
  void get_process_d2Edr2_attribute(
      ArgumentAttribute * const argumentAttribute) const;
  int set_process_dEdr(LanguageName const languageName, func * const fptr);
  int set_process_d2Edr2(LanguageName const languageName, func * const fptr);

  // data functions
  int set_data(ArgumentName const argumentName, int const extent,
               int const * const ptr);
  int set_data(ArgumentName const argumentName, int const extent,
               double const * const ptr);

  // compute functions
  int set_compute(ArgumentName const argumentName, int flag);
  int set_process_dEdr_compute(int flag);
  int set_process_d2Edr2_compute(int flag);

  // who uses this?
  int get_size(ArgumentName const argumentName, int * const size) const;

 private:
  // do not allow copy constructor or operator=
  DISALLOW_COPY_AND_ASSIGN(ModelComputeArguments);

  ModelComputeArguments();
  ~ModelComputeArguments();

  class ModelComputeArgumentsImplementation;
  ModelComputeArgumentsImplementation * pimpl;
};  // class ModelComputeArguments

}  // namespace COMPUTE
}  // namespace KIM
#endif  // KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_HPP_
