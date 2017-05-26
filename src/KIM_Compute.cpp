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

#ifndef KIM_COMPUTE_HPP_
#include "KIM_Compute.hpp"
#endif

namespace KIM
{
namespace COMPUTE
{

ArgumentName::ArgumentName() : argumentID(0){}
ArgumentName::ArgumentName(int const id) : argumentID(id){}

bool ArgumentName::operator==(ArgumentName const & rhs) const
{return argumentID == rhs.argumentID;}
bool ArgumentName::operator!=(ArgumentName const & rhs) const
{return argumentID != rhs.argumentID;}

// Order doesn't matter as long as all values are unique
namespace ARGUMENT_NAME
{
ArgumentName const numberOfParticles(0);
ArgumentName const numberOfSpecies(1);
ArgumentName const particleSpecies(2);
ArgumentName const coordinates(3);
ArgumentName const get_neigh(4);
ArgumentName const process_dEdr(5);
ArgumentName const process_d2Edr2(6);
ArgumentName const neighObject(7);
ArgumentName const compute(8);
ArgumentName const reinit(9);
ArgumentName const destroy(10);
ArgumentName const cutoff(11);
ArgumentName const energy(12);
ArgumentName const forces(13);
ArgumentName const particleEnergy(14);
ArgumentName const virial(15);
ArgumentName const particleVirial(16);
ArgumentName const hessian(17);
ArgumentName const End(-32000);
}  // namespace ARGUMENT_NAME


LanguageName::LanguageName(): languageID(0){}
LanguageName::LanguageName(int const id): languageID(id){}

bool LanguageName::operator==(LanguageName const & rhs) const
{return languageID == rhs.languageID;}
bool LanguageName::operator!=(LanguageName const & rhs) const
{return languageID != rhs.languageID;}

namespace LANGUAGE_NAME
{
LanguageName const Cpp(1);
LanguageName const C(2);
LanguageName const Fortran(3);
}  // namespace LANGUAGE_NAME

}  // namespace KIM
}  // namespace COMPUTE
