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

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
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
ArgumentName const particleContributing(3);
ArgumentName const coordinates(4);
ArgumentName const energy(5);
ArgumentName const forces(6);
ArgumentName const particleEnergy(7);
ArgumentName const virial(8);
ArgumentName const particleVirial(9);
ArgumentName const hessian(10);
}  // namespace ARGUMENT_NAME

}  // namespace COMPUTE
}  // namespace KIM
