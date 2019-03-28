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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.0.2 package.
//

#ifndef KIM_MODEL_DESTROY_HPP_
#include "KIM_ModelDestroy.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#define CONVERT_POINTER \
  ModelImplementation * pImpl = reinterpret_cast<ModelImplementation *>(pimpl)


namespace KIM
{
void ModelDestroy::GetModelBufferPointer(void ** const ptr) const
{
  CONVERT_POINTER;

  pImpl->GetModelBufferPointer(ptr);
}

void ModelDestroy::LogEntry(LogVerbosity const logVerbosity,
                            std::string const & message,
                            int const lineNumber,
                            std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelDestroy::LogEntry(LogVerbosity const logVerbosity,
                            std::stringstream const & message,
                            int const lineNumber,
                            std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelDestroy::ToString() const
{
  CONVERT_POINTER;

  return pImpl->ToString();
}

ModelDestroy::ModelDestroy() : pimpl(NULL) {}

ModelDestroy::~ModelDestroy() {}

}  // namespace KIM
