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

//
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_MODEL_EXTENSION_HPP_
#include "KIM_ModelExtension.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#define CONVERT_POINTER \
  ModelImplementation * pImpl = reinterpret_cast<ModelImplementation *>(pimpl)

namespace KIM
{
void ModelExtension::GetExtensionID(
    std::string const ** const extensionID) const
{
  CONVERT_POINTER;

  pImpl->GetExtensionID(extensionID);
}

Model * ModelExtension::Model() { return reinterpret_cast<KIM::Model *>(this); }
ModelCompute * ModelExtension::ModelCompute()
{
  return reinterpret_cast<KIM::ModelCompute *>(this);
}
ModelCreate * ModelExtension::ModelCreate()
{
  return reinterpret_cast<KIM::ModelCreate *>(this);
}
ModelDestroy * ModelExtension::ModelDestroy()
{
  return reinterpret_cast<KIM::ModelDestroy *>(this);
}
ModelDriverCreate * ModelExtension::ModelDriverCreate()
{
  return reinterpret_cast<KIM::ModelDriverCreate *>(this);
}
ModelRefresh * ModelExtension::ModelRefresh()
{
  return reinterpret_cast<KIM::ModelRefresh *>(this);
}
ModelWriteParameterizedModel * ModelExtension::ModelWriteParameterizedModel()
{
  return reinterpret_cast<KIM::ModelWriteParameterizedModel *>(this);
}

ModelComputeArguments * ModelExtension::ModelComputeArguments(
    ComputeArguments * const computeArguments) const
{
  return reinterpret_cast<KIM::ModelComputeArguments *>(computeArguments);
}
ModelComputeArgumentsCreate * ModelExtension::ModelComputeArgumentsCreate(
    ComputeArguments * const computeArguments) const
{
  return reinterpret_cast<KIM::ModelComputeArgumentsCreate *>(computeArguments);
}
ModelComputeArgumentsDestroy * ModelExtension::ModelComputeArgumentsDestroy(
    ComputeArguments * const computeArguments) const
{
  return reinterpret_cast<KIM::ModelComputeArgumentsDestroy *>(
      computeArguments);
}

void ModelExtension::GetModelBufferPointer(void ** const ptr) const
{
  CONVERT_POINTER;

  pImpl->GetModelBufferPointer(ptr);
}

void ModelExtension::LogEntry(LogVerbosity const logVerbosity,
                              std::string const & message,
                              int const lineNumber,
                              std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelExtension::LogEntry(LogVerbosity const logVerbosity,
                              std::stringstream const & message,
                              int const lineNumber,
                              std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelExtension::ToString() const
{
  CONVERT_POINTER;

  return pImpl->ToString();
}

ModelExtension::ModelExtension() : pimpl(NULL) {}

ModelExtension::~ModelExtension() {}

}  // namespace KIM
