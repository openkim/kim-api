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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_MODEL_EXTENSION_HPP_
#define KIM_MODEL_EXTENSION_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class Model;
class ModelCompute;
class ModelCreate;
class ModelDestroy;
class ModelDriverCreate;
class ModelRefresh;
class ModelWriteParameterizedModel;
class ComputeArguments;
class ModelComputeArguments;
class ModelComputeArgumentsCreate;
class ModelComputeArgumentsDestroy;
class ModelExtensionImplementation;


class ModelExtension
{
 public:
  void GetExtensionID(std::string const ** const extensionID) const;

  KIM::Model * Model();
  KIM::ModelCompute * ModelCompute();
  KIM::ModelCreate * ModelCreate();
  KIM::ModelDestroy * ModelDestroy();
  KIM::ModelDriverCreate * ModelDriverCreate();
  KIM::ModelRefresh * ModelRefresh();
  KIM::ModelWriteParameterizedModel * ModelWriteParameterizedModel();

  KIM::ModelComputeArguments *
  ModelComputeArguments(ComputeArguments * const computeArguments) const;
  KIM::ModelComputeArgumentsCreate *
  ModelComputeArgumentsCreate(ComputeArguments * const computeArguments) const;
  KIM::ModelComputeArgumentsDestroy *
  ModelComputeArgumentsDestroy(ComputeArguments * const computeArguments) const;

  void GetModelBufferPointer(void ** const ptr) const;

  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

  std::string const & String() const;

 private:
  // do not allow copy constructor or operator=
  ModelExtension(ModelExtension const &);
  void operator=(ModelExtension const &);

  ModelExtension();
  ~ModelExtension();

  ModelExtensionImplementation * pimpl;
};  // class ModelExtension
}  // namespace KIM

#endif  // KIM_MODEL_EXTENSION_HPP_
