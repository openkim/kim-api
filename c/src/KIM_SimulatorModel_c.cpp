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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstddef>
#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_SIMULATOR_MODEL_HPP_
#include "KIM_SimulatorModel.hpp"
#endif
extern "C" {
#ifndef KIM_SIMULATOR_MODEL_H_
#include "KIM_SimulatorModel.h"
#endif
}  // extern "C"


struct KIM_SimulatorModel
{
  void * p;
};

#define CONVERT_POINTER                 \
  KIM::SimulatorModel * pSimulatorModel \
      = reinterpret_cast<KIM::SimulatorModel *>(simulatorModel->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


extern "C" {
int KIM_SimulatorModel_Create(char const * const simulatorModelName,
                              KIM_SimulatorModel ** const simulatorModel)
{
  std::string simulatorModelNameC(simulatorModelName);
  KIM::SimulatorModel * pSimulatorModel;
  int error
      = KIM::SimulatorModel::Create(simulatorModelNameC, &pSimulatorModel);
  if (error)
  {
    *simulatorModel = NULL;
    return true;
  }
  else
  {
    (*simulatorModel) = new KIM_SimulatorModel;
    (*simulatorModel)->p = (void *) pSimulatorModel;
    return false;
  }
}

void KIM_SimulatorModel_Destroy(KIM_SimulatorModel ** const simulatorModel)
{
  if (*simulatorModel != NULL)
  {
    KIM::SimulatorModel * pSimulatorModel
        = reinterpret_cast<KIM::SimulatorModel *>((*simulatorModel)->p);

    KIM::SimulatorModel::Destroy(&pSimulatorModel);
  }
  delete (*simulatorModel);
  *simulatorModel = NULL;
}

void KIM_SimulatorModel_GetSimulatorNameAndVersion(
    KIM_SimulatorModel const * const simulatorModel,
    char const ** const simulatorName,
    char const ** const simulatorVersion)
{
  CONVERT_POINTER;

  std::string const * pStrSimulatorName;
  std::string const ** ppStrSimulatorName;
  if (simulatorName == NULL)
    ppStrSimulatorName = NULL;
  else
    ppStrSimulatorName = &pStrSimulatorName;

  std::string const * pStrSimulatorVersion;
  std::string const ** ppStrSimulatorVersion;
  if (simulatorVersion == NULL)
    ppStrSimulatorVersion = NULL;
  else
    ppStrSimulatorVersion = &pStrSimulatorVersion;

  pSimulatorModel->GetSimulatorNameAndVersion(ppStrSimulatorName,
                                              ppStrSimulatorVersion);
  if (simulatorName != NULL) *simulatorName = pStrSimulatorName->c_str();
  if (simulatorVersion != NULL)
    *simulatorVersion = pStrSimulatorVersion->c_str();
}

void KIM_SimulatorModel_GetNumberOfSupportedSpecies(
    KIM_SimulatorModel const * const simulatorModel,
    int * const numberOfSupportedSpecies)
{
  CONVERT_POINTER;

  pSimulatorModel->GetNumberOfSupportedSpecies(numberOfSupportedSpecies);
}

int KIM_SimulatorModel_GetSupportedSpecies(
    KIM_SimulatorModel const * const simulatorModel,
    int const index,
    char const ** const speciesName)
{
  CONVERT_POINTER;

  std::string const * pStrSpecies;
  int error = pSimulatorModel->GetSupportedSpecies(index, &pStrSpecies);
  if (error)
    return true;
  else
  {
    *speciesName = pStrSpecies->c_str();
    return false;
  }
}

void KIM_SimulatorModel_OpenAndInitializeTemplateMap(
    KIM_SimulatorModel const * const simulatorModel)
{
  CONVERT_POINTER;

  pSimulatorModel->OpenAndInitializeTemplateMap();
}

int KIM_SimulatorModel_TemplateMapIsOpen(
    KIM_SimulatorModel const * const simulatorModel)
{
  CONVERT_POINTER;

  return pSimulatorModel->TemplateMapIsOpen();
}

int KIM_SimulatorModel_AddTemplateMap(
    KIM_SimulatorModel const * const simulatorModel,
    char const * const key,
    char const * const value)
{
  CONVERT_POINTER;

  return pSimulatorModel->AddTemplateMap(key, value);
}

void KIM_SimulatorModel_CloseTemplateMap(
    KIM_SimulatorModel const * const simulatorModel)
{
  CONVERT_POINTER;

  pSimulatorModel->CloseTemplateMap();
}

void KIM_SimulatorModel_GetNumberOfSimulatorFields(
    KIM_SimulatorModel const * const simulatorModel,
    int * const numberOfSimulatorFields)
{
  CONVERT_POINTER;

  return pSimulatorModel->GetNumberOfSimulatorFields(numberOfSimulatorFields);
}

int KIM_SimulatorModel_GetSimulatorFieldMetadata(
    KIM_SimulatorModel const * const simulatorModel,
    int const fieldIndex,
    int * const extent,
    char const ** const fieldName)
{
  CONVERT_POINTER;

  std::string const * pStrFieldName;
  std::string const ** ppStrFieldName;
  if (fieldName == NULL)
    ppStrFieldName = NULL;
  else
    ppStrFieldName = &pStrFieldName;

  int error = pSimulatorModel->GetSimulatorFieldMetadata(
      fieldIndex, extent, ppStrFieldName);
  if (error)
    return true;
  else
  {
    if (fieldName != NULL) *fieldName = pStrFieldName->c_str();
    return false;
  }
}

int KIM_SimulatorModel_GetSimulatorFieldLine(
    KIM_SimulatorModel const * const simulatorModel,
    int const fieldIndex,
    int const lineIndex,
    char const ** const lineValue)
{
  CONVERT_POINTER;

  std::string const * pStrLineValue;
  int error = pSimulatorModel->GetSimulatorFieldLine(
      fieldIndex, lineIndex, &pStrLineValue);
  if (error)
    return true;
  else
  {
    *lineValue = pStrLineValue->c_str();
    return false;
  }
}

void KIM_SimulatorModel_GetParameterFileDirectoryName(
    KIM_SimulatorModel const * const simulatorModel,
    char const ** const directoryName)
{
  CONVERT_POINTER;

  std::string const * pStrDirectoryName;
  pSimulatorModel->GetParameterFileDirectoryName(&pStrDirectoryName);
  *directoryName = pStrDirectoryName->c_str();
}

void KIM_SimulatorModel_GetSpecificationFileName(
    KIM_SimulatorModel const * const simulatorModel,
    char const ** const specificationFileName)
{
  CONVERT_POINTER;

  std::string const * pStrSpecificationFileName;
  pSimulatorModel->GetSpecificationFileName(&pStrSpecificationFileName);
  *specificationFileName = pStrSpecificationFileName->c_str();
}

void KIM_SimulatorModel_GetNumberOfParameterFiles(
    KIM_SimulatorModel const * const simulatorModel,
    int * const numberOfParameterFiles)
{
  CONVERT_POINTER;

  pSimulatorModel->GetNumberOfParameterFiles(numberOfParameterFiles);
}

int KIM_SimulatorModel_GetParameterFileName(
    KIM_SimulatorModel const * const simulatorModel,
    int const index,
    char const ** const parameterFileName)
{
  CONVERT_POINTER;

  std::string const * pStrParameterFileName;
  int error
      = pSimulatorModel->GetParameterFileName(index, &pStrParameterFileName);
  if (error)
    return true;
  else
  {
    *parameterFileName = pStrParameterFileName->c_str();
    return false;
  }
}

int KIM_SimulatorModel_GetParameterFileBasename(
    KIM_SimulatorModel const * const simulatorModel,
    int const index,
    char const ** const parameterFileBasename)
{
  CONVERT_POINTER;

  std::string const * pStrParameterFileBasename;
  int error = pSimulatorModel->GetParameterFileBasename(
      index, &pStrParameterFileBasename);
  if (error)
    return true;
  else
  {
    *parameterFileBasename = pStrParameterFileBasename->c_str();
    return false;
  }
}

void KIM_SimulatorModel_SetSimulatorBufferPointer(
    KIM_SimulatorModel * const simulatorModel, void * const ptr)
{
  CONVERT_POINTER;

  pSimulatorModel->SetSimulatorBufferPointer(ptr);
}

void KIM_SimulatorModel_GetSimulatorBufferPointer(
    KIM_SimulatorModel const * const simulatorModel, void ** const ptr)
{
  CONVERT_POINTER;

  pSimulatorModel->GetSimulatorBufferPointer(ptr);
}

char const *
KIM_SimulatorModel_ToString(KIM_SimulatorModel const * const simulatorModel)
{
  CONVERT_POINTER;

  return pSimulatorModel->ToString().c_str();
}

void KIM_SimulatorModel_SetLogID(KIM_SimulatorModel * const simulatorModel,
                                 char const * const logID)
{
  CONVERT_POINTER;

  pSimulatorModel->SetLogID(logID);
}

void KIM_SimulatorModel_PushLogVerbosity(
    KIM_SimulatorModel * const simulatorModel,
    KIM_LogVerbosity const logVerbosity)
{
  CONVERT_POINTER;

  pSimulatorModel->PushLogVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_SimulatorModel_PopLogVerbosity(
    KIM_SimulatorModel * const simulatorModel)
{
  CONVERT_POINTER;

  pSimulatorModel->PopLogVerbosity();
}

}  // extern "C"
