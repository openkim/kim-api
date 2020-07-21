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


#ifndef KIM_MODEL_ROUTINE_NAME_HPP_
#define KIM_MODEL_ROUTINE_NAME_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// ModelRoutineName's supported by the %KIM API.
///
/// The enumeration constants are contained in the MODEL_ROUTINE_NAME
/// namespace.
///
/// \sa KIM_ModelRoutineName,
/// kim_model_routine_name_module::kim_model_routine_name_type
///
/// \since 2.0
class ModelRoutineName
{
 public:
  /// \brief Integer identifying the specific ModelRoutineName represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_ModelRoutineName::modelRoutineNameID,
  /// kim_model_routine_name_module::kim_model_routine_name_type::<!--
  /// -->model_routine_name_id
  ///
  /// \since 2.0
  int modelRoutineNameID;

  /// \brief Create an uninitialized ModelRoutineName object.
  ///
  /// \since 2.0
  ModelRoutineName();

  /// \brief Create a ModelRoutineName object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  ModelRoutineName(int const id);

  /// \brief Create a ModelRoutineName object corresponding to the provided
  /// string.  If the string does not match one of the values defined by the
  /// %KIM API, then an "unknown" object is generated.
  ///
  /// \sa KIM_ModelRoutineName_FromString,
  /// kim_model_routine_name_module::kim_from_string
  ///
  /// \since 2.0
  ModelRoutineName(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// ModelRoutineName's known to the %KIM API are found in the
  /// MODEL_ROUTINE_NAME namespace.
  ///
  /// \sa KIM_ModelRoutineName_Known, kim_model_routine_name_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares ModelRoutineName objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_ModelRoutineName_Equal,
  /// kim_model_routine_name_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(ModelRoutineName const & rhs) const;

  /// \brief Compares ModelRoutineName objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_ModelRoutineName_NotEqual,
  /// kim_model_routine_name_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(ModelRoutineName const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the ModelRoutineName object.
  ///
  /// \note If the ModelRoutineName object does not correspond to a value
  /// defined by the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_ModelRoutineName_ToString,
  /// kim_model_routine_name_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class ModelRoutineName

/// \brief Contains the enumeration constants and the discovery routines for
/// the ModelRoutineName \ref extensible_enumeration "Extensible Enumeration".
namespace MODEL_ROUTINE_NAME
{
/// \brief The standard \c Create routine.
///
/// The C++ prototype for this routine is ModelCreateFunction().  The C
/// prototype for this routine is KIM_ModelCreateFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_Create,
/// kim_model_routine_name_module::kim_model_routine_name_create
///
/// \since 2.0
extern ModelRoutineName const Create;

/// \brief The standard \c ComputeArgumentsCreate routine.
///
/// The C++ prototype for this routine is
/// ModelComputeArgumentsCreateFunction().  The C prototype for this routine is
/// KIM_ModelComputeArgumentsCreateFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate,
/// kim_model_routine_name_module::<!--
/// -->kim_model_routine_name_compute_arguments_create
///
/// \since 2.0
extern ModelRoutineName const ComputeArgumentsCreate;

/// \brief The standard \c Compute routine.
///
/// The C++ prototype for this routine is ModelComputeFunction().  The C
/// prototype for this routine is KIM_ModelComputeFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_Compute,
/// kim_model_routine_name_module::kim_model_routine_name_compute
///
/// \since 2.0
extern ModelRoutineName const Compute;

/// \brief The standard \c Extension routine.
///
/// The C++ prototype for this routine is ModelExtensionFunction().  The C
/// prototype for this routine is KIM_ModelExtensionFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_Extension,
/// kim_model_routine_name_module::kim_model_routine_name_extension
///
/// \since 2.0
extern ModelRoutineName const Extension;

/// \brief The standard \c Refresh routine.
///
/// The C++ prototype for this routine is ModelRefreshFunction().  The C
/// prototype for this routine is KIM_ModelRefreshFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_Refresh,
/// kim_model_routine_name_module::kim_model_routine_name_refresh
///
/// \since 2.0
extern ModelRoutineName const Refresh;

/// \brief The standard \c WriteParameterizedModel routine.
///
/// The C++ prototype for this routine is
/// ModelWriteParameterizedModelFunction().  The C prototype for this routine
/// is KIM_ModelWriteParameterizedModelFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_WriteParameterizedModel,
/// kim_model_routine_name_module::<!--
/// -->kim_model_routine_name_write_parameterized_model
///
/// \since 2.0
extern ModelRoutineName const WriteParameterizedModel;

/// \brief The standard \c ComputeArgumentsDestroy  routine.
///
/// The C++ prototype for this routine is
/// ModelComputeArgumentsDestroyFunction().  The C prototype for this routine
/// is KIM_ModelComputeArgumentsDestroyFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy,
/// kim_model_routine_name_module::<!--
/// -->kim_model_routine_name_compute_arguments_destroy
///
/// \since 2.0
extern ModelRoutineName const ComputeArgumentsDestroy;

/// \brief The standard \c Destroy routine.
///
/// The C++ prototype for this routine is ModelDestroyFunction().  The C
/// prototype for this routine is KIM_ModelDestroyFunction().
///
/// \todo Add more detailed description of routine.
///
/// \sa KIM_MODEL_ROUTINE_NAME_Destroy,
/// kim_model_routine_name_module::kim_model_routine_name_destroy
///
/// \since 2.0
extern ModelRoutineName const Destroy;


/// \brief Get the number of standard ModelRoutineName's defined by the %KIM
/// API.
///
/// \param[out] numberOfModelRoutineNames The number of standard
///             ModelRoutineName's defined by the %KIM API.
///
/// \sa KIM_MODEL_ROUTINE_NAME_GetNumberOfModelRoutineNames,
/// kim_model_routine_name_module::kim_get_number_of_model_routine_names
///
/// \since 2.0
void GetNumberOfModelRoutineNames(int * const numberOfModelRoutineNames);

/// \brief Get the identity of each defined standard ModelRoutineName.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             ModelRoutineName.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] modelRoutineName The ModelRoutineName object associated with \c
///             index.
///
/// \return \c true if `index < 0` or `index >= numberOfModelRoutineNames`.
/// \return \c false otherwise.
///
/// \sa KIM_MODEL_ROUTINE_NAME_GetModelRoutineName,
/// kim_model_routine_name_module::kim_get_model_routine_name
///
/// \since 2.0
int GetModelRoutineName(int const index,
                        ModelRoutineName * const modelRoutineName);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for ModelRoutineName
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(ModelRoutineName const & a, ModelRoutineName const & b) const
  {
    return a.modelRoutineNameID < b.modelRoutineNameID;
  }
};  // struct Comparator
}  // namespace MODEL_ROUTINE_NAME
}  // namespace KIM

#endif  // KIM_MODEL_ROUTINE_NAME_HPP_
