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
// Release: This file is part of the kim-api-2.4.1 package.
//


#undef FATAL_VERBOSITY
/// \brief Defined if FATAL Log entries are to be compiled.
#define FATAL_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_FATAL_)
#undef LOG_FATAL
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_FATAL_)
/// \def LOG_FATAL(message)
/// \brief Convenience macro for FATAL Log entries with compile-time
/// optimization.
#define LOG_FATAL(message)          \
  KIM_LOGGER_OBJECT_NAME->LogEntry( \
      KIM::LOG_VERBOSITY::fatal, message, __LINE__, __FILE__)
#else
#define LOG_FATAL(message)
#endif

#undef ERROR_VERBOSITY
/// \brief Defined if ERROR Log entries are to be compiled.
#define ERROR_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_ERROR_)
#undef LOG_ERROR
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_ERROR_)
/// \def LOG_ERROR(message)
/// \brief Convenience macro for ERROR Log entries with compile-time
/// optimization.
#define LOG_ERROR(message)          \
  KIM_LOGGER_OBJECT_NAME->LogEntry( \
      KIM::LOG_VERBOSITY::error, message, __LINE__, __FILE__)
#else
#define LOG_ERROR(message)
#endif

#undef WARNING_VERBOSITY
/// \brief Defined if WARNING Log entries are to be compiled.
#define WARNING_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_WARNING_)
#undef LOG_WARNING
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_WARNING_)
/// \def LOG_WARNING(message)
/// \brief Convenience macro for WARNING Log entries with compile-time
/// optimization.
#define LOG_WARNING(message)        \
  KIM_LOGGER_OBJECT_NAME->LogEntry( \
      KIM::LOG_VERBOSITY::warning, message, __LINE__, __FILE__)
#else
#define LOG_WARNING(message)
#endif

#undef INFORMATION_VERBOSITY
/// \brief Defined if INFORMATION Log entries are to be compiled.
#define INFORMATION_VERBOSITY \
  (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_INFORMATION_)
#undef LOG_INFORMATION
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_INFORMATION_)
/// \def LOG_INFORMATION(message)
/// \brief Convenience macro for INFORMATION Log entries with compile-time
/// optimization.
#define LOG_INFORMATION(message)    \
  KIM_LOGGER_OBJECT_NAME->LogEntry( \
      KIM::LOG_VERBOSITY::information, message, __LINE__, __FILE__)
#else
#define LOG_INFORMATION(message)
#endif

#undef DEBUG_VERBOSITY
/// \brief Defined if DEBUG Log entries are to be compiled.
#define DEBUG_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_DEBUG_)
#undef LOG_DEBUG
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_DEBUG_)
/// \def LOG_DEBUG(message)
/// \brief Convenience macro for DEBUG Log entries with compile-time
/// optimization.
#define LOG_DEBUG(message)          \
  KIM_LOGGER_OBJECT_NAME->LogEntry( \
      KIM::LOG_VERBOSITY::debug, message, __LINE__, __FILE__)
#else
#define LOG_DEBUG(message)
#endif
