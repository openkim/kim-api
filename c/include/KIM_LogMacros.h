/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api-2.1.0 package.                   */
/*                                                                            */


#undef FATAL_VERBOSITY
/**
 ** \brief Defined if FATAL Log entries are to be compiled.
 **/
#define FATAL_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_FATAL_)
#undef LOG_FATAL
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_FATAL_)
/**
 ** \def LOG_FATAL(message)
 ** \brief Convenience macro for FATAL Log entries with compile-time
 ** optimization.
 **/
#define LOG_FATAL(message)                          \
  KIM_LOGGER_FUNCTION_NAME(KIM_LOGGER_OBJECT_NAME,  \
                           KIM_LOG_VERBOSITY_fatal, \
                           message,                 \
                           __LINE__,                \
                           __FILE__)
#else
#define LOG_FATAL(message)
#endif

#undef ERROR_VERBOSITY
/**
 ** \brief Defined if ERROR Log entries are to be compiled.
 **/
#define ERROR_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_ERROR_)
#undef LOG_ERROR
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_ERROR_)
/**
 ** \def LOG_ERROR(message)
 ** \brief Convenience macro for ERROR Log entries with compile-time
 ** optimization.
 **/
#define LOG_ERROR(message)                          \
  KIM_LOGGER_FUNCTION_NAME(KIM_LOGGER_OBJECT_NAME,  \
                           KIM_LOG_VERBOSITY_error, \
                           message,                 \
                           __LINE__,                \
                           __FILE__)
#else
#define LOG_ERROR(message)
#endif

#undef WARNING_VERBOSITY
/**
 ** \brief Defined if WARNING Log entries are to be compiled.
 **/
#define WARNING_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_WARNING_)
#undef LOG_WARNING
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_WARNING_)
/**
 ** \def LOG_WARNING(message)
 ** \brief Convenience macro for WARNING Log entries with compile-time
 ** optimization.
 **/
#define LOG_WARNING(message)                          \
  KIM_LOGGER_FUNCTION_NAME(KIM_LOGGER_OBJECT_NAME,    \
                           KIM_LOG_VERBOSITY_warning, \
                           message,                   \
                           __LINE__,                  \
                           __FILE__)
#else
#define LOG_WARNING(message)
#endif

#undef INFORMATION_VERBOSITY
/**
 ** \brief Defined if INFORMATION Log entries are to be compiled.
 **/
#define INFORMATION_VERBOSITY \
  (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_INFORMATION_)
#undef LOG_INFORMATION
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_INFORMATION_)
/**
 ** \def LOG_INFORMATION(message)
 ** \brief Convenience macro for INFORMATION Log entries with compile-time
 ** optimization.
 **/
#define LOG_INFORMATION(message)                          \
  KIM_LOGGER_FUNCTION_NAME(KIM_LOGGER_OBJECT_NAME,        \
                           KIM_LOG_VERBOSITY_information, \
                           message,                       \
                           __LINE__,                      \
                           __FILE__)
#else
#define LOG_INFORMATION(message)
#endif

#undef DEBUG_VERBOSITY
/**
 ** \brief Defined if DEBUG Log entries are to be compiled.
 **/
#define DEBUG_VERBOSITY (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_DEBUG_)
#undef LOG_DEBUG
#if (KIM_LOG_MAXIMUM_LEVEL >= KIM_LOG_VERBOSITY_DEBUG_)
/**
 ** \def LOG_DEBUG(message)
 ** \brief Convenience macro for DEBUG Log entries with compile-time
 ** optimization.
 **/
#define LOG_DEBUG(message)                          \
  KIM_LOGGER_FUNCTION_NAME(KIM_LOGGER_OBJECT_NAME,  \
                           KIM_LOG_VERBOSITY_debug, \
                           message,                 \
                           __LINE__,                \
                           __FILE__)
#else
#define LOG_DEBUG(message)
#endif
