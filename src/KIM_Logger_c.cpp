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

#ifndef KIM_LOGGER_HPP_
#include "KIM_Logger.hpp"
#endif

extern "C"
{
#ifndef KIM_LOGGER_H_
#include "KIM_Logger.h"
#endif

void KIM_get_status_msg(int const statusCode, char const ** const statusMsg)
{
  static std::string msg;
  KIM::get_status_msg(statusCode, &msg);
  *statusMsg = msg.c_str();
}

void KIM_report_error(int const line, char const * const file,
                      char const * const userMsg, int const statusCode)
{
  KIM::report_error(line, file, userMsg, statusCode);
}

}  // extern "C"
