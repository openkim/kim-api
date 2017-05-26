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


#include "old_KIM_API.h"

namespace KIM
{

void get_status_msg(int const statusCode, std::string * const statusMsg)
{
  char const * msg;
  // check error?
  OLD_KIM::KIM_API_model::get_status_msg(statusCode, &msg);
  (*statusMsg) = msg;
}

void report_error(int const line, std::string const & file,
                  std::string const & userMsg, int const statusCode)
{
  OLD_KIM::KIM_API_model::report_error(line, file.c_str(), userMsg.c_str(),
                                       statusCode);
}

}  // namespace KIM
