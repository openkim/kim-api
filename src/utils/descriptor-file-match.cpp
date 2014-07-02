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
// Copyright (c) 2013--2014, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <fstream>
#include <sstream>
#include "KIM_API.h"
#include "KIM_API_status.h"

int main(int argc, char* argv[])
{
   if (argc != 3)
   {
      std::cerr << "Usage: " << argv[0] << " TestKimFile ModelKimFile"
                << std::endl;
      std::cout << "NOMATCH" << std::endl;
      return 1;
   }

   std::fstream testkimfile;
   std::fstream modelkimfile;
   testkimfile.open(argv[1], std::fstream::in);
   modelkimfile.open(argv[2], std::fstream::in);

   if (testkimfile.fail() || modelkimfile.fail())
   {
      std::cerr << "file open failed." << std::endl;
      std::cout << "NOMATCH" << std::endl;
      return 1;
   }

   std::stringstream testkimfile_str;
   std::stringstream modelkimfile_str;
   testkimfile_str << testkimfile.rdbuf();
   modelkimfile_str << modelkimfile.rdbuf();

   KIM_API_model kim;
   int error;

   error = kim.match((char*) testkimfile_str.str().c_str(),
                     (char*) modelkimfile_str.str().c_str());

   int retval;
   if (error == KIM_STATUS_OK)
   {
      retval = 0;
      std::cout << "MATCH" << std::endl;
   }
   else
   {
      retval = 2;
      std::cout << "NOMATCH" << std::endl;
   }

   return retval;
}
