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
// Copyright (c) 2013--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2-2.0.0 package.
//


#ifndef KIMHDR_OLD_KIM_API_DIRS_H
#define KIMHDR_OLD_KIM_API_DIRS_H

#include <list>
#include <vector>

#include "KIM_Log.hpp"

namespace OLD_KIM
{
enum DirectoryPathType { KIM_MODEL_DRIVERS_DIR, KIM_MODELS_DIR };

bool findItem(DirectoryPathType type,
              std::string const & name,
              std::vector<std::string> * const Item,
              KIM::Log * const log);

// needed by collection-info
std::vector<std::string> getConfigFileName();
std::string
pushEnvDirs(DirectoryPathType type,
            std::list<std::pair<std::string, std::string> > * const lst);
std::string getSystemLibraryFileName();
std::vector<std::string> getSystemDirs();
std::vector<std::string> getUserDirs(KIM::Log * const log);
enum GET_ITEMS_ENTRIES { IE_COLLECTION, IE_NAME, IE_DIR, IE_VER, IE_FULLPATH };
void getAvailableItems(DirectoryPathType type,
                       std::list<std::vector<std::string> > & list,
                       KIM::Log * const log);

}  // namespace OLD_KIM

#endif /* KIMHDR_OLD_KIM_API_DIRS_H */
