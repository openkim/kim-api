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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIMHDR_OLD_KIM_API_DIRS_H
#define KIMHDR_OLD_KIM_API_DIRS_H

#include <list>
#include <map>
#include <vector>

#include "KIM_Log.hpp"

namespace OLD_KIM
{
enum CollectionItemType {
  KIM_MODEL_DRIVERS,
  KIM_PORTABLE_MODELS,
  KIM_SIMULATOR_MODELS
};
enum CollectionType { KIM_CWD, KIM_ENVIRONMENT, KIM_USER, KIM_SYSTEM };

bool findItem(CollectionItemType type,
              std::string const & name,
              std::vector<std::string> * const Item,
              KIM::Log * const log);

// needed by collection-info
std::vector<std::string> getConfigFileName();
int getEnvironmentVariableNames(
    std::map<CollectionItemType const, std::string> * const map);
int pushDirs(CollectionType collectionType,
             CollectionItemType itemType,
             std::list<std::pair<std::string, std::string> > * const lst,
             KIM::Log * const log);
std::string getSystemLibraryFileName();
enum GET_ITEMS_ENTRIES { IE_COLLECTION, IE_NAME, IE_DIR, IE_VER, IE_FULLPATH };
void getAvailableItems(CollectionItemType type,
                       std::list<std::vector<std::string> > & list,
                       KIM::Log * const log);

}  // namespace OLD_KIM

#endif /* KIMHDR_OLD_KIM_API_DIRS_H */
