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
//    Alexander Stukowski
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <cstdlib>
#include <iostream>
#include <sstream>  // IWYU pragma: keep  // For MinGW/Linux

#ifndef KIM_FILESYSTEM_PATH_HPP_
#include "KIM_FilesystemPath.hpp"
#endif

#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
#include <random>
#else
#include <cstring>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <sstream>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>  // IWYU pragma: keep  // For macOS
#endif

namespace KIM
{
namespace FILESYSTEM
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
const std::string::value_type Path::preferred_separator = ';';
#else
const std::string::value_type Path::preferred_separator = ':';
#endif

bool Path::MakeDirectory() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::error_code ec;
  std::filesystem::create_directories(path_, ec);
  if (ec)
  {
    std::cerr << "Failed to create directory " << *this << ": " << ec.message()
              << std::endl;
    return true;
  }
  return false;
#else
  const size_t len = strlen(path_.c_str());
  char _path[PATH_MAX];
  char * p;

  errno = 0;

  /* Copy string so its mutable */
  if (len > sizeof(_path) - 1)
  {
    errno = ENAMETOOLONG;
    goto errorLabel;
  }
  strcpy(_path, path_.c_str());

  /* Iterate the string */
  for (p = _path + 1; *p; p++)
  {
    if (*p == '/')
    {
      /* Temporarily truncate */
      *p = '\0';

      if (mkdir(_path, S_IRWXU) != 0)
      {
        if (errno != EEXIST)
        {
          *p = '/';
          goto errorLabel;
        }
      }

      *p = '/';
    }
  }

  if (mkdir(_path, S_IRWXU) != 0)
  {
    if (errno != EEXIST) goto errorLabel;
  }
  return false;

errorLabel:
  std::cerr << "Failed to create directory '" << path_ << "'." << std::endl;
  return true;
#endif
}

bool Path::RemoveDirectoryRecursive() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::error_code ec;
  std::filesystem::remove_all(path_, ec);
  if (ec)
  {
    std::cerr << "Failed to remove directory " << *this << ": " << ec.message()
              << std::endl;
    return true;
  }
  return false;
#else
  int error = false;
  struct dirent * dp = NULL;
  DIR * dir = NULL;
  dir = opendir(path_.c_str());
  while ((dp = readdir(dir)))
  {
    // assuming no subdirectories, just files
    if ((0 != strcmp(dp->d_name, ".")) && (0 != strcmp(dp->d_name, "..")))
    {
      Path filePath = *this / dp->d_name;
      error = error || remove(filePath.path_.c_str());
    }
  }
  closedir(dir);
  error = error || remove(path_.c_str());
  if (error) { return true; }
  return false;
#endif
}

std::vector<Path> Path::Subdirectories() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::vector<Path> resultList;
  std::error_code ec;
  if (std::filesystem::is_directory(path_, ec))
  {
    for (auto & p : std::filesystem::directory_iterator(path_))
      resultList.push_back(Path(p.path()));
  }
  return resultList;
#else
  std::vector<Path> resultList;

  DIR * dirp = NULL;
  struct dirent * dp = NULL;

  if (NULL != (dirp = opendir(path_.c_str())))
  {
    do
    {
      struct stat statBuf;
      if ((NULL != (dp = readdir(dirp))) && (0 != strcmp(dp->d_name, "."))
          && (0 != strcmp(dp->d_name, "..")))
      {
        Path fullPath = *this / dp->d_name;
        if ((0 == stat(fullPath.path_.c_str(), &statBuf))
            && (S_ISDIR(statBuf.st_mode)))
        { resultList.push_back(fullPath); }
      }
    } while (NULL != dp);
    closedir(dirp);
  }
  return resultList;
#endif
}

Path Path::CreateTemporaryDirectory(char const * const namePrefix)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  // Get the root directory for temporary files.
  Path temp_dir = std::filesystem::temp_directory_path();
  // Keep generating random subdirectory names until we find one that does not
  // exists yet.
  std::default_random_engine rnd;
  Path temp_subdir;
  do
  {
    int random_number = std::uniform_int_distribution<int>(0)(rnd);
    std::string subdir_name = namePrefix + std::to_string(random_number);
    temp_subdir = temp_dir / subdir_name;
  } while (temp_subdir.exists());
  // Create the subdirectory.
  if (temp_subdir.MakeDirectory()) { return Path(); }
  return temp_subdir;
#else
  std::stringstream templateString;
  templateString << P_tmpdir
                 << ((*(--(std::string(P_tmpdir).end())) == '/') ? "" : "/")
                 << namePrefix << "XXXXXXXXXXXX";
  char * cstr = strdup(templateString.str().c_str());
  char * tmpdir = mkdtemp(cstr);
  if (NULL == tmpdir)
  {
    free(cstr);
    return Path();
  }
  Path result(tmpdir);
  free(cstr);
  return result;
#endif
}

bool Path::exists() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return std::filesystem::exists(path_);
#else
  struct stat statBuf;
  if (0 == stat(path_.c_str(), &statBuf)) return true;
  return false;
#endif
}

Path Path::current_path()
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return Path(std::filesystem::current_path());
#else
  return Path(getenv("PWD"));
#endif
}

Path Path::HomePath()
{
#if defined(KIM_API_USE_FILESYSTEM_LIBRARY) && defined(_WIN32)
  std::cout << " Enter Path::HomePath(): " << std::endl;
  // Detects the user's home directory on Win32.
  // Prefer environment variables %HOMEDRIVE% / %HOMEPATH%.  
  if(wchar_t* envHomeDrive = _wgetenv(L"HOMEDRIVE")) {
    if(wchar_t* envHomePath = _wgetenv(L"HOMEPATH")) {
      std::filesystem::path homeDrive = envHomeDrive;
      std::filesystem::path homePath = envHomePath;
      std::cout << "   homeDrive: " << homeDrive << std::endl;
      std::cout << "   homePath: " << homePath << std::endl;
      return Path(homeDrive / homePath);
    }
  }
  // Fall back to environment variable %USERPROFILE%:
  if(wchar_t* envUserProfile = _wgetenv(L"USERPROFILE")) {
    std::filesystem::path profilePath = envUserProfile;
    std::cout << "   profilePath: " << profilePath << std::endl;
    return Path(profilePath);
  }
  // Fall back to environment variable %HOME%:
  if(wchar_t* envHome = _wgetenv(L"HOME")) {
    std::filesystem::path homePath = envHome;
    std::cout << "   homePath: " << homePath << std::endl;
    return Path(homePath);
  }
  std::cout << "   COULD NOT DETECT HOME DIRECTOY" << std::endl;
  return Path();
#else
  return Path(getenv("HOME"));
#endif
}

Path & Path::remove_filename()
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  path_.remove_filename();
#else
  size_t loc = path_.find_last_of('/');
  if (loc != std::string::npos)
    path_ = path_.substr(0, loc + 1);
  else
    path_ = std::string("");
#endif
  return *this;
}

bool Path::is_relative() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return path_.is_relative();
#else
  return path_.empty() || path_[0] != '/';
#endif
}

Path & Path::operator+=(char const * const s)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  path_ += std::string(s);
#else
  path_ += s;
#endif
  return *this;
}

Path & Path::concat(const std::string & p)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  path_.concat(p);
#else
  path_.append(p);
#endif
  return *this;
}

std::string Path::string() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return path_.string();
#else
  return path_;
#endif
}

Path & Path::make_preferred()
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  path_.make_preferred();
#else
  // No-op, because '/' is the only directory separator on non-Windows
  // platforms.
#endif
  return *this;
}

Path Path::parent_path() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return Path(path_.parent_path());
#else
  Path parent(*this);
  parent.remove_filename();
  if (!parent.empty())
    parent.path_ = std::string(parent.path_.begin(), --(parent.path_.end()));
  return parent;
#endif
}

Path Path::filename() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return Path(path_.filename());
#else
  std::string::size_type pos = path_.rfind('/');
  if (pos == std::string::npos) return *this;
  return Path(path_.substr(pos + 1));
#endif
}

Path & Path::operator/=(const Path & p)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  path_ /= p.path_;
#else
  if (p.empty())
  {
    if (!filename().empty()) { *this += "/"; }
    return *this;
  }
  if (!p.is_relative()) { path_ = p.path_; }
  else
  {
    if (path_[path_.size() - 1] != '/') path_ += '/';
    if (p.path_.size() >= 2 && p.path_[0] == '.' && p.path_[1] == '/')
      path_.append(p.path_.substr(2));
    else
      path_.append(p.path_);
  }
#endif
  return *this;
}

Path Path::operator/(const Path & p) const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return path_ / p.path_;
#else
  Path result(*this);
  result /= p;
  return result;
#endif
}


const std::string::value_type PathList::PreferredSeparator
    = Path::preferred_separator;

const std::string::value_type PathList::HomeDirectoryShortcut = '~';

// Creates all directories in the path list, including parent directories if
// necessary. It's not an error if a directory to be created already exists.
// Returns true on error.
bool PathList::MakeDirectories() const
{
  for (const_iterator path = begin(); path != end(); ++path)
  {
    if (path->MakeDirectory()) return true;
  }
  return false;
}

// Converts the path list into a colon- or semicolon-separated string list.
std::string PathList::ToString() const
{
  std::string result;
  for (const_iterator path = begin(); path != end(); ++path)
  {
    if (path != begin()) result += PreferredSeparator;
    result += path->string();
  }
  return result;
}

// Parses a list of filesystem paths separated by colons (or semi-colons on
// Windows).
// '~' at the beginning of a path is replaced with the user's home directory.
size_t PathList::Parse(std::string::value_type const * const paths)
{
  clear();
  if (!paths) return 0;
  // Split (semi)colon-separated path list:
  std::basic_istringstream<std::string::value_type> iss(paths);
  std::string token;
  while (std::getline(iss, token, PreferredSeparator))
  {
    // Resolve references to home directory (~).
    if (token[0] == HomeDirectoryShortcut)
    { push_back(Path::HomePath().concat(token.substr(1))); }
    else
    {
      push_back(token);
    }
    back().make_preferred();
  }
  return size();
}

}  // namespace FILESYSTEM
}  // namespace KIM
