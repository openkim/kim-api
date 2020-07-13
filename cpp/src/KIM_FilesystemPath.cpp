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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
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

#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
#include <random>
#else
#include <cstring>
#include <dirent.h>
#include <errno.h>
#include <sstream>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#ifndef KIM_FILESYSTEM_PATH_HPP_
#include "KIM_FilesystemPath.hpp"
#endif

namespace KIM
{
namespace FILESYSTEM
{
bool Path::make_directory() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::error_code ec;
  std::filesystem::create_directories(_p, ec);
  if (ec)
  {
    std::cerr << "Failed to create directory " << *this << ": " << ec.message()
              << std::endl;
    return true;
  }
  return false;
#else
  mode_t mode = 0755;
  if (mkdir(_p.c_str(), mode))
  {
    if (EEXIST == errno)
      return false;
    else
    {
      std::cerr << "Failed to create directory '" << _p << "'." << std::endl;
      return true;
    }
  }
  return false;
#endif
}

bool Path::remove_directory_recursive() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::error_code ec;
  std::filesystem::remove_all(_p, ec);
  if (ec)
  {
    std::cerr << "Failed to remove directory " << *this << ": " << ec.message()
              << std::endl;
    return true;
  }
  return false;
#else
  int error;
  struct dirent * dp = NULL;
  DIR * dir = NULL;
  dir = opendir(_p.c_str());
  while ((dp = readdir(dir)))
  {
    // assuming no subdirectories, just files
    if ((0 != strcmp(dp->d_name, ".")) && (0 != strcmp(dp->d_name, "..")))
    {
      Path filePath = *this / dp->d_name;
      remove(filePath._p.c_str());
    }
  }
  closedir(dir);
  error = remove(_p.c_str());
  if (error) { return true; }
  return false;
#endif
}

std::vector<Path> Path::subdirectories() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::vector<Path> resultList;
  std::error_code ec;
  if (std::filesystem::is_directory(_p, ec))
  {
    for (auto & p : std::filesystem::directory_iterator(_p))
      resultList.push_back(p.path());
  }
  return resultList;
#else
  std::vector<Path> resultList;

  DIR * dirp = NULL;
  struct dirent * dp = NULL;

  if (NULL != (dirp = opendir(_p.c_str())))
  {
    do
    {
      struct stat statBuf;
      if ((NULL != (dp = readdir(dirp))) && (0 != strcmp(dp->d_name, "."))
          && (0 != strcmp(dp->d_name, "..")))
      {
        Path fullPath = *this / dp->d_name;
        if ((0 == stat(fullPath._p.c_str(), &statBuf))
            && (S_ISDIR(statBuf.st_mode)))
        { resultList.push_back(fullPath); }
      }
    } while (NULL != dp);
    closedir(dirp);
  }
  return resultList;
#endif
}

Path Path::create_temporary_directory(char const * const namePrefix)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  // Get the root directory for temporary files.
  std::filesystem::path temp_dir = std::filesystem::temp_directory_path();
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
  if (temp_subdir.make_directory()) { return Path(); }
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
  return std::filesystem::exists(_p);
#else
  struct stat statBuf;
  if (0 == stat(_p.c_str(), &statBuf)) return true;
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

Path Path::home_path()
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
#ifndef _WIN32
  return Path(getenv("HOME"));
#else
  Path homeDrive = ::_wgetenv(L"HOMEDRIVE");
  Path homePath = ::_wgetenv(L"HOMEPATH");
  return homeDrive / homePath;
#endif
#else
  return Path(getenv("HOME"));
#endif
}

Path & Path::remove_filename()
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  _p.remove_filename();
#else
  _p = _p.substr(0, _p.find_last_of('/'));
#endif
  return *this;
}

bool Path::is_relative() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return _p.is_relative();
#else
  return _p.empty() || _p[0] != '/';
#endif
}

Path & Path::operator+=(char const * const s)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  _p += std::string(s);
#else
  _p += s;
#endif
  return *this;
}

Path & Path::concat(const std::string & p)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  _p.concat(p);
#else
  _p.append(p);
#endif
  return *this;
}

Path & Path::make_preferred()
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  _p.make_preferred();
#else
  // No-op, because '/' is the only directory separator on non-Windows
  // platforms.
#endif
  return *this;
}

Path Path::filename() const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return Path(_p.filename());
#else
  std::string::size_type pos = _p.rfind('/');
  if (pos == std::string::npos) return *this;
  return Path(_p.substr(pos + 1));
#endif
}

Path & Path::operator/=(const Path & p)
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  _p /= p._p;
#else
  if (p.empty()) return *this;
  if (!p.is_relative()) { _p = p._p; }
  else
  {
    if (_p[_p.size() - 1] != '/') _p += '/';
    if (p._p.size() >= 2 && p._p[0] == '.' && p._p[1] == '/')
      _p.append(p._p.substr(2));
    else
      _p.append(p._p);
  }
#endif
  return *this;
}

Path Path::operator/(const Path & p) const
{
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  return _p / p._p;
#else
  Path result(*this);
  result /= p;
  return result;
#endif
}

// Creates all directories in the path list, including parent directories if
// necessary. It's not an error if a directory to be created already exists.
// Returns true on error.
bool PathList::make_directories() const
{
  for (const_iterator path = begin(); path != end(); ++path)
  {
    if (path->make_directory()) return true;
  }
  return false;
}

// Converts the path list into a colon- or semicolon-separated string list.
std::string PathList::string() const
{
  std::string result;
  for (const_iterator path = begin(); path != end(); ++path)
  {
    if (path != begin()) result += preferred_separator;
    result += path->string();
  }
  return result;
}

// Parses a list of filesystem paths separated by colons (or semi-colons on
// Windows).
// '~' at the beginning of a path is replaced with the user's home directory.
size_t PathList::parse(std::string::value_type const * const paths)
{
  clear();
  if (!paths) return 0;
  // Split (semi)colon-separated path list:
  std::basic_istringstream<std::string::value_type> iss(paths);
  std::string token;
  while (std::getline(iss, token, preferred_separator))
  {
    // Resolve references to home directory (~).
    if (token[0] == home_directory_shortcut)
    { push_back(Path::home_path().concat(token.substr(1))); }
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
