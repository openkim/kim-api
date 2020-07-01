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
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_FILESYSTEM_PATH_HPP_
#define KIM_FILESYSTEM_PATH_HPP_

#include <cstdlib>
#include <iostream>

// If available on the local platform, use the std::filesystem::path class
// for cross-platform file path handling introduced with C++17. Otherwise,
// fall back to our own, minimal implementation of the file path class based
// on std::string.
#if defined(__has_include) && __has_include(<filesystem>)

#include <filesystem>
#include <random>

namespace KIM
{
namespace FILESYSTEM
{
class Path : public std::filesystem::path
{
 public:
  // Inherit constructors from base class.
  using std::filesystem::path::path;
  // Inherit assignment operators from base class.
  using std::filesystem::path::operator=;

  // Copy constructor.
  Path(const Path & p) = default;
  // Conversion constructor.
  Path(const std::filesystem::path & p) : std::filesystem::path(p) {}
  // Conversion constructor.
  Path(std::filesystem::path && p) : std::filesystem::path(std::move(p)) {}
  // Copy assignment.
  Path & operator=(const Path & p) = default;

  Path & operator+=(char const * const s)
  {
    std::filesystem::path::operator+=(std::string(s));
    return *this;
  }

  // Creates a new directory, including parent directories if necessary.
  // It's not an error if the directory to be created already exists.
  // Returns true on error.
  bool make_directory() const
  {
    std::error_code ec;
    std::filesystem::create_directories(*this, ec);
    if (ec)
    {
      std::cerr << "Failed to create directory '" << *this
                << "': " << ec.message() << std::endl;
      return true;
    }
    return false;
  }

  // Deletes the contents of this path (if it is a directory) and the contents
  // of all its subdirectories, recursively, then deletes the file path itself.
  // Returns true on error.
  bool remove_directory_recursive() const
  {
    std::error_code ec;
    std::filesystem::remove_all(*this, ec);
    if (ec)
    {
      std::cerr << "Failed to remove directory '" << *this
                << "': " << ec.message() << std::endl;
      return true;
    }
    return false;
  }

  // Returns the list of subdirectories of this directory.
  std::vector<Path> subdirectories() const
  {
    std::vector<Path> resultList;
    std::error_code ec;
    if (std::filesystem::is_directory(*this, ec))
    {
      for (auto & p : std::filesystem::directory_iterator(*this))
        resultList.push_back(p.path());
    }
    return resultList;
  }

  // Checks whether the file or directory exists.
  bool exists() const { return std::filesystem::exists(*this); }

  // Returns the current working directory.
  static Path current_path() { return Path(std::filesystem::current_path()); }

  // Returns the user's home directory.
  static Path home_path()
  {
#ifndef _WIN32
    return Path(::getenv("HOME"));
#else
    Path homeDrive = ::_wgetenv(L"HOMEDRIVE");
    Path homePath = ::_wgetenv(L"HOMEPATH");
    return homeDrive / homePath;
#endif
  }

  // Creates a new empty directory that can be used to write temporary files
  // into. Returns an empty path on failure.
  static Path create_temporary_directory(char const * const namePrefix)
  {
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
  }
};
}  // namespace FILESYSTEM
}  // namespace KIM

#else

#include <cstring>
#include <sys/stat.h>
#include <sys/types.h>

namespace KIM
{
namespace FILESYSTEM
{
class Path : public std::string
{
 public:
  // Inherit constructors from base class.
  using std::string::string;
  // Inherit assignment operators from base class.
  using std::string::string::operator=;

  // Copy constructor.
  Path(const Path & p) = default;
  // Copy assignment.
  Path & operator=(const Path & p) = default;

  // Platform-dependent character for separating path entries.
#ifndef _WIN32
  static std::string::value_type preferred_separator = '/';
#else
  static std::string::value_type preferred_separator = '\\';
#endif

  // Checks whether the path is relative.
  bool is_relative() const { return empty() || (*this)[0] != '/'; }

  // Removes a single filename component from the path.
  Path & remove_filename()
  {
    *this = substr(0, find_last_of('/'));
    return *this;
  }

  // Creates a new directory, including parent directories if necessary.
  // It's not an error if the directory to be created already exists.
  // Returns true on error.
  bool make_directory() const
  {
    mode_t mode = 0755;
    if (mkdir(c_str(), mode))
    {
      if (EEXIST == errno)
        return false;
      else
      {
        std::cerr << "Failed to create directory '" << *this << "'."
                  << std::endl;
        return true;
      }
    }
    return false;
  }

  // Deletes the contents of this path (if it is a directory) and the contents
  // of all its subdirectories, recursively, then deletes the file path itself.
  // Returns true on error.
  bool remove_directory_recursive() const
  {
    int error;
    struct dirent * dp = NULL;
    DIR * dir = NULL;
    dir = opendir(c_str());
    while ((dp = readdir(dir)))
    {
      // assuming no subdirectories, just files
      if ((0 != strcmp(dp->d_name, ".")) && (0 != strcmp(dp->d_name, "..")))
      {
        Path filePath = *this / dp->d_name;
        remove(filePath.c_str());
      }
    }
    closedir(dir);
    error = remove(c_str());
    if (error) { return true; }
    return false;
  }

  // Returns the list of subdirectories of this directory.
  std::vector<Path> subdirectories() const
  {
    std::vector<Path> resultList;

    DIR * dirp = NULL;
    struct dirent * dp = NULL;

    if (NULL != (dirp = opendir(c_str())))
    {
      do
      {
        struct stat statBuf;
        if ((NULL != (dp = readdir(dirp))) && (0 != strcmp(dp->d_name, "."))
            && (0 != strcmp(dp->d_name, "..")))
        {
          Path fullPath = *this / dp->d_name;
          if ((0 == stat(fullPath.c_str(), &statBuf))
              && (S_ISDIR(statBuf.st_mode)))
          { resultList.push_back(fullPath); }
        }
      } while (NULL != dp);
      closedir(dirp);
    }
    return resultList;
  }

  // Checks whether the file or directory exists.
  bool exists() const
  {
    struct stat statBuf;
    if (0 == stat(c_str(), &statBuf)) return true;
    return false;
  }

  // Turns the path object into a conventional string, which can be passed to
  // I/O functions.
  const std::string & string() const { return *this; }

  // Returns the current working directory.
  static Path current_path() { return getenv("PWD"); }

  // Returns the user's home directory.
  static Path home_path() { return getenv("HOME"); }

  // Creates a new empty directory that can be used to write temporary files
  // into. Returns an empty path on failure.
  static Path create_temporary_directory(char const * const namePrefix)
  {
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
    Path result = tmpdir;
    free(cstr);
    return result;
  }
};

}  // namespace FILESYSTEM
}  // namespace KIM

#endif

namespace KIM
{
namespace FILESYSTEM
{
class PathList : public std::vector<Path>
{
 public:
  // Platform-dependent character for separating paths in the lists.
#ifndef _WIN32
  static const std::string::value_type preferred_separator = ':';
#else
  static const std::string::value_type preferred_separator = ';';
#endif

  // Platform-dependent character for home directory.
  static const std::string::value_type home_directory_shortcut = '~';

  // Creates all directories in the path list, including parent directories if
  // necessary. It's not an error if a directory to be created already exists.
  // Returns true on error.
  bool make_directories() const
  {
    for (const_iterator path = begin(); path != end(); ++path)
    {
      if (path->make_directory()) return true;
    }
    return false;
  }

  // Converts the path list into a colon- or semicolon-separated string list.
  std::string string() const
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
  size_t parse(std::string::value_type const * const paths)
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
};

}  // namespace FILESYSTEM
}  // namespace KIM

#endif  // KIM_FILESYSTEM_PATH_HPP_
