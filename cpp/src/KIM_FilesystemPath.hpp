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
// Copyright (c) 2016--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Alexander Stukowski
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_FILESYSTEM_PATH_HPP_
#define KIM_FILESYSTEM_PATH_HPP_

#include <cstdlib>
#include <iostream>
#include <vector>

// If available, use the std::filesystem::path class
// for cross-platform file path handling introduced with C++17.
// Older versions of the GCC compiler support C++17 but do not include an
// implementation of the std::filesystem library. Then fall back to the GHC
// drop-in replacement library. In all other cases, in particular in C++98 mode,
// fall back to a minimal implementation based on std::string.
#if defined(__cplusplus) && __cplusplus >= 201703L && defined(__has_include)
// C++17 mode:
#if __has_include(<filesystem>)
// Have std::filesystem implementation:
#include <filesystem>
#else
// Fall back to drop-in library:
#define GHC_WIN_WSTRING_STRING_TYPE
#include "ghc-filesystem/filesystem.hpp"
// We need the alias from ghc::filesystem to std::filesystem
namespace std
{
namespace filesystem = ghc::filesystem;
}
#endif
#define KIM_API_USE_FILESYSTEM_LIBRARY 1
#endif

#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
namespace
{
typedef std::filesystem::path KIM_Path;
}
#else
#include <string>
namespace
{
typedef std::string KIM_Path;
}
#endif

namespace KIM
{
namespace FILESYSTEM
{
class Path
{
 public:
  // Platform-dependent character for separating path entries.
  static const std::string::value_type preferred_separator;

  Path() {}
  Path(const char * str)
  {
    if (str != NULL) path_ = str;
  }
  Path(const std::string & str) : path_(str) {}
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  Path(const std::filesystem::path & p) : path_(p) {}
  Path(std::filesystem::path && p) : path_(std::move(p)) {}
#endif

  Path & operator=(const std::string & other)
  {
    path_ = other;
    return *this;
  }

  Path & operator=(const char * other)
  {
    path_ = other;
    return *this;
  }

  Path & operator+=(char const * const s);
  Path & operator/=(const Path & p);
  Path operator/(const Path & p) const;
  friend bool operator<(const Path & lhs, const Path & rhs)
  {
    return lhs.path_ < rhs.path_;
  }
  friend bool operator==(const Path & lhs, const Path & rhs)
  {
    return lhs.path_ == rhs.path_;
  }
  friend bool operator!=(const Path & lhs, const Path & rhs)
  {
    return !(lhs == rhs);
  }

  // Concatenates the current path and the argument.
  Path & concat(const std::string & p);

  // Turns the path object into a conventional string, which can be passed to
  // I/O functions.
  std::string string() const;

  // Returns the native string representation of the path, using native syntax,
  // native character type, and native character encoding. This string is
  // suitable for use with OS APIs.
  const KIM_Path::value_type * c_str() const { return path_.c_str(); }

  // Resets the path to an empty string.
  void clear() { path_.clear(); }

  // Returns whether this path is the empty string.
  bool empty() const { return path_.empty(); }

  // Returns the path of the parent path
  Path parent_path() const;

  // Returns the last component of the path.
  Path filename() const;

  // Removes a single filename component from the path.
  Path & remove_filename();

  // Converts all directory separators to the preferred directory separator of
  // the current platform.
  Path & make_preferred();

  // Creates a new directory, including parent directories if necessary.
  // It's not an error if the directory to be created already exists.
  // Returns true on error.
  bool MakeDirectory() const;

  // Deletes the contents of this path (if it is a directory) and the contents
  // of all its subdirectories, recursively, then deletes the file path itself.
  // Returns true on error.
  bool RemoveDirectoryRecursive() const;

  // Returns the list of subdirectories of this directory.
  std::vector<Path> Subdirectories() const;

  // Checks whether the file or directory exists.
  bool exists() const;

  // Checks whether the path is relative.
  bool is_relative() const;

  // Returns the current working directory.
  static Path current_path();

  // Returns the user's home directory.
  static Path HomePath();

  // Creates a new empty directory that can be used to write temporary files
  // into. Returns an empty path on failure.
  static Path CreateTemporaryDirectory(char const * const namePrefix);

  // Performs stream output on the path (operator <<).
  template<class CharT, class Traits>
  friend std::basic_ostream<CharT, Traits> &
  operator<<(std::basic_ostream<CharT, Traits> & os, const Path & p)
  {
    return os << p.path_;
  }

 private:
  // The internal path storage:
  KIM_Path path_;
};

class PathList : public std::vector<Path>
{
 public:
  // Platform-dependent character for separating paths in the lists.
  static const std::string::value_type PreferredSeparator;

  // Platform-dependent character for home directory.
  static const std::string::value_type HomeDirectoryShortcut;

  // Creates all directories in the path list, including parent directories if
  // necessary. It's not an error if a directory to be created already exists.
  // Returns true on error.
  bool MakeDirectories() const;

  // Converts the path list into a colon- or semicolon-separated string list.
  std::string ToString() const;

  // Parses a list of filesystem paths separated by colons (or semi-colons on
  // Windows).
  // '~' at the beginning of a path is replaced with the user's home directory.
  size_t Parse(std::string::value_type const * const paths);

  // Performs stream output on the path (operator <<).
  template<class CharT, class Traits>
  friend std::basic_ostream<CharT, Traits> &
  operator<<(std::basic_ostream<CharT, Traits> & os, const PathList & p)
  {
    return os << p.ToString();
  }
};

}  // namespace FILESYSTEM
}  // namespace KIM

#endif  // KIM_FILESYSTEM_PATH_HPP_
