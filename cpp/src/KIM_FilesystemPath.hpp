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


#ifndef KIM_FILESYSTEM_PATH_HPP_
#define KIM_FILESYSTEM_PATH_HPP_

#include <cstdlib>
#include <iostream>
#include <vector>

// If available on the local platform, use the std::filesystem::path class
// for cross-platform file path handling introduced with C++17. Otherwise,
// fall back to a minimal implementation based on std::string.
#if __cplusplus >= 201703L && defined(__has_include) && __has_include(<filesystem>)
#define KIM_API_USE_FILESYSTEM_LIBRARY 1
#include <filesystem>
#else
#include <string>
#endif

namespace KIM
{
namespace FILESYSTEM
{
class Path
{
 public:
  Path() {}
  Path(const char * str) : _p(str) {}
  Path(const std::string & str) : _p(str) {}

  Path & operator=(const std::string & other)
  {
    _p = other;
    return *this;
  }

  Path & operator=(const char * other)
  {
    _p = other;
    return *this;
  }

  Path & operator+=(char const * const s);
  Path & operator/=(const Path & p);
  Path operator/(const Path & p) const;
  friend bool operator<(const Path & lhs, const Path & rhs)
  {
    return lhs._p < rhs._p;
  }
  friend bool operator==(const Path & lhs, const Path & rhs)
  {
    return lhs._p == rhs._p;
  }

  // Concatenates the current path and the argument.
  Path & concat(const std::string & p);

  // Turns the path object into a conventional string, which can be passed to
  // I/O functions.
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::string string() const { return _p.string(); }
#else
  const std::string & string() const { return _p; }
#endif

  // Resets the path to an empty string.
  void clear() { _p.clear(); }

  // Returns whether this path is the empty string.
  bool empty() const { return _p.empty(); }

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
  bool make_directory() const;

  // Deletes the contents of this path (if it is a directory) and the contents
  // of all its subdirectories, recursively, then deletes the file path itself.
  // Returns true on error.
  bool remove_directory_recursive() const;

  // Returns the list of subdirectories of this directory.
  std::vector<Path> subdirectories() const;

  // Checks whether the file or directory exists.
  bool exists() const;

  // Checks whether the path is relative.
  bool is_relative() const;

  // Returns the current working directory.
  static Path current_path();

  // Returns the user's home directory.
  static Path home_path();

  // Creates a new empty directory that can be used to write temporary files
  // into. Returns an empty path on failure.
  static Path create_temporary_directory(char const * const namePrefix);

  // Performs stream output on the path (operator <<).
  template<class CharT, class Traits>
  friend std::basic_ostream<CharT, Traits> &
  operator<<(std::basic_ostream<CharT, Traits> & os, const Path & p)
  {
    return os << p._p;
  }

#ifndef KIM_API_USE_FILESYSTEM_LIBRARY
  // Platform-dependent character for separating path entries.
#ifndef _WIN32
  static const std::string::value_type preferred_separator = '/';
#else
  static const std::string::value_type preferred_separator = '\\';
#endif
#endif

 private:
  // The internal path storage:
#ifdef KIM_API_USE_FILESYSTEM_LIBRARY
  std::filesystem::path _p;
#else
  std::string _p;
#endif
};

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
  bool make_directories() const;

  // Converts the path list into a colon- or semicolon-separated string list.
  std::string string() const;

  // Parses a list of filesystem paths separated by colons (or semi-colons on
  // Windows).
  // '~' at the beginning of a path is replaced with the user's home directory.
  size_t parse(std::string::value_type const * const paths);
};

}  // namespace FILESYSTEM
}  // namespace KIM

#endif  // KIM_FILESYSTEM_PATH_HPP_
