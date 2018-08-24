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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <time.h>

#ifndef KIM_LOG_IMPLEMENTATION_HPP_
#include "KIM_LogImplementation.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#define LOG_DIR "."
#define LOG_FILE "kim.log"

// log helper
#define SPTR( x ) static_cast<std::ostringstream &>(                    \
    std::ostringstream() << static_cast<void const *>(x) ).str()


namespace KIM
{
namespace
{
LogVerbosity const defaultLogVerbosity(KIM_LOG_MAXIMUM_LEVEL);

int Validate(LogVerbosity const logVerbosity)
{
  int numberOfLogVerbosities;
  LOG_VERBOSITY::GetNumberOfLogVerbosities(& numberOfLogVerbosities);

  for (int i = 0; i < numberOfLogVerbosities; ++i)
  {
    LogVerbosity logVerb;
    LOG_VERBOSITY::GetLogVerbosity(i, &logVerb);

    if (logVerbosity == logVerb)
    {
      return true;
    }
  }

  return false;
}
}  // namespace

int LogImplementation::Create(LogImplementation ** const logImplementation)
{
  *logImplementation = new LogImplementation();

  std::stringstream ss;
  ss << "Log object created.  Default verbosity level is '"
     << defaultLogVerbosity.String() << "'.";
  (*logImplementation)->LogEntry(LOG_VERBOSITY::information,
                                 ss.str(),
                                 __LINE__, __FILE__);

  return false;
}

void LogImplementation::Destroy(LogImplementation ** const logImplementation)
{
  (*logImplementation)->LogEntry(LOG_VERBOSITY::information,
                                 "Log object destroyed.",
                                 __LINE__, __FILE__);
  delete (*logImplementation);
  *logImplementation = NULL;
}

std::string const & LogImplementation::GetID() const
{
  return idString_;
}

namespace
{
std::string SanitizeID(std::string const & id)
{
  std::string idCopy = id;
  std::string::iterator itr;
  for (itr=idCopy.begin(); itr != idCopy.end(); ++itr)
  {
    if (isspace(*itr))
    {
      *itr = '_';
    }
    else if ('*' == *itr)
    {
      *itr = '_';
    }
  }

  return idCopy;
}
}  // namespace

void LogImplementation::SetID(std::string const & id)
{
  std::string const sanitizedID = SanitizeID(id);
  std::stringstream ss;
  ss << "Log object renamed.  ID changed to '" << sanitizedID << "'.";
  std::stringstream tt;
  tt << "Log object renamed.  ID changed from '" << idString_ << "'.";

  LogEntry(LOG_VERBOSITY::information, ss.str(),
           __LINE__, __FILE__);

  idString_ = sanitizedID;

  LogEntry(LOG_VERBOSITY::information, tt.str(),
           __LINE__, __FILE__);
}

void LogImplementation::PushVerbosity(LogVerbosity const logVerbosity)
{
  LogVerbosity logVerb(logVerbosity);
  if (! Validate(logVerbosity)) logVerb = verbosity_.top();

  std::stringstream ss;
  ss << "Log verbosity '" << logVerb.String() << "' pushed (on top of "
     << verbosity_.top().String() << ").";
  LogEntry(LOG_VERBOSITY::information, ss.str(),
           __LINE__, __FILE__);

  verbosity_.push(logVerb);
}

void LogImplementation::PopVerbosity()
{
  std::stringstream ss;
  ss << "Log verbosity '" << verbosity_.top().String()
     << "' popped, revealing '";

  verbosity_.pop();
  if (verbosity_.empty())
  {
    verbosity_.push(defaultLogVerbosity);
  }

  ss << verbosity_.top().String() << "'.";
  LogEntry(LOG_VERBOSITY::information, ss.str(),
           __LINE__, __FILE__);
}

void LogImplementation::LogEntry(LogVerbosity const logVerbosity,
                                 std::string const & message,
                                 int const lineNumber,
                                 std::string const & fileName) const
{
  LogVerbosity logVerb(logVerbosity);
  if (! Validate(logVerbosity)) logVerb = verbosity_.top();

  if ((logVerb != LOG_VERBOSITY::silent) && (logVerb <= verbosity_.top()))
  {
    // Need to figure out how to do file locking to make this work for
    // parallel computations.

    FILE * file = fopen(LOG_DIR "/" LOG_FILE, "a");
    if (file == NULL)
    {
      std::cerr << "Unable to open " LOG_DIR "/" LOG_FILE " file."
                << std::endl;
    }
    else
    {
      std::string tm(GetTimeStamp());
      std::string entry(EntryString(logVerb.String(),
                                    tm,
                                    sequence_,
                                    idString_,
                                    message,
                                    lineNumber,
                                    fileName));
      fwrite(entry.c_str(), sizeof(char), entry.length(), file);
      fclose(file);
    }
  }
}

LogImplementation::LogImplementation() :
    idString_(SPTR(this)),
    latestTimeStamp_(""),
    sequence_(0)
{
  verbosity_.push(defaultLogVerbosity);
}

LogImplementation::~LogImplementation()
{
}

std::string LogImplementation::EntryString(std::string const & logVerbosity,
                                           std::string const & date,
                                           int const sequence,
                                           std::string const & idString,
                                           std::string const & message,
                                           int const lineNumberString,
                                           std::string const & fileName)
{
  std::stringstream ssPrefix;
  ssPrefix << date << " * " << sequence
           << " * "
           << logVerbosity
           << " * "
           << idString
           << " * "
           << fileName << ":" << lineNumberString
           << " * ";
  std::string const prefix(ssPrefix.str());

  std::string line;
  std::stringstream ssMessage(message);
  std::stringstream ss;

  while (std::getline(ssMessage, line, '\n'))
  {
    ss << prefix << line << "\n";
  }

  return ss.str();
}

std::string LogImplementation::GetTimeStamp() const
{
  time_t rawTime;
  time(&rawTime);
  struct tm * timeInfo;
  timeInfo = localtime(&rawTime);
  char date[1024];
  strftime(date, 1023, "%Y-%m-%d:%H:%M:%S%Z", timeInfo);

  std::string dateString(date);
  if (dateString == latestTimeStamp_)
  {
    ++sequence_;
  }
  else
  {
    sequence_ = 0;
    latestTimeStamp_ = dateString;
  }

  return dateString;
}

}  // namespace KIM
