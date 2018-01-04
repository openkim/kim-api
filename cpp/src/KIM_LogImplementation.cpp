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

namespace KIM
{
std::ofstream LogImplementation::logStream_;
int LogImplementation::numberOfObjectsCreated_ = 0;
int LogImplementation::numberOfObjectsDestroyed_ = 0;

namespace
{
LogVerbosity const defaultLogVerbosity(KIM_LOG_MAXIMUM_LEVEL);
}  // namespace

int LogImplementation::Create(LogImplementation ** const logImplementation)
{
  LogImplementation * pLogImplementation;
  pLogImplementation = new LogImplementation();

  if (numberOfObjectsCreated_ == numberOfObjectsDestroyed_)
  {
    logStream_.open(LOG_DIR "/" LOG_FILE, std::ios::app);
    if (logStream_.fail())
    {
      std::cerr << "Unable to open " LOG_DIR "/" LOG_FILE " file."
                << std::endl;
      delete pLogImplementation;
      return true;
    }
    if (defaultLogVerbosity > LOG_VERBOSITY::silent)
    {
      std::stringstream ss;
      ss << "Log file opened.  Default verbosity level is '"
         << defaultLogVerbosity.String() << "'.";
      logStream_ << EntryString("system",
                                GetTimeStamp(),
                                "system",
                                ss.str(),
                                __LINE__,
                                __FILE__);
    }
  }
  ++numberOfObjectsCreated_;
  *logImplementation = pLogImplementation;
  pLogImplementation->LogEntry(LOG_VERBOSITY::information,
                               "Log object created.",
                               __LINE__, __FILE__);

  return false;
}

void LogImplementation::Destroy(LogImplementation ** const logImplementation)
{
  ++numberOfObjectsDestroyed_;
  (*logImplementation)->LogEntry(LOG_VERBOSITY::information,
                                 "Log object destroyed.",
                                 __LINE__, __FILE__);
  delete (*logImplementation);
  *logImplementation = 0;

  if (numberOfObjectsCreated_ == numberOfObjectsDestroyed_)
  {
    if (defaultLogVerbosity > LOG_VERBOSITY::silent)
    {
      logStream_ << EntryString("system",
                                GetTimeStamp(),
                                "system",
                                "Log file closed.",
                                __LINE__,
                                __FILE__);
    }
    logStream_.close();
  }
}

std::string LogImplementation::GetID() const
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
  std::stringstream ss;
  ss << "Log verbosity '" << logVerbosity.String() << "' pushed (on top of "
     << verbosity_.top().String() << ").";
  LogEntry(LOG_VERBOSITY::information, ss.str(),
           __LINE__, __FILE__);

  verbosity_.push(logVerbosity);
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
  if ((logVerbosity != LOG_VERBOSITY::silent) &&
      (logVerbosity <= verbosity_.top()))
    logStream_ << EntryString(logVerbosity.String(),
                              GetTimeStamp(),
                              idString_,
                              message,
                              lineNumber,
                              fileName);
}

LogImplementation::LogImplementation() :
    idNumber_(numberOfObjectsCreated_)
{
  std::stringstream ss;
  ss << numberOfObjectsCreated_;
  idString_ = ss.str();
  verbosity_.push(defaultLogVerbosity);
}

LogImplementation::~LogImplementation()
{
}

std::string LogImplementation::EntryString(std::string const & logVerbosity,
                                           std::string const & date,
                                           std::string const & idString,
                                           std::string const & message,
                                           int const lineNumberString,
                                           std::string const & fileName)
{
  std::stringstream ssPrefix;
  ssPrefix << date
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

std::string LogImplementation::GetTimeStamp()
{
  time_t rawTime;
  time(&rawTime);
  struct tm * timeInfo;
  timeInfo = localtime(&rawTime);
  char date[1024];
  strftime(date, 1023, "%F:%T%z", timeInfo);

  return std::string(date);
}

}  // namespace KIM
