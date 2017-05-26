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
// Copyright (c) 2013--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//    Ryan S. Elliott
//    Ellad B. Tadmor
//    Tobias Brink
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstdlib>
#include <cctype>
#include <iostream>
#include <fstream>
#include <cstring>
#include <sstream>

// String trimming utility routines
#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>

#include "KIM_Model.hpp"
#include "KIM_Model.h"

// trim from start (in place)
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
            std::not1(std::ptr_fun<int, int>(std::isspace))));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(),
            std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}

#include "old_KIM_API.h"
#include "old_KIM_API_status.h"
#include "old_KIM_API_DIRS.h"

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
#include <unistd.h>
#include <dlfcn.h>
#endif

extern const unsigned char STANDARD_KIM_STR_NAME[];

namespace OLD_KIM
{

static void strip_char_string(char* nm)
{
   //strip spaces and tabs from back
   for(int i=(int)strlen(nm); i>0; i--){
      if((nm[i-1]!=' ') && (nm[i-1]!='\t')){nm[i]='\0'; break;}
   }
   //strip spaces and tabs from front
   int c=0,key=0;
   for(int i=0;i<=(int)strlen(nm);i++){
      if((nm[i]!=' ') && (nm[i]!='\t')){key=1;}
      if(key==1){nm[c]=nm[i]; c++;}
   }
}

KIMBaseElementFlag:: KIMBaseElementFlag(){
    calculate=0;
 }

 KIMBaseElementUnit:: KIMBaseElementUnit(){
    init();
}
void KIMBaseElementUnit::init(){
  dim = "none";
}
int Atom_Map::comparator(const void* a1, const void* a2){
    Atom_Map *am1 =(Atom_Map *)a1;
    Atom_Map *am2 =(Atom_Map *)a2;
    return (am1->symbol.compare(am2->symbol));
}

KIM_IOline::KIM_IOline(){
    goodformat=false;init2empty();
}

bool KIM_IOline:: getFields(char * const inString){
            char *tmp;

            init2empty();
            //check for comments part and get it removed
            tmp = strpbrk(inString,"#");
            if(tmp !=NULL) {
                comments = tmp;
                tmp[0]='\0';

            }
             strip_char_string(inString);

            if(inString[0] == '\0') return false;
            //check if it is input or output section
            strip_char_string(inString);

            if(isitinput(inString)){
                input = true;
                output = false;

                return false;
            }else if(isitoutput(inString)){

                input = false;
                output = true;
                return false;
            }

            //parse field
            tmp = strtok(inString," \t");if(tmp == NULL) return false;
            name = tmp;

            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            type = tmp;

            if(type == ":=") return false;

            if(type == "flag") {
                dim = "none";
                return true;
            }

            if(type == "spec") {
                tmp = strtok(NULL," \t");
                if(tmp == NULL)
                  return false;
                else
                  dim = tmp;
                return true;
            }

            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            dim = tmp;

            tmp = strtok(NULL," \t");if(tmp == NULL) return true;
            requirements = tmp;

            return true;
}

bool KIM_IOline::isitoptional(){
            if(this->requirements == "optional") return true;
            return false;
}


void KIM_IOline::strip(){
            trim(name);
            trim(type);
            trim(dim);
            trim(requirements);
 }
void KIM_IOline:: init2empty(){
            name="";
            type="";
            dim="";
            requirements="";
            comments="";
}
bool KIM_IOline:: isitinput(const char*str){
            char tocmp [] ="MODEL_INPUT:";
            if(strlen(str)<strlen(tocmp)) return false;
            if(strncmp(str,tocmp,strlen(tocmp))==0) return true;
            return false;
}
bool KIM_IOline:: isitoutput(const char*str){
            char tocmp [] ="MODEL_OUTPUT:";
            if(strlen(str)<strlen(tocmp)) return false;
            if(strncmp(str,tocmp,strlen(tocmp))==0) return true;
            return false;
}


void IOline:: strip(){
            trim(name);
            trim(value);
            trim(comment);
}

IOline::IOline(){
    goodformat=false;
    comment="";
    name="";
    value="";
}
bool IOline:: getFields(char * const inputString){
                int i;
                name="";
                value="";
                comment="";
                for(i=0; i<(int)strlen(inputString); i++){
                    if(*(inputString+i)=='#'){return false;};
                    if(*(inputString+i)==':' && *(inputString+i+1)=='='){i+=2;break;};
                    name.append(1,*(inputString+i));
                }
                if(i>=(int)strlen(inputString)){return false;};

                for(;i<(int)strlen(inputString); i++){
                        if(*(inputString+i)=='#'){i+=2;break;};
                        value.append(1,*(inputString+i));
                }

                if(i>(int)strlen(inputString)){strip();return true;};
                for(;i<(int)strlen(inputString); i++){
                        comment.append(1,*(inputString + i));
                }

                strip();

                return true;
}

int IOline::readlines_str(const char* instrn, IOline** inlines, bool& success){
   success = true;
    int counter=0;
        IOline inlne;
        *inlines=NULL;
        std::string in_instrn=instrn;
        std::stringstream myfile (in_instrn,std::stringstream::in|std::stringstream::out) ;
        std::stringstream myfile1 (in_instrn,std::stringstream::in|std::stringstream::out) ;
         if(!myfile){
            std::cout<<"* Error (IOline::readlines_str): can not parse instrn."<< std::endl
                     <<"        Offending line is: `" << in_instrn << "'" << std::endl;
             success = false;
             return -1;
         }
         myfile.seekp(std::stringstream::beg);
          while(!myfile.eof()){
                myfile>> inlne;
                if(inlne.goodformat) counter++;
        }


        if (counter < 1) return counter;


        (*inlines) =  new IOline [counter] ;


        myfile1.seekp(std::stringstream::beg);

        counter=0;
        while(!myfile1.eof()){
                myfile1 >> inlne;

                if(inlne.goodformat) {

                    (*inlines)[counter]=inlne;

                    counter++;
                }
        }
        return counter;
}


KIMBaseElement:: KIMBaseElement(){
            nullify();
}
KIMBaseElement::~KIMBaseElement(){

}
bool KIMBaseElement:: init(std::string const & nm,std::string const & tp,
                           intptr_t sz,void * pdata){
            flag = new KIMBaseElementFlag;

            unit = new KIMBaseElementUnit;
            name = nm;
            type = tp;

            size = sz;
            data.p = (void *)  (*(( intptr_t **)pdata));


            flag->calculate = 1;

            return true;
}
void KIMBaseElement::free(){
           if(flag!=NULL) delete  flag;
           if(unit!=NULL) delete unit;
           nullify();

}
void KIMBaseElement::nullify(){
            data.p=NULL;
            size=0;
            name="";
            type ="";
            flag = NULL;
            unit = NULL;

}
bool KIMBaseElement::equiv(KIM_IOline& kimioline){
   if(kimioline.name == name)
      if(kimioline.type == type){
         if(kimioline.type == "flag") return true;
         if(kimioline.dim == unit->dim) return true;
      }
   return false;
}

int KIMBaseElement::getelemsize(const char *tp, bool& success){
            success = true;
            char const realkey[]="real";      //key defenitions
            char const doublekey[]="double";
            char const integerkey[]="integer";
            char const ptrkey[]="pointer";
            char const methodkey[]="method";
            char const flagkey[]="flag";// add here to expand...

            if(strcmp(realkey,tp)==0){
                return(sizeof(float));
            }else if (strcmp(integerkey,tp)==0){
                return(sizeof(int));
            }else if (strcmp(ptrkey,tp)==0){
                return(sizeof(int *));
            }else if (strcmp(doublekey,tp)==0){
                return (sizeof(double));
            }else if (strcmp(methodkey,tp)==0){
                return (sizeof(int *));
             }else if (strcmp(flagkey,tp)==0){
                return 0;
            }else{// add here more in else if block...
               std::cout << "* Error (KIMBaseElement::getelemsize): Unknown Type in KIM descriptor file line." << std::endl
                    << "         `" << tp <<"' is not one of: " << realkey << ", "
                    << doublekey << ", " << integerkey << ", " << ptrkey << ", "
                    << flagkey << std::endl;
               success = false;
               return -1;
            }
}

char const* const KIM_API_model::required_arguments[] =
{"numberOfSpecies",
 "numberOfParticles",
 "particleSpecies",
 "particleContributing",
 "coordinates",
 "neighObject",
 "get_neigh",
 "compute",
 "reinit",
 "destroy"
};

std::map<std::string,std::string> KIM_API_model::kim_str_map;


KIM_API_model:: KIM_API_model(){
       inlines=NULL;

       model_index_shift=0;
       neiOfAnAtom = NULL;
       neiOfAnAtomSize = 0;
       ErrorCode=1;
       AtomsTypes = NULL;
       nAtomsTypes = 0;
       model_buffer=NULL;

       model_lib_handle = NULL;

       influenceDistance = NULL;
       numberOfCutoffs = 0;
       cutoffs = NULL;
}
KIM_API_model:: ~KIM_API_model(){
      // free();

      if (neiOfAnAtomSize > 0) {
         delete [] neiOfAnAtom;
      }
}

int KIM_API_model::prestring_init(const char *instrn){
        if (!read_file_str(instrn,&inlines,&numlines))
        {
          ErrorCode = KIM_STATUS_FAIL;
          return ErrorCode;
        }

        char pointer_str [] = "pointer";
        //get Atoms Types and nAtomsTypes
        if (! init_AtomsTypes()) {
            ErrorCode=KIM_STATUS_FAIL;
            return ErrorCode;
        }
        bool bl_dummy;
        char* tmp_data = new char[(numlines-nAtomsTypes)*KIMBaseElement::getelemsize("pointer",bl_dummy)];
        model.init(name_temp,pointer_str,(intptr_t)(numlines-nAtomsTypes+3),&tmp_data);
        model.size =(intptr_t)(numlines-nAtomsTypes);

        int ii=0;
        for (int i=0; i< numlines;i++){

            //check for spec type
            if (inlines[i].type != "spec") {

                KIMBaseElement *el = new KIMBaseElement ;

                std::string & name = inlines[i].name;
                std::string & type = inlines[i].type;

                int *dummy=NULL;
                if (el->init(name,type,0, &dummy)) //preinit element with zero size
                {
                   //here to add checking is it derived or is it base units
                   el->unit->dim = inlines[i].dim;

                   el->flag->calculate = 1;
                }
                else
                {
                   ErrorCode = KIM_STATUS_FAIL;
                }
                KIMBaseElement **pel =(KIMBaseElement**) model.data.p;
                pel[ii] =  el;
                ii++;
            }
        }
        if (ErrorCode == KIM_STATUS_FAIL) return ErrorCode;

        //resize inlines (remove spec type variables)
        KIM_IOline * inlinesnew = new KIM_IOline[numlines - nAtomsTypes+3];
        ii=0;
        for (int i=0; i< numlines;i++){
            //check for spec type
            if (inlines[i].type != "spec") {
                inlinesnew[ii]=inlines[i];
                ii++;
            }
        }
        delete [] inlines;
        inlines = inlinesnew;
        inlinesnew = NULL;
        numlines=numlines - nAtomsTypes;
        //end resize inlines


        //extra input like unitFixed flag and,later may be, authors
        IOline *extrainput;
        bool readlines_str_success;
        IOline::readlines_str(instrn,&extrainput, readlines_str_success);
       if (!readlines_str_success)
        {
          ErrorCode = KIM_STATUS_FAIL;
          return ErrorCode;
        }
        // do nothing for now
        delete [] extrainput;
        unit_h.init_str(instrn,&ErrorCode);
        if(ErrorCode < KIM_STATUS_OK) return ErrorCode;

        // check for version number reported by Model/Simulator
        if(!does_it_have_a_version_number(instrn) && (name_temp != "standard"))
        {
          std::cout << "* Error (KIM_API_model::prestring_init()): "<<name_temp << " '.kim' file contains invalid KIM_API_Version setting." << std::endl;
          ErrorCode = KIM_STATUS_FAIL;
          return ErrorCode;
        }

        ErrorCode = KIM_STATUS_OK;
        return ErrorCode;
}

bool KIM_API_model::does_it_have_a_version_number(
    const char* const instrn)
{
  IOline * IOlines=NULL;
  bool readlines_str_success;
  int nlines = IOline::readlines_str((char*) instrn,&IOlines,readlines_str_success);
  if (!readlines_str_success) return false;

  for (int i=0; i<nlines;++i)
  {
    if (IOlines[i].name == "KIM_API_Version")
    {
      char * temp_str = new char[IOlines[i].value.length()+1];
      strcpy(temp_str, IOlines[i].value.c_str());

      // Get Major version
      char* tmp = strtok(temp_str, ".");
      if (tmp == NULL)
      {
        delete [] temp_str;
        delete [] IOlines;
        return false;
      }
      char* end;
      tempVersionMajor = strtol(tmp, &end, 10);
      if ('\0' != *end)
      {
        delete [] temp_str;
        delete [] IOlines;
        return false;
      }

      // Get Minor version
      tmp = strtok(NULL, ".");
      if (tmp == NULL)
      {
        delete [] temp_str;
        delete [] IOlines;
        return false;
      }
      tempVersionMinor = strtol(tmp, &end, 10);
      if ('\0' != *end)
      {
        delete [] temp_str;
        delete [] IOlines;
        return false;
      }

      // Check that remaining string is a valid PATCH value
      tmp = strtok(NULL, ".");
      if (NULL != tmp)
      {
        strtol(tmp, &end, 10);
        if ('\0' != *end)
        {
          delete [] temp_str;
          delete [] IOlines;
          return false;
        }
      }

      delete [] temp_str;
      delete [] IOlines;
      return true;
    }
  }

  if (IOlines != NULL) delete [] IOlines;
  return false;
}

int KIM_API_model::get_version(const char** const version)
{
  *version = KIM_API_VERSION;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_major(int* const major)
{
  *major = KIM_API_VERSION_MAJOR;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_minor(int* const minor)
{
  *minor = KIM_API_VERSION_MINOR;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_patch(int* const patch)
{
  *patch = KIM_API_VERSION_PATCH;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_prerelease(const char** const prerelease)
{
  *prerelease = KIM_API_VERSION_PRERELEASE;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_build_metadata(const char** const build_metadata)
{
  *build_metadata = KIM_API_VERSION_BUILD_METADATA;
  return KIM_STATUS_OK;
}

int KIM_API_model::version_newer(const char* const versionA,
                                 const char* const versionB,
                                 int* const result)
{
  unsigned int len = strlen(versionA);
  if (len < strlen(versionB)) len = strlen(versionB);
  len = len + 1;

  int A_Major;
  int A_Minor;
  int A_Patch;
  char* const A_Prerelease = new char[len];
  char* const A_Build = new char[len];

  int B_Major;
  int B_Minor;
  int B_Patch;
  char* const B_Prerelease = new char[len];
  char* const B_Build = new char[len];

  std::stringstream A_PR;
  std::string A_tok;
  std::stringstream B_PR;
  std::string B_tok;

  int retval = KIM_STATUS_FAIL;
  *result = -1;

  if (!parse_semver(versionA, &A_Major, &A_Minor, &A_Patch, A_Prerelease,
                    A_Build))
  {
    retval = KIM_STATUS_FAIL;
    goto exit;
  }

  if (!parse_semver(versionB, &B_Major, &B_Minor, &B_Patch, B_Prerelease,
                    B_Build))
  {
    retval = KIM_STATUS_FAIL;
    goto exit;
  }

  retval = KIM_STATUS_OK;

  if (A_Major > B_Major)
  {
    *result = 1;
    goto exit;
  }
  else if (A_Major < B_Major)
  {
    *result = 0;
    goto exit;
  }
  // Major values are equal

  if (A_Minor > B_Minor)
  {
    *result = 1;
    goto exit;
  }
  else if (A_Minor < B_Minor)
  {
    *result = 0;
    goto exit;
  }
  // Minor values are equal

  if (A_Patch > B_Patch)
  {
    *result = 1;
    goto exit;
  }
  else if (A_Patch < B_Patch)
  {
    *result = 0;
    goto exit;
  }
  // Patch values are equal

  if ('\0' == A_Prerelease[0] && '\0' != B_Prerelease[0])
  {
    *result = 1;
    goto exit;
  }
  else if ('\0' != A_Prerelease[0] && '\0' == B_Prerelease[0])
  {
    *result = 0;
    goto exit;
  }
  else if ('\0' == A_Prerelease[0] && '\0' == B_Prerelease[0])
  {
    *result = 0; // A and B are equal
    goto exit;
  }
  // Both are prereleases of the same version

  A_PR << A_Prerelease;
  B_PR << B_Prerelease;

  std::getline(A_PR, A_tok, '.');
  std::getline(B_PR, B_tok, '.');

  while (!A_PR.eof() && !B_PR.eof())
  {
    int Aint;
    char* A_End;
    Aint = strtol(A_tok.c_str(), &A_End, 10);
    int Bint;
    char* B_End;
    Bint = strtol(B_tok.c_str(), &B_End, 10);
    if ('\0' != *A_End && '\0' == *B_End) // A numeric & B alpha
    {
      *result = 1;
      goto exit;
    }
    else if ('\0' == *A_End && '\0' != *B_End) // A alpha & B numeric
    {
      *result = 0;
      goto exit;
    }
    else if ('\0' != *A_End && '\0' != *B_End) // Both alpha
    {
      if (A_tok > B_tok)
      {
        *result = 1;
        goto exit;
      }
      else if (A_tok < B_tok)
      {
        *result = 0;
        goto exit;
      }
      // equal strings
    }
    else // Both numeric
    {
      if (Aint > Bint)
      {
        *result = 1;
        goto exit;
      }
      else if (Aint < Bint)
      {
        *result = 0;
        goto exit;
      }
      // equal numbers
    }
    // equal identifiers

    std::getline(A_PR, A_tok, '.');
    std::getline(B_PR, B_tok, '.');
  }

  if (A_PR.eof() && !B_PR.eof())
  {
    *result = 1;
    goto exit;
  }
  else if (!A_PR.eof() &&  B_PR.eof())
  {
    *result = 0;
    goto exit;
  }
  else
  {
    // versions are equal (thus, A is not newer than B)
    *result = 0;
    goto exit;
  }

 exit:
  delete [] A_Prerelease;
  delete [] A_Build;
  delete [] B_Prerelease;
  delete [] B_Build;

  return retval;
}

bool KIM_API_model::parse_semver(const char* const version, int* const major,
                                 int* const minor, int* const patch,
                                 char* const prerelease,
                                 char* const build_metadata)
{
  unsigned int len = strlen(version) + 1;
  char* const ver = new char[len];
  strcpy(ver, version);

  bool hasPrerelease = false;
  bool hasBuild = false;
  for (unsigned int i=0; i<len; ++i)
  {
    if ('-' == ver[i])
    {
      hasPrerelease = true;

      for (; i<len; ++i)
      {
        if ('+' == ver[i])
        {
          hasBuild = true;
          break;
        }
      }
      break;
    }
  }

  char* tok;
  tok = strtok(ver, ".");

  char* end;
  int val;

  val = strtol(tok, &end, 10);
  if ('\0' != *end)
  {
    delete [] ver;
    return false;
  }
  *major = val;

  tok = strtok(NULL, ".");
  if (NULL == tok)
  {
    delete [] ver;
    return false;
  }
  val = strtol(tok, &end, 10);
  if ('\0' != *end)
  {
    delete [] ver;
    return false;
  }
  *minor = val;

  tok = strtok(NULL, "-");
  if (NULL == tok)
  {
    delete [] ver;
    return false;
  }
  val = strtol(tok, &end, 10);
  if ('\0' != *end)
  {
    delete [] ver;
    return false;
  }
  *patch = val;

  if (hasPrerelease)
  {
    tok = strtok(NULL, "+");
    if (NULL == tok) // empty prerelease
    {
      delete [] ver;
      return false;
    }
    // validate prerelease
    for (unsigned int i=0; i < strlen(tok); ++i)
    {
      if (!isalnum(tok[i]) && !('-' == tok[i]) && !('.' == tok[i]))
      {
        delete [] ver;
        return false;
      }
    }
    strcpy(prerelease, tok);

    if (hasBuild)
    {
      tok = strtok(NULL, "+");
      if (NULL == tok) // empty build
      {
        delete [] ver;
        return false;
      }
      // validate build
      for (unsigned int i=0; i < strlen(tok); ++i)
      {
        if (!isalnum(tok[i]) && !('-' == tok[i]) && !('.' == tok[i]))
        {
          delete [] ver;
          return false;
        }
      }
      strcpy(build_metadata, tok);
    }
  }

  delete [] ver;
  return true;
}

int KIM_API_model::get_version_model_major(int* const major) const
{
  *major = modelVersionMajor;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_model_minor(int* const minor) const
{
  *minor = modelVersionMinor;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_simulator_major(int* const major) const
{
  *major = simulatorVersionMajor;
  return KIM_STATUS_OK;
}

int KIM_API_model::get_version_simulator_minor(int* const minor) const
{
  *minor = simulatorVersionMinor;
  return KIM_STATUS_OK;
}

void KIM_API_model::free(int *error){
   free();
   *error = KIM_STATUS_OK; // no failures detectable
}

 void KIM_API_model::free(){
         KIMBaseElement **pel =  (KIMBaseElement **)  model.data.p;
        if(model.data.p != NULL)  for (int i =0;i<model.size;i++) {
            pel[i]->free();
            delete pel[i];
            pel[i]=NULL;
        }
        delete [] (char *) model.data.p;
        model.free();

        if(inlines != NULL) {
            delete [] inlines;
            inlines=NULL;
        }


        numlines=0;

        if(AtomsTypes !=NULL) {
            delete [] AtomsTypes;
            AtomsTypes=NULL;
            nAtomsTypes=0;
        }
 }

int KIM_API_model::set_data(const char * nm, intptr_t size, void *dt){
        // set data into preinit element correctly calculates all
        int error;
        int ind=get_index((char*) nm, &error);
    if (ind<0) {
            return KIM_STATUS_FAIL;
        } //no data in KIM_API_model
       (*this)[ind].data.p = dt;

        (*this)[ind].size = size;

        return KIM_STATUS_OK;
}
int KIM_API_model::set_method(const char * nm, intptr_t size, int const language, func_ptr dt){
        // set data into preinit element correctly calculates all
        int error;
        int ind=get_index((char*) nm, &error);
    if (ind<0) {
            return KIM_STATUS_FAIL;
        } //no data in KIM_API_model
       (*this)[ind].data.fp = dt;

        (*this)[ind].size = size;

        (*this)[ind].lang = language;

        return KIM_STATUS_OK;
}

void * KIM_API_model::get_data(const char *nm,int *error){
   int ind=get_index(nm, error);
        *error = KIM_STATUS_FAIL;
        if (ind<0) return NULL;
        *error =KIM_STATUS_OK;
        return (*this)[ind].data.p;
}

func_ptr KIM_API_model::get_method(const char *nm,int * const language, int *error){
   int ind=get_index(nm, error);
        *error = KIM_STATUS_FAIL;
        if (ind<0) return NULL;
        *error =KIM_STATUS_OK;
        *language = (*this)[ind].lang;
        return (*this)[ind].data.fp;
}

int KIM_API_model::get_index(const char *nm,int *error){
        for(int i=0; i< model.size;i++){
            if((*this)[i].name == nm) {
                *error =KIM_STATUS_OK;
                return i;
            }
        }
        *error = KIM_STATUS_FAIL;
        return -1;
}

KIMBaseElement & KIM_API_model::operator[](int i){
        if ((i > (*this).model.size) || (i < 0)){
          std::cout<<"* Error (KIM_API_model::operator[](int i): invalid index: " << i <<std::endl;
           KIM_API_model::fatal_error_print();
           exit(326);
        }
        KIMBaseElement **pel =(KIMBaseElement**) model.data.p;
        return *pel[i];
}
KIMBaseElement & KIM_API_model::operator[](const char *nm){
        int error;
        int ind=get_index(nm,&error);
        if (error == KIM_STATUS_FAIL){
          std::cout<<"* Error (KIM_API_model::operator[](char *nm): name not found: " << nm <<std::endl;
           KIM_API_model::fatal_error_print();
           exit(325);
        }
        KIMBaseElement **pel =(KIMBaseElement**) model.data.p;
        return *pel[ind];
}

bool KIM_API_model::read_file_str(const char* strstream, KIM_IOline** lns, int* numlns){
        int counter=0;
        KIM_IOline inln;

        //open string as stream from char *
        std::string in_strstream=strstream ;
        std::stringstream myfile (in_strstream, std::stringstream::in|std::stringstream::out);
        std::stringstream myfile1 (in_strstream, std::stringstream::in|std::stringstream::out);
        if(!myfile){
            std::cout<<"* Error (KIM_API_model::read_file_str): can not access KIM descriptor file as input string."<<std::endl;
            return false;
        }


        myfile.seekp(std::stringstream::beg);//set to the begining
        while(!myfile.eof()){
                myfile >> inln;
                if(inln.goodformat) counter++;
        }

        myfile1.seekp(std::stringstream::beg);//set to the begining

        *numlns = counter;
        *lns = new KIM_IOline[counter+3];

        //myfile.open(initfile);
        counter=0;


        while(!myfile1.eof()){
                myfile1 >> inln;

                if(inln.goodformat) {
                    (*lns)[counter]=inln;
                    counter++;
                }
        }

        return true;
}

bool KIM_API_model::is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns, bool ignore_optional){
    bool match;
    //check if lines are match with Model api variable
    match =true;
    for (int i=0; i<nlns;i++){
        match=false;

        if(!ignore_optional && IOlines[i].isitoptional()){
            match=true;
        }

        if(IOlines[i].type == "spec"){
            match=true;
        }
        if ( is_it_par(IOlines[i].name) ) match=true;

        for(int j=0;j<mdtst.model.size;j++){
            if(mdtst[j].equiv(IOlines[i])) {
                match = true;
                break;
            }
        }
        if(!match) {
           std::cout << "* Info (KIM_API_model::is_it_match): The following descriptor file line may not match with " << mdtst.model.name << "'s descriptor file."<<std::endl;
            std::cout<<IOlines[i]<<std::endl;
            return match;
        }
    }
    return match;
}

bool KIM_API_model::is_it_match_noFlagCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns, bool ignore_optional){
    bool match;
    //check if lines are match with Model api variable
    match =true;
    for (int i=0; i<nlns;i++){
        match=false;

        if(!ignore_optional && IOlines[i].isitoptional()){
            match=true;
        }

        if(IOlines[i].type == "spec"){
            match=true;
        }

        if ( is_it_par(IOlines[i].name) ) match=true;

        if(IOlines[i].type == "flag"){
            match=true;
        }
        for(int j=0;j<mdtst.model.size;j++){
            if(mdtst[j].equiv(IOlines[i])) {
                match = true;
            }else if(IOlines[i].type == "flag"){
                match = true;
            }else if(is_it_par(IOlines[i].name)){
                match = true;
            }

            if (!match){
                  for (int m=0; m<NUMBER_REQUIRED_ARGUMENTS; ++m){
                     if (mdtst.required_arguments[m] == IOlines[i].name){
                        match=true;
                        break;
                     }
                  }
            }
        }
        if(!match) {
            std::cout << "* Warning (KIM_API_model::is_it_match_noFlagCount): The following line in the Model descriptor file does not match."<<std::endl;
            std::cout<<IOlines[i]<<std::endl;
            return match;
        }
    }
    return match;
}

bool KIM_API_model::is_it_match(KIM_API_model &test,KIM_API_model & mdl){
    //preinit model from standard template kim file
   KIM_API_model stdmdl;

   stdmdl.name_temp = "standard";
   if(!stdmdl.prestring_init((char*) STANDARD_KIM_STR_NAME)){
      std::cout<<" preinit of :"<<"standard.kim"<<" failed"<<std::endl;
      stdmdl.free();
      return false;
   }

    // test and mdl must be preinit.
    bool test2modelmatch= is_it_match(test,mdl.inlines,mdl.numlines,false);
    bool model2testmatch= is_it_match(mdl,test.inlines,test.numlines,true);

    bool test2modelmatch_noDC= is_it_match_noFlagCount(test,mdl.inlines,mdl.numlines,false);
    bool model2testmatch_noDC= is_it_match_noFlagCount(mdl,test.inlines,test.numlines,true);

    bool test2standardmatch = is_it_match(stdmdl,test.inlines,test.numlines,true);

    bool model2standardmatch = is_it_match(stdmdl,mdl.inlines,mdl.numlines,true);

    bool test2standardAtomsTypesMatch = do_AtomsTypes_match(test,stdmdl);
    bool model2standardAtomsTypesMatch = do_AtomsTypes_match(mdl,stdmdl);
    bool test2modelAtomsTypesMatch = do_AtomsTypes_match(test,mdl);
    bool AtomsTypesMatch=test2standardAtomsTypesMatch&&model2standardAtomsTypesMatch&&test2modelAtomsTypesMatch;

    stdmdl.free();

    bool required_args_match=test.check_required_arguments();
    required_args_match=required_args_match&&mdl.check_required_arguments();
    bool units_match = Unit_Handling::do_unit_match(test.unit_h,mdl.unit_h);

    if(!test2standardmatch) std::cout<<"* Error (KIM_API_model::is_it_match): There are non-standard variables in Simulator descriptor file:"<<std::endl;
    if(!model2standardmatch) std::cout<<"* Error (KIM_API_model::is_it_match): There are non-standard variables in Model descriptor file:"<<std::endl;
    if(!test2standardAtomsTypesMatch) std::cout<<"* Error (KIM_API_model::is_it_match): There are non-standard Species in Simulator descriptor file:"<<std::endl;
    if(!model2standardAtomsTypesMatch) std::cout<<"* Error (KIM_API_model::is_it_match):there are non-standard Species in Model descriptor file:"<<std::endl;
    if(!test2modelAtomsTypesMatch) std::cout<<"* Error (KIM_API_model::is_it_match): Simulator-Model Species do not match:"<<std::endl;
    if(!required_args_match) std::cout<<"* Error (KIM_API_model::is_it_match): Missing required arguments:"<<std::endl;
    if(!units_match){
       std::cout<<"* Error (KIM_API_model::is_it_match): units do not match:"<<std::endl;
    }else{
        this->unit_h = mdl.unit_h;
    }

    bool flag_match = do_flag_match(test,mdl);

    if (test2modelmatch && model2testmatch && test2standardmatch &&
            model2standardmatch && AtomsTypesMatch && required_args_match && units_match) return true;
    if (test2modelmatch_noDC && model2testmatch_noDC && test2standardmatch &&
             model2standardmatch && AtomsTypesMatch && required_args_match && units_match){
       return flag_match;
    }
    return false;
}

bool KIM_API_model::is_it_in_and_is_it_flag(KIM_API_model& mdl,const char * name){
   int error;
   int i = mdl.get_index(name,&error);
   if (i<0) return false;
   if (mdl[i].type != "flag") return false;
   return true;
}
bool KIM_API_model::is_it_in(KIM_API_model& mdl, const char* name){
   int error;
   int i = mdl.get_index(name,&error);
   if (i<0) return false;
   return true;
}
bool KIM_API_model::do_flag_match(KIM_API_model& tst, KIM_API_model& mdl){
    // here the assumption : besides flag type , everything is a match

    // check flag for tst
   bool ZeroBasedLists_tst =is_it_in_and_is_it_flag(tst, (char*) "ZeroBasedLists");
   bool OneBasedLists_tst =is_it_in_and_is_it_flag(tst, (char*) "OneBasedLists");



    // check flag for mdl
   bool ZeroBasedLists_mdl =is_it_in_and_is_it_flag(mdl, (char*) "ZeroBasedLists");
   bool OneBasedLists_mdl =is_it_in_and_is_it_flag(mdl, (char*) "OneBasedLists");

    //logic for Zero or One base list handling
    if ((!ZeroBasedLists_tst && !OneBasedLists_tst)||(ZeroBasedLists_tst && OneBasedLists_tst) ) {
        std::cout<< "* Error (KIM_API_model::do_flag_match): Simulator descriptor file must have ONE of ZeroBasedLists or OneBasedLists."<<std::endl;
        return false;
    }
     if ((!ZeroBasedLists_mdl && !OneBasedLists_mdl)||(ZeroBasedLists_mdl && OneBasedLists_mdl)) {
        std::cout<< "* Error (KIM_API_model::do_flag_match): Model descriptor file must have ONE of ZeroBasedLists or OneBasedLists."<<std::endl;
        return false;
    }
    model_index_shift = 0;
    if (ZeroBasedLists_tst && OneBasedLists_mdl) model_index_shift = 1;
    if (OneBasedLists_tst && ZeroBasedLists_mdl) model_index_shift = -1;

    return true;

}
bool KIM_API_model::do_AtomsTypes_match(KIM_API_model& test, KIM_API_model& mdl){
    bool match;

    if (test.nAtomsTypes == 0 || mdl.nAtomsTypes == 0) return false;
    for (int i=0;i < test.nAtomsTypes; i++){
        match = false;
        for (int j=0;j<mdl.nAtomsTypes;j++){
            if(test.AtomsTypes[i].symbol == mdl.AtomsTypes[j].symbol){
                mdl.AtomsTypes[j].requestedByTest = true;
                match = true;
                break;
            }
        }
        if (!match) {
            std::cout <<"* Error (KIM_API_model::do_AtomsTypes_match): The following symbol: "<<test.AtomsTypes[i].symbol<<" in ";
            std::cout<< test.model.name << " is not found in "<<mdl.model.name<<std::endl;
            return false;
        }
    }
    return true;
}

bool KIM_API_model::is_it_par(std::string const & name){
     std::size_t loc = name.find("_");
     if (loc == std::string::npos) return false;
     if (name.substr(0,loc) == "PARAM") return true;
     return false;
}


#if KIM_LINK_VALUE != KIM_LINK_DYNAMIC_LOAD
extern "C"{
  #include "model_kim_str_include.h"
}

int KIM_API_model::get_model_kim_str_len(const char* const modelname,
                                         int* const kimStringLen)
{
     //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    unsigned int in_mdlstr_len = 0;
    const unsigned char* in_mdlstr = NULL;

    #include "model_kim_str_include.cpp"

    if (in_mdlstr == NULL){
       std::cout<<"* Error (KIM_API_model::get_model_kim_str_len): Unknown KIM Model name " << modelname << "." << std::endl;
       *kimStringLen = 0;
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();
       return KIM_STATUS_FAIL;
    }

    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    *kimStringLen = (int) in_mdlstr_len;
    return KIM_STATUS_OK;
}

int KIM_API_model::get_model_kim_str(const char* const modelname,
                                     const char** const kimString)
{
     //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    unsigned int in_mdlstr_len = 0;
    const unsigned char* in_mdlstr = NULL;

    #include "model_kim_str_include.cpp"

    if (in_mdlstr == NULL){
       std::cout<<"* Error (KIM_API_model::get_model_kim_str): Unknown KIM Model name " << modelname << "." << std::endl;
       *kimString = NULL;
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();
       return KIM_STATUS_FAIL;
    }

    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    *kimString = in_mdlstr;
    return KIM_STATUS_OK;
}

#else

int KIM_API_model::get_model_kim_str_len(const char* const modelname,
                                         int* const kimStringLen)
{
    void * tmp_model_lib_handle = NULL;
    std::stringstream model_kim_str_name;
    model_kim_str_name << modelname << "_" << KIM_STR_NAME;
    std::stringstream model_kim_str_len_name;
    model_kim_str_len_name << modelname << "_" << KIM_STR_NAME << "_len";

    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    std::vector<std::string> item;
    bool accessible = findItem(KIM_MODELS_DIR, modelname, &item);
    if (accessible)
    {
      std::string libFileName
          = item[1] + "/" + item[0] + "/" + MODELLIBFILE + ".so";
      tmp_model_lib_handle = dlopen(libFileName.c_str(), RTLD_NOW);
    }
    if (!accessible)
    {
      //redirecting back to > std::cout
      std::cout<< "* Error (KIM_API_model::get_model_kim_str_len): The Model shared library file is not readable for Model name: '";
      std::cout<<modelname<<"'"<<std::endl;
      std::cout.rdbuf(backup); filekimlog.close();
      fprintf(stderr,"The Model shared library file is not readable for Model name: '%s'.\n",modelname);
      *kimStringLen = 0;
      return KIM_STATUS_FAIL;
    }
    else if(tmp_model_lib_handle == NULL)
    {
      //redirecting back to > std::cout
      std::cout<< "* Error (KIM_API_model::get_model_kim_str_len): A problem occurred with the Model shared library file for Model name: '";
      std::cout<<modelname<<"'" <<std::endl<<dlerror()<<std::endl;
      std::cout.rdbuf(backup); filekimlog.close();
      fprintf(stderr,"A problem occurred with the Model shared library file for Model name: '%s'.\n",modelname);
      *kimStringLen = 0;
      return KIM_STATUS_FAIL;
    }
    else
    {
      std::cout<< "* Info (KIM_API_model::get_model_kim_str_len): Found Model shared library file for Model name: '" << modelname << "'" << std::endl;
    }

    const unsigned int* const model_str_len = (const unsigned int* const) dlsym(tmp_model_lib_handle, model_kim_str_len_name.str().c_str());
    char* dlsym_error = dlerror();
    if (dlsym_error) {
      std::cout << "* Error (KIM_API_model::get_model_kim_str): Cannot load symbol: " << dlsym_error <<std::endl;
      dlclose(tmp_model_lib_handle);

      //redirecting back to > std::cout
      std::cout.rdbuf(backup); filekimlog.close();

      *kimStringLen = 0;
      return KIM_STATUS_FAIL;
    }

    *kimStringLen = (int) *model_str_len;

    dlclose(tmp_model_lib_handle);
    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    return KIM_STATUS_OK;
}

int KIM_API_model::get_model_kim_str(const char* const modelname,
                                     const char** const kimString)
{
    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

  if (kim_str_map.find(modelname) == kim_str_map.end()) {
    void * tmp_model_lib_handle = NULL;
    std::stringstream model_kim_str_name;
    model_kim_str_name << modelname << "_" << KIM_STR_NAME;
    std::stringstream model_kim_str_len_name;
    model_kim_str_len_name << modelname << "_" << KIM_STR_NAME << "_len";

    std::vector<std::string> item;
    bool accessible = findItem(KIM_MODELS_DIR, modelname, &item);
    if (accessible)
    {
      std::string libFileName
          = item[1] + "/" + item[0] + "/" + MODELLIBFILE + ".so";
      tmp_model_lib_handle = dlopen(libFileName.c_str(), RTLD_NOW);
    }
    if(!accessible)
    {
       //redirecting back to > std::cout
       std::cout<< "* Error (KIM_API_model::get_model_kim_str): The Model shared library file is not readable for Model name: '";
       std::cout<<modelname<<"'"<<std::endl;
       std::cout.rdbuf(backup); filekimlog.close();
       fprintf(stderr,"The Model shared library file is not readable for Model name: '%s'.\n",modelname);
       *kimString = NULL;
       return KIM_STATUS_FAIL;
    }
    else if(tmp_model_lib_handle == NULL) {
       //redirecting back to > std::cout
       std::cout<< "* Error (KIM_API_model::get_model_kim_str): A problem occurred with the Model shared library file for Model name: '";
       std::cout<<modelname<<"'" <<std::endl<<dlerror()<<std::endl;
       std::cout.rdbuf(backup); filekimlog.close();
       fprintf(stderr,"A problem occurred with the Model shared library file for Model name: '%s'.\n",modelname);
       *kimString = NULL;
       return KIM_STATUS_FAIL;
    }
    else
    {
      std::cout<< "* Info (KIM_API_model::get_model_kim_str): Found Model shared library file for Model name: '" << modelname << "'" <<std::endl;
    }


    char* dlsym_error = dlerror();
    if (dlsym_error) {
        std::cout << "* Error (KIM_API_model::get_model_kim_str): Cannot load symbol: " << dlsym_error <<std::endl;
        dlclose(tmp_model_lib_handle);

        //redirecting back to > std::cout
        std::cout.rdbuf(backup); filekimlog.close();

        *kimString = NULL;
        return KIM_STATUS_FAIL;
    }
    const unsigned char* model_str_ptr = (const unsigned char* ) dlsym(tmp_model_lib_handle, model_kim_str_name.str().c_str());
    dlsym_error = dlerror();
    if (dlsym_error) {
        std::cout << "* Error (KIM_API_model::get_model_kim_str): Cannot load symbol: " << dlsym_error <<std::endl;
        dlclose(tmp_model_lib_handle);

        //redirecting back to > std::cout
        std::cout.rdbuf(backup); filekimlog.close();

        *kimString = NULL;
        return KIM_STATUS_FAIL;
    }

    kim_str_map[modelname] = (char const *) model_str_ptr;
    *kimString = kim_str_map[modelname].c_str();

    dlclose(tmp_model_lib_handle);

  } else {
    *kimString = kim_str_map[modelname].c_str();
  }

   //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    return KIM_STATUS_OK;
}
#endif

void KIM_API_model::fatal_error_print(){
    printf("* KIM FATAL ERROR: See kim.log file for details\n");
}


int KIM_API_model::preinit(const char* modelname){
    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);
    //preinit model

    int error;
    int result = KIM_STATUS_FAIL;
    const char* in_mdlstr;
    error = get_model_kim_str(modelname, &in_mdlstr);
    if (error == KIM_STATUS_OK)
    {
       this->name_temp = modelname;
       result = this->prestring_init(in_mdlstr);
    }
    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    return result;
}

int KIM_API_model::string_init(const char* in_tststr, const char* modelname){
   int error;
    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    //check test-model match and preinit test-model-API
    KIM_API_model test,mdl;
    const char* in_mdlstr;
    error = get_model_kim_str(modelname, &in_mdlstr);
    if (error != KIM_STATUS_OK) {
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();
       return error;
    }

    mdl.name_temp = modelname;
    error = mdl.prestring_init(in_mdlstr);
    if(error != KIM_STATUS_OK)
    {
      const char* msg;
      get_status_msg(mdl.ErrorCode, &msg);
       std::cout<<"mdl.prestring_init failed with error status:"<<msg<<std::endl;
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();
       return error;
    }

    //preinit test and model API object
    test.name_temp = (char*) "simulator_name";
    error = test.prestring_init(in_tststr);
    if(error != KIM_STATUS_OK)
    {
      const char* msg;
      get_status_msg(test.ErrorCode, &msg);
       std::cout<<"test.prestring_init failed with error status:"<<msg<<std::endl;
       mdl.free();
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();
       return error;
    }

    //check if they match
    if (is_it_match(test,mdl)){
       this->name_temp = mdl.model.name;
        this->prestring_init(in_mdlstr);
        this->unit_h=test.unit_h;
        if (!(this->irrelevantVars2donotcompute(test,*this))) return KIM_STATUS_FAIL;
        for (int i=0;i<this->nAtomsTypes;++i) {
           this->AtomsTypes[i].requestedByTest = mdl.AtomsTypes[i].requestedByTest;
        }

        this->modelVersionMajor = mdl.tempVersionMajor;
        this->modelVersionMinor = mdl.tempVersionMinor;
        this->simulatorVersionMajor = test.tempVersionMajor;
        this->simulatorVersionMinor = test.tempVersionMinor;

        test.free(); mdl.free();
        char computestr [] = "compute";
        compute_index = get_index(computestr, &error);
        get_neigh_index = get_index((char*) "get_neigh", &error);
    }else{
       mdl.free();
       std::cout<<"Do not match  " << modelname << " and "<< test.model.name <<std::endl;
       test.free();
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();
       return KIM_STATUS_FAIL;
    }

    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    return KIM_STATUS_OK;
}

int KIM_API_model::match(const char* teststr, const char* modelstr){
   int error;
    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log");
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    //check test-model match and preinit test-model-API
    KIM_API_model test,mdl;

    //preinit test and model API object
    test.name_temp = (char*) "simulator_name";
    error = test.prestring_init(teststr);
    if(error != KIM_STATUS_OK)
    {
       test.free();
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();

       return KIM_STATUS_FAIL;
    }

    mdl.name_temp = (char*) "model_name";
    error = mdl.prestring_init(modelstr);
    if(error != KIM_STATUS_OK)
    {
       test.free();
       mdl.free();
       //redirecting back to > std::cout
       std::cout.rdbuf(backup); filekimlog.close();

       return KIM_STATUS_FAIL;
    }

    //check if they match
    bool match;
    match = is_it_match(test,mdl);
    mdl.free();
    test.free();

    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();

    if (match)
    {
       error = KIM_STATUS_OK;
    }
    else
    {
       error = KIM_STATUS_FAIL;
    }

    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    return error;
}


int KIM_API_model::model_reinit(){
   int error;
   int reinit_ind = get_index((char*) "reinit", &error);
   if (error != KIM_STATUS_OK) return error;

   typedef int (*Model_Reinit_cpp)(KIM::Model * const);//prototype for model_reinit for c++
   typedef int (*Model_Reinit_c)(KIM_Model * const);//prototype for model_reinit for c
   KIM::Model MI;
   MI.pimpl = (KIM::Model::ModelImplementation *) this;
   KIM_Model cMI;
   cMI.p = (void *) &MI;
   Model_Reinit_cpp mdl_reinit_cpp = (Model_Reinit_cpp)(*this)[reinit_ind].data.fp;
   Model_Reinit_c mdl_reinit_c = (Model_Reinit_c)(*this)[reinit_ind].data.fp;
   if (mdl_reinit_cpp == NULL) return KIM_STATUS_FAIL;
   if ((*this)[reinit_ind].lang)
     return (*mdl_reinit_cpp)(&MI);
   else
     return (*mdl_reinit_c)(&cMI);
}

#if KIM_LINK_VALUE != KIM_LINK_DYNAMIC_LOAD
extern "C" {
#include "model_init_include.h"
}
int KIM_API_model::model_init(){
    std::string modelname(this->model.name);
    KIM_API_model * kim;
    void ** pkim;
    kim=this;
    pkim =(void**) &kim;

    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log",std::ofstream::app);
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

std::cout<< "* Info: (KIM_API_model::model_init): call statically linked initialize routine for::"<<modelname<<std::endl;
    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();

#include "model_init_include.cpp"

    //redirecting std::cout > kimlog
    filekimlog.open(kimlog,std::ofstream::app);
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    std::cout<< "* Info: (KIM_API_model::model_init): model initiliser failed for ";
    std::cout<<modelname<<std::endl;

     //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();

    return KIM_STATUS_FAIL;
}
#else
int KIM_API_model::model_init(){
    std::string modelname(this->model.name);

    //redirecting std::cout > kimlog
    std::streambuf * psbuf, * backup; std::ofstream filekimlog;
    filekimlog.open("./kim.log", std::ofstream::app);
    backup = std::cout.rdbuf();psbuf = filekimlog.rdbuf();std::cout.rdbuf(psbuf);

    std::vector<std::string> item;
    bool accessible = findItem(KIM_MODELS_DIR, modelname, &item);
    if (accessible)
    {
      std::string libFileName
          = item[1] + "/" + item[0] + "/" + MODELLIBFILE + ".so";
      model_lib_handle = dlopen(libFileName.c_str(), RTLD_NOW);
    }
    if(!accessible)
    {
       //redirecting back to > std::cout
      std::cout<< "* Error (KIM_API_model::model_init): The Model shared library file is not readable for Model name: '" << modelname << "'" <<std::endl;
      std::cout<<dlerror()<<std::endl;
      std::cout.rdbuf(backup); filekimlog.close();
      fprintf(stderr,"The Model shared library file is not readable for Model name: '%s'.\n",modelname.c_str());
      return KIM_STATUS_FAIL;
    }
    else if(NULL == model_lib_handle) {
       //redirecting back to > std::cout
      std::cout<< "* Error (KIM_API_model::model_init): A problem occurred with the Model shared library file for Model name: '" << modelname << "'" <<std::endl;
      std::cout<<dlerror()<<std::endl;
      std::cout.rdbuf(backup); filekimlog.close();
      fprintf(stderr,"A problem occurred with the Model shared library file for Model name: '%s'.\n",modelname.c_str());
      return KIM_STATUS_FAIL;
    }

    std::cout<<"* Info: (KIM_API_model::model_init): call dynamically linked initialize routine for:"<<modelname<<std::endl;
    std::cout<<"               from the shared library in:"<< item[1] << "/" << item[0] <<std::endl;

    std::stringstream model_lang_name;
    model_lang_name << modelname << "_language";
    model_language =  *(int*) dlsym(model_lib_handle,model_lang_name.str().c_str());
    typedef int (*Model_Init_cpp)(KIM::Model * const);//prototype for model_init for c++
    typedef int (*Model_Init_c)(KIM_Model * const);//prototype for model_init for c
    typedef void (*Model_Init_Fortran)(KIM_Model * const, int * const ierr);//prototype for model_init for Fortran
    std::stringstream model_init_routine_name;
    model_init_routine_name << modelname << "_init_pointer";
    Model_Init_cpp mdl_init_cpp = 0;
    Model_Init_c mdl_init_c = 0;
    Model_Init_Fortran mdl_init_fortran = 0;

    if (model_language == 1) // C++
    {
      mdl_init_cpp = *((Model_Init_cpp*)dlsym(model_lib_handle,model_init_routine_name.str().c_str()));
    }
    else if (model_language == 2) // C
    {
      mdl_init_c = *((Model_Init_c*)dlsym(model_lib_handle,model_init_routine_name.str().c_str()));
    }
    else // model_language == 3 // Fortran
    {
      mdl_init_fortran = *((Model_Init_Fortran*)dlsym(model_lib_handle,model_init_routine_name.str().c_str()));
    }
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        std::cout << "* Error (KIM_API_model::model_init): Cannot load symbol: " << dlsym_error <<std::endl;
        dlclose(model_lib_handle);

        //redirecting back to > std::cout
        std::cout.rdbuf(backup); filekimlog.close();

        return KIM_STATUS_FAIL;
    }

    //redirecting back to > std::cout
    std::cout.rdbuf(backup); filekimlog.close();
    int err;
    KIM::Model MI;
    MI.pimpl = (KIM::Model::ModelImplementation *) this;
    if (model_language == 1)
    {
      err = (*mdl_init_cpp)(&MI);
    }
    else if (model_language == 2)
    {
      KIM_Model cMI;
      cMI.p = (void *) &MI;
      err = (*mdl_init_c)(&cMI);
    }
    else // model_language == 3 // Fortran
    {
      KIM_Model cMI;
      cMI.p = (void *) &MI;
      (*mdl_init_fortran)(&cMI, &err);
    }
    return err;
}
#endif

int KIM_API_model::model_destroy(){
  typedef int (*Model_Destroy_cpp)(KIM::Model * const);//prototype for model_destroy for c++
  typedef int (*Model_Destroy_c)(KIM_Model * const);//prototype for model_destroy for c
  typedef void (*Model_Destroy_Fortran)(KIM_Model * const, int * const ierr);//prototype for model_destroy for Fortran
  Model_Destroy_cpp mdl_destroy_cpp = (Model_Destroy_cpp) (*this)[(char*) "destroy"].data.fp;
  Model_Destroy_c mdl_destroy_c = (Model_Destroy_c) (*this)[(char*) "destroy"].data.fp;
  Model_Destroy_Fortran mdl_destroy_fortran = (Model_Destroy_Fortran) (*this)[(char*) "destroy"].data.fp;
  //call model_destroy
  KIM::Model MI;
  MI.pimpl = (KIM::Model::ModelImplementation *) this;
  KIM_Model cMI;
  cMI.p = (void *) &MI;

  int error = false;
  if (mdl_destroy_cpp != NULL) {
    if ((*this)[(char*) "destroy"].lang == 1)
      error = (*mdl_destroy_cpp)(&MI);
    else if ((*this)[(char*) "destroy"].lang == 2)
      error = (*mdl_destroy_c)(&cMI);
    else // Fortran
      (*mdl_destroy_fortran)(&cMI, &error);
  }

#if KIM_LINK_VALUE == KIM_LINK_DYNAMIC_LOAD
  dlclose(model_lib_handle);
#endif
  return error;
}
int KIM_API_model::model_compute(){
  // set model_compute pointer
  typedef int (*Model_Compute_cpp)(KIM::Model const * const);//prototype for model_compute for c++
  typedef int (*Model_Compute_c)(KIM_Model const * const);//prototype for model_compute for c
  typedef void (*Model_Compute_Fortran)(KIM_Model const * const, int * const ierr);//prototype for model_compute for fortran
  int error = KIM_STATUS_FAIL;
  Model_Compute_cpp mdl_compute_cpp = (Model_Compute_cpp) (*this)[compute_index].data.fp;
  Model_Compute_c mdl_compute_c = (Model_Compute_c) (*this)[compute_index].data.fp;
  Model_Compute_Fortran mdl_compute_fortran = (Model_Compute_Fortran) (*this)[compute_index].data.fp;
  KIM::Model MI;
  MI.pimpl = (KIM::Model::ModelImplementation *) this;
  if ((*this)[compute_index].lang == 1)
  {
    if (mdl_compute_cpp == NULL) return error;
    //call model_compute
    error = (*mdl_compute_cpp)(&MI);
  }
  else if ((*this)[compute_index].lang == 2)
  {
    if (mdl_compute_c == NULL) return error;
    //call model_compute
    KIM_Model cMI;
    cMI.p = (void *) &MI;
    error = (*mdl_compute_c)(&cMI);
  }
  else // Fortran
  {
    if (mdl_compute_fortran == NULL) return error;
    //call model_compute
    KIM_Model cMI;
    cMI.p = (void *) &MI;
    (*mdl_compute_fortran)(&cMI, &error);
  }
  return error;
}

int KIM_API_model::get_neigh(int neighborListIndex, int request, int *numnei, int** nei1part){
  typedef int (*Get_Neigh_cpp)(KIM::Model const * const, int, int, int *, int **);
  typedef int (*Get_Neigh_c)(KIM_Model const * const, int, int, int *, int **);
  typedef void (*Get_Neigh_Fortran)(KIM_Model const * const, int, int, int *, int **, int *);

  if (get_neigh_index < 0) return KIM_STATUS_API_OBJECT_INVALID;
    Get_Neigh_cpp get_neigh_cpp = (Get_Neigh_cpp)(*this)[get_neigh_index].data.fp;
    Get_Neigh_c get_neigh_c = (Get_Neigh_c)(*this)[get_neigh_index].data.fp;
    Get_Neigh_Fortran get_neigh_fortran = (Get_Neigh_Fortran)(*this)[get_neigh_index].data.fp;
    KIM::Model MI;
    MI.pimpl = (KIM::Model::ModelImplementation *) this;
    KIM_Model cMI;
    cMI.p = (void *) &MI;

    if (model_index_shift==0) {
      int erkey;
      if ((*this)[get_neigh_index].lang == 1)
      {
        erkey = (*get_neigh_cpp)(&MI, neighborListIndex, request, numnei, nei1part);
      }
      else if ((*this)[get_neigh_index].lang == 2)
      {
        erkey = (*get_neigh_c)(&cMI, neighborListIndex, request, numnei, nei1part);
      }
      else // Fortran
      {
        (*get_neigh_fortran)(&cMI, neighborListIndex+1, request, numnei, nei1part, &erkey);
      }
      return erkey;
    }
    else if (model_index_shift == 1 || model_index_shift == -1){

        int req=request - model_index_shift;

        int erkey;
        if ((*this)[get_neigh_index].lang == 1)
        {
          erkey = (*get_neigh_cpp)(&MI, neighborListIndex, req, numnei, nei1part);
        }
        else if ((*this)[get_neigh_index].lang == 2)
        {
          erkey = (*get_neigh_c)(&cMI, neighborListIndex, req, numnei, nei1part);
        }
        else // Fortran
        {
          (*get_neigh_fortran)(&cMI, neighborListIndex+1, req, numnei, nei1part, &erkey);
        }
            if (!erkey){
                if (neiOfAnAtomSize < *numnei) {
                   delete [] neiOfAnAtom;
                   neiOfAnAtom = new int[*numnei];
                   if (neiOfAnAtom == NULL) {
                      neiOfAnAtomSize = 0;
                      std::cout << std::endl << "* Error (KIM_API_model::get_neigh): numnei too big to allocate memory for index conversion: " << *numnei << std::endl;
                      return KIM_STATUS_NEIGH_TOO_MANY_NEIGHBORS;
                   }
                   neiOfAnAtomSize = *numnei;
                }
                for (int i = 0; i<(*numnei);i++){
                   neiOfAnAtom[i] = (*nei1part)[i] + model_index_shift;
                }
                *nei1part = &(neiOfAnAtom[0]);
            }
            return erkey;
    }else{
        std::cout<<std::endl<< "* Error (KIM_API_model::get_neigh): wrong base convert key,model_index_shift =";
        std::cout<< model_index_shift <<"  (must be 0,1 or -1)"<<std::endl;
        return KIM_STATUS_API_OBJECT_INVALID;
    }
}


bool KIM_API_model::irrelevantVars2donotcompute(KIM_API_model & test, KIM_API_model & mdl){
   if(! is_it_match_noFlagCount(test,mdl.inlines,mdl.numlines,false)) {
        std::cout<<"* Error (KIM_API_model::irrelevantVars2donotcompute): Simulator and Model descriptor files are incompatible (do not match)."<<std::endl;
        return false;
    }
    for(int i=0; i<mdl.numlines;i++){
        if(mdl.inlines[i].isitoptional()) {
            mdl[i].flag->calculate = 0;
            for (int j=0;j<test.model.size;j++){
               if(test[j].equiv(mdl.inlines[i])) mdl[i].flag->calculate = 1;
            }
        }
    }

    return true;
}

bool KIM_API_model::init_AtomsTypes(){
    nAtomsTypes=0;
    for(int i=0;i < numlines;i++){
        if (inlines[i].type == "spec") nAtomsTypes++;
    }
    if (nAtomsTypes==0) return false;

    AtomsTypes = new Atom_Map[nAtomsTypes];
    int ii=0;
    for(int i=0;i < numlines;i++){
        if (inlines[i].type == "spec"){
            AtomsTypes[ii].symbol = inlines[i].name;
            AtomsTypes[ii].code = atoi(inlines[i].dim.c_str());
            ii++;
        }
    }
    qsort((void *) AtomsTypes,(size_t) nAtomsTypes,sizeof(Atom_Map),&(Atom_Map::comparator));
    ErrorCode=1;
    return true;
}

int KIM_API_model::get_num_model_species(int* numberSpecies,
                                         int* maxStringLength)
{
  *numberSpecies = nAtomsTypes;
  *maxStringLength = 0;
  for (int i=0; i<nAtomsTypes; ++i)
  {
    int len = AtomsTypes[i].symbol.length();
    if (len > *maxStringLength)
    {
      *maxStringLength = len;
    }
  }

  return KIM_STATUS_OK;
}
int KIM_API_model::get_model_species(const int index,
                                     const char** const speciesString)
{
  if (0 == nAtomsTypes) return KIM_STATUS_FAIL;
  if ((index < 0) || (index>nAtomsTypes))
  {
    return KIM_STATUS_FAIL;
  }

  *speciesString = AtomsTypes[index].symbol.c_str();
  return KIM_STATUS_OK;
}

int KIM_API_model::get_num_sim_species(int* numberSpecies,
                                       int* maxStringLength)
{
  *numberSpecies = 0;
  *maxStringLength = 0;
  for (int i=0; i<nAtomsTypes; ++i)
  {
    if (AtomsTypes[i].requestedByTest)
    {
      ++(*numberSpecies);
      int len = AtomsTypes[i].symbol.length();
      if (len > *maxStringLength)
      {
        *maxStringLength = len;
      }
    }
  }

  return KIM_STATUS_OK;
}

int KIM_API_model::get_sim_species(const int index,
                                   const char** const speciesString)
{
  int count = 0;
  for (int i=0; i<nAtomsTypes; ++i)
  {
    if (AtomsTypes[i].requestedByTest) ++count;
  }
  if ((index < 0) || (index>count))
  {
    return KIM_STATUS_FAIL;
  }

  int correspondingIndex = -1;
  for (int i=0; i<nAtomsTypes; ++i)
  {
    if (AtomsTypes[i].requestedByTest) ++correspondingIndex;
    if (correspondingIndex == index)
    {
      *speciesString = AtomsTypes[i].symbol.c_str();
      return KIM_STATUS_OK;
    }
  }

  // should never get here
  return KIM_STATUS_FAIL;
}

int KIM_API_model::get_num_params(int* numberParameters, int* maxStringLength)
{
  *numberParameters = 0;
  *maxStringLength = 0;
  for (int i=0; i<model.size; ++i)
  {
    if (is_it_par((*this)[i].name))
    {
      ++(*numberParameters);
      int len = (*this)[i].name.length();
      if (len > *maxStringLength)
      {
        *maxStringLength = len;
      }
    }
  }

  return KIM_STATUS_OK;
}

int KIM_API_model::get_parameter(const int index, const char** const parameterString)
{
  int correspondingIndex = -1;
  for (int i=0; i<model.size; ++i)
  {
    if (is_it_par((*this)[i].name)) ++correspondingIndex;
    if (correspondingIndex == index)
    {
      *parameterString = (*this)[i].name.c_str();
      return KIM_STATUS_OK;
    }
  }

  // In case the requested index is not valid
  return KIM_STATUS_FAIL;
}

int KIM_API_model::get_species_code(const char* species, int * error){
    *error =KIM_STATUS_FAIL;
    if (species == NULL)  {
      *error = KIM_STATUS_FAIL;
      return *error; //no atom symbol provided
    }
    Atom_Map key, *res=NULL;
    key.symbol = species;
    res = (Atom_Map *)bsearch((void *)&key,AtomsTypes,nAtomsTypes,sizeof(Atom_Map),&(Atom_Map::comparator));
    if (res == NULL) {
        *error = KIM_STATUS_PARTICLE_INVALID_SPECIES;
        return  KIM_STATUS_PARTICLE_INVALID_SPECIES; //did not find atom symbol among atom species
    }
    *error=KIM_STATUS_OK;
    return res->code;
}

void KIM_API_model::set_species_code(const char* species, int code, int* error){
   *error = KIM_STATUS_FAIL;
    if (species == NULL)  {
      *error = KIM_STATUS_FAIL;
      return; //no atom symbol provided
    }
    Atom_Map key, *res=NULL;
    key.symbol = species;
    res = (Atom_Map *)bsearch((void *)&key,AtomsTypes,nAtomsTypes,sizeof(Atom_Map),&(Atom_Map::comparator));
    if (res == NULL) {
        *error = KIM_STATUS_PARTICLE_INVALID_SPECIES;
        return; //did not find atom symbol among atom species
    }
    if (res->readOnly) {
       *error = KIM_STATUS_FAIL;
       return;
    }

    res->code = code;

    *error=KIM_STATUS_OK;
    return;
}

bool KIM_API_model::check_required_arguments(){
   int error;
    //will check if all required arguments are in the object
    for (int j=0;j<NUMBER_REQUIRED_ARGUMENTS; j++){
       if (get_index(required_arguments[j], &error) == -1){
            std::cout<<"* Error (KIM_API_model::check_required_arguments): Argument "<< required_arguments[j];
            std::cout<<" is not in KIM API object."<<std::endl;
            return false;
        }
    }
    return true;
}
int KIM_API_model::get_status_msg(const int status_code,
                                  const char** const status_msg)
{
    int mincode=-19,maxcode=1,offset=19;

    static const char* KIM_STATUS_MSG[]=
        {"configuration is not supported by the Model",
         "base units: are not supported or not the same phys.dimensions",
         "unsupported Unit_time",
         "unsupported Unit_temperature",
         "unsupported Unit_charge",
         "unsupported Unit_energy",
         "unsupported Unit_length",
         "Unit_Handling must be \"flexible\" or \"fixed\" ",

         "group argument must be 1 or 0 (in KIM_API...multiple routine)",
         "numargs is not divisiable by 4(in KIM_API...multiple routine)",
         "wrong optional arguments (in a kim_api_...multiple routine)",
         "numargs is not divisible by 2 (in KIM_API...multiple routine)",
         "numargs is not divisiable by 3 (in KIM_API...multiple routine)",
         "get_neigh method in KIM API object is not set (NULL value)",
         "number of neighs of particle too big to allocate for conversion",
         "invalid KIM API object",
         "invalid particle id requested (request out of range)",
         "symbol is not among supported particle symbols",
         "argument name provided is not in KIM API object",
         "unsuccessful completion",
         "successful completion"};

    if (status_code < mincode || status_code > maxcode) {
      *status_msg = "the error code is not among KIM_STATUS codes";
      return KIM_STATUS_FAIL;
    }else{
        int ind = offset + status_code;
        *status_msg = KIM_STATUS_MSG[ind];
        return KIM_STATUS_OK;
    }
}

int KIM_API_model::report_error(int ln,const char * fl,const char * usermsg,int ier){
    if(ier){
        std::cout<<"* Error: at line "<<ln<<" in "<<fl<< std::endl<<"\tMessage: "<<usermsg<<std::endl;
        return KIM_STATUS_FAIL;
    }
    return KIM_STATUS_OK;
}

void KIM_API_model::set_model_buffer(void* o, int* ier){
    *ier = KIM_STATUS_OK;
    model_buffer = o;
}
void * KIM_API_model::get_model_buffer(int* ier){
    *ier = KIM_STATUS_FAIL;
    if (model_buffer == NULL) return NULL;
    *ier = KIM_STATUS_OK;
    return model_buffer;
}
void KIM_API_model::set_sim_buffer(void* o, int* ier){
    *ier = KIM_STATUS_OK;
    test_buffer = o;
}

void * KIM_API_model::get_sim_buffer(int* ier){
    *ier = KIM_STATUS_FAIL;
    if (test_buffer == NULL) return NULL;
    *ier = KIM_STATUS_OK;
    return test_buffer;
}

int KIM_API_model::process_dEdr(KIM_API_model** ppkim, double* dE, double* r,
        double** dx,int *i, int *j){
   int ier = KIM_STATUS_OK;;
    KIM_API_model * pkim= *ppkim;
    typedef int (*Process_d1Edr)(KIM_API_model **, double *, double *, double **,int *,int *);

    Process_d1Edr process = (Process_d1Edr) (*pkim)["process_dEdr"].data.fp;
    int process_flag =0;
    process_flag = (*pkim)["process_dEdr"].flag->calculate;

    if (process != NULL && process_flag == 1 && pkim->model_index_shift == 0) {
        ier = (*process)(ppkim,dE,r,dx,i,j);
    }else{
        int i2send = *i-pkim->model_index_shift;
        int j2send = *j-pkim->model_index_shift;
        ier = (*process)(ppkim,dE,r,dx,&i2send,&j2send);
    }

    return ier;
}

int KIM_API_model::process_d2Edr2(KIM_API_model **ppkim,double *de,double **r,double ** pdx,int **i,int **j){
   int ier = KIM_STATUS_OK;
    KIM_API_model * pkim= *ppkim;
    typedef int (*Process_d2Edr)(KIM_API_model **, double *, double **, double **,int **,int **);

    Process_d2Edr process = (Process_d2Edr) (*pkim)["process_d2Edr2"].data.fp;
    int process_flag =0;
    process_flag = (*pkim)["process_d2Edr2"].flag->calculate;

    if (process != NULL && process_flag == 1 && pkim->model_index_shift == 0) {
       ier = (*process)(ppkim,de,r,pdx,i,j);
    }else{
        int k=pkim->model_index_shift;
        int i2send[2];   i2send[0]=(*i)[0]-k; i2send[1]=(*i)[1]-k;
        int j2send[2];   j2send[0]=(*j)[0]-k; j2send[1]=(*j)[1]-k;
        int *pi = &i2send[0];
        int *pj = &j2send[0];
        ier = (*process)(ppkim,de,r,pdx,&pi,&pj);
    }

    return ier;
}


//related to Unit_Handling
double KIM_API_model::get_scale_conversion( const char* u_from,const char * u_to, int *error){
    return Unit_Handling::get_scale_conversion(u_from,u_to,error);
}
int KIM_API_model::get_unit_handling(int *error){
    return unit_h.get_unit_handling(error);
}
char const * const KIM_API_model::get_unit_length(int *error){
  return unit_h.get_unit_length(error).c_str();
}
char const * const KIM_API_model::get_unit_energy(int *error){
  return unit_h.get_unit_energy(error).c_str();
}
char const * const KIM_API_model::get_unit_charge(int *error){
  return unit_h.get_unit_charge(error).c_str();
}
char const * const KIM_API_model::get_unit_temperature(int *error){
  return unit_h.get_unit_temperature(error).c_str();
}
char const * const KIM_API_model::get_unit_time(int *error){
  return unit_h.get_unit_time(error).c_str();
}

double KIM_API_model::convert_to_act_unit(
   const char * length, const char* energy, const char* charge,
   const char * temperature, const char * time,
   double length_exponent, double energy_exponent, double charge_exponent,
   double temperature_exponent, double time_exponent, int* kimerror){
   return Unit_Handling::convert_to_act_unit((void *)this, length, energy, charge, temperature, time,
   length_exponent, energy_exponent, charge_exponent,  temperature_exponent, time_exponent, kimerror);
}

//multiple data set/get methods
//
void KIM_API_model::setm_data(int *err, int numargs, ... ){
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"setm_data: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        void *dt      = va_arg(listPointer, void *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_data: WARNING: for "<<nm<<" data is NULL\n";
        if(!this->set_data(nm,size,dt)){
            std::cout<<"setm_data: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);

}
void KIM_API_model::setm_method(int *err, int numargs, ... ){
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 5 != 0) {
        std::cout<<"setm_method: numargs must be multiple of 5"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/5; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        int lang = va_arg(listPointer, int);
        func_ptr dt      = va_arg(listPointer, func_ptr);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_method: WARNING: for "<<nm<<" data is NULL\n";
        if(!this->set_method(nm,size,lang,dt)){
            std::cout<<"setm_method: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::getm_data(int *err,int numargs, ...){

    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_data: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        void **dt      = va_arg(listPointer, void **);
        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = this->get_data(nm,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_data: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_model::getm_method(int *err,int numargs, ...){

    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"getm_data: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int * lang = va_arg(listPointer, int *);
        func_ptr *dt      = va_arg(listPointer, func_ptr *);
        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = this->get_method(nm,lang,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_data: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::setm_compute(int *err, int numargs, ...){
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"setm_compute: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int compute_flag = va_arg(listPointer, int);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        int index = this->get_index(nm,err);
        if (*err != KIM_STATUS_OK){
           std::cout<<"setm_compute:  name "<<nm<<" not in KIM\n";
           va_end(listPointer);
           return;
        }
        if (compute_flag ==1){
            (*this)[index].flag->calculate = 1;
        }else if (compute_flag ==0){
            (*this)[index].flag->calculate = 0;
        }else{
            std::cout<<"setm_compute:  for "<<nm<<" failed: compute_flag must be 0 or 1\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::set_compute(const char *nm, int flag, int *error){
   *error = KIM_STATUS_FAIL;
   int ind = get_index(nm, error);
   *error = KIM_STATUS_FAIL;
   if ((flag == 1) || (flag == 0)){
      (*this)[ind].flag->calculate = flag;
      *error = KIM_STATUS_OK;
   }
   else
      *error = KIM_STATUS_FAIL;

   return;
}

void KIM_API_model::getm_compute(int *err,int numargs, ...){
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_compute: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *compute_flag = va_arg(listPointer, int*);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        int index = this->get_index(nm,err);
        if (*err != KIM_STATUS_OK){
           std::cout<<"getm_compute:  name "<<nm<<" not in KIM\n";
           va_end(listPointer);
           return;
        }
        *compute_flag =(*this)[index].flag->calculate;
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::print(int* error){
    *error =KIM_STATUS_FAIL;
    std::cout<<(*this);
    *error=KIM_STATUS_OK;
}

intptr_t KIM_API_model::get_size(const char *nm,int *error){
        int I=get_index(nm,error);
    *error =KIM_STATUS_FAIL;
    *error =KIM_STATUS_OK;
    return (*this)[I].size;
}

int KIM_API_model::get_compute(const char *nm, int* error){
   int I = get_index(nm, error);
   if (*error != KIM_STATUS_OK) return KIM_STATUS_ARG_UNKNOWN;
    *error = KIM_STATUS_FAIL;
    if ((I < 0) || (I >= model.size)) return KIM_STATUS_ARG_UNKNOWN;
    *error = KIM_STATUS_OK;
    return (*this)[I].flag->calculate;
 }

} // namespace OLD_KIM

std::ostream &operator<<(std::ostream &stream, OLD_KIM::KIM_IOline a){
        stream<<a.name<<" "<<a.type<<" "<<a.dim<<" ";
        stream<<a.requirements;
        stream << std::endl;
        return stream;
}
std::istream &operator>>(std::istream &stream, OLD_KIM::KIM_IOline &a){
        std::string inputline;
        std::getline(stream, inputline);
        if(stream.fail() && !stream.eof()){
          std::cout << "* Error (operator>> KIM_IOline): unable to read .kim file line for some reason.\n";
           a.goodformat=false;
        }else{
          //@@@@@
          char * const tmp = new char[inputline.length()+1];
          strcpy(tmp, inputline.c_str());
           if(a.getFields(tmp)){
              a.goodformat=true;
           }else{
              a.goodformat=false;
           }
           delete [] tmp;
        }
        return stream;
}

std::ostream &operator<<(std::ostream &stream, OLD_KIM::IOline a){
        stream<<a.name<<" := "<<a.value<<" # "<<a.comment;
        return stream;
}
std::istream &operator>>(std::istream &stream, OLD_KIM::IOline &a){
        std::string inputline;
        std::getline(stream, inputline);
        char * const tmp = new char[inputline.length()+1];
        strcpy(tmp, inputline.c_str());
        if(a.getFields(tmp)){
                a.goodformat=true;
        }else{
                a.goodformat=false;
        }
        delete [] tmp;
        return stream;
}

std::ostream &operator<<(std::ostream &stream, OLD_KIM::KIMBaseElement a){
    if (a.data.p==NULL && a.name == "" && a.type== "") {
        stream <<" KIMBaseElement is nullified "<<std::endl;
        return stream;
    }
    stream<<"name  : "<<a.name<< std::endl
          <<" type          : "<<a.type << std::endl;
    stream<<" size          : "<<a.size<<std::endl;
    stream<<" flag calculate: "<<a.flag->calculate<<"   // 0 -- do not calculate, 1 -- calculate"<<std::endl;
    stream<<" dimension     : "<<a.unit->dim<<std::endl;
    // printin gata itself


    stream<<" data: ";


    if(a.data.p == NULL) {
        stream <<"NULL"<<std::endl;
        return stream;
    }else if(a.type == "double"){

        for(int i=0;i<a.size;i++) stream<< ((double*)(a.data.p))[i]<<" ";

    }else if(a.type == "real"){
        for(int i=0;i<a.size;i++) stream<< ((float*)(a.data.p))[i]<<" ";
    }else if(a.type == "integer"){
        for(int i=0;i<a.size;i++) stream<< ((int*)(a.data.p))[i]<<" ";
    }else if(a.type == "method"){
      stream<<"address:"<<(func *)a.data.fp << ", language:" << a.lang;
    }else{
        stream<<"address:"<<(intptr_t)a.data.p;
    }
    stream<<std::endl;

    return stream;
}

std::ostream &operator<<(std::ostream &stream, OLD_KIM::KIM_API_model &a){
    stream<<"*************************************"<<std::endl;
    stream<<"KIM API Object details:" << std::endl << std::endl;
    stream << a.model;
    stream<<"Active Units" << std::endl;
    stream<< a.unit_h << std::endl;
    stream<<"List of items in KIM API Ojbect" << std::endl;
    stream<<"-------------------------------------"<<std::endl;
    OLD_KIM::KIMBaseElement **pel =  (OLD_KIM::KIMBaseElement **)  a.model.data.p;
    for(int i=0;i<a.model.size;i++)
    {
       stream << "index : " << i << std::endl
              << *(pel[i]) << std::endl;
    }
    stream<<"*************************************"<<std::endl;

    return stream;
}
