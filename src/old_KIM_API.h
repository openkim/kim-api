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
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIMHDR_OLD_KIM_API_H
#define KIMHDR_OLD_KIM_API_H

#include <iostream>
#include <map>
#include <stdint.h>
#include <stdarg.h>

#define NUMBER_REQUIRED_ARGUMENTS 10

#include "old_KIM_API_Version.h"
#include "old_Unit_Handling.h"

#include "KIM_COMPUTE_SimulatorComputeArguments.hpp"

namespace KIM
{
class Model;
}

namespace OLD_KIM
{

// A macro to disallow the copy constructor and operator= functions.
// This should be used in the private: declarations for a class
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

class KIMBaseElementFlag{
public:
        int calculate; // 0 -- do not calculate, 1 -- calculate
        KIMBaseElementFlag();
};


class KIMBaseElementUnit{
public:
      std::string  dim;  // dimension (length, energy, time, mass, or derivatives...
      KIMBaseElementUnit();
      void init();
};
class Atom_Map{
public:
    Atom_Map(): requestedByTest(false) {}
    std::string symbol;
    bool readOnly;
    bool requestedByTest;
    int code;
    static int comparator(const void * a1,const void *a2);
};
class KIM_IOline{
public:
        std::string name;
        std::string type;
        std::string dim;
        std::string requirements;
        std::string comments;
        bool goodformat,input,output;

        KIM_IOline();

        bool getFields(char * const inString);
        bool isitoptional();

 private:
        void strip();
        void init2empty();
        bool isitinput(const char*str);
        bool isitoutput(const char*str);
};//main input line handler

class IOline{
public:
        std::string name;
        std::string value;
        std::string comment;
        bool goodformat;
        void strip();

        IOline();
        bool getFields(char * const inputString);
        static int readlines_str(const char * inputstr, IOline ** lines, bool& success);
}; //secondary input line handler //cout<<"SystemOfUnit:  file:"<<infile<<":"<<endl;

class KIMBaseElement{
public:
   union {
      void *p;
      void (* fp)();
   } data;
  int lang;
        intptr_t size; //Size in words defined by type
        std::string name;
        std::string type;
        KIMBaseElementUnit * unit;
        KIMBaseElementFlag *flag;
        KIMBaseElement();
        ~KIMBaseElement();
        bool init(std::string const & nm,std::string const & tp,
                  intptr_t sz,void * pdata);
        void free();
        void nullify();
        bool equiv(KIM_IOline& kimioline);

static  int getelemsize(const char *tp, bool& success);
};

typedef void (*func_ptr)();
class KIM_API_model{
public:
    KIMBaseElement model;
    KIM_API_model();
    ~KIM_API_model();

  static int get_version(const char** const version);
  static int get_version_major(int* const major);
  static int get_version_minor(int* const minor);
  static int get_version_patch(int* const patch);
  static int get_version_prerelease(const char** const prerelease);
  static int get_version_build_metadata(const char** const build_metadata);
  static int version_newer(const char* const versionA,
                           const char* const versionB,
                           int* const result);

  int get_version_model_major(int* const major) const;
  int get_version_model_minor(int* const minor) const;
  int get_version_simulator_major(int* const major) const;
  int get_version_simulator_minor(int* const minor) const;

    void free(int *error);
    int set_data(const char * nm, intptr_t size, void *dt);
    int set_method(const char * nm, intptr_t size, int const language, func_ptr dt);

    void * get_data(const char *nm,int *error);
    func_ptr get_method(const char *nm, int * const language, int *error);

    intptr_t get_size(const char *nm,int *error);
    void set_compute(const char *nm, int flag, int *error);
    int get_compute(const char *nm, int* error);
    void print(int *error);

    int string_init(const char * intststr,const char * modelname);
   int match(const char* simstring, const char* modelstring);
  int model_compute(KIM::COMPUTE::SimulatorComputeArguments const * const arguments);
  int get_neigh(KIM::COMPUTE::SimulatorComputeArguments const * const arguments, int neighborListIndex, int request, int *numnei, int **nei1part);
    int model_init();
    int model_reinit();
    int model_destroy();

  int get_num_model_species(int* numberSpecies, int* maxStringLength);
  int get_model_species(const int index, const char** const speciesString);
  int get_num_sim_species(int* numberSpecies, int* maxStringLength);
  int get_sim_species(const int index, const char** const speciesString);

int get_species_code(const char *species, int * error);
void set_species_code(const char *species, int code, int* error);

  int get_num_params(int* numberParameters, int* maxStringLength);
  int get_parameter(const int index, const char** const parameterString);

  static int get_model_kim_str_len(const char* const modelname, int* const kimStirngLen);
  static int get_model_kim_str(const char* const modelname, const char** const kimString);


  static int get_status_msg(const int status_code, const char** const status_msg);
    static int report_error(int line, const char * fl, const char * usermsg, int error);
    void set_model_buffer(void * o,int *error);
    void * get_model_buffer(int *error);
    void set_sim_buffer(void * o,int *error);
    void * get_sim_buffer(int *error);

   static int process_dEdr(KIM_API_model **ppkim,double *dr,double *r,double ** dx, int *i,int *j);

   static int process_d2Edr2(KIM_API_model **ppkim,double *de,double **rr,double ** pdx,int **ii,int **jj);

  static int get_model_kim_str(const char* const modelname, char** const kimString);

   //multiple data set/get methods
   //
  void setm_data(int *err, int numargs, ... );      //++
  void setm_method(int *err, int numargs, ... );      //++
  void getm_data(int *err,int numargs, ...);        //++
  void getm_method(int *err,int numargs, ...);        //++
  void setm_compute(int *err, int numargs, ...);    //++
  void getm_compute(int *err,int numargs, ...);  //  ++

    //Unit_Handling object
    Unit_Handling unit_h;

    //Unit_Handling related routines

    static double get_scale_conversion(const char * u_from,const char * u_to, int *error);
    int get_unit_handling(int *error);
    char const * const get_unit_length(int *error);
    char const * const get_unit_energy(int *error);
    char const * const get_unit_charge(int *error);
    char const * const get_unit_temperature(int *error);
    char const * const get_unit_time(int *error);
    double convert_to_act_unit( const char * length, const char * energy,
      const char * charge,const char * temperature, const char * time,
      double length_exponent, double energy_exponent, double charge_exponent,
      double temperature_exponent, double time_exponent, int* kimerror);


  double * influenceDistance;
  // cutoffs
  int numberOfCutoffs;
  double * cutoffs;

private:
  DISALLOW_COPY_AND_ASSIGN(KIM_API_model);

    KIM_IOline *inlines;
    int numlines;
    std::string name_temp;

  int model_language;
    int ErrorCode;// reserved for proper errors handling
    int compute_index;
    int get_neigh_index;
    int* neiOfAnAtom;
    int neiOfAnAtomSize;

    int model_index_shift; //0--no conversion, 1 -- from 0 to 1, -1 -- from 1 to 0
    void * model_buffer; // stores everything that is not reflected  in .kim
                         // but nessssery for model instantiation
    void * test_buffer; // stores everything that is not reflected  in .kim
                        // but nessssery for test instantiation

    Atom_Map * AtomsTypes;
    int nAtomsTypes;

  int tempVersionMajor;
  int tempVersionMinor;
  int modelVersionMajor;
  int modelVersionMinor;
  int simulatorVersionMajor;
  int simulatorVersionMinor;


  //Required arguments
  static std::map<std::string,std::string> kim_str_map;
  static char const * const required_arguments[];

    // other..
    KIMBaseElement &operator[](int i);
    KIMBaseElement &operator[](const char *nm);

    int get_index(const char *nm, int * error);
    static void fatal_error_print();
    int preinit(const char * modelname);
    int prestring_init(const char * instrn);
    static   bool read_file_str(const char * strstream,KIM_IOline ** lns, int * numlns );

    static bool irrelevantVars2donotcompute(KIM_API_model & sim, KIM_API_model & mdl);
    bool check_required_arguments();
    static bool is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns, bool ignore_optional);
    static bool is_it_match_noFlagCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns, bool ignore_optional);

    static bool is_it_par(std::string const & name);

    bool is_it_match(KIM_API_model &test,KIM_API_model & mdl);
    bool do_AtomsTypes_match(KIM_API_model &test,KIM_API_model & mdl);

  static bool parse_semver(const char* const version, int* const major,
                           int* const minor, int* const patch,
                           char* const prerelease, char* const build_metadata);
  bool does_it_have_a_version_number(const char* const instrn);

    bool init_AtomsTypes();
    bool do_flag_match(KIM_API_model & tst, KIM_API_model &mdl);
    bool is_it_in_and_is_it_flag(KIM_API_model &mdl,const char *name);
    bool is_it_in(KIM_API_model &mdl,const char *name);
    void * model_lib_handle;
    void free();

  friend class KIM::Model;
};

} /* OLD_KIM namespace */

std::ostream &operator<<(std::ostream &stream, OLD_KIM::KIM_IOline a);
std::istream &operator>>(std::istream &stream, OLD_KIM::KIM_IOline &a);
//stringstream &operator>>(stringstream &stream, KIM_IOline &a);

std::ostream &operator<<(std::ostream &stream, OLD_KIM::IOline a);
std::istream &operator>>(std::istream &stream, OLD_KIM::IOline &a);

std::ostream &operator<<(std::ostream &stream, OLD_KIM::KIMBaseElement a);

std::ostream &operator<<(std::ostream &stream, OLD_KIM::KIM_API_model &a);

#endif  /* KIMHDR_OLD_KIM_API_H */
