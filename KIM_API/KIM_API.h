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
// Copyright (c) 2013, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//    Ryan S. Elliott
//    Ellad B. Tadmor
//

//
// Release: This file is part of the openkim-api.git repository.
//


#ifndef KIMHDR_KIM_API_H
#define KIMHDR_KIM_API_H

#include <iostream>
#include <stdint.h>
#include <stdarg.h>

#define number_NBC_methods 7
#define KIM_KEY_STRING_LENGTH 128

#include "KIM_AUX.h"

//#define intptr_t int  // for 32 bit machines

class KIMBaseElementFlag{
public:
        int peratom; //0 -- peratom, 1--per something else
        int freeable; // 0--freeable , 1 is not freeable, ... - error
        int pointerchanged;// 0 -- changed therefore update, 1 don't update, ... =error
        int calculate; // 0 -- do not calculate, 1 -- calculate
        KIMBaseElementFlag();
};


class KIMBaseElementUnit{
public:
        char  dim [KIM_KEY_STRING_LENGTH];  // dimension (length, energy, time, mass, or derivatives...
      KIMBaseElementUnit();
      void init();
};
class Atom_Map{
public:
    Atom_Map(): requestedByTest(false) {}
    char symbol[KIM_KEY_STRING_LENGTH];
    bool readOnly;
    bool requestedByTest;
    int code;
    static int comparator(const void * a1,const void *a2);
};
class KIM_IOline{
public:
        char name[KIM_KEY_STRING_LENGTH];
        char type[KIM_KEY_STRING_LENGTH];
        char dim[KIM_KEY_STRING_LENGTH];
        char shape[KIM_KEY_STRING_LENGTH];
        char requirements[KIM_KEY_STRING_LENGTH];
        char comments[181];
        bool goodformat,input,output;

        KIM_IOline();

        bool getFields(char *inString);
        int get_rank();
        int * get_shape();
        int * get_shape(int natoms, int ntypes);
        bool isitsizedefined();
        bool isitperatom();
        bool isitoptional();

 private:
        void strip(char * strv);
        void strip();
        void init2empty();
        bool isitinput(const char*str);
        bool isitoutput(const char*str);
};//main input line handler
std::ostream &operator<<(std::ostream &stream, KIM_IOline a);
std::istream &operator>>(std::istream &stream, KIM_IOline &a);
//stringstream &operator>>(stringstream &stream, KIM_IOline &a);
class IOline{
public:
        char name[101];
        char value[101];
        char comment[121];
        bool goodformat;
        void strip();

        void strip(char *nm);
        IOline();
        bool getFields(char *inputString);
        static int readlines_str(const char * inputstr, IOline ** lines, bool& success);
}; //secondary input line handler //cout<<"SystemOfUnit:  file:"<<infile<<":"<<endl;
std::ostream &operator<<(std::ostream &stream, IOline a);
std::istream &operator>>(std::istream &stream, IOline &a);


#include "Unit_Handling.h"

class KIMBaseElement{
public:
   union {
      void *p;
      void (* fp)();
   } data;
        intptr_t size; //Size in words defined by type
        intptr_t rank; // number of indexes
        int * shape; //1d array of integer showing the size of each index
        void * ptrptr; // Auxilary structure for holding pointer to pointers
        char *name;
        char *type;
        KIMBaseElementUnit * unit;
        KIMBaseElementFlag *flag;
        void *reserved;
        KIMBaseElement();
        ~KIMBaseElement();
        bool init(const char *nm,const char * tp,intptr_t sz, intptr_t rnk, int *shp,void * pdata);
        bool init(const char *nm,const char * tp,intptr_t sz, intptr_t rnk, int *shp);
        void free();
        void nullify();
        bool equiv(KIM_IOline& kimioline, bool skip_specials);

static  int getelemsize(const char *tp, bool& success);
};
std::ostream &operator<<(std::ostream &stream, KIMBaseElement a);

typedef void (*func_ptr)();
class KIM_API_model{
public:
    KIMBaseElement model;
    KIM_API_model();
    ~KIM_API_model();

    int model_info(const char * modelname) {return preinit(modelname);} //does not have error
                                                                  // because it returns OK or FAIL

    void free(int *error);
    int set_data(const char * nm, intptr_t size, void *dt);
    int set_method(const char * nm, intptr_t size, func_ptr dt);
    int set_data_by_index(int ind,intptr_t size, void *dt);
    int set_method_by_index(int ind,intptr_t size, func_ptr dt);

    void * get_data(const char *nm,int *error);
    func_ptr get_method(const char *nm,int *error);
    void * get_data_by_index(int ind,int *error);
    func_ptr get_method_by_index(int ind,int *error);


    int get_index(const char *nm, int * error);
    intptr_t get_size(const char *nm,int *error);
    intptr_t get_rank(const char *nm,int *error);
    intptr_t get_shape(const char *nm,int * shape,int *error);
    void set_shape(const char * nm, int * shape, int rank, int *error);
    void set_compute(const char *nm, int flag, int *error);
    void set_compute_by_index(int ind, int flag, int *error);
    int get_compute(const char *nm, int* error);
    KIMBaseElement &operator[](int i);
    KIMBaseElement &operator[](const char *nm);
    void print(int *error);

    intptr_t get_size_by_index(int I,int *error);
    intptr_t get_rank_by_index(int I,int *error);
    intptr_t get_shape_by_index(int I, int * shape,int *error);
    int get_compute_by_index(int I,int * error);

    int init(const char * testname,const char * modelname);
    int string_init(const char * intststr,const char * modelname);
   int match(const char* teststring, const char* modelstring);
    int model_compute();
    int get_neigh(int mode,int request, int *atom, int *numnei, int **nei1atom, double **Rij);
    int model_init();
    int model_reinit();
    int model_destroy();

void allocate( int natoms, int ntypes,int * error);

char* get_model_partcl_typs(int *nparticleTypes,int* error);
char* get_test_partcl_typs(int *nparticleTypes,int* error);
int get_partcl_type_code(const char *atom, int * error);
void set_partcl_type_code(const char *atom, int code, int* error);

char * get_params(int *nVpar,int *error);
char * get_free_params(int *nVpar,int *error);
char * get_fixed_params(int *nVpar,int *error);

static char * get_model_kim_str(const char * modelname,int *kimerr);


char * get_NBC_method(int *error);
int is_half_neighbors(int *error);


    int get_neigh_mode(int *error);
    static char * get_status_msg(int status_code);
    static int report_error(int line, const char * fl, const char * usermsg, int error);
    int get_model_index_shift();
    void set_model_buffer(void * o,int *error);
    void * get_model_buffer(int *error);
    void set_test_buffer(void * o,int *error);
    void * get_test_buffer(int *error);

   static int process_dEdr(KIM_API_model **ppkim,double *dr,double *r,double ** dx, int *i,int *j);

   static int process_d2Edr2(KIM_API_model **ppkim,double *de,double **rr,double ** pdx,int **ii,int **jj);

   //multiple data set/get methods
   //
  void setm_data(int *err, int numargs, ... );      //++
  void setm_method(int *err, int numargs, ... );      //++
  void setm_data_by_index(int *err, int numargs, ... );  //++
  void setm_method_by_index(int *err, int numargs, ... );  //++
  void getm_data(int *err,int numargs, ...);        //++
  void getm_method(int *err,int numargs, ...);        //++
  void getm_data_by_index(int *err,int numargs, ...);    //++
  void getm_method_by_index(int *err,int numargs, ...);    //++
  void getm_index(int *err, int numargs, ...);      //++
  void setm_compute(int *err, int numargs, ...);    //++
  void setm_compute_by_index(int *err, int numargs, ...);//++
  void getm_compute(int *err,int numargs, ...);  //  ++
  void getm_compute_by_index(int *err,int numargs, ...); //++

   //related to process fij public variables
    bool test_doing_process_dEdr;
    bool test_doing_process_d2Edr2;
    bool virial_need2add;
    bool particleVirial_need2add;
    bool hessian_need2add;

    //Unit_Handling object
    Unit_Handling unit_h;
    //Unit_Handling related routines

    static double get_scale_conversion(const char * u_from,const char * u_to, int *error);
    int get_unit_handling(int *error);
    char * get_unit_length(int *error);
    char * get_unit_energy(int *error);
    char * get_unit_charge(int *error);
    char * get_unit_temperature(int *error);
    char * get_unit_time(int *error);
    double convert_to_act_unit( const char * length, const char * energy,
      const char * charge,const char * temperature, const char * time,
      double length_exponent, double energy_exponent, double charge_exponent,
      double temperature_exponent, double time_exponent, int* kimerror);

    KIM_AUX::Process_DE * get_process_DE_instance(){//used in KIM_AUX
        return &process_DE_instance;
    }
private:
    KIM_IOline *inlines;
    int numlines;
   const char* name_temp;

    bool locator_neigh_mode;
    bool iterator_neigh_mode;
    bool both_neigh_mode;
    int ErrorCode;// reserved for proper errors handling
    int compute_index;
    int get_neigh_index;
    int* neiOfAnAtom;
    int neiOfAnAtomSize;

    int model_index_shift; //0--no conversion, 1 -- from 0 to 1, -1 -- from 1 to 0
    int AUX_index_shift; //0--noconversion, 1 -- from 0 to 1
    KIM_AUX::Process_DE process_DE_instance;
    void * model_buffer; // stores everything that is not reflected  in .kim
                         // but nessssery for model instantiation
    void * test_buffer; // stores everything that is not reflected  in .kim
                        // but nessssery for test instantiation

    Atom_Map * AtomsTypes;
    int nAtomsTypes;

    //"CLUSTER"
    char NBC_method_A[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_A[1][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_A;


    //"MI_OPBC_H"
    char NBC_method_B[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_B[5][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_B;


    //"MI_OPBC_F"
    char NBC_method_C[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_C[4][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_C;


    //"NEIGH_RVEC_F"
    char NBC_method_D[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_D[3][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_D;


    //"NEIGH_RVEC_H"
    char NBC_method_E[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_E[4][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_E;


    //"NEIGH_PURE_F"
    char NBC_method_F[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_F[3][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_F;

    //"NEIGH_PURE_H"
    char NBC_method_G[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_G[4][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_G;

    int n_NBC_methods;
    int * nnarg_NBC;
    char ** NBC_methods;
    char *** arg_NBC_methods;

    //related to process fij variables

    int virial_ind;
    int particleVirial_ind;
    int hessian_ind;
    int process_dEdr_ind;
    int process_d2Edr2_ind;

    // other..
    static void fatal_error_print();
    int preinit(const char * initfile,const char *modelname);
    int preinit(const char * modelname);
    int prestring_init(const char * instrn);
    int init_str_modelname(const char *testname,const char *inmdlstr);
    static   bool read_file_str(const char * strstream,KIM_IOline ** lns, int * numlns );

    static bool irrelevantVars2donotcompute(KIM_API_model & test, KIM_API_model & mdl);
    bool check_consistance_NBC_method();
    static bool is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns, bool ignore_optional, bool match_regular);
    static bool is_it_match_noFlagCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns, bool ignore_optional);

   char* get_a_type_of_params(int* nVpar, int* error, int typecode);
    static bool is_it_par(const char * name);
    static bool is_it_fixed_par(const char * name);
    static bool is_it_free_par(const char * name);

    bool is_it_match(KIM_API_model &test,KIM_API_model & mdl);
    bool do_AtomsTypes_match(KIM_API_model &test,KIM_API_model & mdl);

    char NBC_method_current[KIM_KEY_STRING_LENGTH];
    bool NBC_methods_match(KIM_API_model &test,KIM_API_model &mdl);
    bool fij_related_things_match(KIM_API_model &test,KIM_API_model &mdl);
    bool fij_related_things_add_set_index();
    void add_auxiliaries_if_needed();
    bool init_AtomsTypes();
    bool do_flag_match(KIM_API_model & tst, KIM_API_model &mdl);
    bool is_it_in_and_is_it_flag(KIM_API_model &mdl,const char *name);
    bool is_it_in(KIM_API_model &mdl,const char *name);
    void * model_lib_handle;
    bool add_element(const char * inln);
    void free();
};
std::ostream &operator<<(std::ostream &stream, KIM_API_model &a);
#endif  /* KIMHDR_KIM_API_H */
