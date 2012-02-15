
//
// Release: This file is part of the openkim-api.git repository.
//
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
// All rights reserved.
//
// Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
//


//#include <stdlib.h>

#include <iostream>
#include <fstream>
////#include <cctype>
//#include <string.h>
#include <stdint.h>
#include <stdarg.h>
using namespace std;
#ifndef _KIMSERVICE_H
#define	_KIMSERVICE_H

#include "KIM_API_status.h"
#include "KIM_AUX.h"

extern "C" {
        char * standard_kim_str();
 }
#define KIM_KEY_STRING_LENGTH 64
#define number_NBC_methods 6
#ifndef KIM_DIR_API
#define KIM_DIR_API "../../KIM_API/"
#endif

#ifndef KIM_DIR_MODELS
#define KIM_DIR_MODELS "../../MODELs/"
#endif

#ifndef KIM_DIR_TESTS
#define KIM_DIR_TESTS "../../TESTs/"
#endif

#ifndef KIM_DIR
#define KIM_DIR "./"
#endif

#ifndef KIM_API_MAX_NEIGHBORS
#define KIM_API_MAX_NEIGHBORS 512
#endif

#define FREEABLE 0



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
    char symbol[KIM_KEY_STRING_LENGTH];
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
        bool isitpernatomtypes();
        bool isitsizedefined();
        bool isitperatom();
        bool isitoptional();

 private:
        void strip(char * strv);
        void strip();
        void init2empty();
        bool isitinput(char*str);
        bool isitoutput(char*str);
};//main input line handler
ostream &operator<<(ostream &stream, KIM_IOline a);
istream &operator>>(istream &stream, KIM_IOline &a);
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
	bool getFields(const char *inputString);
        static int readlines_str(char * inputstr, IOline ** lines);
        static int  readlines(char * infile, IOline **inlines);
}; //secondary input line handler //cout<<"SystemOfUnit:  file:"<<infile<<":"<<endl;
ostream &operator<<(ostream &stream, IOline a);
istream &operator>>(istream &stream, IOline &a);


/*
class SystemOfUnit {
public:
    char UnitsSystemName[KIM_KEY_STRING_LENGTH];
    IOline * inlines;
    int numlines;
    float mass,distance,time,energy,velocity;
    float force,torque,temperature,pressure;
    float dynamic_viscosity,charge,dipole,electric_field,density;
    float garbage;

    SystemOfUnit();
    ~SystemOfUnit();
    void init(char *infile);
    void free();
    float &operator[](char * unit);
 static int readlines(char * infile, IOline **inlines);
 static int readlines_str(char * instrn, IOline **inlines);

    bool isitunit(char * unit);
};
*/
class KIMBaseElement{
public:
        void *data;
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
        void init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp,void * pdata);
        void init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp);
        void free();
        void nullefy();
        bool operator==(KIM_IOline& kimioline);

static  int getelemsize(char *tp);
};
ostream &operator<<(ostream &stream, KIMBaseElement a);

#ifndef UNIT_HANDLING_H
#include "Unit_Handling.h"
#endif



class KIM_API_model{
public:
    KIMBaseElement model;
    KIM_API_model();
    ~KIM_API_model();
    bool preinit(char * initfile,char *modelname);
    bool preinit(char * modelname);
    bool model_info(char * modelname) {return preinit(modelname);}
    bool preinit_str_testname(char * instrn);
    void free_e(int *error);
    void free();
    bool set_data(char *nm, intptr_t size, void *dt);
    bool set_data_by_index(int ind,intptr_t size, void *dt);

    void * get_data(char *nm,int *error);
    void * get_data_by_index(int ind,int *error);

    void * get_data(char *nm);
    int get_index(char *nm, int * error);
    int get_index(char *nm);
    intptr_t get_size(char *nm,int *error);
    intptr_t get_shape(char *nm,int * shape,int *error);
    void set_shape(char *nm, int * shape, int rank, int *error);
    void set_compute(char *nm, int flag, int *error);
    void set_compute_by_index(int ind, int flag, int *error);
    bool get_compute(char *nm);
    KIMBaseElement &operator[](int i);
    KIMBaseElement &operator[](char *nm);

static   void read_file(char * initfile,KIM_IOline ** lns, int * numlns);
static   void read_file_str_testname(char * strstream,KIM_IOline ** lns, int * numlns );
static bool is_it_match(KIM_API_model & mdtst,char * inputinitfile);
static bool is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns);
static bool is_it_match_noFlagCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns);
static bool is_it_par(char * name);
static bool is_it_fixed_par(char * name);
static bool is_it_free_par(char * name);

static void fatal_error_print();
bool is_it_match(KIM_API_model &test,KIM_API_model & mdl);
bool do_AtomsTypes_match(KIM_API_model &test,KIM_API_model & mdl);


    bool init(char * testinputfile,char* testname, char * modelinputfile,char *modelname);
    bool init(char * testname,char * modelname);
    bool init_str_testname(char * intststr,char * modelname);
    bool init_str_modelname(char *testname,char *inmdlstr);
     void model_compute(int *error);
    int get_neigh(int mode,int request, int *atom, int *numnei, int **nei1atom, double **Rij);
    bool model_init();
    bool model_reinit();
   void model_destroy(int *error);
static void irrelevantVars2donotcompute(KIM_API_model & test, KIM_API_model & mdl);
static void allocateinitialized(KIM_API_model * mdl, int natoms, int ntypes,int * error);

char * get_partcl_types(int *nparticleTypes,int *error);
int get_partcl_type_code(char *atom, int * error);

char * get_params(int *nVpar,int *error);
char * get_free_params(int *nVpar,int *error);
char * get_fixed_params(int *nVpar,int *error);

static char * get_model_kim_str(char * modelname,int *kimerr);


char * get_NBC_method(int *error);
bool requiresFullNeighbors();

    KIM_IOline *inlines;
    int numlines;
    bool support_Rij;
    int get_neigh_mode(int *);
    static char * get_status_msg(int status_code);
    static int report_error(int line, char * fl, char * usermsg, int ier);
    int get_model_index_shift();
    void set_model_buffer(void * o,int * ier);
    void * get_model_buffer(int * ier);
    void set_test_buffer(void * o,int * ier);
    void * get_test_buffer(int * ier);

   static void process_dEdr(KIM_API_model **ppkim,double *dr,double *r,double ** dx, int *i,int *j,int *ier);

   static void process_d2Edr2(KIM_API_model **ppkim,double *de,double **rr,double ** pdx,int **ii,int **jj,int *ier);

   //multiple data set/get methods
   //
  void setm_data(int *err, int numargs, ... );      //++
  void setm_data_by_index(int *err, int numargs, ... );  //++
  void getm_data(int *err,int numargs, ...);        //++
  void getm_data_by_index(int *err,int numargs, ...);    //++
  void getm_index(int *err, int numargs, ...);      //++
  void setm_compute(int *err, int numargs, ...);    //++
  void setm_compute_by_index(int *err, int numargs, ...);//++
  void getm_compute(int *err,int numargs, ...);  //?  ++
  void getm_compute_by_index(int *err,int numargs, ...); //++

   //related to process fij public variables   
    bool virial_need2add;
    bool particleVirial_need2add;
    bool hessian_need2add;

    //Unit_Handling object
    Unit_Handling unit_h;
    //Unit_Handling related routines

    static double get_scale_conversion( char *u_from,char *u_to, int *error);
    int get_unit_handling(int *error);
    char * get_unit_length(int *error);
    char * get_unit_energy(int *error);
    char * get_unit_charge(int *error);
    char * get_unit_temperature(int *error);
    char * get_unit_time(int *error);
    double convert_to_act_unit( char* length, char* energy, char* charge,char* temperature, char* time,
      double length_exponent, double energy_exponent, double charge_exponent,
      double temperature_exponent, double time_exponent, int* kimerror);

    KIM_AUX::Process_DE * get_process_DE_instance(){
        return &process_DE_instance;
    }
private:

    bool locator_neigh_mode;
    bool iterator_neigh_mode;
    bool both_neigh_mode;
    int ErrorCode;// reserved for proper errors handling
    int compute_index;
    int get_neigh_index;
    int neiOfAnAtom_half[KIM_API_MAX_NEIGHBORS];

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


    //"NEIGH_PURE_H"
    char NBC_method_E[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_E[4][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_E;
    
    
    //"NEIGH_PURE_F"
    char NBC_method_F[KIM_KEY_STRING_LENGTH];
    char arg_NBC_method_F[3][KIM_KEY_STRING_LENGTH];
    int narg_NBC_method_F;

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

    

    bool check_consistance_NBC_method(); 

    char NBC_method_current[KIM_KEY_STRING_LENGTH];
    bool NBC_methods_match(KIM_API_model &test,KIM_API_model &mdl);
    bool fij_related_things_match(KIM_API_model &test,KIM_API_model &mdl);
    void fij_related_things_add_set_index();
    void add_auxiliaries_if_needed();
    bool init_AtomsTypes();
    bool do_flag_match(KIM_API_model & tst, KIM_API_model &mdl);
    bool is_it_in_and_is_it_flag(KIM_API_model &mdl,char *name);
    bool is_it_in(KIM_API_model &mdl,char *name);
    void data_multiply_a(void *dt,char* type,intptr_t sz,float a);
    void * model_lib_handle;
    void add_element(char * inln);
};
ostream &operator<<(ostream &stream, KIM_API_model &a);

#endif	/* _KIMSERVICE_H */
