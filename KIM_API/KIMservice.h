
//                                                                      
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
// All rights reserved.                                                 
//                                                                     
// Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
// 


//#include <stdlib.h>

//#include <iostream>
//#include <fstream>
////#include <cctype>
//#include <string.h>
#include <stdint.h>

using namespace std;
#ifndef _KIMSERVICE_H
#define	_KIMSERVICE_H

#define KEY_CHAR_LENGTH 64
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
        char units[KEY_CHAR_LENGTH]; //name of units system, "none" if not set
        char  dim [KEY_CHAR_LENGTH];  // dimension (length, energy, time, mass, or derivatives...
        float scale;// scale to the standard unit: " 1.0 if it is standard unit"
      KIMBaseElementUnit();
      void init();
};
class Atom_Map{
public:
    char symbol[KEY_CHAR_LENGTH];
    int code;
    static int comparator(const void * a1,const void *a2);
};
class KIM_IOline{
public:
	char name[KEY_CHAR_LENGTH];
	char type[KEY_CHAR_LENGTH];
	char dim[KEY_CHAR_LENGTH];
        char units[KEY_CHAR_LENGTH];
        char shape[KEY_CHAR_LENGTH];
        char requirements[KEY_CHAR_LENGTH];
        char comments[181];
	bool goodformat,input,output;

	KIM_IOline();
      
	bool getFields(char *inString);
        double get_unitscale();
        int get_rank();
        int * get_shape();
        int * get_shape(intptr_t natoms, int ntypes);
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

class IOline{
public:
	char name[101];
	char value[101];
	char comment[101];
	bool goodformat;
	void strip();

        void strip(char *nm);
	IOline();
	bool getFields(const char *inputString);
}; //secondary input line handler
ostream &operator<<(ostream &stream, IOline a);
istream &operator>>(istream &stream, IOline &a);

class SystemOfUnit {
public:
    char UnitsSystemName[KEY_CHAR_LENGTH];
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
    bool isitunit(char * unit);
};

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

class KIM_API_model{
public:
    KIMBaseElement model;
    KIM_API_model();
    ~KIM_API_model();
    bool preinit(char * initfile,char *modelname);
    void free(int *error);
    void free();
    bool set_data(char *nm, intptr_t size, void *dt);
    bool set_data_byi(int ind,intptr_t size, void *dt);

    void * get_data(char *nm,int *error);
    void * get_data(char *nm);
    int get_index(char *nm, int * error);
    int get_index(char *nm);
    intptr_t get_size(char *nm,int *error);
    intptr_t get_rank_shape(char *nm,int * shape,int *error);
    void set_rank_shape(char *nm, int * shape, int rank, int *error);
    float get_unit_scalefactor(char*nm,int *error);
    void set2_compute(char *nm);
    void set2_donotcompute(char *nm);
    bool isit_compute(char *nm);
    KIMBaseElement &operator[](int i);
    KIMBaseElement &operator[](char *nm);

static   void read_file(char * initfile,KIM_IOline ** lns, int * numlns);
static bool is_it_match(KIM_API_model & mdtst,char * inputinitfile);
static bool is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns);
static bool is_it_match_noDummyCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns);
static bool is_it_par(char * name);
static bool is_it_fixed_par(char * name);
static bool is_it_free_par(char * name);

bool is_it_match(KIM_API_model &test,KIM_API_model & mdl);
bool do_AtomsTypes_match(KIM_API_model &test,KIM_API_model & mdl);


    bool init(char * testinputfile,char* testname, char * modelinputfile,char *modelname);
    bool init(char * testname,char * modelname);
     void model_compute(int *error);
    int get_half_neigh(int mode,int request, int *atom, int *numnei, int **nei1atom, double **Rij);
    int get_full_neigh(int mode,int request, int *atom, int *numnei, int **nei1atom, double **Rij);
    bool model_init();
    bool model_reinit();
   void model_destroy(int *error);
static void irrelevantVars2donotcompute(KIM_API_model & test, KIM_API_model & mdl);
static void allocateinitialized(KIM_API_model * mdl, intptr_t natoms, int ntypes,int * error);
bool is_unitsfixed();
void set_unitsfixed(bool f);
void transform_units_to(char * unitS,int *error);
bool set_units(char * unitS);
void get_units(char * units,int *error);
void get_originalUnits(char * unitS,int *error);

void * get_listAtomsTypes(int *nAtomTypes,int *error);
int get_aTypeCode(char *atom, int * error);

void * get_listParams(int *nVpar,int *error);
void * get_listFreeParams(int *nVpar,int *error);
void * get_listFixedParams(int *nVpar,int *error);

void * get_NBC_method(int *error);
bool requiresFullNeighbors();

    KIM_IOline *inlines;
    int numlines;
    SystemOfUnit * UnitsSet;
    int numUnitsSet;
    char originalUnits[KEY_CHAR_LENGTH];
    char currentUnits[KEY_CHAR_LENGTH];
    bool unitsFixed;
    bool support_Rij;
    int get_neigh_mode(int *);
private:
    bool locator_neigh_mode;
    bool iterator_neigh_mode;
    bool both_neigh_mode;
    int ErrorCode;// reserved for proper errors handling
    int compute_index;
    int get_full_neigh_index;
    int get_half_neigh_index;
    int neiOfAnAtom_half[KIM_API_MAX_NEIGHBORS];
    int neiOfAnAtom_full[KIM_API_MAX_NEIGHBORS];
    int baseConvertKey; //0--no conversion, 1 -- from 0 to 1, -1 -- from 1 to 0
    Atom_Map * AtomsTypes;
    int nAtomsTypes;

    //"CLUSTER"
    char NBC_method_A[KEY_CHAR_LENGTH];
    char arg_NBC_method_A[1][KEY_CHAR_LENGTH];
    int narg_NBC_method_A;

    
    //"MI-OPBC-H"
    char NBC_method_B[KEY_CHAR_LENGTH];
    char arg_NBC_method_B[4][KEY_CHAR_LENGTH];
    int narg_NBC_method_B;

    
    //"MI-OPBC-F"
    char NBC_method_C[KEY_CHAR_LENGTH];
    char arg_NBC_method_C[4][KEY_CHAR_LENGTH];
    int narg_NBC_method_C;
 
    
    //"NEIGH-RVEC-F"
    char NBC_method_D[KEY_CHAR_LENGTH];
    char arg_NBC_method_D[3][KEY_CHAR_LENGTH];
    int narg_NBC_method_D;


    //"NEIGH-PURE-H"
    char NBC_method_E[KEY_CHAR_LENGTH];
    char arg_NBC_method_E[3][KEY_CHAR_LENGTH];
    int narg_NBC_method_E;
    
    
    //"NEIGH-PURE-F"
    char NBC_method_F[KEY_CHAR_LENGTH];
    char arg_NBC_method_F[3][KEY_CHAR_LENGTH];
    int narg_NBC_method_F;

    int n_NBC_methods;
    int * nnarg_NBC;
    char ** NBC_methods;
    char *** arg_NBC_methods;

    bool check_consistance_NBC_method(); 

    char NBC_method_current[KEY_CHAR_LENGTH];
    bool NBC_methods_match(KIM_API_model &test,KIM_API_model &mdl);
    bool init_AtomsTypes();
    void supported_units_init();
    bool are_infileunits_consistent();
    bool among_supported_units(char *unitS);
    bool do_dummy_match(KIM_API_model & tst, KIM_API_model &mdl);
    bool is_it_in_and_is_it_dummy(KIM_API_model &mdl,char *name);
    int get_indexOfsupportedUnits(char * unitS);
    void setScale(int index);
    void data_multiply_a(void *dt,char* type,intptr_t sz,float a);
    void * model_lib_handle;
};
ostream &operator<<(ostream &stream, KIM_API_model &a);

#endif	/* _KIMSERVICE_H */
