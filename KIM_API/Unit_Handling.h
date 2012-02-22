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
// Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//

//
// Release: This file is part of the openkim-api.git repository.
//


#ifndef _KIMSERVICE_H
#include "KIM_API.h"
#endif
#ifndef UNIT_HANDLING_H
#define UNIT_HANDLING_H

class Unit_Handling{
public:
    Unit_Handling();
    ~Unit_Handling();
    void init_str(char * inputstr, int * error);
    void init(char *inputfile, int * error);
    static double get_scale_conversion( char *u_from,char *u_to, int *error);
    static bool is_it_base(char * unit);
    static bool is_it_derived(char * unit);
    static bool do_unit_match(Unit_Handling  &tst, Unit_Handling &mdl);
    void print();
    void print(ostream &stream);
    int get_unit_handling(int *error);
    char * get_unit_length(int *error);
    char * get_unit_energy(int *error);
    char * get_unit_charge(int *error);
    char * get_unit_temperature(int *error);
    char * get_unit_time(int *error);

    static double convert_to_act_unit(void *kimmdl,
                                char* length,
                                char* energy,
                                char* charge,
                                char* temperature,
                                char* time,
                                double length_exponent,
                                double energy_exponent,
                                double charge_exponent,
                                double temperature_exponent,
                                double time_exponent,
                                int* kimerror);

private:
    // list of  base & derived units
    static char *base_list[];     static int nbase_list;
    static char *derived_list[];  static int nderived_list;

    // list of supported base units for Unit_length
    static char *length_list[];   static int nlength_list; static double length_scale[];
    // list of supported base units for Unit_energy
    static char *energy_list[];   static int nenergy_list; static double energy_scale[];
    // list of supported base units for Unit_charge
    static char *charge_list[];   static int ncharge_list; static double charge_scale[];
    // list of supported base units for Unit_temperature
    static char *temperature_list[];  static int ntemperature_list; static double temperature_scale[];
    // list of supported base units for Unit_time
    static char *time_list[];     static int ntime_list;   static double time_scale[];


    //
    char Unit_length[KIM_KEY_STRING_LENGTH];
    char Unit_energy[KIM_KEY_STRING_LENGTH];
    char Unit_charge[KIM_KEY_STRING_LENGTH];
    char Unit_temperature[KIM_KEY_STRING_LENGTH];
    char Unit_time[KIM_KEY_STRING_LENGTH];
    bool flexible_handling;
    void check_base_set_flexible(IOline *lines, int nlines, int *error);
    bool is_Unit_length_supported();
    bool is_Unit_energy_supported();
    bool is_Unit_charge_supported();
    bool is_Unit_temperature_supported();
     bool is_Unit_time_supported();
    static bool is_it_Unit_length(char * unit,int *index);
    static bool is_it_Unit_energy(char * unit, int * index);
    static bool is_it_Unit_charge(char * unit, int *index);
    static bool is_it_Unit_temperature(char * unit, int *index);
    static bool is_it_Unit_time(char * unit, int *index);

};
ostream &operator<<(ostream &stream, Unit_Handling &a);
#endif  /* UNIT_HANDLING_H */
