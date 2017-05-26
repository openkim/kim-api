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


#ifndef KIMHDR_OLD_UNIT_HANDLING_H
#define KIMHDR_OLD_UNIT_HANDLING_H

#include <iostream>

namespace OLD_KIM
{

class IOline;

class Unit_Handling{
public:
    std::string Unit_length;
    std::string Unit_energy;
    std::string Unit_charge;
    std::string Unit_temperature;
    std::string Unit_time;
    bool flexible_handling;
    Unit_Handling();
    ~Unit_Handling();
    void init_str(char const* inputstr, int * error);
    static double get_scale_conversion(std::string const & u_from,std::string const & u_to, int *error);
    static bool is_it_base(std::string const & unit);
    static bool is_it_derived(std::string const & unit);
    static bool do_unit_match(Unit_Handling  &tst, Unit_Handling &mdl);
    void print();
    void print(std::ostream &stream);
    int get_unit_handling(int *error);
    std::string const & get_unit_length(int *error);
    std::string const & get_unit_energy(int *error);
    std::string const & get_unit_charge(int *error);
    std::string const & get_unit_temperature(int *error);
    std::string const & get_unit_time(int *error);

    static double convert_to_act_unit(void *kimmdl,
                                std::string const & length,
                                std::string const & energy,
                                std::string const & charge,
                                std::string const & temperature,
                                std::string const & time,
                                double length_exponent,
                                double energy_exponent,
                                double charge_exponent,
                                double temperature_exponent,
                                double time_exponent,
                                int* kimerror);

private:
    // list of  base & derived units
    static std::string const base_list[];     static int nbase_list;
    static std::string const derived_list[];  static int nderived_list;

    // list of supported base units for Unit_length
    static std::string const length_list[];   static int nlength_list; static double length_scale[];
    // list of supported base units for Unit_energy
    static std::string const energy_list[];   static int nenergy_list; static double energy_scale[];
    // list of supported base units for Unit_charge
    static std::string const charge_list[];   static int ncharge_list; static double charge_scale[];
    // list of supported base units for Unit_temperature
    static std::string const temperature_list[];  static int ntemperature_list; static double temperature_scale[];
    // list of supported base units for Unit_time
    static std::string const time_list[];     static int ntime_list;   static double time_scale[];


    //
    void check_base_set_flexible(IOline *lines, int nlines, int *error);
    bool is_Unit_length_supported();
    bool is_Unit_energy_supported();
    bool is_Unit_charge_supported();
    bool is_Unit_temperature_supported();
     bool is_Unit_time_supported();
    static bool is_it_Unit_length(std::string const & unit, int *index);
    static bool is_it_Unit_energy(std::string const & unit, int *index);
    static bool is_it_Unit_charge(std::string const & unit, int *index);
    static bool is_it_Unit_temperature(std::string const & unit, int *index);
    static bool is_it_Unit_time(std::string const & unit, int *index);

};

} // namespace OLD_KIM

std::ostream &operator<<(std::ostream &stream, OLD_KIM::Unit_Handling &a);

#endif  /* KIM_HDR_OLD_UNIT_HANDLING_H */
