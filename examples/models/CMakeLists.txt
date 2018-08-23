#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013--2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


cmake_minimum_required(VERSION 3.4)

add_subdirectory(ex_model_Ar_P_LJ)
add_subdirectory(ex_model_Ar_P_MLJ_Fortran)
add_subdirectory(ex_model_Ar_P_Morse)
add_subdirectory(ex_model_Ar_P_Morse_07C)
add_subdirectory(ex_model_Ar_SLJ_MultiCutoff)
add_subdirectory(LennardJones612_UniversalShifted__MO_959249795837_003)
add_subdirectory(LennardJones_Ar)
add_subdirectory(Sim_LAMMPS_ADP_StarikovLopanitsynaSmirnova_2018_SiAu__SM_985135773293_000)
