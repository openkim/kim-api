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
# Copyright (c) 2013--2017, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Ellad B. Tadmor
#

#######################################################################################################
#
# Release: This file is part of the kim-api-v1.8.0 package.
#
# See src/standard.kim for documentation about this file
#
#######################################################################################################


KIM_API_Version := 1.8.0

Unit_Handling    := flexible
Unit_length      := A
Unit_energy      := eV
Unit_charge      := e
Unit_temperature := K
Unit_time        := ps


#######################################################################################################
PARTICLE_SPECIES:
# Symbol/name               Type                    code

SPECIES_001_NAME_STR        spec                    1


#######################################################################################################
CONVENTIONS:
# Name                      Type

OneBasedLists               flag


#######################################################################################################
MODEL_INPUT:
# Name                      Type         Unit    Requirements

numberOfParticles           integer      none

numberOfSpecies             integer      none

particleSpecies             integer      none

particleContributing        integer      none

coordinates                 double       length

get_neigh                   method       none    optional

neighObject                 pointer      none    optional

process_dEdr                method       none    optional

process_d2Edr2              method       none    optional


#######################################################################################################
MODEL_OUTPUT:
# Name                      Type         Unit     Requirements

destroy                     method       none

compute                     method       none

reinit                      method       none     optional

energy                      double       energy   optional

forces                      double       force    optional

particleEnergy              double       energy   optional


#######################################################################################################
MODEL_PARAMETERS:
# Name                      Type         Unit

PARAM_cutoff                double       length

PARAM_epsilon               double       energy

PARAM_sigma                 double       length^-1
