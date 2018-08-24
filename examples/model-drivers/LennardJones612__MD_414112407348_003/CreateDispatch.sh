#!/bin/sh
#

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
# Copyright (c) 2013--2015, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Stephen M. Whalen
#


flName=LennardJones612ImplementationComputeDispatch.cpp

printf "   switch(GetComputeIndex(isComputeProcess_dEdr,\n"   >  $flName
printf "                          isComputeProcess_d2Edr2,\n" >> $flName
printf "                          isComputeEnergy,\n"         >> $flName
printf "                          isComputeForces,\n"         >> $flName
printf "                          isComputeParticleEnergy,\n" >> $flName
printf "                          isComputeVirial,\n"         >> $flName
printf "                          isComputeParticleVirial,\n" >> $flName
printf "                          isShift))\n"                >> $flName
printf "   {\n"                                               >> $flName

i=0
for processdE in false true; do
  for processd2E in false true; do
    for energy in false true; do
      for force in false true; do
        for particleEnergy in false true; do
          for virial in false true; do
            for particleVirial in false true; do
              for sshift in false true; do
                printf "      case $i:\n"                                      >> $flName
                printf "         ier = Compute< $processdE, $processd2E,\n"    >> $flName
                printf "                        $energy, $force,\n"            >> $flName
                printf "                        $particleEnergy, $virial,\n"   >> $flName
                printf "                        $particleVirial, $sshift >(\n" >> $flName
                printf "                  modelCompute,\n"                     >> $flName
                printf "                  modelComputeArguments,\n"            >> $flName
                printf "                  particleSpeciesCodes,\n"             >> $flName
                printf "                  particleContributing,\n"             >> $flName
                printf "                  coordinates,\n"                      >> $flName
                printf "                  energy,\n"                           >> $flName
                printf "                  forces,\n"                           >> $flName
                printf "                  particleEnergy,\n"                   >> $flName
                printf "                  *virial,\n"                          >> $flName
                printf "                  particleVirial);\n"                  >> $flName
                printf "         break;\n"                                     >> $flName
                i=`expr $i + 1`
              done  # sshift
            done  # particleVirial
          done  # virial
        done  # particleEnergy
      done  # force
    done  # energy
  done  # processd2E
done  # processdE

printf "      default:\n"                                                         >> $flName
printf "         std::cout << \"Unknown compute function index\" << std::endl;\n" >> $flName
printf "         ier = true;\n"                                                   >> $flName
printf "         break;\n"                                                        >> $flName
printf "   }\n"                                                                   >> $flName
