#!/bin/sh
#

#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2021, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Stephen M. Whalen
#
# SPDX-License-Identifier: LGPL-2.1-or-later
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#


flName=LennardJones612ImplementationComputeDispatch.cpp

printf "switch (GetComputeIndex(isComputeProcess_dEdr,\n"   >  $flName
printf "                        isComputeProcess_d2Edr2,\n" >> $flName
printf "                        isComputeEnergy,\n"         >> $flName
printf "                        isComputeForces,\n"         >> $flName
printf "                        isComputeParticleEnergy,\n" >> $flName
printf "                        isComputeVirial,\n"         >> $flName
printf "                        isComputeParticleVirial,\n" >> $flName
printf "                        isShift))\n"                >> $flName
printf "{\n"                                                >> $flName

i=0
for processdE in false true; do
  for processd2E in false true; do
    for energy in false true; do
      for force in false true; do
        for particleEnergy in false true; do
          for virial in false true; do
            for particleVirial in false true; do
              for sshift in false true; do
                printf "  case $i:\n"                                     >> $flName
                printf "    ier = Compute<$processdE, $processd2E, "      >> $flName
                printf "$energy, $force, "                                >> $flName
                printf "$particleEnergy, $virial, "                       >> $flName
                printf "$particleVirial, $sshift>(\n"                     >> $flName
                printf "        modelCompute,\n"                          >> $flName
                printf "        modelComputeArguments,\n"                 >> $flName
                printf "        particleSpeciesCodes,\n"                  >> $flName
                printf "        particleContributing,\n"                  >> $flName
                printf "        coordinates,\n"                           >> $flName
                printf "        energy,\n"                                >> $flName
                printf "        forces,\n"                                >> $flName
                printf "        particleEnergy,\n"                        >> $flName
                printf "        *virial,\n"                               >> $flName
                printf "        particleVirial);\n"                       >> $flName
                printf "    break;\n"                                     >> $flName
                i=`expr $i + 1`
              done  # sshift
            done  # particleVirial
          done  # virial
        done  # particleEnergy
      done  # force
    done  # energy
  done  # processd2E
done  # processdE

printf "  default:\n"                                                        >> $flName
printf "    std::cout << \"Unknown compute function index\" << std::endl;\n" >> $flName
printf "    ier = true;\n"                                                   >> $flName
printf "    break;\n"                                                        >> $flName
printf "}\n"                                                                 >> $flName
