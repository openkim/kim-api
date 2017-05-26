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
# Copyright (c) 2015, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Andrew Akerson
#


# load all basic KIM make configuration
ifeq ($(wildcard ../Makefile.KIM_Config),)
  $(error ../Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include ../Makefile.KIM_Config

# set model driver specific details
MODEL_DRIVER_NAME   := LennardJones612__MD_414112407348_002
MODEL_NAME          := LennardJones612_Universal__MO_826355984548_002
SPECIES_001_NAME    := H
SPECIES_002_NAME    := He
SPECIES_003_NAME    := Li
SPECIES_004_NAME    := Be
SPECIES_005_NAME    := B
SPECIES_006_NAME    := C
SPECIES_007_NAME    := N
SPECIES_008_NAME    := O
SPECIES_009_NAME    := F
SPECIES_010_NAME    := Ne
SPECIES_011_NAME    := Na
SPECIES_012_NAME    := Mg
SPECIES_013_NAME    := Al
SPECIES_014_NAME    := Si
SPECIES_015_NAME    := P
SPECIES_016_NAME    := S
SPECIES_017_NAME    := Cl
SPECIES_018_NAME    := Ar
SPECIES_019_NAME    := K
SPECIES_020_NAME    := Ca
SPECIES_021_NAME    := Sc
SPECIES_022_NAME    := Ti
SPECIES_023_NAME    := V
SPECIES_024_NAME    := Cr
SPECIES_025_NAME    := Mn
SPECIES_026_NAME    := Fe
SPECIES_027_NAME    := Co
SPECIES_028_NAME    := Ni
SPECIES_029_NAME    := Cu
SPECIES_030_NAME    := Zn
SPECIES_031_NAME    := Ga
SPECIES_032_NAME    := Ge
SPECIES_033_NAME    := As
SPECIES_034_NAME    := Se
SPECIES_035_NAME    := Br
SPECIES_036_NAME    := Kr
SPECIES_037_NAME    := Rb
SPECIES_038_NAME    := Sr
SPECIES_039_NAME    := Y
SPECIES_040_NAME    := Zr
SPECIES_041_NAME    := Nb
SPECIES_042_NAME    := Mo
SPECIES_043_NAME    := Tc
SPECIES_044_NAME    := Ru
SPECIES_045_NAME    := Rh
SPECIES_046_NAME    := Pd
SPECIES_047_NAME    := Ag
SPECIES_048_NAME    := Cd
SPECIES_049_NAME    := In
SPECIES_050_NAME    := Sn
SPECIES_051_NAME    := Sb
SPECIES_052_NAME    := Te
SPECIES_053_NAME    := I
SPECIES_054_NAME    := Xe
SPECIES_055_NAME    := Cs
SPECIES_056_NAME    := Ba
SPECIES_057_NAME    := La
SPECIES_058_NAME    := Ce
SPECIES_059_NAME    := Pr
SPECIES_060_NAME    := Nd
SPECIES_061_NAME    := Pm
SPECIES_062_NAME    := Sm
SPECIES_063_NAME    := Eu
SPECIES_064_NAME    := Gd
SPECIES_065_NAME    := Tb
SPECIES_066_NAME    := Dy
SPECIES_067_NAME    := Ho
SPECIES_068_NAME    := Er
SPECIES_069_NAME    := Tm
SPECIES_070_NAME    := Yb
SPECIES_071_NAME    := Lu
SPECIES_072_NAME    := Hf
SPECIES_073_NAME    := Ta
SPECIES_074_NAME    := W
SPECIES_075_NAME    := Re
SPECIES_076_NAME    := Os
SPECIES_077_NAME    := Ir
SPECIES_078_NAME    := Pt
SPECIES_079_NAME    := Au
SPECIES_080_NAME    := Hg
SPECIES_081_NAME    := Tl
SPECIES_082_NAME    := Pb
SPECIES_083_NAME    := Bi
SPECIES_084_NAME    := Po
SPECIES_085_NAME    := At
SPECIES_086_NAME    := Rn
SPECIES_087_NAME    := Fr
SPECIES_088_NAME    := Ra
SPECIES_089_NAME    := Ac
SPECIES_090_NAME    := Th
SPECIES_091_NAME    := Pa
SPECIES_092_NAME    := U
SPECIES_093_NAME    := Np
SPECIES_094_NAME    := Pu
SPECIES_095_NAME    := Am
SPECIES_096_NAME    := Cm
SPECIES_097_NAME    := Bk
SPECIES_098_NAME    := Cf
SPECIES_099_NAME    := Es
SPECIES_100_NAME    := Fm
SPECIES_101_NAME    := Md
SPECIES_102_NAME    := No
SPECIES_103_NAME    := Lr
SPECIES_104_NAME    := Rf
SPECIES_105_NAME    := Db
SPECIES_106_NAME    := Sg
SPECIES_107_NAME    := Bh
SPECIES_108_NAME    := Hs
SPECIES_109_NAME    := Mt
SPECIES_110_NAME    := Ds
SPECIES_111_NAME    := Rg
SPECIES_112_NAME    := Cn
SPECIES_113_NAME    := Uut
SPECIES_114_NAME    := Fl
SPECIES_115_NAME    := Uup
SPECIES_116_NAME    := Lv
SPECIES_117_NAME    := Uus
SPECIES_118_NAME    := Uuo
SPECIES_119_NAME    := electron
SPECIES_120_NAME    := user01
SPECIES_121_NAME    := user02
SPECIES_122_NAME    := user03
SPECIES_123_NAME    := user04
SPECIES_124_NAME    := user05
SPECIES_125_NAME    := user06
SPECIES_126_NAME    := user07
SPECIES_127_NAME    := user08
SPECIES_128_NAME    := user09
SPECIES_129_NAME    := user10
SPECIES_130_NAME    := user11
SPECIES_131_NAME    := user12
SPECIES_132_NAME    := user13
SPECIES_133_NAME    := user14
SPECIES_134_NAME    := user15
SPECIES_135_NAME    := user16
SPECIES_136_NAME    := user17
SPECIES_137_NAME    := user18
SPECIES_138_NAME    := user19
SPECIES_139_NAME    := user20
PARAM_FILE_001_NAME := LennardJones612_Universal.params

# APPEND to compiler option flag lists
#FFLAGS   +=
#CFLAGS   +=
#CXXFLAGS +=
#LDFLAGS  +=
#LDLIBS   +=

# load remaining KIM make configuration
include $(KIM_DIR)/$(builddir)/Makefile.ParameterizedModel
