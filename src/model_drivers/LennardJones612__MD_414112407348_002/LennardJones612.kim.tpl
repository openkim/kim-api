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

################################################################################
#
# See src/standard.kim for documentation about this file
#
################################################################################


KIM_API_Version := 1.6.3

Unit_Handling    := flexible
Unit_length      := A
Unit_energy      := eV
Unit_charge      := e
Unit_temperature := K
Unit_time        := ps


################################################################################
PARTICLE_SPECIES:
# Symbol/name               Type                    code

SPECIES_001_NAME_STR                          spec                     0
SPECIES_002_NAME_STR                          spec                     1
SPECIES_003_NAME_STR                          spec                     2
SPECIES_004_NAME_STR                          spec                     3
SPECIES_005_NAME_STR                          spec                     4
SPECIES_006_NAME_STR                          spec                     5
SPECIES_007_NAME_STR                          spec                     6
SPECIES_008_NAME_STR                          spec                     7
SPECIES_009_NAME_STR                          spec                     8
SPECIES_010_NAME_STR                          spec                     9
SPECIES_011_NAME_STR                          spec                    10
SPECIES_012_NAME_STR                          spec                    11
SPECIES_013_NAME_STR                          spec                    12
SPECIES_014_NAME_STR                          spec                    13
SPECIES_015_NAME_STR                          spec                    14
SPECIES_016_NAME_STR                          spec                    15
SPECIES_017_NAME_STR                          spec                    16
SPECIES_018_NAME_STR                          spec                    17
SPECIES_019_NAME_STR                          spec                    18
SPECIES_020_NAME_STR                          spec                    19
SPECIES_021_NAME_STR                          spec                    20
SPECIES_022_NAME_STR                          spec                    21
SPECIES_023_NAME_STR                          spec                    22
SPECIES_024_NAME_STR                          spec                    23
SPECIES_025_NAME_STR                          spec                    24
SPECIES_026_NAME_STR                          spec                    25
SPECIES_027_NAME_STR                          spec                    26
SPECIES_028_NAME_STR                          spec                    27
SPECIES_029_NAME_STR                          spec                    28
SPECIES_030_NAME_STR                          spec                    29
SPECIES_031_NAME_STR                          spec                    30
SPECIES_032_NAME_STR                          spec                    31
SPECIES_033_NAME_STR                          spec                    32
SPECIES_034_NAME_STR                          spec                    33
SPECIES_035_NAME_STR                          spec                    34
SPECIES_036_NAME_STR                          spec                    35
SPECIES_037_NAME_STR                          spec                    36
SPECIES_038_NAME_STR                          spec                    37
SPECIES_039_NAME_STR                          spec                    38
SPECIES_040_NAME_STR                          spec                    39
SPECIES_041_NAME_STR                          spec                    40
SPECIES_042_NAME_STR                          spec                    41
SPECIES_043_NAME_STR                          spec                    42
SPECIES_044_NAME_STR                          spec                    43
SPECIES_045_NAME_STR                          spec                    44
SPECIES_046_NAME_STR                          spec                    45
SPECIES_047_NAME_STR                          spec                    46
SPECIES_048_NAME_STR                          spec                    47
SPECIES_049_NAME_STR                          spec                    48
SPECIES_050_NAME_STR                          spec                    49
SPECIES_051_NAME_STR                          spec                    50
SPECIES_052_NAME_STR                          spec                    51
SPECIES_053_NAME_STR                          spec                    52
SPECIES_054_NAME_STR                          spec                    53
SPECIES_055_NAME_STR                          spec                    54
SPECIES_056_NAME_STR                          spec                    55
SPECIES_057_NAME_STR                          spec                    56
SPECIES_058_NAME_STR                          spec                    57
SPECIES_059_NAME_STR                          spec                    58
SPECIES_060_NAME_STR                          spec                    59
SPECIES_061_NAME_STR                          spec                    60
SPECIES_062_NAME_STR                          spec                    61
SPECIES_063_NAME_STR                          spec                    62
SPECIES_064_NAME_STR                          spec                    63
SPECIES_065_NAME_STR                          spec                    64
SPECIES_066_NAME_STR                          spec                    65
SPECIES_067_NAME_STR                          spec                    66
SPECIES_068_NAME_STR                          spec                    67
SPECIES_069_NAME_STR                          spec                    68
SPECIES_070_NAME_STR                          spec                    69
SPECIES_071_NAME_STR                          spec                    70
SPECIES_072_NAME_STR                          spec                    71
SPECIES_073_NAME_STR                          spec                    72
SPECIES_074_NAME_STR                          spec                    73
SPECIES_075_NAME_STR                          spec                    74
SPECIES_076_NAME_STR                          spec                    75
SPECIES_077_NAME_STR                          spec                    76
SPECIES_078_NAME_STR                          spec                    77
SPECIES_079_NAME_STR                          spec                    78
SPECIES_080_NAME_STR                          spec                    79
SPECIES_081_NAME_STR                          spec                    80
SPECIES_082_NAME_STR                          spec                    81
SPECIES_083_NAME_STR                          spec                    82
SPECIES_084_NAME_STR                          spec                    83
SPECIES_085_NAME_STR                          spec                    84
SPECIES_086_NAME_STR                          spec                    85
SPECIES_087_NAME_STR                          spec                    86
SPECIES_088_NAME_STR                          spec                    87
SPECIES_089_NAME_STR                          spec                    88
SPECIES_090_NAME_STR                          spec                    89
SPECIES_091_NAME_STR                          spec                    90
SPECIES_092_NAME_STR                          spec                    91
SPECIES_093_NAME_STR                          spec                    92
SPECIES_094_NAME_STR                          spec                    93
SPECIES_095_NAME_STR                          spec                    94
SPECIES_096_NAME_STR                          spec                    95
SPECIES_097_NAME_STR                          spec                    96
SPECIES_098_NAME_STR                          spec                    97
SPECIES_099_NAME_STR                          spec                    98
SPECIES_100_NAME_STR                          spec                    99
SPECIES_101_NAME_STR                          spec                   100
SPECIES_102_NAME_STR                          spec                   101
SPECIES_103_NAME_STR                          spec                   102
SPECIES_104_NAME_STR                          spec                   103
SPECIES_105_NAME_STR                          spec                   104
SPECIES_106_NAME_STR                          spec                   105
SPECIES_107_NAME_STR                          spec                   106
SPECIES_108_NAME_STR                          spec                   107
SPECIES_109_NAME_STR                          spec                   108
SPECIES_110_NAME_STR                          spec                   109
SPECIES_111_NAME_STR                          spec                   110
SPECIES_112_NAME_STR                          spec                   111
SPECIES_113_NAME_STR                          spec                   112
SPECIES_114_NAME_STR                          spec                   113
SPECIES_115_NAME_STR                          spec                   114
SPECIES_116_NAME_STR                          spec                   115
SPECIES_117_NAME_STR                          spec                   116
SPECIES_118_NAME_STR                          spec                   117
SPECIES_119_NAME_STR                          spec                   118
SPECIES_120_NAME_STR                          spec                   119
SPECIES_121_NAME_STR                          spec                   120
SPECIES_122_NAME_STR                          spec                   121
SPECIES_123_NAME_STR                          spec                   122
SPECIES_124_NAME_STR                          spec                   123
SPECIES_125_NAME_STR                          spec                   124
SPECIES_126_NAME_STR                          spec                   125
SPECIES_127_NAME_STR                          spec                   126
SPECIES_128_NAME_STR                          spec                   127
SPECIES_129_NAME_STR                          spec                   128
SPECIES_130_NAME_STR                          spec                   129
SPECIES_131_NAME_STR                          spec                   130
SPECIES_132_NAME_STR                          spec                   131
SPECIES_133_NAME_STR                          spec                   132
SPECIES_134_NAME_STR                          spec                   133
SPECIES_135_NAME_STR                          spec                   134
SPECIES_136_NAME_STR                          spec                   135
SPECIES_137_NAME_STR                          spec                   136
SPECIES_138_NAME_STR                          spec                   137
SPECIES_139_NAME_STR                          spec                   138


################################################################################
CONVENTIONS:
# Name                      Type

ZeroBasedLists              flag

Neigh_IterAccess            flag

Neigh_LocaAccess            flag

NEIGH_RVEC_H                flag

NEIGH_PURE_H                flag

NEIGH_RVEC_F                flag

NEIGH_PURE_F                flag

MI_OPBC_H                   flag

MI_OPBC_F                   flag

CLUSTER                     flag


################################################################################
MODEL_INPUT:
# Name                      Type         Unit                Shape              Requirements

numberOfParticles           integer      none                []

numberContributingParticles integer      none                []                 optional

numberOfSpecies             integer      none                []

particleSpecies             integer      none                [numberOfParticles]

coordinates                 double       length              [numberOfParticles,3]

boxSideLengths              double       length              [3]                optional

get_neigh                   method       none                []                 optional

neighObject                 pointer      none                []                 optional

process_dEdr                method       none                []                 optional

process_d2Edr2              method       none                []                 optional


################################################################################
MODEL_OUTPUT:
# Name                      Type         Unit                Shape              Requirements

destroy                     method       none                []

compute                     method       none                []

reinit                      method       none                []                 optional

cutoff                      double       length              []

energy                      double       energy              []                 optional

forces                      double       force               [numberOfParticles,3]  optional

particleEnergy              double       energy              [numberOfParticles]    optional


################################################################################
MODEL_PARAMETERS:
# Name                      Type         Unit                Shape              Requirements

PARAM_FREE_shift            integer      none                []

PARAM_FREE_cutoffs          double       length              [:]  # upper triangular row-based arrangement

PARAM_FREE_epsilons         double       energy              [:]  # upper triangular row-based arrangement

PARAM_FREE_sigmas           double       length              [:]  # upper triangular row-based arrangement
