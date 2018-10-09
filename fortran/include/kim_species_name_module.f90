!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2016--2018, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_species_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_species_name_type, &


    ! Constants
    KIM_SPECIES_NAME_ELECTRON, &
    KIM_SPECIES_NAME_H, &
    KIM_SPECIES_NAME_HE, &
    KIM_SPECIES_NAME_LI, &
    KIM_SPECIES_NAME_BE, &
    KIM_SPECIES_NAME_B, &
    KIM_SPECIES_NAME_C, &
    KIM_SPECIES_NAME_N, &
    KIM_SPECIES_NAME_O, &
    KIM_SPECIES_NAME_F, &
    KIM_SPECIES_NAME_NE, &
    KIM_SPECIES_NAME_NA, &
    KIM_SPECIES_NAME_MG, &
    KIM_SPECIES_NAME_AL, &
    KIM_SPECIES_NAME_SI, &
    KIM_SPECIES_NAME_P, &
    KIM_SPECIES_NAME_S, &
    KIM_SPECIES_NAME_CL, &
    KIM_SPECIES_NAME_AR, &
    KIM_SPECIES_NAME_K, &
    KIM_SPECIES_NAME_CA, &
    KIM_SPECIES_NAME_SC, &
    KIM_SPECIES_NAME_TI, &
    KIM_SPECIES_NAME_V, &
    KIM_SPECIES_NAME_CR, &
    KIM_SPECIES_NAME_MN, &
    KIM_SPECIES_NAME_FE, &
    KIM_SPECIES_NAME_CO, &
    KIM_SPECIES_NAME_NI, &
    KIM_SPECIES_NAME_CU, &
    KIM_SPECIES_NAME_ZN, &
    KIM_SPECIES_NAME_GA, &
    KIM_SPECIES_NAME_GE, &
    KIM_SPECIES_NAME_AS, &
    KIM_SPECIES_NAME_SE, &
    KIM_SPECIES_NAME_BR, &
    KIM_SPECIES_NAME_KR, &
    KIM_SPECIES_NAME_RB, &
    KIM_SPECIES_NAME_SR, &
    KIM_SPECIES_NAME_Y, &
    KIM_SPECIES_NAME_ZR, &
    KIM_SPECIES_NAME_NB, &
    KIM_SPECIES_NAME_MO, &
    KIM_SPECIES_NAME_TC, &
    KIM_SPECIES_NAME_RU, &
    KIM_SPECIES_NAME_RH, &
    KIM_SPECIES_NAME_PD, &
    KIM_SPECIES_NAME_AG, &
    KIM_SPECIES_NAME_CD, &
    KIM_SPECIES_NAME_IN, &
    KIM_SPECIES_NAME_SN, &
    KIM_SPECIES_NAME_SB, &
    KIM_SPECIES_NAME_TE, &
    KIM_SPECIES_NAME_I, &
    KIM_SPECIES_NAME_XE, &
    KIM_SPECIES_NAME_CS, &
    KIM_SPECIES_NAME_BA, &
    KIM_SPECIES_NAME_LA, &
    KIM_SPECIES_NAME_CE, &
    KIM_SPECIES_NAME_PR, &
    KIM_SPECIES_NAME_ND, &
    KIM_SPECIES_NAME_PM, &
    KIM_SPECIES_NAME_SM, &
    KIM_SPECIES_NAME_EU, &
    KIM_SPECIES_NAME_GD, &
    KIM_SPECIES_NAME_TB, &
    KIM_SPECIES_NAME_DY, &
    KIM_SPECIES_NAME_HO, &
    KIM_SPECIES_NAME_ER, &
    KIM_SPECIES_NAME_TM, &
    KIM_SPECIES_NAME_YB, &
    KIM_SPECIES_NAME_LU, &
    KIM_SPECIES_NAME_HF, &
    KIM_SPECIES_NAME_TA, &
    KIM_SPECIES_NAME_W, &
    KIM_SPECIES_NAME_RE, &
    KIM_SPECIES_NAME_OS, &
    KIM_SPECIES_NAME_IR, &
    KIM_SPECIES_NAME_PT, &
    KIM_SPECIES_NAME_AU, &
    KIM_SPECIES_NAME_HG, &
    KIM_SPECIES_NAME_TL, &
    KIM_SPECIES_NAME_PB, &
    KIM_SPECIES_NAME_BI, &
    KIM_SPECIES_NAME_PO, &
    KIM_SPECIES_NAME_AT, &
    KIM_SPECIES_NAME_RN, &
    KIM_SPECIES_NAME_FR, &
    KIM_SPECIES_NAME_RA, &
    KIM_SPECIES_NAME_AC, &
    KIM_SPECIES_NAME_TH, &
    KIM_SPECIES_NAME_PA, &
    KIM_SPECIES_NAME_U, &
    KIM_SPECIES_NAME_NP, &
    KIM_SPECIES_NAME_PU, &
    KIM_SPECIES_NAME_AM, &
    KIM_SPECIES_NAME_CM, &
    KIM_SPECIES_NAME_BK, &
    KIM_SPECIES_NAME_CF, &
    KIM_SPECIES_NAME_ES, &
    KIM_SPECIES_NAME_FM, &
    KIM_SPECIES_NAME_MD, &
    KIM_SPECIES_NAME_NO, &
    KIM_SPECIES_NAME_LR, &
    KIM_SPECIES_NAME_RF, &
    KIM_SPECIES_NAME_DB, &
    KIM_SPECIES_NAME_SG, &
    KIM_SPECIES_NAME_BH, &
    KIM_SPECIES_NAME_HS, &
    KIM_SPECIES_NAME_MT, &
    KIM_SPECIES_NAME_DS, &
    KIM_SPECIES_NAME_RG, &
    KIM_SPECIES_NAME_CN, &
    KIM_SPECIES_NAME_UUT, &
    KIM_SPECIES_NAME_FL, &
    KIM_SPECIES_NAME_UUP, &
    KIM_SPECIES_NAME_LV, &
    KIM_SPECIES_NAME_UUS, &
    KIM_SPECIES_NAME_UUO, &
    KIM_SPECIES_NAME_USER01, &
    KIM_SPECIES_NAME_USER02, &
    KIM_SPECIES_NAME_USER03, &
    KIM_SPECIES_NAME_USER04, &
    KIM_SPECIES_NAME_USER05, &
    KIM_SPECIES_NAME_USER06, &
    KIM_SPECIES_NAME_USER07, &
    KIM_SPECIES_NAME_USER08, &
    KIM_SPECIES_NAME_USER09, &
    KIM_SPECIES_NAME_USER10, &
    KIM_SPECIES_NAME_USER11, &
    KIM_SPECIES_NAME_USER12, &
    KIM_SPECIES_NAME_USER13, &
    KIM_SPECIES_NAME_USER14, &
    KIM_SPECIES_NAME_USER15, &
    KIM_SPECIES_NAME_USER16, &
    KIM_SPECIES_NAME_USER17, &
    KIM_SPECIES_NAME_USER18, &
    KIM_SPECIES_NAME_USER19, &
    KIM_SPECIES_NAME_USER20, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_species_names, &
    kim_get_species_name


  type, bind(c) :: kim_species_name_type
    integer(c_int) species_name_id
  end type kim_species_name_type

  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_electron") &
    :: KIM_SPECIES_NAME_ELECTRON
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_H") &
    :: KIM_SPECIES_NAME_H
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_He") &
    :: KIM_SPECIES_NAME_HE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Li") &
    :: KIM_SPECIES_NAME_LI
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Be") &
    :: KIM_SPECIES_NAME_BE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_B") &
    :: KIM_SPECIES_NAME_B
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_C") &
    :: KIM_SPECIES_NAME_C
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_N") &
    :: KIM_SPECIES_NAME_N
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_O") &
    :: KIM_SPECIES_NAME_O
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_F") &
    :: KIM_SPECIES_NAME_F
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ne") &
    :: KIM_SPECIES_NAME_NE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Na") &
    :: KIM_SPECIES_NAME_NA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mg") &
    :: KIM_SPECIES_NAME_MG
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Al") &
    :: KIM_SPECIES_NAME_AL
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Si") &
    :: KIM_SPECIES_NAME_SI
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_P") &
    :: KIM_SPECIES_NAME_P
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_S") &
    :: KIM_SPECIES_NAME_S
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cl") &
    :: KIM_SPECIES_NAME_CL
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ar") &
    :: KIM_SPECIES_NAME_AR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_K") &
    :: KIM_SPECIES_NAME_K
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ca") &
    :: KIM_SPECIES_NAME_CA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sc") &
    :: KIM_SPECIES_NAME_SC
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ti") &
    :: KIM_SPECIES_NAME_TI
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_V") &
    :: KIM_SPECIES_NAME_V
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cr") &
    :: KIM_SPECIES_NAME_CR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mn") &
    :: KIM_SPECIES_NAME_MN
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fe") &
    :: KIM_SPECIES_NAME_FE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Co") &
    :: KIM_SPECIES_NAME_CO
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ni") &
    :: KIM_SPECIES_NAME_NI
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cu") &
    :: KIM_SPECIES_NAME_CU
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Zn") &
    :: KIM_SPECIES_NAME_ZN
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ga") &
    :: KIM_SPECIES_NAME_GA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ge") &
    :: KIM_SPECIES_NAME_GE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_As") &
    :: KIM_SPECIES_NAME_AS
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Se") &
    :: KIM_SPECIES_NAME_SE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Br") &
    :: KIM_SPECIES_NAME_BR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Kr") &
    :: KIM_SPECIES_NAME_KR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rb") &
    :: KIM_SPECIES_NAME_RB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sr") &
    :: KIM_SPECIES_NAME_SR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Y") &
    :: KIM_SPECIES_NAME_Y
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Zr") &
    :: KIM_SPECIES_NAME_ZR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Nb") &
    :: KIM_SPECIES_NAME_NB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mo") &
    :: KIM_SPECIES_NAME_MO
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tc") &
    :: KIM_SPECIES_NAME_TC
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ru") &
    :: KIM_SPECIES_NAME_RU
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rh") &
    :: KIM_SPECIES_NAME_RH
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pd") &
    :: KIM_SPECIES_NAME_PD
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ag") &
    :: KIM_SPECIES_NAME_AG
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cd") &
    :: KIM_SPECIES_NAME_CD
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_In") &
    :: KIM_SPECIES_NAME_IN
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sn") &
    :: KIM_SPECIES_NAME_SN
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sb") &
    :: KIM_SPECIES_NAME_SB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Te") &
    :: KIM_SPECIES_NAME_TE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_I") &
    :: KIM_SPECIES_NAME_I
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Xe") &
    :: KIM_SPECIES_NAME_XE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cs") &
    :: KIM_SPECIES_NAME_CS
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ba") &
    :: KIM_SPECIES_NAME_BA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_La") &
    :: KIM_SPECIES_NAME_LA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ce") &
    :: KIM_SPECIES_NAME_CE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pr") &
    :: KIM_SPECIES_NAME_PR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Nd") &
    :: KIM_SPECIES_NAME_ND
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pm") &
    :: KIM_SPECIES_NAME_PM
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sm") &
    :: KIM_SPECIES_NAME_SM
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Eu") &
    :: KIM_SPECIES_NAME_EU
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Gd") &
    :: KIM_SPECIES_NAME_GD
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tb") &
    :: KIM_SPECIES_NAME_TB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Dy") &
    :: KIM_SPECIES_NAME_DY
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ho") &
    :: KIM_SPECIES_NAME_HO
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Er") &
    :: KIM_SPECIES_NAME_ER
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tm") &
    :: KIM_SPECIES_NAME_TM
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Yb") &
    :: KIM_SPECIES_NAME_YB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Lu") &
    :: KIM_SPECIES_NAME_LU
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Hf") &
    :: KIM_SPECIES_NAME_HF
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ta") &
    :: KIM_SPECIES_NAME_TA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_W") &
    :: KIM_SPECIES_NAME_W
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Re") &
    :: KIM_SPECIES_NAME_RE
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Os") &
    :: KIM_SPECIES_NAME_OS
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ir") &
    :: KIM_SPECIES_NAME_IR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pt") &
    :: KIM_SPECIES_NAME_PT
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Au") &
    :: KIM_SPECIES_NAME_AU
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Hg") &
    :: KIM_SPECIES_NAME_HG
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tl") &
    :: KIM_SPECIES_NAME_TL
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pb") &
    :: KIM_SPECIES_NAME_PB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Bi") &
    :: KIM_SPECIES_NAME_BI
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Po") &
    :: KIM_SPECIES_NAME_PO
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_At") &
    :: KIM_SPECIES_NAME_AT
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rn") &
    :: KIM_SPECIES_NAME_RN
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fr") &
    :: KIM_SPECIES_NAME_FR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ra") &
    :: KIM_SPECIES_NAME_RA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ac") &
    :: KIM_SPECIES_NAME_AC
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Th") &
    :: KIM_SPECIES_NAME_TH
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pa") &
    :: KIM_SPECIES_NAME_PA
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_U") &
    :: KIM_SPECIES_NAME_U
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Np") &
    :: KIM_SPECIES_NAME_NP
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pu") &
    :: KIM_SPECIES_NAME_PU
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Am") &
    :: KIM_SPECIES_NAME_AM
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cm") &
    :: KIM_SPECIES_NAME_CM
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Bk") &
    :: KIM_SPECIES_NAME_BK
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cf") &
    :: KIM_SPECIES_NAME_CF
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Es") &
    :: KIM_SPECIES_NAME_ES
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fm") &
    :: KIM_SPECIES_NAME_FM
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Md") &
    :: KIM_SPECIES_NAME_MD
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_No") &
    :: KIM_SPECIES_NAME_NO
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Lr") &
    :: KIM_SPECIES_NAME_LR
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rf") &
    :: KIM_SPECIES_NAME_RF
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Db") &
    :: KIM_SPECIES_NAME_DB
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sg") &
    :: KIM_SPECIES_NAME_SG
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Bh") &
    :: KIM_SPECIES_NAME_BH
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Hs") &
    :: KIM_SPECIES_NAME_HS
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mt") &
    :: KIM_SPECIES_NAME_MT
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ds") &
    :: KIM_SPECIES_NAME_DS
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rg") &
    :: KIM_SPECIES_NAME_RG
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cn") &
    :: KIM_SPECIES_NAME_CN
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uut") &
    :: KIM_SPECIES_NAME_UUT
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fl") &
    :: KIM_SPECIES_NAME_FL
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uup") &
    :: KIM_SPECIES_NAME_UUP
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Lv") &
    :: KIM_SPECIES_NAME_LV
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uus") &
    :: KIM_SPECIES_NAME_UUS
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uuo") &
    :: KIM_SPECIES_NAME_UUO
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User01") &
    :: KIM_SPECIES_NAME_USER01
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User02") &
    :: KIM_SPECIES_NAME_USER02
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User03") &
    :: KIM_SPECIES_NAME_USER03
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User04") &
    :: KIM_SPECIES_NAME_USER04
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User05") &
    :: KIM_SPECIES_NAME_USER05
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User06") &
    :: KIM_SPECIES_NAME_USER06
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User07") &
    :: KIM_SPECIES_NAME_USER07
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User08") &
    :: KIM_SPECIES_NAME_USER08
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User09") &
    :: KIM_SPECIES_NAME_USER09
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User10") &
    :: KIM_SPECIES_NAME_USER10
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User11") &
    :: KIM_SPECIES_NAME_USER11
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User12") &
    :: KIM_SPECIES_NAME_USER12
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User13") &
    :: KIM_SPECIES_NAME_USER13
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User14") &
    :: KIM_SPECIES_NAME_USER14
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User15") &
    :: KIM_SPECIES_NAME_USER15
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User16") &
    :: KIM_SPECIES_NAME_USER16
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User17") &
    :: KIM_SPECIES_NAME_USER17
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User18") &
    :: KIM_SPECIES_NAME_USER18
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User19") &
    :: KIM_SPECIES_NAME_USER19
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User20") &
    :: KIM_SPECIES_NAME_USER20

  interface operator (.eq.)
    module procedure kim_species_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_species_name_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_species_name_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_species_name_to_string
  end interface kim_to_string

contains
  logical function kim_species_name_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_species_name_type), intent(in) :: left
    type(kim_species_name_type), intent(in) :: right

    kim_species_name_equal &
      = (left%species_name_id .eq. right%species_name_id)
  end function kim_species_name_equal

  logical function kim_species_name_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_species_name_type), intent(in) :: left
    type(kim_species_name_type), intent(in) :: right

    kim_species_name_not_equal = .not. (left .eq. right)
  end function kim_species_name_not_equal

  subroutine kim_species_name_from_string(string, species_name)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      type(kim_species_name_type) function from_string(string) &
        bind(c, name="KIM_SpeciesName_FromString")
        use, intrinsic :: iso_c_binding
        import kim_species_name_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_species_name_type), intent(out) :: species_name

    species_name = from_string(trim(string)//c_null_char)
  end subroutine kim_species_name_from_string

  subroutine kim_species_name_to_string(species_name, string)
    use, intrinsic :: iso_c_binding
    use kim_convert_string_module, only : kim_convert_string
    implicit none
    interface
      type(c_ptr) function get_string(species_name) &
        bind(c, name="KIM_SpeciesName_ToString")
        use, intrinsic :: iso_c_binding
        import kim_species_name_type
        implicit none
        type(kim_species_name_type), intent(in), value :: species_name
      end function get_string
    end interface
    type(kim_species_name_type), intent(in), value :: species_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(species_name)
    if (c_associated(p)) then
      call kim_convert_string(p, string)
    else
      string = ""
    end if
  end subroutine kim_species_name_to_string

  subroutine kim_get_number_of_species_names(number_of_species_names)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine get_number_of_species_names(number_of_species_names) &
        bind(c, name="KIM_SPECIES_NAME_GetNumberOfSpeciesNames")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_species_names
      end subroutine get_number_of_species_names
    end interface
    integer(c_int), intent(out) :: number_of_species_names

    call get_number_of_species_names(number_of_species_names)
  end subroutine kim_get_number_of_species_names

  subroutine kim_get_species_name(index, species_name, ierr)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      integer(c_int) function get_species_name(index, species_name) &
        bind(c, name="KIM_SPECIES_NAME_GetSpeciesName")
        use, intrinsic :: iso_c_binding
        import kim_species_name_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_species_name_type), intent(out) :: species_name
      end function get_species_name
    end interface
    integer(c_int), intent(in), value :: index
    type(kim_species_name_type), intent(out) :: species_name
    integer(c_int), intent(out) :: ierr

    ierr = get_species_name(index-1, species_name)
  end subroutine kim_get_species_name
end module kim_species_name_module
