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
    kim_species_name_type, &
    kim_species_name_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_species_name_string, &

    kim_species_name_electron, &
    kim_species_name_h, &
    kim_species_name_he, &
    kim_species_name_li, &
    kim_species_name_be, &
    kim_species_name_b, &
    kim_species_name_c, &
    kim_species_name_n, &
    kim_species_name_o, &
    kim_species_name_f, &
    kim_species_name_ne, &
    kim_species_name_na, &
    kim_species_name_mg, &
    kim_species_name_al, &
    kim_species_name_si, &
    kim_species_name_p, &
    kim_species_name_s, &
    kim_species_name_cl, &
    kim_species_name_ar, &
    kim_species_name_k, &
    kim_species_name_ca, &
    kim_species_name_sc, &
    kim_species_name_ti, &
    kim_species_name_v, &
    kim_species_name_cr, &
    kim_species_name_mn, &
    kim_species_name_fe, &
    kim_species_name_co, &
    kim_species_name_ni, &
    kim_species_name_cu, &
    kim_species_name_zn, &
    kim_species_name_ga, &
    kim_species_name_ge, &
    kim_species_name_as, &
    kim_species_name_se, &
    kim_species_name_br, &
    kim_species_name_kr, &
    kim_species_name_rb, &
    kim_species_name_sr, &
    kim_species_name_y, &
    kim_species_name_zr, &
    kim_species_name_nb, &
    kim_species_name_mo, &
    kim_species_name_tc, &
    kim_species_name_ru, &
    kim_species_name_rh, &
    kim_species_name_pd, &
    kim_species_name_ag, &
    kim_species_name_cd, &
    kim_species_name_in, &
    kim_species_name_sn, &
    kim_species_name_sb, &
    kim_species_name_te, &
    kim_species_name_i, &
    kim_species_name_xe, &
    kim_species_name_cs, &
    kim_species_name_ba, &
    kim_species_name_la, &
    kim_species_name_ce, &
    kim_species_name_pr, &
    kim_species_name_nd, &
    kim_species_name_pm, &
    kim_species_name_sm, &
    kim_species_name_eu, &
    kim_species_name_gd, &
    kim_species_name_tb, &
    kim_species_name_dy, &
    kim_species_name_ho, &
    kim_species_name_er, &
    kim_species_name_tm, &
    kim_species_name_yb, &
    kim_species_name_lu, &
    kim_species_name_hf, &
    kim_species_name_ta, &
    kim_species_name_w, &
    kim_species_name_re, &
    kim_species_name_os, &
    kim_species_name_ir, &
    kim_species_name_pt, &
    kim_species_name_au, &
    kim_species_name_hg, &
    kim_species_name_tl, &
    kim_species_name_pb, &
    kim_species_name_bi, &
    kim_species_name_po, &
    kim_species_name_at, &
    kim_species_name_rn, &
    kim_species_name_fr, &
    kim_species_name_ra, &
    kim_species_name_ac, &
    kim_species_name_th, &
    kim_species_name_pa, &
    kim_species_name_u, &
    kim_species_name_np, &
    kim_species_name_pu, &
    kim_species_name_am, &
    kim_species_name_cm, &
    kim_species_name_bk, &
    kim_species_name_cf, &
    kim_species_name_es, &
    kim_species_name_fm, &
    kim_species_name_md, &
    kim_species_name_no, &
    kim_species_name_lr, &
    kim_species_name_rf, &
    kim_species_name_db, &
    kim_species_name_sg, &
    kim_species_name_bh, &
    kim_species_name_hs, &
    kim_species_name_mt, &
    kim_species_name_ds, &
    kim_species_name_rg, &
    kim_species_name_cn, &
    kim_species_name_uut, &
    kim_species_name_fl, &
    kim_species_name_uup, &
    kim_species_name_lv, &
    kim_species_name_uus, &
    kim_species_name_uuo, &
    kim_species_name_user01, &
    kim_species_name_user02, &
    kim_species_name_user03, &
    kim_species_name_user04, &
    kim_species_name_user05, &
    kim_species_name_user06, &
    kim_species_name_user07, &
    kim_species_name_user08, &
    kim_species_name_user09, &
    kim_species_name_user10, &
    kim_species_name_user11, &
    kim_species_name_user12, &
    kim_species_name_user13, &
    kim_species_name_user14, &
    kim_species_name_user15, &
    kim_species_name_user16, &
    kim_species_name_user17, &
    kim_species_name_user18, &
    kim_species_name_user19, &
    kim_species_name_user20, &

    kim_species_name_get_number_of_species, &
    kim_species_name_get_species_name

  type, bind(c) :: kim_species_name_type
    integer(c_int) species_name_id
  end type kim_species_name_type

  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_electron") &
    :: kim_species_name_electron
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_H") &
    :: kim_species_name_h
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_He") &
    :: kim_species_name_he
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Li") &
    :: kim_species_name_li
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Be") &
    :: kim_species_name_be
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_B") &
    :: kim_species_name_b
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_C") &
    :: kim_species_name_c
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_N") &
    :: kim_species_name_n
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_O") &
    :: kim_species_name_o
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_F") &
    :: kim_species_name_f
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ne") &
    :: kim_species_name_ne
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Na") &
    :: kim_species_name_na
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mg") &
    :: kim_species_name_mg
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Al") &
    :: kim_species_name_al
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Si") &
    :: kim_species_name_si
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_P") &
    :: kim_species_name_p
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_S") &
    :: kim_species_name_s
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cl") &
    :: kim_species_name_cl
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ar") &
    :: kim_species_name_ar
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_K") &
    :: kim_species_name_k
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ca") &
    :: kim_species_name_ca
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sc") &
    :: kim_species_name_sc
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ti") &
    :: kim_species_name_ti
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_V") &
    :: kim_species_name_v
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cr") &
    :: kim_species_name_cr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mn") &
    :: kim_species_name_mn
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fe") &
    :: kim_species_name_fe
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Co") &
    :: kim_species_name_co
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ni") &
    :: kim_species_name_ni
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cu") &
    :: kim_species_name_cu
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Zn") &
    :: kim_species_name_zn
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ga") &
    :: kim_species_name_ga
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ge") &
    :: kim_species_name_ge
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_As") &
    :: kim_species_name_as
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Se") &
    :: kim_species_name_se
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Br") &
    :: kim_species_name_br
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Kr") &
    :: kim_species_name_kr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rb") &
    :: kim_species_name_rb
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sr") &
    :: kim_species_name_sr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Y") &
    :: kim_species_name_y
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Zr") &
    :: kim_species_name_zr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Nb") &
    :: kim_species_name_nb
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mo") &
    :: kim_species_name_mo
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tc") &
    :: kim_species_name_tc
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ru") &
    :: kim_species_name_ru
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rh") &
    :: kim_species_name_rh
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pd") &
    :: kim_species_name_pd
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ag") &
    :: kim_species_name_ag
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cd") &
    :: kim_species_name_cd
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_In") &
    :: kim_species_name_in
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sn") &
    :: kim_species_name_sn
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sb") &
    :: kim_species_name_sb
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Te") &
    :: kim_species_name_te
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_I") &
    :: kim_species_name_i
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Xe") &
    :: kim_species_name_xe
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cs") &
    :: kim_species_name_cs
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ba") &
    :: kim_species_name_ba
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_La") &
    :: kim_species_name_la
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ce") &
    :: kim_species_name_ce
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pr") &
    :: kim_species_name_pr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Nd") &
    :: kim_species_name_nd
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pm") &
    :: kim_species_name_pm
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sm") &
    :: kim_species_name_sm
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Eu") &
    :: kim_species_name_eu
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Gd") &
    :: kim_species_name_gd
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tb") &
    :: kim_species_name_tb
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Dy") &
    :: kim_species_name_dy
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ho") &
    :: kim_species_name_ho
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Er") &
    :: kim_species_name_er
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tm") &
    :: kim_species_name_tm
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Yb") &
    :: kim_species_name_yb
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Lu") &
    :: kim_species_name_lu
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Hf") &
    :: kim_species_name_hf
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ta") &
    :: kim_species_name_ta
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_W") &
    :: kim_species_name_w
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Re") &
    :: kim_species_name_re
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Os") &
    :: kim_species_name_os
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ir") &
    :: kim_species_name_ir
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pt") &
    :: kim_species_name_pt
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Au") &
    :: kim_species_name_au
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Hg") &
    :: kim_species_name_hg
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Tl") &
    :: kim_species_name_tl
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pb") &
    :: kim_species_name_pb
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Bi") &
    :: kim_species_name_bi
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Po") &
    :: kim_species_name_po
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_At") &
    :: kim_species_name_at
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rn") &
    :: kim_species_name_rn
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fr") &
    :: kim_species_name_fr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ra") &
    :: kim_species_name_ra
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ac") &
    :: kim_species_name_ac
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Th") &
    :: kim_species_name_th
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pa") &
    :: kim_species_name_pa
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_U") &
    :: kim_species_name_u
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Np") &
    :: kim_species_name_np
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Pu") &
    :: kim_species_name_pu
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Am") &
    :: kim_species_name_am
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cm") &
    :: kim_species_name_cm
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Bk") &
    :: kim_species_name_bk
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cf") &
    :: kim_species_name_cf
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Es") &
    :: kim_species_name_es
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fm") &
    :: kim_species_name_fm
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Md") &
    :: kim_species_name_md
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_No") &
    :: kim_species_name_no
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Lr") &
    :: kim_species_name_lr
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rf") &
    :: kim_species_name_rf
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Db") &
    :: kim_species_name_db
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Sg") &
    :: kim_species_name_sg
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Bh") &
    :: kim_species_name_bh
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Hs") &
    :: kim_species_name_hs
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Mt") &
    :: kim_species_name_mt
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Ds") &
    :: kim_species_name_ds
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Rg") &
    :: kim_species_name_rg
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Cn") &
    :: kim_species_name_cn
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uut") &
    :: kim_species_name_uut
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Fl") &
    :: kim_species_name_fl
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uup") &
    :: kim_species_name_uup
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Lv") &
    :: kim_species_name_lv
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uus") &
    :: kim_species_name_uus
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_Uuo") &
    :: kim_species_name_uuo
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User01") &
    :: kim_species_name_user01
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User02") &
    :: kim_species_name_user02
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User03") &
    :: kim_species_name_user03
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User04") &
    :: kim_species_name_user04
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User05") &
    :: kim_species_name_user05
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User06") &
    :: kim_species_name_user06
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User07") &
    :: kim_species_name_user07
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User08") &
    :: kim_species_name_user08
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User09") &
    :: kim_species_name_user09
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User10") &
    :: kim_species_name_user10
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User11") &
    :: kim_species_name_user11
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User12") &
    :: kim_species_name_user12
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User13") &
    :: kim_species_name_user13
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User14") &
    :: kim_species_name_user14
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User15") &
    :: kim_species_name_user15
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User16") &
    :: kim_species_name_user16
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User17") &
    :: kim_species_name_user17
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User18") &
    :: kim_species_name_user18
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User19") &
    :: kim_species_name_user19
  type(kim_species_name_type), protected, &
    bind(c, name="KIM_SPECIES_NAME_User20") &
    :: kim_species_name_user20

  interface operator (.eq.)
    logical function kim_species_name_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_species_name_type
      implicit none
      type(kim_species_name_type), intent(in) :: left
      type(kim_species_name_type), intent(in) :: right
    end function kim_species_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_species_name_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_species_name_type
      implicit none
      type(kim_species_name_type), intent(in) :: left
      type(kim_species_name_type), intent(in) :: right
    end function kim_species_name_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_species_name_from_string(string, species_name)
      use, intrinsic :: iso_c_binding
      import kim_species_name_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_species_name_type), intent(out) :: species_name
    end subroutine kim_species_name_from_string

    subroutine kim_species_name_string(species_name, string)
      import kim_species_name_type
      implicit none
      type(kim_species_name_type), intent(in), value :: species_name
      character(len=*), intent(out) :: string
    end subroutine kim_species_name_string

    subroutine kim_species_name_get_number_of_species(&
      number_of_species)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_species
    end subroutine kim_species_name_get_number_of_species

    subroutine kim_species_name_get_species_name(index, species_name, ierr)
      use, intrinsic :: iso_c_binding
      import kim_species_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_species_name_get_species_name
  end interface
end module kim_species_name_module
