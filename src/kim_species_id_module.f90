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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_species_id_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    electron_id, &
    h_id, &
    he_id, &
    li_id, &
    be_id, &
    b_id, &
    c_id, &
    n_id, &
    o_id, &
    f_id, &
    ne_id, &
    na_id, &
    mg_id, &
    al_id, &
    si_id, &
    p_id, &
    s_id, &
    cl_id, &
    ar_id, &
    k_id, &
    ca_id, &
    sc_id, &
    ti_id, &
    v_id, &
    cr_id, &
    mn_id, &
    fe_id, &
    co_id, &
    ni_id, &
    cu_id, &
    zn_id, &
    ga_id, &
    ge_id, &
    as_id, &
    se_id, &
    br_id, &
    kr_id, &
    rb_id, &
    sr_id, &
    y_id, &
    zr_id, &
    nb_id, &
    mo_id, &
    tc_id, &
    ru_id, &
    rh_id, &
    pd_id, &
    ag_id, &
    cd_id, &
    in_id, &
    sn_id, &
    sb_id, &
    te_id, &
    i_id, &
    xe_id, &
    cs_id, &
    ba_id, &
    la_id, &
    ce_id, &
    pr_id, &
    nd_id, &
    pm_id, &
    sm_id, &
    eu_id, &
    gd_id, &
    tb_id, &
    dy_id, &
    ho_id, &
    er_id, &
    tm_id, &
    yb_id, &
    lu_id, &
    hf_id, &
    ta_id, &
    w_id, &
    re_id, &
    os_id, &
    ir_id, &
    pt_id, &
    au_id, &
    hg_id, &
    tl_id, &
    pb_id, &
    bi_id, &
    po_id, &
    at_id, &
    rn_id, &
    fr_id, &
    ra_id, &
    ac_id, &
    th_id, &
    pa_id, &
    u_id, &
    np_id, &
    pu_id, &
    am_id, &
    cm_id, &
    bk_id, &
    cf_id, &
    es_id, &
    fm_id, &
    md_id, &
    no_id, &
    lr_id, &
    rf_id, &
    db_id, &
    sg_id, &
    bh_id, &
    hs_id, &
    mt_id, &
    ds_id, &
    rg_id, &
    cn_id, &
    uut_id, &
    fl_id, &
    uup_id, &
    lv_id, &
    uus_id, &
    uuo_id, &
    user01_id, &
    user02_id, &
    user03_id, &
    user04_id, &
    user05_id, &
    user06_id, &
    user07_id, &
    user08_id, &
    user09_id, &
    user10_id, &
    user11_id, &
    user12_id, &
    user13_id, &
    user14_id, &
    user15_id, &
    user16_id, &
    user17_id, &
    user18_id, &
    user19_id, &
    user20_id, &
    end_id

  integer(c_int), parameter :: electron_id = 0
  integer(c_int), parameter :: h_id = 1
  integer(c_int), parameter :: he_id = 2
  integer(c_int), parameter :: li_id = 3
  integer(c_int), parameter :: be_id = 4
  integer(c_int), parameter :: b_id = 5
  integer(c_int), parameter :: c_id = 6
  integer(c_int), parameter :: n_id = 7
  integer(c_int), parameter :: o_id = 8
  integer(c_int), parameter :: f_id = 9
  integer(c_int), parameter :: ne_id = 10
  integer(c_int), parameter :: na_id = 11
  integer(c_int), parameter :: mg_id = 12
  integer(c_int), parameter :: al_id = 13
  integer(c_int), parameter :: si_id = 14
  integer(c_int), parameter :: p_id = 15
  integer(c_int), parameter :: s_id = 16
  integer(c_int), parameter :: cl_id = 17
  integer(c_int), parameter :: ar_id = 18
  integer(c_int), parameter :: k_id = 19
  integer(c_int), parameter :: ca_id = 20
  integer(c_int), parameter :: sc_id = 21
  integer(c_int), parameter :: ti_id = 22
  integer(c_int), parameter :: v_id = 23
  integer(c_int), parameter :: cr_id = 24
  integer(c_int), parameter :: mn_id = 25
  integer(c_int), parameter :: fe_id = 26
  integer(c_int), parameter :: co_id = 27
  integer(c_int), parameter :: ni_id = 28
  integer(c_int), parameter :: cu_id = 29
  integer(c_int), parameter :: zn_id = 30
  integer(c_int), parameter :: ga_id = 31
  integer(c_int), parameter :: ge_id = 32
  integer(c_int), parameter :: as_id = 33
  integer(c_int), parameter :: se_id = 34
  integer(c_int), parameter :: br_id = 35
  integer(c_int), parameter :: kr_id = 36
  integer(c_int), parameter :: rb_id = 37
  integer(c_int), parameter :: sr_id = 38
  integer(c_int), parameter :: y_id = 39
  integer(c_int), parameter :: zr_id = 40
  integer(c_int), parameter :: nb_id = 41
  integer(c_int), parameter :: mo_id = 42
  integer(c_int), parameter :: tc_id = 43
  integer(c_int), parameter :: ru_id = 44
  integer(c_int), parameter :: rh_id = 45
  integer(c_int), parameter :: pd_id = 46
  integer(c_int), parameter :: ag_id = 47
  integer(c_int), parameter :: cd_id = 48
  integer(c_int), parameter :: in_id = 49
  integer(c_int), parameter :: sn_id = 50
  integer(c_int), parameter :: sb_id = 51
  integer(c_int), parameter :: te_id = 52
  integer(c_int), parameter :: i_id = 53
  integer(c_int), parameter :: xe_id = 54
  integer(c_int), parameter :: cs_id = 55
  integer(c_int), parameter :: ba_id = 56
  integer(c_int), parameter :: la_id = 57
  integer(c_int), parameter :: ce_id = 58
  integer(c_int), parameter :: pr_id = 59
  integer(c_int), parameter :: nd_id = 60
  integer(c_int), parameter :: pm_id = 61
  integer(c_int), parameter :: sm_id = 62
  integer(c_int), parameter :: eu_id = 63
  integer(c_int), parameter :: gd_id = 64
  integer(c_int), parameter :: tb_id = 65
  integer(c_int), parameter :: dy_id = 66
  integer(c_int), parameter :: ho_id = 67
  integer(c_int), parameter :: er_id = 68
  integer(c_int), parameter :: tm_id = 69
  integer(c_int), parameter :: yb_id = 70
  integer(c_int), parameter :: lu_id = 71
  integer(c_int), parameter :: hf_id = 72
  integer(c_int), parameter :: ta_id = 73
  integer(c_int), parameter :: w_id = 74
  integer(c_int), parameter :: re_id = 75
  integer(c_int), parameter :: os_id = 76
  integer(c_int), parameter :: ir_id = 77
  integer(c_int), parameter :: pt_id = 78
  integer(c_int), parameter :: au_id = 79
  integer(c_int), parameter :: hg_id = 80
  integer(c_int), parameter :: tl_id = 81
  integer(c_int), parameter :: pb_id = 82
  integer(c_int), parameter :: bi_id = 83
  integer(c_int), parameter :: po_id = 84
  integer(c_int), parameter :: at_id = 85
  integer(c_int), parameter :: rn_id = 86
  integer(c_int), parameter :: fr_id = 87
  integer(c_int), parameter :: ra_id = 88
  integer(c_int), parameter :: ac_id = 89
  integer(c_int), parameter :: th_id = 90
  integer(c_int), parameter :: pa_id = 91
  integer(c_int), parameter :: u_id = 92
  integer(c_int), parameter :: np_id = 93
  integer(c_int), parameter :: pu_id = 94
  integer(c_int), parameter :: am_id = 95
  integer(c_int), parameter :: cm_id = 96
  integer(c_int), parameter :: bk_id = 97
  integer(c_int), parameter :: cf_id = 98
  integer(c_int), parameter :: es_id = 99
  integer(c_int), parameter :: fm_id = 100
  integer(c_int), parameter :: md_id = 101
  integer(c_int), parameter :: no_id = 102
  integer(c_int), parameter :: lr_id = 103
  integer(c_int), parameter :: rf_id = 104
  integer(c_int), parameter :: db_id = 105
  integer(c_int), parameter :: sg_id = 106
  integer(c_int), parameter :: bh_id = 107
  integer(c_int), parameter :: hs_id = 108
  integer(c_int), parameter :: mt_id = 109
  integer(c_int), parameter :: ds_id = 110
  integer(c_int), parameter :: rg_id = 111
  integer(c_int), parameter :: cn_id = 112
  integer(c_int), parameter :: uut_id = 113
  integer(c_int), parameter :: fl_id = 114
  integer(c_int), parameter :: uup_id = 115
  integer(c_int), parameter :: lv_id = 116
  integer(c_int), parameter :: uus_id = 117
  integer(c_int), parameter :: uuo_id = 118
  integer(c_int), parameter :: user01_id = 201
  integer(c_int), parameter :: user02_id = 202
  integer(c_int), parameter :: user03_id = 203
  integer(c_int), parameter :: user04_id = 204
  integer(c_int), parameter :: user05_id = 205
  integer(c_int), parameter :: user06_id = 206
  integer(c_int), parameter :: user07_id = 207
  integer(c_int), parameter :: user08_id = 208
  integer(c_int), parameter :: user09_id = 209
  integer(c_int), parameter :: user10_id = 210
  integer(c_int), parameter :: user11_id = 211
  integer(c_int), parameter :: user12_id = 212
  integer(c_int), parameter :: user13_id = 213
  integer(c_int), parameter :: user14_id = 214
  integer(c_int), parameter :: user15_id = 215
  integer(c_int), parameter :: user16_id = 216
  integer(c_int), parameter :: user17_id = 217
  integer(c_int), parameter :: user18_id = 218
  integer(c_int), parameter :: user19_id = 219
  integer(c_int), parameter :: user20_id = 220
  integer(c_int), parameter :: end_id = 32000
end module kim_species_id_module
