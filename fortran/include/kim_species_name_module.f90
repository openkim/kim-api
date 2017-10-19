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


module kim_species_name_module
  use, intrinsic :: iso_c_binding
  use kim_species_name_id_module
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

  type(kim_species_name_type), parameter :: kim_species_name_electron = &
    kim_species_name_type(electron_id)
  type(kim_species_name_type), parameter :: kim_species_name_h = &
    kim_species_name_type(h_id)
  type(kim_species_name_type), parameter :: kim_species_name_he = &
    kim_species_name_type(he_id)
  type(kim_species_name_type), parameter :: kim_species_name_li = &
    kim_species_name_type(li_id)
  type(kim_species_name_type), parameter :: kim_species_name_be = &
    kim_species_name_type(be_id)
  type(kim_species_name_type), parameter :: kim_species_name_b = &
    kim_species_name_type(b_id)
  type(kim_species_name_type), parameter :: kim_species_name_c = &
    kim_species_name_type(c_id)
  type(kim_species_name_type), parameter :: kim_species_name_n = &
    kim_species_name_type(n_id)
  type(kim_species_name_type), parameter :: kim_species_name_o = &
    kim_species_name_type(o_id)
  type(kim_species_name_type), parameter :: kim_species_name_f = &
    kim_species_name_type(f_id)
  type(kim_species_name_type), parameter :: kim_species_name_ne = &
    kim_species_name_type(ne_id)
  type(kim_species_name_type), parameter :: kim_species_name_na = &
    kim_species_name_type(na_id)
  type(kim_species_name_type), parameter :: kim_species_name_mg = &
    kim_species_name_type(mg_id)
  type(kim_species_name_type), parameter :: kim_species_name_al = &
    kim_species_name_type(al_id)
  type(kim_species_name_type), parameter :: kim_species_name_si = &
    kim_species_name_type(si_id)
  type(kim_species_name_type), parameter :: kim_species_name_p = &
    kim_species_name_type(p_id)
  type(kim_species_name_type), parameter :: kim_species_name_s = &
    kim_species_name_type(s_id)
  type(kim_species_name_type), parameter :: kim_species_name_cl = &
    kim_species_name_type(cl_id)
  type(kim_species_name_type), parameter :: kim_species_name_ar = &
    kim_species_name_type(ar_id)
  type(kim_species_name_type), parameter :: kim_species_name_k = &
    kim_species_name_type(k_id)
  type(kim_species_name_type), parameter :: kim_species_name_ca = &
    kim_species_name_type(ca_id)
  type(kim_species_name_type), parameter :: kim_species_name_sc = &
    kim_species_name_type(sc_id)
  type(kim_species_name_type), parameter :: kim_species_name_ti = &
    kim_species_name_type(ti_id)
  type(kim_species_name_type), parameter :: kim_species_name_v = &
    kim_species_name_type(v_id)
  type(kim_species_name_type), parameter :: kim_species_name_cr = &
    kim_species_name_type(cr_id)
  type(kim_species_name_type), parameter :: kim_species_name_mn = &
    kim_species_name_type(mn_id)
  type(kim_species_name_type), parameter :: kim_species_name_fe = &
    kim_species_name_type(fe_id)
  type(kim_species_name_type), parameter :: kim_species_name_co = &
    kim_species_name_type(co_id)
  type(kim_species_name_type), parameter :: kim_species_name_ni = &
    kim_species_name_type(ni_id)
  type(kim_species_name_type), parameter :: kim_species_name_cu = &
    kim_species_name_type(cu_id)
  type(kim_species_name_type), parameter :: kim_species_name_zn = &
    kim_species_name_type(zn_id)
  type(kim_species_name_type), parameter :: kim_species_name_ga = &
    kim_species_name_type(ga_id)
  type(kim_species_name_type), parameter :: kim_species_name_ge = &
    kim_species_name_type(ge_id)
  type(kim_species_name_type), parameter :: kim_species_name_as = &
    kim_species_name_type(as_id)
  type(kim_species_name_type), parameter :: kim_species_name_se = &
    kim_species_name_type(se_id)
  type(kim_species_name_type), parameter :: kim_species_name_br = &
    kim_species_name_type(br_id)
  type(kim_species_name_type), parameter :: kim_species_name_kr = &
    kim_species_name_type(kr_id)
  type(kim_species_name_type), parameter :: kim_species_name_rb = &
    kim_species_name_type(rb_id)
  type(kim_species_name_type), parameter :: kim_species_name_sr = &
    kim_species_name_type(sr_id)
  type(kim_species_name_type), parameter :: kim_species_name_y = &
    kim_species_name_type(y_id)
  type(kim_species_name_type), parameter :: kim_species_name_zr = &
    kim_species_name_type(zr_id)
  type(kim_species_name_type), parameter :: kim_species_name_nb = &
    kim_species_name_type(nb_id)
  type(kim_species_name_type), parameter :: kim_species_name_mo = &
    kim_species_name_type(mo_id)
  type(kim_species_name_type), parameter :: kim_species_name_tc = &
    kim_species_name_type(tc_id)
  type(kim_species_name_type), parameter :: kim_species_name_ru = &
    kim_species_name_type(ru_id)
  type(kim_species_name_type), parameter :: kim_species_name_rh = &
    kim_species_name_type(rh_id)
  type(kim_species_name_type), parameter :: kim_species_name_pd = &
    kim_species_name_type(pd_id)
  type(kim_species_name_type), parameter :: kim_species_name_ag = &
    kim_species_name_type(ag_id)
  type(kim_species_name_type), parameter :: kim_species_name_cd = &
    kim_species_name_type(cd_id)
  type(kim_species_name_type), parameter :: kim_species_name_in = &
    kim_species_name_type(in_id)
  type(kim_species_name_type), parameter :: kim_species_name_sn = &
    kim_species_name_type(sn_id)
  type(kim_species_name_type), parameter :: kim_species_name_sb = &
    kim_species_name_type(sb_id)
  type(kim_species_name_type), parameter :: kim_species_name_te = &
    kim_species_name_type(te_id)
  type(kim_species_name_type), parameter :: kim_species_name_i = &
    kim_species_name_type(i_id)
  type(kim_species_name_type), parameter :: kim_species_name_xe = &
    kim_species_name_type(xe_id)
  type(kim_species_name_type), parameter :: kim_species_name_cs = &
    kim_species_name_type(cs_id)
  type(kim_species_name_type), parameter :: kim_species_name_ba = &
    kim_species_name_type(ba_id)
  type(kim_species_name_type), parameter :: kim_species_name_la = &
    kim_species_name_type(la_id)
  type(kim_species_name_type), parameter :: kim_species_name_ce = &
    kim_species_name_type(ce_id)
  type(kim_species_name_type), parameter :: kim_species_name_pr = &
    kim_species_name_type(pr_id)
  type(kim_species_name_type), parameter :: kim_species_name_nd = &
    kim_species_name_type(nd_id)
  type(kim_species_name_type), parameter :: kim_species_name_pm = &
    kim_species_name_type(pm_id)
  type(kim_species_name_type), parameter :: kim_species_name_sm = &
    kim_species_name_type(sm_id)
  type(kim_species_name_type), parameter :: kim_species_name_eu = &
    kim_species_name_type(eu_id)
  type(kim_species_name_type), parameter :: kim_species_name_gd = &
    kim_species_name_type(gd_id)
  type(kim_species_name_type), parameter :: kim_species_name_tb = &
    kim_species_name_type(tb_id)
  type(kim_species_name_type), parameter :: kim_species_name_dy = &
    kim_species_name_type(dy_id)
  type(kim_species_name_type), parameter :: kim_species_name_ho = &
    kim_species_name_type(ho_id)
  type(kim_species_name_type), parameter :: kim_species_name_er = &
    kim_species_name_type(er_id)
  type(kim_species_name_type), parameter :: kim_species_name_tm = &
    kim_species_name_type(tm_id)
  type(kim_species_name_type), parameter :: kim_species_name_yb = &
    kim_species_name_type(yb_id)
  type(kim_species_name_type), parameter :: kim_species_name_lu = &
    kim_species_name_type(lu_id)
  type(kim_species_name_type), parameter :: kim_species_name_hf = &
    kim_species_name_type(hf_id)
  type(kim_species_name_type), parameter :: kim_species_name_ta = &
    kim_species_name_type(ta_id)
  type(kim_species_name_type), parameter :: kim_species_name_w = &
    kim_species_name_type(w_id)
  type(kim_species_name_type), parameter :: kim_species_name_re = &
    kim_species_name_type(re_id)
  type(kim_species_name_type), parameter :: kim_species_name_os = &
    kim_species_name_type(os_id)
  type(kim_species_name_type), parameter :: kim_species_name_ir = &
    kim_species_name_type(ir_id)
  type(kim_species_name_type), parameter :: kim_species_name_pt = &
    kim_species_name_type(pt_id)
  type(kim_species_name_type), parameter :: kim_species_name_au = &
    kim_species_name_type(au_id)
  type(kim_species_name_type), parameter :: kim_species_name_hg = &
    kim_species_name_type(hg_id)
  type(kim_species_name_type), parameter :: kim_species_name_tl = &
    kim_species_name_type(tl_id)
  type(kim_species_name_type), parameter :: kim_species_name_pb = &
    kim_species_name_type(pb_id)
  type(kim_species_name_type), parameter :: kim_species_name_bi = &
    kim_species_name_type(bi_id)
  type(kim_species_name_type), parameter :: kim_species_name_po = &
    kim_species_name_type(po_id)
  type(kim_species_name_type), parameter :: kim_species_name_at = &
    kim_species_name_type(at_id)
  type(kim_species_name_type), parameter :: kim_species_name_rn = &
    kim_species_name_type(rn_id)
  type(kim_species_name_type), parameter :: kim_species_name_fr = &
    kim_species_name_type(fr_id)
  type(kim_species_name_type), parameter :: kim_species_name_ra = &
    kim_species_name_type(ra_id)
  type(kim_species_name_type), parameter :: kim_species_name_ac = &
    kim_species_name_type(ac_id)
  type(kim_species_name_type), parameter :: kim_species_name_th = &
    kim_species_name_type(th_id)
  type(kim_species_name_type), parameter :: kim_species_name_pa = &
    kim_species_name_type(pa_id)
  type(kim_species_name_type), parameter :: kim_species_name_u = &
    kim_species_name_type(u_id)
  type(kim_species_name_type), parameter :: kim_species_name_np = &
    kim_species_name_type(np_id)
  type(kim_species_name_type), parameter :: kim_species_name_pu = &
    kim_species_name_type(pu_id)
  type(kim_species_name_type), parameter :: kim_species_name_am = &
    kim_species_name_type(am_id)
  type(kim_species_name_type), parameter :: kim_species_name_cm = &
    kim_species_name_type(cm_id)
  type(kim_species_name_type), parameter :: kim_species_name_bk = &
    kim_species_name_type(bk_id)
  type(kim_species_name_type), parameter :: kim_species_name_cf = &
    kim_species_name_type(cf_id)
  type(kim_species_name_type), parameter :: kim_species_name_es = &
    kim_species_name_type(es_id)
  type(kim_species_name_type), parameter :: kim_species_name_fm = &
    kim_species_name_type(fm_id)
  type(kim_species_name_type), parameter :: kim_species_name_md = &
    kim_species_name_type(md_id)
  type(kim_species_name_type), parameter :: kim_species_name_no = &
    kim_species_name_type(no_id)
  type(kim_species_name_type), parameter :: kim_species_name_lr = &
    kim_species_name_type(lr_id)
  type(kim_species_name_type), parameter :: kim_species_name_rf = &
    kim_species_name_type(rf_id)
  type(kim_species_name_type), parameter :: kim_species_name_db = &
    kim_species_name_type(db_id)
  type(kim_species_name_type), parameter :: kim_species_name_sg = &
    kim_species_name_type(sg_id)
  type(kim_species_name_type), parameter :: kim_species_name_bh = &
    kim_species_name_type(bh_id)
  type(kim_species_name_type), parameter :: kim_species_name_hs = &
    kim_species_name_type(hs_id)
  type(kim_species_name_type), parameter :: kim_species_name_mt = &
    kim_species_name_type(mt_id)
  type(kim_species_name_type), parameter :: kim_species_name_ds = &
    kim_species_name_type(ds_id)
  type(kim_species_name_type), parameter :: kim_species_name_rg = &
    kim_species_name_type(rg_id)
  type(kim_species_name_type), parameter :: kim_species_name_cn = &
    kim_species_name_type(cn_id)
  type(kim_species_name_type), parameter :: kim_species_name_uut = &
    kim_species_name_type(uut_id)
  type(kim_species_name_type), parameter :: kim_species_name_fl = &
    kim_species_name_type(fl_id)
  type(kim_species_name_type), parameter :: kim_species_name_uup = &
    kim_species_name_type(uup_id)
  type(kim_species_name_type), parameter :: kim_species_name_lv = &
    kim_species_name_type(lv_id)
  type(kim_species_name_type), parameter :: kim_species_name_uus = &
    kim_species_name_type(uus_id)
  type(kim_species_name_type), parameter :: kim_species_name_uuo = &
    kim_species_name_type(uuo_id)
  type(kim_species_name_type), parameter :: kim_species_name_user01 = &
    kim_species_name_type(user01_id)
  type(kim_species_name_type), parameter :: kim_species_name_user02 = &
    kim_species_name_type(user02_id)
  type(kim_species_name_type), parameter :: kim_species_name_user03 = &
    kim_species_name_type(user03_id)
  type(kim_species_name_type), parameter :: kim_species_name_user04 = &
    kim_species_name_type(user04_id)
  type(kim_species_name_type), parameter :: kim_species_name_user05 = &
    kim_species_name_type(user05_id)
  type(kim_species_name_type), parameter :: kim_species_name_user06 = &
    kim_species_name_type(user06_id)
  type(kim_species_name_type), parameter :: kim_species_name_user07 = &
    kim_species_name_type(user07_id)
  type(kim_species_name_type), parameter :: kim_species_name_user08 = &
    kim_species_name_type(user08_id)
  type(kim_species_name_type), parameter :: kim_species_name_user09 = &
    kim_species_name_type(user09_id)
  type(kim_species_name_type), parameter :: kim_species_name_user10 = &
    kim_species_name_type(user10_id)
  type(kim_species_name_type), parameter :: kim_species_name_user11 = &
    kim_species_name_type(user11_id)
  type(kim_species_name_type), parameter :: kim_species_name_user12 = &
    kim_species_name_type(user12_id)
  type(kim_species_name_type), parameter :: kim_species_name_user13 = &
    kim_species_name_type(user13_id)
  type(kim_species_name_type), parameter :: kim_species_name_user14 = &
    kim_species_name_type(user14_id)
  type(kim_species_name_type), parameter :: kim_species_name_user15 = &
    kim_species_name_type(user15_id)
  type(kim_species_name_type), parameter :: kim_species_name_user16 = &
    kim_species_name_type(user16_id)
  type(kim_species_name_type), parameter :: kim_species_name_user17 = &
    kim_species_name_type(user17_id)
  type(kim_species_name_type), parameter :: kim_species_name_user18 = &
    kim_species_name_type(user18_id)
  type(kim_species_name_type), parameter :: kim_species_name_user19 = &
    kim_species_name_type(user19_id)
  type(kim_species_name_type), parameter :: kim_species_name_user20 = &
    kim_species_name_type(user20_id)

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
