!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!

module model_demo_P_LJ_NEIGH_PURE_H_F77
	use  KIMservice
	implicit none
	!defining the structure that holds variables for calculation
	!type lj_test_object
		real*8 :: x(3,1), f(3,1),ea(1) ! position, forces and saved position
		pointer(px,x); pointer(af,f); pointer(aea,ea) 
		!real*8 :: a= 1.0*0.5**(12),b=2.0*0.5**(6),cutof=2.3! parameters of lj potential
		real*8 :: potenergy,xcutof ! holder for energy pointer
		pointer(apotenergy,potenergy)
		pointer(pcutoff,xcutof)
		integer*8::numberofatoms;pointer(anumberofatoms,numberofatoms)
	!end type lj_test_object

	
	!type (lj_test_object) :: lj_obj
	!SAVE ::	lj_obj

contains
	! actual initialization routine

	
	
	
	
	!calculates forces per atom and total energy (f90 wrapper that calls actual f77 routine)
	subroutine lj_calculate_wrap_f77(pkim,kimerr) ! compute routine with KIM interface
		implicit none
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim)
		integer ::natom,kimerr,f_flag,e_flag
		external LJ_f77_calculate
		natom = numberofatoms
                px=kim_api_get_data_f(pkim,"coordinates",kimerr)
		af=kim_api_get_data_f(pkim,"forces",kimerr)
		aea = kim_api_get_data_f(pkim,"energyPerAtom",kimerr)
                f_flag=kim_api_isit_compute_f(pkim,"forces",kimerr)
		e_flag=kim_api_isit_compute_f(pkim,"energyPerAtom",kimerr)
		call LJ_f77_calculate(pkim,x,f,ea,natom,potenergy,xcutof,f_flag,e_flag,kim_api_get_half_neigh_f,kimerr)
	end subroutine lj_calculate_wrap_f77



end module model_demo_P_LJ_NEIGH_PURE_H_F77

!  Model Initiation routine (it calls actual initialization routine in the module lj_test_mod)
subroutine model_demo_p_lj_neigh_pure_h_f77_init(pkim)
	use model_demo_P_LJ_NEIGH_PURE_H_F77
	use KIMservice
	implicit none
	integer(kind=kim_intptr) :: kim; pointer(pkim,kim) 
	integer::kimerr
	integer(kind=kim_intptr) ::sz
	!getting pointers data from kim
	anumberofatoms = kim_api_get_data_f(pkim,"numberOfAtoms",kimerr)
	if (kimerr.ne.1) then
		print *,"lj_init: kim_api_get_data_f:  numberOfAtoms not in KIM : error code = ", kimerr
		stop
	end if
	px=kim_api_get_data_f(pkim,"coordinates",kimerr)
	if (kimerr.ne.1) then
		print *,"lj_init: kim_api_get_data_f: coordinates not in KIM : error code = ", kimerr
		stop
	end if
	af=kim_api_get_data_f(pkim,"forces",kimerr)
	if (kimerr.ne.1) then
		print *,"lj_init: kim_api_get_data_f: forces not in KIM : error code = ", kimerr
		stop
	end if
	pcutoff = kim_api_get_data_f(pkim,"cutoff",kimerr)
	if (kimerr.ne.1) then
		print *,"lj_init: kim_api_get_data_f: cutoff not in KIM : error code = ", kimerr
		stop
	end if
	xcutof =1.8
	apotenergy = kim_api_get_data_f(pkim,"energy",kimerr)
	if (kimerr.ne.1) then
		print *,"lj_init: kim_api_get_data_f: energy not in KIM : error code = ", kimerr
		stop
	end if
	aea = kim_api_get_data_f(pkim,"energyPerAtom",kimerr)
	if (kimerr.ne.1) then
		print *,"lj_init: kim_api_get_data_f: energyPerAtom not in KIM : error code = ", kimerr
		stop
	end if
	!setting pointer to compute method
	sz=1
	if(kim_api_set_data_f(pkim,"compute",sz,loc(lj_calculate_wrap_f77)).ne.1)  then
		stop ' compute not found in kim'
	end if

end subroutine model_demo_p_lj_neigh_pure_h_f77_init
