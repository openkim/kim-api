!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor                            
!


module lj_neigh_pure_full_mod
	use  KIMservice
	implicit none
	!defining the structure that holds variables for calculation
	type lj_test_object
		integer :: numberatoms
		real*8,pointer :: x(:,:), f(:,:),e(:) ! position, forces and energy per atom
		!real*8 :: a= 1.0*0.5**(12),b=2.0*0.5**(6),cutof=2.3! parameters of lj potential
		real*8 :: a= 0.0002441,b=0.03125! parameters of lj potential
		integer(kind=kim_intptr) :: a_cutof,a_energy !holder for cutoff and energy pointer/address
		integer :: x_ind,f_ind,e_ind, numberatoms_ind! index of x,f,e within kim api object (for fast access)
	end type lj_test_object
!	interface
!		subroutine neighborsiterate(pneiobj,pnei1atom,numnei,restart)
!			use KIMservice
!			integer(kind=kim_intptr) ::pneiobj,pnei1atom
!			integer :: numnei,restart! if restart = 0 set iterator in the begining
!						  ! if restart != 0 proseed next
!		end subroutine neighborsiterate
!	end interface

	!defining cray pointer to neighbor iterator
         external neighborsiterate
	 pointer (piterator,neighborsiterate)
	
	
	type (lj_test_object) :: lj_obj
	SAVE ::	lj_obj

contains
	! actual initialization routine
	subroutine lj_init(pkim )
		use KIMservice
		implicit none
		! KIM API related declaration
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim) 
		
		integer(kind=kim_intptr) ::sz; integer(kind=8):: numatoms; pointer(patoms,numatoms) ! .. to number of atoms
		real*8::cutoff; pointer(pcutoff,cutoff)
		real*8::skin; pointer(pskin,skin)
		integer::kimerr
		!getting pointers data from kim
		patoms = kim_api_get_data_f(pkim,"numberOfAtoms",kimerr) ; lj_obj%numberatoms=numatoms
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_data_f:  numberOfAtoms not in KIM : error code = ", kimerr
			stop
		end if
                lj_obj%numberatoms_ind= kim_api_get_index_f(pkim,"numberOfAtoms",kimerr)
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_index_f:  numberOfAtoms not in KIM : error code = ", kimerr
			stop
		end if
		lj_obj%x_ind=kim_api_get_index_f(pkim,"coordinates",kimerr); 
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_index_f:  coordinates not in KIM : error code = ", kimerr
			stop
		end if		
		
	
		lj_obj%f_ind=kim_api_get_index_f(pkim,"forces",kimerr)
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_index_f:  forces not in KIM : error code = ", kimerr
			stop
		end if
		lj_obj%e_ind=kim_api_get_index_f(pkim,"energyPerAtom",kimerr)
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_index_f:  energyPerAtom not in KIM : error code = ", kimerr
			stop
		end if
                
	        
		lj_obj%a_cutof = kim_api_get_data_f(pkim,"cutoff",kimerr)
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_data_f:  cutoff not in KIM : error code = ", kimerr
			stop
		end if
		pcutoff=lj_obj%a_cutof
		cutoff = 1.8;  !initialize cutoff                
		lj_obj%a_energy = kim_api_get_data_f(pkim,"energy",kimerr)     !get pointer to energy
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_data_f:  energy not in KIM : error code = ", kimerr
			stop
		end if

		!setting pointer to compute method
		sz=1
		if(kim_api_set_data_f(pkim,"compute",sz,loc(lj_calculate)).ne.1)  then
			stop ' compute not found in kim'
		end if
	end subroutine lj_init
	
	
	
	
	
	!calculates forces per atom and total energy
		subroutine lj_calculate(pkim,kimerr) ! compute routine with KIM interface
		implicit none
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim)
		integer kimerr, f_flag,e_flag
		real*8 :: xstub(3,1); pointer(px,xstub)  ! cray pointer to position
		real*8 :: fstub(3,1); pointer(pf,fstub)  ! cray pointer to forces
		real*8 :: estub(1); pointer(pe,estub)    ! cray pointer to energy per atom
                integer(kind=8):: numatoms; pointer(patoms,numatoms)
                patoms=kim_api_get_data_byi(pkim,lj_obj%numberatoms_ind,kimerr);  
		lj_obj%numberatoms=numatoms
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_index_byi: numberOfAtoms : error code = ", kimerr
			stop
		end if
                px=kim_api_get_data_byi(pkim,lj_obj%x_ind,kimerr)
		
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_data_byi:  coordinates  : error code = ", kimerr
			stop
		end if		
		call toRealArrayWithDescriptor2d(xstub,lj_obj%x,3,lj_obj%numberatoms)
	
		pf=kim_api_get_data_byi(pkim,lj_obj%f_ind,kimerr)
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_data_byi:  forces  : error code = ", kimerr
			stop
		end if
		pe=kim_api_get_data_byi(pkim,lj_obj%e_ind,kimerr)
		call toRealArrayWithDescriptor2d(fstub,lj_obj%f,3,lj_obj%numberatoms)
		if (kimerr.ne.1) then
			print *,"lj_init: kim_api_get_data_byi:  energyPerAtom  : error code = ", kimerr
			stop
		end if
                call toRealArrayWithDescriptor1d(estub,lj_obj%e,lj_obj%numberatoms)
	        
		f_flag=kim_api_isit_compute_byi(pkim,lj_obj%f_ind,kimerr)
                e_flag=kim_api_isit_compute_byi(pkim,lj_obj%e_ind,kimerr)
		call lj_calculate2(pkim,lj_obj%x,lj_obj%f,lj_obj%e,f_flag,e_flag,kimerr)
	end subroutine lj_calculate

	subroutine lj_calculate2(pkim,x,f,ea,f_flag,e_flag,kimerr) ! actual compute routine
		use KIMservice
		implicit none		
		
		!KIM related declaration
    		integer::f_flag,e_flag;
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim)          
		real*8,pointer,dimension(:,:) :: x,f
                real*8,pointer,dimension(:)::ea
		real*8 :: vij,dvmr,v,sumv,cut2,energycutof
		integer :: i,j,jj,numnei=0,kimerr;      real*8 :: r2,dv;   real*8,dimension(3):: xi,xj,dx,fij
		real*8::energy;pointer(penergy,energy)
		real*8::cutof;pointer(pcutof,cutof)
		integer(kind=kim_intptr) ::pRij;
		integer:: nei1atom(1); pointer (pnei1atom,nei1atom)
		integer :: retcode,mode,request,atom=0;
		
		penergy = lj_obj%a_energy ! now energy is a variable that dirrectly stored in KIM API object
                pcutof = lj_obj%a_cutof	  ! now cutof is a variable that dirrectly stored in KIM API object-"-
	
        	sumv=0.0d0; 
		if (f_flag.eq.1) f(:,:)= 0.0d0
                if (e_flag.eq.1) ea(:)=0.0d0
	        numnei = 0
		cut2 = cutof*cutof
		! store energy of LJ at rcut in 'energycutof'
		! to be used for shifting LJ so energy is zero at cutoff 
		call  ljpotr(cut2,energycutof,dvmr)              
	        
		mode=0;   ! iterator mode
		request=0;!reset neighbor iterator to beginning
		retcode = kim_api_get_full_neigh(pkim,mode,request,atom,numnei,pnei1atom,pRij)
		if(retcode .ne. 2) then
			kimerr=retcode
			print*,"lj.._neigh_pure_full_f_calculate: iterator get_full_neigh has not been reset successfully:retcode= ",retcode
			return
		end if            
	
		retcode=1
        	do while (retcode .eq. 1)
			!increment iterator
			mode=0; request=1;
			retcode = kim_api_get_full_neigh(pkim,mode,request,atom,numnei,pnei1atom,pRij)
			if(retcode.lt.0) then
				kimerr=retcode
				print*,"lj..._neigh_pure_full_f_calculate: error iterator get_full_neigh :retcode= ",retcode
				return
			else if(retcode.eq.0) then
				kimerr=1
				exit
			end if

			i=atom
			xi = x(:,i)
			do jj=1, numnei
				j=nei1atom(jj)
				xj = x(:,j)
				dx = xi-xj
				r2=dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3)
				if(r2.lt.0.000000000000001) print*,"xi,xj",xi,xj
				if(r2.lt.0.000000000000001) print*,"i,j",i,j
				if (r2.le.cut2) then
					call ljpotr(r2,vij,dvmr)
					sumv = sumv + (vij-energycutof)/2
					if (e_flag.eq.1) ea(i)=ea(i)+(vij-energycutof)/2
					if (f_flag.eq.1) f(:,i) =f(:,i) - dvmr*dx 
										
				end if
			end do
		end do
		v=sumv
		energy = v !set energy in KIM API object
		!forces are already stored in KIM API object
		return
		contains
		!Computational core of LJ potential
		subroutine ljpotr(rr,v,dvmr)
			implicit none
			real*8 rr,v,dvmr,       a,b,r1,r2,rm6,rm8
			a=lj_obj%a; b=lj_obj%b; ! LJ parameters
			if (rr.lt.0.000000000000001) stop 'rr is zero'
			r2=rr; rm6=1.0/(r2*r2*r2); rm8=rm6/r2
			v=(a*rm6 - b)*rm6; dvmr = 6.0*rm8*(-2*rm6*a + b)
		end subroutine ljpotr
	end subroutine lj_calculate2

end module lj_neigh_pure_full_mod

!  Model Initiation routine (it calls actual initialization routine in the module lj_neigh_pure_full_mod)
subroutine sample_01_lj_cutoff_neigh_pure_f_f_init(pkim)
	use lj_neigh_pure_full_mod
	implicit none
	integer(kind=kim_intptr) :: kim; pointer(pkim,kim)
	call lj_init(pkim)
end subroutine sample_01_lj_cutoff_neigh_pure_f_f_init

