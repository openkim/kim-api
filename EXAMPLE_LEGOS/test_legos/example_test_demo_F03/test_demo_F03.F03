!
! Release: This file is part of the openkim-api.git repository.
!
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
! All rights reserved
!
! Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!

module KIMserviceF2003 ! this module will be in KIM_API as part of 
			!KIMserviceF2003.F90 will add it later with all service routines
			! the  interface block in this nodule describes direct call to
			!c-stle  service routines defined in KIMserviceC.h/cpp through f2003 standard 

	use iso_c_binding
	implicit none
#ifdef SYSTEM32
	integer, parameter :: kim_intptr_f03=4
#else
	integer,parameter :: kim_intptr_f03 = 8
#endif	
	
	interface
		integer function kim_api_init_f03(pkim,tst,mdl) bind(c,name='KIM_API_init')
			import :: c_ptr

			type(c_ptr):: pkim
			type(c_ptr),value::tst,mdl                
		end function kim_api_init_f03
		
		subroutine kim_api_allocate_f03(pkim,natom,ntypes,kimerr) bind(c,name='KIM_API_allocate')
			import :: c_ptr,kim_intptr_f03
		
			type(c_ptr),value:: pkim
			integer,value::ntypes
 			integer ::kimerr

			integer(kind=kim_intptr_f03), value ::natom
		end subroutine kim_api_allocate_f03

		function kim_api_get_data_f03(pkim, nm,kimerr) bind(c,name='KIM_API_get_data')
			import :: c_ptr	
			type(c_ptr),value:: pkim
			type(c_ptr),value:: nm
			type(c_ptr) :: kim_api_get_data_f03
			integer ::kimerr
		end function kim_api_get_data_f03
				
		function kim_api_get_index_f03(pkim, nm,kimerr) bind(c,name='KIM_API_get_index')
			import :: c_ptr
			type(c_ptr),value:: pkim
			type(c_ptr),value:: nm
			integer :: kim_api_get_index_f03
			integer ::kimerr
		end function kim_api_get_index_f03
		
                integer function kim_api_set_data_f03(pkim,nm,sz,dat) bind(c,name='KIM_API_set_data')
			import :: c_ptr, kim_intptr_f03
		type(c_ptr),value:: pkim
			type(c_ptr),value:: nm,dat
	
			integer(kind=kim_intptr_f03), value ::sz
		end function kim_api_set_data_f03
		
		subroutine kim_api_print_f03(pkim,kimerr) bind(c,name='KIM_API_print')
			import :: c_ptr
			type(c_ptr),value:: pkim
			integer ::kimerr			
		end subroutine kim_api_print_f03
		
		integer function kim_api_model_init_f03(pkim) bind(c,name='KIM_API_model_init')
			import :: c_ptr
			type(c_ptr),value:: pkim
		end function kim_api_model_init_f03
	        
                subroutine kim_api_model_compute_f03(pkim,kimerr) bind(c,name='KIM_API_model_compute')
			import :: c_ptr
			type(c_ptr),value:: pkim
			integer ::kimerr
		end subroutine kim_api_model_compute_f03
		
		subroutine kim_api_model_destroy_f03(pkim,kimerr) bind(c,name='KIM_API_model_destroy')
			import :: c_ptr
			type(c_ptr),value:: pkim
			integer ::kimerr
		end subroutine kim_api_model_destroy_f03

		subroutine kim_api_free_f03(pkim,kimerr) bind(c,name='KIM_API_free')
			import :: c_ptr
			type(c_ptr):: pkim
			integer ::kimerr
		end subroutine kim_api_free_f03

	end interface 
          
end module KIMserviceF2003

program ljtest
	use  neighborlistmod
	use KIMserviceF2003
	use iso_c_binding
	implicit none
	!input atomic configuration file
        character*80 :: infile = "./data/dumpval10.xyz"

	!KIM API related declaration pointer declarations 	
	character*80,target :: testname ="test_demo_F03"
	character*80,target :: modelname =""   !string for the modelname
	character*80,target :: nm =""
        real*8,pointer, dimension(:,:)::x,f
	real*8,pointer, dimension(:)::ea !energy per atom
	type(c_ptr) :: px,pf,pea,penergy,pcutoff,pnatoms! c_type pointers:x,f,energyPerAtom,energy,cutoff,natoms
	type(c_ptr) :: pkim                !kim world object pointer
	real*8, pointer:: energy,cutoff ! f90 pointers: c_type pointers will be converted to f90 pointers
        integer*8,pointer :: natoms     ! f90 pointer

	type(c_ptr)::pneigh_both;
	
	
	real*8 ::cutofeps
	integer ::i,id,ni4,kimerr; integer(kind=kim_intptr_f03) ::n,one


	!read the model name from the std input (screen)
	print *," KIM model name? :"
	read(*,*) modelname
	! initialize KIM_api object
	!if(kim_api_init_f(pkim,testname,modelname).ne.1) stop 'not a test-model match'
        testname=trim(testname)//char(0)
	modelname=trim(modelname)//char(0)
	
	
	if(kim_api_init_f03(pkim,c_loc(testname(1:1)),c_loc(modelname(1:1))).ne.1) stop 'not a test-model match'
       
	!read number of atoms in configuration		
	open(33,FILE=infile)
	read(33,*) n
	
	ni4=n
	!Allocate memory and associated it with the KIM API object
	call kim_api_allocate_f03(pkim,n,1,kimerr)

	! Make local pointers point to allocated memory (in KIM API object)
	nm = "coordinates"; nm=trim(nm)//char(0); 
	px=kim_api_get_data_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
        call c_f_pointer(px,x,shape=[3,ni4])

	nm = "forces"; nm=trim(nm)//char(0); 
	pf=kim_api_get_data_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
        call c_f_pointer(pf,f,shape=[3,ni4])

	nm = "energyPerAtom"; nm=trim(nm)//char(0); 
	pea=kim_api_get_data_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
        call c_f_pointer(pea,ea,shape=[ni4])

	
	nm = "energy"; nm=trim(nm)//char(0); 
	penergy=kim_api_get_data_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
        call c_f_pointer(penergy,energy)

	nm = "cutoff"; nm=trim(nm)//char(0); 
	pcutoff=kim_api_get_data_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
        call c_f_pointer(pcutoff,cutoff)

	nm = "numberOfAtoms"; nm=trim(nm)//char(0); 
	pnatoms=kim_api_get_data_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
        call c_f_pointer(pnatoms,natoms)

	nm = "neighObject"; nm=trim(nm)//char(0); 
	kim_neighObj_index=kim_api_get_index_f03(pkim,c_loc(nm(1:1)),kimerr);call kimerr_handle(nm,kimerr)
       
        natoms = n
	!  Read in the atomic positions for all atoms	
	do i=1,n
		read(33,*) id, x(:,i)
	end do
	close(33)
	f=0.0
	
	! calculate neighbor list for the configuration
	cutofeps=2.1

        call neighobj_both_allocate(pneigh_both) 

        call neighborscalculate_both(pneigh_both,loc(x(1,1)),n,cutofeps)


	!Inform KIM API object about neighbor list iterator and object

	one=1
	
 	nm = "neighObject"; nm=trim(nm)//char(0);
	if(kim_api_set_data_f03(pkim,c_loc(nm(1:1)),one,pneigh_both).ne.1) stop' neighObjec not in kim'
	nm = "get_half_neigh"; nm=trim(nm)//char(0);
        if(kim_api_set_data_f03(pkim,c_loc(nm(1:1)),one,c_funloc(get_half_neigh_kim)).ne.1) stop' get_half_neigh not in kim'
	nm = "get_full_neigh"; nm=trim(nm)//char(0);
	if(kim_api_set_data_f03(pkim,c_loc(nm(1:1)),one,c_funloc(get_full_neigh_kim)).ne.1) stop' get_full_neigh not in kim'

 

	! READY to call Model Initiation routine
	if( kim_api_model_init_f03(pkim).ne.1) stop ' model initialiser failed'
	       


	!All setup finished -- ready to compute
	!compute the model -- e.g., compute energy & force
        call kim_api_print_f03(pkim,kimerr)

	call kim_api_model_compute_f03(pkim,kimerr); call kimerr_handle("kim_api_model_compute",kimerr)

	!output KIM API object to screen (optional)
	call kim_api_print_f03(pkim,kimerr); call kimerr_handle("kim_api_print_f",kimerr)
        print*," kimerr=",kimerr;

	! print energy
	print *,"energy =", energy
        print*,"energy as sum of energy per atom",sum(ea)



  
	!clean up   

	call neighobj_both_deallocate(pneigh_both)
	


	call kim_api_model_destroy_f03(pkim,kimerr); call kimerr_handle("kim_api_model_destroy",kimerr)


	call kim_api_free_f03(pkim,kimerr); 	
	call kimerr_handle("kim_api_free",kimerr)

stop	

end program ljtest

subroutine kimerr_handle(nm,kimerr)
	integer ::kimerr
	character(len=*):: nm
	if (kimerr.ne.1)then
		print*,"error in: "//nm
		print *,"kim error code = ",kimerr
		stop
	end if
	return
end subroutine kimerr_handle
