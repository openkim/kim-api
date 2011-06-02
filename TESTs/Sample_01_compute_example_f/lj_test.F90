!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Author: Valeriu Smirichinski                                         
!

program ljtest
	use  neighborlistmod
	use KIMservice
	implicit none
	!input atomic configuration file
        character*80 :: infile = "./data/dumpval10.xyz"

	!KIM API related declaration pointer declarations 	
	character*80 :: testname ="Sample_01_compute_example_f"
	character*80 :: modelname =""   !string for the modelname
        real*8,pointer, dimension(:,:)::x,f
	
	type (neighborlist_object),pointer :: neigh_obj  ! , nei_obj; !pointer(pnei_obj,nei_obj)
        type (neighborlist_object_both) :: neigh_both; pointer(pneigh_both,neigh_both)
	type (neighborlist_object_both),pointer :: pnei_both
	integer(kind=kim_intptr) :: pkim                   !kim world object pointer
	real*8::xstub(3,1),fstub(3,1); pointer(px,xstub); pointer(pf,fstub) ! cray pointer to coordinates and forces
	real*8::energy; pointer(penergy,energy)		!cray pointer to energy
	real*8::cutoff; pointer(pcutoff,cutoff)		!		 cuttof
	integer*8 :: natoms; pointer(pnatoms,natoms)		!cray pointer to numberOfAtoms
	real*8 ::cutofeps
	integer ::i,id,ni4,kimerr; integer(kind=kim_intptr) ::n,one

	character(len=KEY_CHAR_LENGTH) ::  list_of_AtomsTypes(1); pointer(plist_of_AtomsTypes,list_of_AtomsTypes)
        integer :: nAtomsTypes;
	character(len=KEY_CHAR_LENGTH) :: txtstr; pointer(ptxtstr,txtstr)
	allocate(neigh_obj)
	!read the model name from the std input (screen)
	print *," KIM model name? :"
	read(*,*) modelname
	! initialize KIM_api object
	if(kim_api_init_f(pkim,testname,modelname).ne.1) stop 'not a test-model match'

	!read number of atoms in configuration		
	open(33,FILE=infile)
	read(33,*) n
	
	ni4=n
	!Allocate memory and associated it with the KIM API object
	call kim_api_allocate_f(pkim,n,1,kimerr)

	! Make local pointers point to allocated memory (in KIM API object)
	px=kim_api_get_data_f(pkim,"coordinates",kimerr);call kimerr_handle("coordinates",kimerr)
	call toRealArrayWithDescriptor2d(xstub,x,3,ni4)	
	pf=kim_api_get_data_f(pkim,"forces",kimerr); call kimerr_handle("forces",kimerr)     
	call toRealArrayWithDescriptor2d(fstub,f,3,ni4)
	penergy = kim_api_get_data_f(pkim,"energy",kimerr); call kimerr_handle("energy",kimerr)
	pcutoff = kim_api_get_data_f(pkim,"cutoff",kimerr); call kimerr_handle("cutoff",kimerr)
	pnatoms = kim_api_get_data_f(pkim,"numberOfAtoms",kimerr); call kimerr_handle("numberOfAtoms",kimerr)
        kim_neighObj_index = kim_api_get_index_f(pkim,"neighObject",kimerr) !index for internal usage of neigh locator/iterator
	call kimerr_handle("kim_api_get_index_f:neighObject",kimerr)
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
	

	if(kim_api_set_data_f(pkim,"neighObject",one,loc(neigh_both)).ne.1) stop' neighObjec not in kim'
        if(kim_api_set_data_f(pkim,"get_half_neigh",one,loc(get_half_neigh_kim)).ne.1) stop' get_half_neigh not in kim'

	! READY to call Model Initiation routine
	if( kim_api_model_init(pkim).ne.1) stop ' model initialiser failed'
	! setup parameter for lj potential   
	cutoff=1.8        


	!All setup finished -- ready to compute
	!compute the model -- e.g., compute energy & force

	call kim_api_model_compute(pkim,kimerr); call kimerr_handle("kim_api_model_compute",kimerr)

	!output KIM API object to screen (optional)
	call kim_api_print_f(pkim,kimerr); call kimerr_handle("kim_api_print_f",kimerr)
        print*," kimerr=",kimerr;

	! print energy
	print *,"energy =", energy


        plist_of_AtomsTypes = KIM_API_get_listAtomsTypes(pkim,nAtomsTypes,kimerr)
	if ((kimerr.eq.1).and.(nAtomsTypes.gt.0)) then
		write(*,"('The supported atoms types : ',10( A,' '))")  list_of_AtomsTypes(1:nAtomsTypes)
	end if
	print *," supported atom code of K= ",kim_api_get_atypecode_f(pkim,"K",kimerr), " kimerrorcode= ",kimerr
	print *," supported atom code of W= ",kim_api_get_atypecode_f(pkim,"W",kimerr), " kimerrorcode= ",kimerr
	call free(plist_of_AtomsTypes)

        plist_of_AtomsTypes = KIM_API_get_listparams(pkim,nAtomsTypes,kimerr)
        print *,"kimerr=", kimerr, " nParameters= ", nAtomsTypes
	if ((kimerr.eq.1).and.(nAtomsTypes.gt.0)) then
		write(*,"('parameters of the models are : ',10( A,' '))")  list_of_AtomsTypes(1:nAtomsTypes)
	end if
	call free(plist_of_AtomsTypes)

 	plist_of_AtomsTypes = KIM_API_get_listfreeparams(pkim,nAtomsTypes,kimerr)
        print *,"kimerr=", kimerr, " nParameters= ", nAtomsTypes
	if ((kimerr.eq.1).and.(nAtomsTypes.gt.0)) then
		write(*,"('FREE parameters of the models are : ',10( A,' '))")  list_of_AtomsTypes(1:nAtomsTypes)
	end if
	call free(plist_of_AtomsTypes)

	plist_of_AtomsTypes = KIM_API_get_listfixedparams(pkim,nAtomsTypes,kimerr)
        print *,"kimerr=", kimerr, " nParameters= ", nAtomsTypes
	if ((kimerr.eq.1).and.(nAtomsTypes.gt.0)) then
		write(*,"('FIXED parameters of the models are : ',10( A,' '))")  list_of_AtomsTypes(1:nAtomsTypes)
	end if
	
        ptxtstr=kim_api_get_nbc_method(pkim,kimerr)
        print*," NBC method: ",txtstr," kimerror= ",kimerr
	!clean up
        call free(plist_of_AtomsTypes)   
         

	call neighobj_both_deallocate(pneigh_both)
	!clean up

	call kim_api_model_destroy(pkim,kimerr); call kimerr_handle("kim_api_model_destroy",kimerr)


	call kim_api_free(pkim,kimerr); 	
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
