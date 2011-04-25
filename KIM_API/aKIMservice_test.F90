!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Author: Valeriu Smirichinski                                         
!

program akimservice_test
    use kimservice
    implicit none
    integer(kind=kim_intptr) kim,sz; pointer(pkim,kim)
    real*8 ::xstub(3,1);pointer(px,xstub) ! cray pointer to x
    real*8 ::fcarray(3,1);pointer(pf,fcarray) !              to f
    real*8,allocatable,dimension(:,:) :: x
    real*8,pointer,dimension(:,:) ::f
    integer init,ind,i
    integer,pointer::shp(:)
    character (len=KEY_CHAR_LENGTH):: testinfile ="../../KIM_API/test_val01.kim"
    character (len=KEY_CHAR_LENGTH):: testname ="test01"
    character (len=KEY_CHAR_LENGTH):: modelinfile ="../../KIM_API/model_val01.kim"
    character (len=KEY_CHAR_LENGTH):: modelname ="model01"
    character (len=KEY_CHAR_LENGTH):: unitsystem
    allocate (x(3,2))
    allocate (shp(4))
    px=loc(x)
    x(:,1)=1.0
    x(:,2)=2.0

   PRINT*, 'Hello World 01'
    init = kim_api_init1_f(pkim,testinfile,testname,modelinfile,modelname)
    !call kim_api_print_f(pkim)

    print*," kim_api_set_units_f= ",kim_api_set_units_f(pkim,"standard")
    print*," kim_isunits_fixed_f= ",kim_api_isunits_fixed_f(pkim)
    sz=6
    !print*,"kim_api_set_data_f = ", kim_api_set_data_f(pkim,"coordinates",sz,px)
    ind=kim_api_get_index_f(pkim,"coordinates")
    call kim_api_set_data_byi_f(pkim,ind,sz,px)
    print*,"kim_api_get_size_f = ", kim_api_get_size_f(pkim,"coordinates")

    print*,"kim_api_get_rank_shape_f", kim_api_get_rank_shape_f(pkim,"coordinates",shp)
    print*,"shape= ",shp(:)
    
    print*,"kim_api_isit_compute_f(forces) =", kim_api_isit_compute_f(pkim,"forces")
    print*,"kim_api_isit_compute_f(force) =", kim_api_isit_compute_f(pkim,"force")
    call kim_api_set2_uncompute_f(pkim,"forces")
    call kim_api_set2_compute_f(pkim,"force")
    print*,"kim_api_isit_compute_f(forces) =", kim_api_isit_compute_f(pkim,"forces")
    print*,"kim_api_isit_compute_f(force) =", kim_api_isit_compute_f(pkim,"force")
    ind = kim_api_get_index_f(pkim,"forces")
    call kim_api_set2_compute_byi_f(pkim,ind)
    print*,"kim_api_isit_compute_f(forces) (byi)=", kim_api_isit_compute_byi_f(pkim,ind)
    ind = kim_api_get_index_f(pkim,"force")
    call kim_api_set2_uncompute_byi_f(pkim,ind)
    print*,"kim_api_isit_compute_f(force) (byi)=", kim_api_isit_compute_byi_f(pkim,ind)
    
    print*,"kim_api_get_index_f(force)= ", kim_api_get_index_f(pkim,"force")
    print*,"kim_api_get_size_byi_f(3)= ", kim_api_get_size_byi_f(pkim,3)
    print*,"kim_api_get_rank_shape_byi_f =",kim_api_get_rank_shape_byi_f(pkim,3,shp)
    print*,"shape= ",shp(:)
    print*,"kim_api_isit_compute_byi_f =", kim_api_isit_compute_byi_f(pkim,3)
    call kim_api_print_f(pkim)
    call kim_api_free(pkim)

    !init = kim_api_init1_f(pkim,testinfile,testname,modelinfile,modelname)

    init =kim_api_init_f(pkim,"Sample_01_compute_example_f","Sample_01_lj_cutoff")
 
    !print*," kim_api_set_units_f= ",kim_api_set_units_f(pkim,"standard")
   
    if(init==1) print*,"kim_api_init_f -- successful"
    if(init.ne.1) print*,"kim_api_init_f -- failed"

    stop 

    call kim_api_allocate_f(pkim,sz,1)

   
    call kim_api_transform_units_to_f(pkim,"standard")
    call kim_api_get_units_f(pkim,unitsystem)
    print *,"unitsystem:",unitsystem,":"
    call kim_api_get_originalunits_f(pkim,unitsystem)
    print *,"originalunitsystem:",unitsystem,":"

    !pf = kim_api_get_data_f(pkim,"forces")
    ind = kim_api_get_index_f(pkim,"forces")
    pf = kim_api_get_data_byi_f(pkim,ind) !fast way to access
    call toRealArrayWithDescriptor2d(fcarray,f,3,6)
    do i=1,6
        f(:,i)=1.0*i/2
    end do
    call kim_api_print_f(pkim)


    call kim_api_free_f(pkim)

    !call kim_api_print_f(pkim)  !1 check if free works fine
    !call kim_api_free_f(pkim)   !2
    !call kim_api_print_f(pkim)  !3
    
    PRINT*, 'Hello World'
    
end program akimservice_test

