    ! Unpack data from KIM object
    !
    pnAtoms = kim_api_get_data_f(pkim,"numberOfAtoms",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pnAtomTypes = kim_api_get_data_f(pkim,"numberAtomTypes",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    patomTypes = kim_api_get_data_f(pkim,"atomTypes",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pcutoff = kim_api_get_data_f(pkim,"cutoff",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif

    pcoor = kim_api_get_data_f(pkim,"coordinates",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif
    
    ! Check to see if we have been asked to compute the energy, forces, energyperatom, 
    ! and virial
    !
    comp_energy = kim_api_isit_compute_f(pkim,"energy",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute", ier)
       return
    endif

    comp_force  = kim_api_isit_compute_f(pkim,"forces",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute", ier)
       return
    endif

    comp_enepot = kim_api_isit_compute_f(pkim,"energyPerAtom",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute", ier)
       return
    endif

    comp_virial = kim_api_isit_compute_f(pkim,"virialGlobal",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute", ier)
       return
    endif
    
    ! Cast to F90 arrays
    !
    if (comp_energy.eq.1) then
       penergy = kim_api_get_data_f(pkim,"energy",ier)
       if (ier.lt.KIM_STATUS_OK) then
          call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
          return
       endif
    endif
    if (comp_force.eq.1) then 
       pforce  = kim_api_get_data_f(pkim,"forces",ier)
       if (ier.lt.KIM_STATUS_OK) then
          call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
          return
       endif
       call toRealArrayWithDescriptor2d(forcedum,force,DIM,numberOfAtoms)
    endif
    if (comp_enepot.eq.1) then 
       penepot = kim_api_get_data_f(pkim,"energyPerAtom",ier) 
       if (ier.lt.KIM_STATUS_OK) then
          call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
          return
       endif
       call toRealArrayWithDescriptor1d(enepotdum,ene_pot,numberOfAtoms)
    endif
    if (comp_virial.eq.1) then
       pvirialGlobal = kim_api_get_data_f(pkim,"virialGlobal",ier)
       if (ier.lt.KIM_STATUS_OK) then
          call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
          return
       endif
       call toRealArrayWithDescriptor1d(virialGlobaldum,virial_global,6)
    endif

    call toRealArrayWithDescriptor2d(coordum,coor,DIM,numberOfAtoms)


    ! Check to be sure that the atom types are correct by comparing
    ! the provided species codes to the value given here (which should
    ! be the same as that given in the .kim file).
    !
    ier = KIM_STATUS_FAIL ! assume an error
    do i = 1,numberOfAtoms
       if (.not. (atomTypes(i) .eq. SPECIES_CODE_STR)) then
          call kim_api_report_error_f(__LINE__, __FILE__, "Wrong Atom Type", ier)
          return
       endif
    enddo
    ier = KIM_STATUS_OK ! everything is ok

    
    ! Initialize potential energies, forces, virial term
    !
    if (comp_enepot.eq.1) ene_pot(1:numberOfAtoms) = 0.d0
    if (comp_energy.eq.1) energy = 0.d0
    if (comp_force.eq.1)  force(1:3,1:numberOfAtoms) = 0.d0
    if (comp_virial.eq.1) virial_global = 0.d0
