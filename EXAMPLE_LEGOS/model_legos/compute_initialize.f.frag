    ! Check to see if we have been asked to compute the energy, forces, energyperatom, 
    ! and virial
    !
    call kim_api_get_compute_multiple_f(pkim, ier, &
         "energy",        comp_energy, 1, &
         "forces",        comp_force,  1, &
         "energyPerAtom", comp_enepot, 1, &
         "virialGlobal",  comp_virial, 1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_compute_multiple_f", ier)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_get_data_multiple_f(pkim, ier,             &
         "numberOfAtoms",       pnAtoms,       1,           &
         "numberAtomTypes",     pnAtomTypes,   1,           &
         "atomTypes",           patomTypes,    1,           &
         "cutoff",              pcutoff,       1,           &
         "coordinates",         pcoor,         1,           &
         "energy",              penergy,       comp_energy, &
         "forces",              pforce,        comp_force,  &
         "energyPerAtom",       penepot,       comp_enepot, &
         "virialGlobal",        pvirialGlobal, comp_virial)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
       return
    endif

    call kim_api_get_data_multiple_f(pkim, ier,   &
         "PARAM_FREE_epsilon",  pepsilon,      1, &
         "PARAM_FREE_sigma",    psigma,        1, &
         "PARAM_FIXED_cutnorm", pcutnorm,      1, &
         "PARAM_FIXED_A",       pA,            1, &
         "PARAM_FIXED_B",       pB,            1, &
         "PARAM_FIXED_C",       pC,            1, &
         "PARAM_FIXED_sigmasq", psigmasq,      1, &
         "PARAM_FIXED_cutsq",   pcutsq,        1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
       return
    endif
    
    ! Cast to F90 arrays
    !
    call toRealArrayWithDescriptor2d(coordum,coor,DIM,numberOfAtoms)
    if (comp_force.eq.1) &
       call toRealArrayWithDescriptor2d(forcedum,force,DIM,numberOfAtoms)
    if (comp_enepot.eq.1) &
       call toRealArrayWithDescriptor1d(enepotdum,ene_pot,numberOfAtoms)
    if (comp_virial.eq.1) &
       call toRealArrayWithDescriptor1d(virialGlobaldum,virial_global,6)


    ! Check to be sure that the atom types are correct by comparing
    ! the provided species codes to the value given here (which should
    ! be the same as that given in the .kim file).
    !
    ier = KIM_STATUS_FAIL ! assume an error
    do i = 1,numberOfAtoms
       if (.not. (atomTypes(i) .eq. SPECIES_CODE_STR)) then
          idum = kim_api_report_error_f(__LINE__, __FILE__, "Wrong Atom Type", ier)
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
