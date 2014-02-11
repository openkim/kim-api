    ! Check to see if we have been asked to compute the energy, forces, energyperatom,
    ! and virial
    !
    call kim_api_getm_compute(pkim, Compute_Energy_Forces, &
         "energy",         comp_energy, 1, &
         "forces",         comp_force,  1, &
         "particleEnergy", comp_enepot, 1, &
         "virial",         comp_virial, 1)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_compute", Compute_Energy_Forces)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_getm_data(pkim, Compute_Energy_Forces,     &
         "numberOfParticles",   pnAtoms,         1,           &
         "numberParticleTypes", pnparticleTypes, 1,           &
         "particleTypes",       pparticleTypes,  1,           &
         "cutoff",              pcutoff,         1,           &
         "coordinates",         pcoor,           1,           &
         "energy",              penergy,         comp_energy, &
         "forces",              pforce,          comp_force,  &
         "particleEnergy",      penepot,         comp_enepot, &
         "virial",              pvirial,         comp_virial)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Compute_Energy_Forces)
       return
    endif

    call c_f_pointer(pnAtoms,         numberOfParticles)
    call c_f_pointer(pnparticleTypes, nparticleTypes)
    call c_f_pointer(pparticleTypes,  particleTypes, [numberOfParticles])
    call c_f_pointer(pcutoff,         model_cutoff)
    call c_f_pointer(pcoor,           coor,          [DIM,numberOfParticles])
    if (comp_energy.eq.1) call c_f_pointer(penergy, energy)
    if (comp_force.eq.1)  call c_f_pointer(pforce,  force,  [DIM,numberOfParticles])
    if (comp_enepot.eq.1) call c_f_pointer(penepot, enepot, [numberOfParticles])
    if (comp_virial.eq.1) call c_f_pointer(pvirial, virial, [6])

    call kim_api_getm_data(pkim, Compute_Energy_Forces, &
         "PARAM_FREE_epsilon",  pepsilon,      1,         &
         "PARAM_FREE_sigma",    psigma,        1,         &
         "PARAM_FIXED_cutnorm", pcutnorm,      1,         &
         "PARAM_FIXED_A",       pA,            1,         &
         "PARAM_FIXED_B",       pB,            1,         &
         "PARAM_FIXED_C",       pC,            1,         &
         "PARAM_FIXED_sigmasq", psigmasq,      1,         &
         "PARAM_FIXED_cutsq",   pcutsq,        1)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Compute_Energy_Forces)
       return
    endif

    call c_f_pointer(pepsilon, model_epsilon)
    call c_f_pointer(psigma,   model_sigma)
    call c_f_pointer(pcutnorm, model_cutnorm)
    call c_f_pointer(pA,       model_A)
    call c_f_pointer(pB,       model_B)
    call c_f_pointer(pC,       model_C)
    call c_f_pointer(psigmasq, model_sigmasq)
    call c_f_pointer(pcutsq,   model_cutsq)

    ! Check to be sure that the atom types are correct by comparing
    ! the provided species codes to the value given here (which should
    ! be the same as that given in the .kim file).
    !
    Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
    do i = 1,numberOfParticles
       if (.not. (particleTypes(i) .eq. SPECIES_CODE_STR)) then
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "Wrong Atom Type", Compute_Energy_Forces)
          return
       endif
    enddo
    Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok


    ! Initialize potential energies, forces, virial term
    !
    if (comp_enepot.eq.1) enepot = 0.d0
    if (comp_energy.eq.1) energy = 0.d0
    if (comp_force.eq.1)  force  = 0.d0
    if (comp_virial.eq.1) virial = 0.d0
