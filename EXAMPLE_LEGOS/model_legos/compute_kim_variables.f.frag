    !-- KIM variables
    integer(c_int), pointer :: numberOfParticles; type(c_ptr) :: pnAtoms
    integer(c_int), pointer :: nparticleTypes;    type(c_ptr) :: pnparticleTypes
    integer(c_int), pointer :: particleTypes(:);  type(c_ptr) :: pparticleTypes
    real(c_double), pointer :: model_cutoff;      type(c_ptr) :: pcutoff
    real(c_double), pointer :: model_epsilon;     type(c_ptr) :: pepsilon
    real(c_double), pointer :: model_sigma;       type(c_ptr) :: psigma
    real(c_double), pointer :: model_cutnorm;     type(c_ptr) :: pcutnorm
    real(c_double), pointer :: model_A;           type(c_ptr) :: pA
    real(c_double), pointer :: model_B;           type(c_ptr) :: pB
    real(c_double), pointer :: model_C;           type(c_ptr) :: pC
    real(c_double), pointer :: model_sigmasq;     type(c_ptr) :: psigmasq
    real(c_double), pointer :: model_cutsq;       type(c_ptr) :: pcutsq
    real(c_double), pointer :: energy;            type(c_ptr) :: penergy
    real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
    real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
    real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
    real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial
    integer(c_int) comp_energy, comp_force, comp_enepot, comp_virial
