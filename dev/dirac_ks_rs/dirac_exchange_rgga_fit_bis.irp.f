program dirac_exchange_rgga_fit_bis_6p
  BEGIN_DOC
  ! produce the exchange energy given by the relativistic rgga
  END_DOC
   call run_exchange_rgga_fit_bis_6p
 end

 subroutine run_exchange_rgga_fit_bis_6p
  BEGIN_DOC
  ! Gives the energy for a given value of mu_erf
  END_DOC
  use bitmasks
  implicit none
  integer :: i,j,k,l,counter
  double precision :: dirac_a1_bis_6p_coef, dirac_a2_bis_6p_coef, dirac_b2_bis_6p_coef
 !Choose Interaction
  dirac_a1_bis_6p = -1.d0
  dirac_a2_bis_6p = -1.d0
  dirac_b2_bis_6p = 4.235d0
  call ezfio_set_dirac_dft_utils_one_e_dirac_b2_bis_6p(dirac_b2_bis_6p)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a2_bis_6p(dirac_a2_bis_6p)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a1_bis_6p(dirac_a1_bis_6p)
  dirac_a1_bis_6p_coef = dirac_a1_bis_6p
  dirac_a2_bis_6p_coef = dirac_a2_bis_6p
  dirac_b2_bis_6p_coef = dirac_b2_bis_6p
  counter = 1
  print*,'**********'
  print*,'short-range coulomb interaction'
  open (9, file='counter.dat')
  open (10, file='exchange_rgga_Z.dat',position ='append')
  open (11, file='squared_exchange_rgga_Z.dat',position ='append')
  do l = 1,5
   if (l == 1) then
    dirac_b2_bis_6p = 0.975d0*dirac_b2_bis_6p_coef
   elseif (l == 2) then
    dirac_b2_bis_6p = 0.9875d0*dirac_b2_bis_6p_coef
   elseif (l == 3) then
    dirac_b2_bis_6p = dirac_b2_bis_6p_coef
   elseif (l == 4) then
    dirac_b2_bis_6p = 1.0125d0*dirac_b2_bis_6p_coef
   elseif (l == 5) then
    dirac_b2_bis_6p = 1.025d0*dirac_b2_bis_6p_coef
   endif
   call ezfio_set_dirac_dft_utils_one_e_dirac_b2_bis_6p(dirac_b2_bis_6p)
   TOUCH dirac_b2_bis_6p
   do k = 1,2
    if (k == 1) then
     dirac_a2_bis_6p = 0.95d0*dirac_a2_bis_6p_coef
    elseif (k == 2) then
     dirac_a2_bis_6p = dirac_a2_bis_6p_coef
   !elseif (k == 3) then
   ! dirac_a2_bis_6p = 1.1d0*dirac_a2_bis_6p_coef
   !elseif (k == 4) then
   ! dirac_a2_bis_6p = 1.2d0*dirac_a2_bis_6p_coef
   !elseif (k == 5) then
   ! dirac_a2_bis_6p = -1.d0
    endif
    call ezfio_set_dirac_dft_utils_one_e_dirac_a2_bis_6p(dirac_a2_bis_6p)
    TOUCH dirac_a2_bis_6p
    do j = 1,2
     if (j == 1) then
      dirac_a1_bis_6p = 0.95d0*dirac_a1_bis_6p_coef
     elseif (j == 2) then
      dirac_a1_bis_6p = dirac_a1_bis_6p_coef
    !elseif (j == 3) then
    ! dirac_a1_bis_6p = 1.1d0*dirac_a1_bis_6p_coef
    !elseif (j == 4) then
    ! dirac_a1_bis_6p = 1.2d0*dirac_a1_bis_6p_coef 
    !elseif (j == 5) then
    ! dirac_a1_bis_6p = -1.d0
     endif
     call ezfio_set_dirac_dft_utils_one_e_dirac_a1_bis_6p(dirac_a1_bis_6p)
     TOUCH dirac_a1_bis_6p
     print*, counter,dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
     write(9,*) counter
     write(10,*) counter,dirac_a1_bis_6p,dirac_a2_bis_6p,dirac_b2_bis_6p, dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
     write(11,*) (100.d0*(dirac_energy_x_rgga(1) - (dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy))/(dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy))**2
     counter += 1
    enddo
   enddo
  enddo
  dirac_a1_bis_6p = dirac_a1_bis_6p_coef
  dirac_a2_bis_6p = dirac_a2_bis_6p_coef
  dirac_b2_bis_6p = dirac_b2_bis_6p_coef
  call ezfio_set_dirac_dft_utils_one_e_dirac_b2_bis_6p(dirac_b2_bis_6p)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a2_bis_6p(dirac_a2_bis_6p)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a1_bis_6p(dirac_a1_bis_6p)

  mu_erf = mu_erf*2.d0
  call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)
  TOUCH mu_erf
 end

