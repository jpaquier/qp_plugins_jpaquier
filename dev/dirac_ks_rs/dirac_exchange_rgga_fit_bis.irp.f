program dirac_exchange_rgga_fit_bis
  BEGIN_DOC
  ! produce the exchange energy given by the relativistic rgga
  END_DOC
   call run_exchange_rgga_fit_bis
 end

 subroutine run_exchange_rgga_fit_bis
  BEGIN_DOC
  ! Gives the energy for a given value of mu_erf
  END_DOC
  use bitmasks
  implicit none
  integer :: i,j,k,l,counter
  double precision :: dirac_a1_bis_coef, dirac_a2_bis_coef, dirac_b2_bis_coef
 !Choose Interaction
  dirac_a1_bis = 0.69984d0
  dirac_a2_bis = 0.99792d0
  dirac_b2_bis = 9.8496d0
  call ezfio_set_dirac_dft_utils_one_e_dirac_b2_bis(dirac_b2_bis)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a2_bis(dirac_a2_bis)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a1_bis(dirac_a1_bis)
  dirac_a1_bis_coef = dirac_a1_bis
  dirac_a2_bis_coef = dirac_a2_bis
  dirac_b2_bis_coef = dirac_b2_bis
  counter = 1
  print*,'**********'
  print*,'short-range coulomb interaction'
  open (9, file='counter.dat')
  open (10, file='exchange_rgga_Z.dat',position ='append')
  open (11, file='squared_exchange_rgga_Z.dat',position ='append')
  do l = 1,5
   if (l == 1) then
    dirac_b2_bis = 0.9d0*dirac_b2_bis_coef
   elseif (l == 2) then
    dirac_b2_bis = 0.95d0*dirac_b2_bis_coef
   elseif (l == 3) then
    dirac_b2_bis = dirac_b2_bis_coef
   elseif (l == 4) then
    dirac_b2_bis = 1.05d0*dirac_b2_bis_coef
   elseif (l == 5) then
    dirac_b2_bis = 1.1d0*dirac_b2_bis_coef
   endif
   call ezfio_set_dirac_dft_utils_one_e_dirac_b2_bis(dirac_b2_bis)
   TOUCH dirac_b2_bis
   do k = 1,5
    if (k == 1) then
     dirac_a2_bis = 0.95d0*dirac_a2_bis_coef
    elseif (k == 2) then
     dirac_a2_bis = 0.975d0*dirac_a2_bis_coef
    elseif (k == 3) then
     dirac_a2_bis = dirac_a2_bis_coef
    elseif (k == 4) then
     dirac_a2_bis = 1.025d0*dirac_a2_bis_coef
    elseif (k == 5) then
     dirac_a2_bis = 1.05d0*dirac_a2_bis_coef
    endif
    call ezfio_set_dirac_dft_utils_one_e_dirac_a2_bis(dirac_a2_bis)
    TOUCH dirac_a2_bis
    do j = 1,5
     if (j == 1) then
      dirac_a1_bis = 0.9d0*dirac_a1_bis_coef
     elseif (j == 2) then
      dirac_a1_bis = 0.95d0*dirac_a1_bis_coef
     elseif (j == 3) then
      dirac_a1_bis = dirac_a1_bis_coef
     elseif (j == 4) then
      dirac_a1_bis = 1.05d0*dirac_a1_bis_coef
     elseif (j == 5) then
      dirac_a1_bis = 1.1d0*dirac_a1_bis_coef
     endif
     call ezfio_set_dirac_dft_utils_one_e_dirac_a1_bis(dirac_a1_bis)
     TOUCH dirac_a1_bis
     print*, counter,dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
     write(9,*) counter
     write(10,*) counter,dirac_a1_bis,dirac_a2_bis,dirac_b2_bis, dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
     write(11,*) (100.d0*(dirac_energy_x_rgga(1) - (dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy))/(dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy))**2
     counter += 1
    enddo
   enddo
  enddo
  dirac_a1_bis = dirac_a1_bis_coef
  dirac_a2_bis = dirac_a2_bis_coef
  dirac_b2_bis = dirac_b2_bis_coef
  call ezfio_set_dirac_dft_utils_one_e_dirac_b2_bis(dirac_b2_bis)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a2_bis(dirac_a2_bis)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a1_bis(dirac_a1_bis)

  mu_erf = mu_erf*2.d0
  call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)
  TOUCH mu_erf
 end

