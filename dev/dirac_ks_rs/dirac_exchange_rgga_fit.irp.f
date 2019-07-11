program dirac_exchange_rgga_fit
  BEGIN_DOC
  ! produce the exchange energy given by the relativistic rgga
  END_DOC
   call run_exchange_rgga_fit 
 end

 subroutine run_exchange_rgga_fit
  BEGIN_DOC
  ! Gives the energy for a given value of mu_erf
  END_DOC
  use bitmasks
  implicit none 
  integer :: i,j,k,l,counter
  double precision :: dirac_a1_coef, dirac_a2_coef, dirac_b1_coef, dirac_b2_coef
 !Choose Interaction
  dirac_a1_coef = dirac_a1
  dirac_a2_coef = dirac_a2
  dirac_b1_coef = dirac_b1
  dirac_b2_coef = dirac_b2
  counter = 1
  print*,'**********'
  print*,'short-range coulomb interaction'   
  print*, dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
  open (9, file='counter.dat', position = 'append')
  open (10, file='exchange_rgga_Z.dat',position ='append')
  open (11, file='exchange_squared_Z.dat',position ='append')
  do l = 1,3
   if (l == 1) then 
    dirac_b2 = 0.8d0*dirac_b2_coef
   elseif (l == 2) then
    dirac_b2 = dirac_b2_coef
   elseif (l == 3) then
    dirac_b2 = 1.2d0*dirac_b2_coef
   endif
   call ezfio_set_dirac_dft_utils_one_e_dirac_b2(dirac_b2)
   TOUCH dirac_b2
   do k = 1,3
    if (k == 1) then 
     dirac_a2 = 0.8d0*dirac_a2_coef
    elseif (k == 2) then
     dirac_a2 = dirac_a2_coef
    elseif (k == 3) then
     dirac_a2 = 1.2d0*dirac_a2_coef
    endif
    call ezfio_set_dirac_dft_utils_one_e_dirac_a2(dirac_a2)
    TOUCH dirac_a2
    do j = 1,3
     if (j == 1) then  
      dirac_b1 = 0.8d0*dirac_b1_coef
     elseif (j == 2) then
      dirac_b1 = dirac_b1_coef
     elseif (j == 3) then
      dirac_b1 = 1.2d0*dirac_b1_coef
     endif
     call ezfio_set_dirac_dft_utils_one_e_dirac_b1(dirac_b1)
     TOUCH dirac_b1
     do i = 1,3 
      if (i == 1) then 
       dirac_a1 = 0.8d0*dirac_a1_coef
      elseif (i == 2) then
       dirac_a1 = dirac_a1_coef 
      elseif (i == 3) then
       dirac_a1 = 1.2d0*dirac_a1_coef
      endif
      call ezfio_set_dirac_dft_utils_one_e_dirac_a1(dirac_a1)
      TOUCH dirac_a1
      print*, counter,dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
      write(9,*) counter
     !write(10,*) counter,dirac_a1,dirac_a2,dirac_b1,dirac_b2, dirac_energy_x_rgga(1),dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
      write(11,*) 100.d0*Abs((dirac_energy_x_rgga(1) - (dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy))/(dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy))
      counter += 1
     enddo
    enddo
   enddo
  enddo
  dirac_a1 = dirac_a1_coef
  dirac_a2 = dirac_a2_coef
  dirac_b1 = dirac_b1_coef
  dirac_b2 = dirac_b2_coef
  call ezfio_set_dirac_dft_utils_one_e_dirac_b2(dirac_b2)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a2(dirac_a2)
  call ezfio_set_dirac_dft_utils_one_e_dirac_b1(dirac_b1)
  call ezfio_set_dirac_dft_utils_one_e_dirac_a1(dirac_a1)


end
