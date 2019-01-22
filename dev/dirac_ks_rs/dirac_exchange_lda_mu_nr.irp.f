program dirac_exchange_lda_mu_nr
  BEGIN_DOC
  ! produce the exchange energy given by the non-relativistic lda
  END_DOC
   call run_exchange_lda_mu 
 end

 subroutine run_exchange_lda_mu
  BEGIN_DOC
  ! Gives the energy for a given value of mu_erf
  END_DOC
  use bitmasks
  implicit none 
  integer :: i,length
   print*,'short-range coulomb interaction'   
   print*, mu_erf, energy_sr_x_LDA(1)
   open (10, file='exchange_lda_X_B_nr.dat',position ='append') 
   write(10,*) mu_erf, energy_sr_x_LDA(1)

 !For helium
  if (mu_erf .lt. 1.25) then
   mu_erf += 0.125d0
  elseif (mu_erf .lt. 2.5) then
   mu_erf+=0.25d0
  elseif (mu_erf .lt. 25) then
   mu_erf+=1.0d0
  endif

 !For Neon
 !if (mu_erf .lt. 2.5) then
 ! mu_erf += 0.25d0
 !elseif (mu_erf .lt. 5) then
 ! mu_erf+=0.5d0
 !elseif (mu_erf .lt. 50) then
 ! mu_erf+=2.5d0
 !endif

 !For argon
 !if (mu_erf .lt. 5) then
 ! mu_erf += 0.5d0
 !elseif (mu_erf .lt. 10) then
 ! mu_erf+=1.d0
 !elseif (mu_erf .lt. 100) then
 ! mu_erf+=5.d0
 !endif

 !For Krypton
 !if (mu_erf .lt. 10) then
 ! mu_erf += 1d0
 !elseif (mu_erf .lt. 20) then
 ! mu_erf+=2.d0
 !elseif (mu_erf .lt. 200) then
 ! mu_erf+=10.d0
 !endif

 !For Xenon
 !if (mu_erf .lt. 20) then
 ! mu_erf += 2d0
 !elseif (mu_erf .lt. 40) then
 ! mu_erf+=4.d0
 !elseif (mu_erf .lt. 400) then
 ! mu_erf+=20.d0
 !endif

  call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)
 end

