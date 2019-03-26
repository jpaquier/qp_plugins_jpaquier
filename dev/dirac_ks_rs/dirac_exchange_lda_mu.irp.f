program dirac_exchange_lda_mu
  BEGIN_DOC
  ! produce the exchange energy given by the relativistic lda
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
 !Choose Interaction
  if (dirac_interaction == "Coulomb") then
   print*,'**********'
   print*,'short-range coulomb interaction'   
   print*, mu_erf, dirac_energy_x_lda(1)
   open (10, file='exchange_lda_Z.dat',position ='append') 
   write(10,*) mu_erf, dirac_energy_x_lda(1)
  elseif (dirac_interaction == "Coulomb_Gaunt") then
   print*,'**********'
   print*,'Short-range Coulomb-Gaunt interaction'
   open (10, file='exchange_lda_Z_CG.dat',position ='append')
   write(10,*) mu_erf, dirac_energy_x_lda(1)
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif


 !For helium
 !if (mu_erf .lt. 1.25) then
 ! mu_erf += 0.125d0
 !elseif (mu_erf .lt. 2.5) then
 ! mu_erf+=0.25d0
 !elseif (mu_erf .lt. 25) then
 ! mu_erf+=1.25d0
 !else 
 ! mu_erf+=2.5d0
 !endif

 !For berylium
 !if (mu_erf .lt. 1.25) then
 ! mu_erf += 0.125d0
 !elseif (mu_erf .lt. 2.5) then
 ! mu_erf+=0.25d0
 !elseif (mu_erf .lt. 25) then
 ! mu_erf+=1.25d0
 !else
 ! mu_erf+=5d0
 !endif

 !For Neon
 !if (mu_erf .lt. 2.5) then
 ! mu_erf += 0.25d0
 !elseif (mu_erf .lt. 10) then
 ! mu_erf+=0.5d0
 !elseif (mu_erf .lt. 50) then
 ! mu_erf+=2.5d0
 !elseif (mu_erf .lt. 100) then
 ! mu_erf+=5.0d0
 !else
 ! mu_erf+=10.d0
 !endif

 !For argon
  if (mu_erf .lt. 5) then
   mu_erf += 0.5d0
  elseif (mu_erf .lt. 20) then
   mu_erf+=1.d0
  elseif (mu_erf .lt. 100) then
   mu_erf+=5.d0
  elseif (mu_erf .lt. 200) then
   mu_erf+=10.0d0
  else
   mu_erf+=20.d0
  endif

 !For Krypton
 !if (mu_erf .lt. 10) then
 ! mu_erf += 1d0
 !elseif (mu_erf .lt. 40) then
 ! mu_erf+=2.d0
 !elseif (mu_erf .lt. 200) then
 ! mu_erf+=10.d0
 !elseif (mu_erf .lt. 400) then
 ! mu_erf+=20.0d0
 !elseif (mu_erf .lt. 1200) then
 ! mu_erf+=40.d0
 !else
 ! mu_erf+=80.d0
 !endif

 !For Xenon
 !if (mu_erf .lt. 20) then
 ! mu_erf += 2d0
 !elseif (mu_erf .lt. 80) then
 ! mu_erf+=4.d0
 !elseif (mu_erf .lt. 400) then
 ! mu_erf+=20.d0
 !elseif (mu_erf .lt. 800) then
 ! mu_erf+=40.0d0
 !elseif (mu_erf .lt. 2400) then
 ! mu_erf+=80.d0
 !else
 ! mu_erf+=160.d0
 !endif

 !For Radon
 !if (mu_erf .lt. 40) then
 ! mu_erf += 4d0
 !elseif (mu_erf .lt. 160) then
 ! mu_erf+=8.d0
 !elseif (mu_erf .lt. 800) then
 ! mu_erf+=40.d0
 !elseif (mu_erf .lt. 1600) then
 ! mu_erf+=80.0d0
 !elseif (mu_erf .lt. 4800) then
 ! mu_erf+=160.d0
 !else
 ! mu_erf+=320.d0
 !endif

 !For Oganesson
 !if (mu_erf .lt. 80) then
 ! mu_erf += 8d0
 !elseif (mu_erf .lt. 320) then
 ! mu_erf+=16.d0
 !elseif (mu_erf .lt. 1600) then
 ! mu_erf+=80.d0
 !elseif (mu_erf .lt. 3200) then
 ! mu_erf+=160.0d0
 !elseif (mu_erf .lt. 9600) then
 ! mu_erf+=320.d0
 !else
 ! mu_erf+=640.d0
 !endif

  call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)

end
