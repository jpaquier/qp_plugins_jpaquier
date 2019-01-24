program dirac_exchange_dhf_mu
  BEGIN_DOC
  ! produce the dirac energy
  END_DOC
   call run_exchange_mu 
 end

 subroutine run_exchange_mu
  BEGIN_DOC
  ! Gives the energy for a given value of mu_erf
  END_DOC
  use bitmasks
  implicit none 
  integer :: i,length
 !Choose Interaction
  if (dirac_interaction == "Coulomb") then
   print*,'**********'
   print*,'Short-range Coulomb interaction'   
   print*, 'mu_erf =',mu_erf
   print*, 'dirac_HF_two_electron_C_Exchange_energy=', dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
 ! print*, 'dirac_HF_two_electron_C_Hartree_energy=', dirac_C_Hartree_Energy - dirac_HF_two_electron_C_Hartree_energy
   open (10, file='exchange_dhf_X.dat',position ='append') 
   write(10,*) mu_erf, dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
 ! open (11, file='Energy_DHF_SRH_C.dat',position ='append')
 ! write(11,*) mu_erf, dirac_C_Hartree_Energy - dirac_HF_two_electron_C_Hartree_energy
 !elseif (dirac_interaction == "Coulomb_Gaunt") then
 ! print*,'**********'
 ! print*,'Short-range Coulomb-Gaunt interaction'
 ! print*, 'mu_erf =',mu_erf
 ! print*, 'dirac_HF_two_electron_C_Exchange_energy=', dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
 ! print*, 'dirac_HF_two_electron_C_Hartree_energy=', dirac_C_Hartree_Energy - dirac_HF_two_electron_C_Hartree_energy
 ! print*, 'dirac_HF_two_electron_G_Exchange_energy=', dirac_G_Exchange_Energy - dirac_HF_two_electron_G_Exchange_energy
 ! print*, 'dirac_HF_two_electron_G_Hartree_energy=', dirac_G_Hartree_Energy - dirac_HF_two_electron_G_Hartree_energy
 ! print*, 'dirac_HF_two_electron_C_G_Exchange_energy=', dirac_C_G_Hartree_Energy - dirac_HF_two_electron_C_G_Exchange_energy
 ! print*, 'dirac_HF_two_electron_C_G_Hartree_energy=', dirac_C_G_Hartree_Energy - dirac_HF_two_electron_C_G_Hartree_energy
 ! open (10, file='Energy_DHF_SRX_C.dat',position ='append')
 ! write(10,*) mu_erf, dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
 ! open (11, file='Energy_DHF_SRH_C.dat',position ='append')
 ! write(11,*) mu_erf, dirac_C_Hartree_Energy - dirac_HF_two_electron_C_Hartree_energy
 ! open (12, file='Energy_DHF_SRX_G.dat',position ='append')
 ! write(12,*) mu_erf, dirac_G_Exchange_Energy - dirac_HF_two_electron_G_Exchange_energy
 ! open (13, file='Energy_DHF_SRH_G.dat',position ='append')
 ! write(13,*) mu_erf, dirac_G_Hartree_Energy - dirac_HF_two_electron_G_Hartree_energy
 ! open (14, file='Energy_DHF_SRX_C_G.dat',position ='append')
 ! write(14,*) mu_erf, dirac_C_G_Exchange_Energy - dirac_HF_two_electron_C_G_Exchange_energy
 ! open (20, file='Energy_DHF_SRH_C_G.dat',position ='append')
 ! write(20,*) mu_erf, dirac_C_G_Hartree_Energy - dirac_HF_two_electron_C_G_Hartree_energy
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
 ! mu_erf+=1.0d0
 !endif

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

 !For Radon
  if (mu_erf .lt. 40) then
   mu_erf += 4d0
  elseif (mu_erf .lt. 80) then
   mu_erf+=8.d0
  elseif (mu_erf .lt. 1600) then
   mu_erf+=40.d0
  endif

 !For Oganesson
 !if (mu_erf .lt. 80) then
 ! mu_erf += 8d0
 !elseif (mu_erf .lt. 160) then
 ! mu_erf+=16.d0
 !elseif (mu_erf .lt. 3200) then
 ! mu_erf+=80.d0
 !endif
 
  call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)

end
