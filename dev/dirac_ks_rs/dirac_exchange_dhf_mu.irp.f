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
   open (10, file='exchange_dhf_Z.dat',position ='append') 
   write(10,*) dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
  elseif (dirac_interaction == "Coulomb_Gaunt") then
   print*,'**********'
   print*,'Short-range Coulomb-Gaunt interaction'
   print*, 'mu_erf =',mu_erf
 ! print*, 'dirac_HF_two_electron_C_Exchange_energy=', dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
 ! print*, 'dirac_HF_two_electron_G_Exchange_energy=', dirac_G_Exchange_Energy - dirac_HF_two_electron_G_Exchange_energy
   print*, 'dirac_HF_two_electron_C_G_Exchange_energy=', dirac_C_G_Exchange_Energy - dirac_HF_two_electron_C_G_Exchange_energy
 ! open (10, file='Energy_DHF_SRX_C.dat',position ='append')
 ! write(10,*) mu_erf, dirac_C_Exchange_Energy - dirac_HF_two_electron_C_Exchange_energy
 ! open (12, file='Energy_DHF_SRX_G.dat',position ='append')
 ! write(12,*) mu_erf, dirac_G_Exchange_Energy - dirac_HF_two_electron_G_Exchange_energy
   open (14, file='exchange_dhf_Z_CG.dat',position ='append')
   write(14,*) mu_erf, dirac_C_G_Exchange_Energy - dirac_HF_two_electron_C_G_Exchange_energy
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif

 if (nucl_charge(1) .gt. 1.9d0 .and. nucl_charge(1) .lt. 2.1d0 ) then
 !!For helium
  if (mu_erf .lt. 1.25) then
   mu_erf += 0.125d0
  elseif (mu_erf .lt. 2.5) then
   mu_erf+=0.25d0
  elseif (mu_erf .lt. 25) then
   mu_erf+=1.25d0
  else 
   mu_erf+=2.5d0
  endif
 elseif (nucl_charge(1) .gt. 3.9d0 .and. nucl_charge(1) .lt. 4.1d0 ) then
 !!For berylium
  if (mu_erf .lt. 1.25) then
   mu_erf += 0.125d0
  elseif (mu_erf .lt. 2.5) then
   mu_erf+=0.25d0
  elseif (mu_erf .lt. 25) then
   mu_erf+=1.25d0
  else
   mu_erf+=5d0
  endif
 elseif (nucl_charge(1) .gt. 9.9d0 .and. nucl_charge(1) .lt. 10.1d0 ) then
 !!For Neon
  if (mu_erf .lt. 2.5) then
   mu_erf += 0.25d0
  elseif (mu_erf .lt. 10) then
   mu_erf+=0.5d0
  elseif (mu_erf .lt. 50) then
   mu_erf+=2.5d0
  elseif (mu_erf .lt. 100) then
   mu_erf+=5.0d0
  else
   mu_erf+=10.d0
  endif
 elseif (nucl_charge(1) .gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0 ) then
 !!For argon
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
 elseif (nucl_charge(1) .gt. 19.9d0 .and. nucl_charge(1) .lt. 20.1d0 ) then
 !!For Calcium
  if (mu_erf .lt. 5.5) then
   mu_erf += 0.55d0
  elseif (mu_erf .lt. 22) then
   mu_erf+=1.1d0
  elseif (mu_erf .lt. 110) then
   mu_erf+=5.5d0
  elseif (mu_erf .lt. 220) then
   mu_erf+=11.0d0
  else
   mu_erf+=22.d0
  endif
 elseif (nucl_charge(1) .gt. 29.9d0 .and. nucl_charge(1) .lt. 30.1d0 ) then
 !!For Zinc
  if (mu_erf .lt. 8) then
   mu_erf += 0.8d0
  elseif (mu_erf .lt. 32) then
   mu_erf+=1.6d0
  elseif (mu_erf .lt. 160) then
   mu_erf+=8.d0
  elseif (mu_erf .lt. 320) then
   mu_erf+=16.0d0
  else
   mu_erf+=32.d0
  endif
 elseif (nucl_charge(1) .gt. 35.9d0 .and. nucl_charge(1) .lt. 36.1d0 ) then
 !!For Krypton
  if (mu_erf .lt. 10) then
   mu_erf += 1d0
  elseif (mu_erf .lt. 40) then
   mu_erf+=2.d0
  elseif (mu_erf .lt. 200) then
   mu_erf+=10.d0
  elseif (mu_erf .lt. 400) then
   mu_erf+=20.0d0
  elseif (mu_erf .lt. 1200) then
   mu_erf+=40.d0
  else
   mu_erf+=80.d0
  endif
 elseif (nucl_charge(1) .gt. 53.9d0 .and. nucl_charge(1) .lt. 54.1d0 ) then
 !!For Xenon
  if (mu_erf .lt. 20) then
   mu_erf += 2d0
  elseif (mu_erf .lt. 80) then
   mu_erf+=4.d0
  elseif (mu_erf .lt. 400) then
   mu_erf+=20.d0
  elseif (mu_erf .lt. 800) then
   mu_erf+=40.0d0
  elseif (mu_erf .lt. 2400) then
   mu_erf+=80.d0
  else
   mu_erf+=160.d0
  endif
 elseif (nucl_charge(1) .gt. 69.9d0 .and. nucl_charge(1) .lt. 70.1d0 ) then
 !!For Ytterbium
  if (mu_erf .lt. 30) then
   mu_erf += 3d0
  elseif (mu_erf .lt. 120) then
   mu_erf+=6.d0
  elseif (mu_erf .lt. 600) then
   mu_erf+=30.d0
  elseif (mu_erf .lt. 1200) then
   mu_erf+=60.0d0
  elseif (mu_erf .lt. 3600) then
   mu_erf+=120.d0
  else
   mu_erf+=240.d0
  endif
 elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
 !!For Radon
  if (mu_erf .lt. 40) then
   mu_erf += 4d0
  elseif (mu_erf .lt. 160) then
   mu_erf+=8.d0
  elseif (mu_erf .lt. 800) then
   mu_erf+=40.d0
  elseif (mu_erf .lt. 1600) then
   mu_erf+=80.0d0
  elseif (mu_erf .lt. 4800) then
   mu_erf+=160.d0
  else
   mu_erf+=320.d0
  endif
 elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
 !!For Uranium 
  if (mu_erf .lt. 50) then
   mu_erf += 5d0
  elseif (mu_erf .lt. 200) then
   mu_erf+=10.d0
  elseif (mu_erf .lt. 1000) then
   mu_erf+=50.d0
  elseif (mu_erf .lt. 2000) then
   mu_erf+=100.0d0
  elseif (mu_erf .lt. 600) then
   mu_erf+=200.d0
  else
   mu_erf+=400.d0
  endif
 elseif (nucl_charge(1) .gt. 117.9d0 .and. nucl_charge(1) .lt. 118.1d0 ) then
 !!For Oganesson
  if (mu_erf .lt. 80) then
   mu_erf += 8d0
  elseif (mu_erf .lt. 320) then
   mu_erf+=16.d0
  elseif (mu_erf .lt. 1600) then
   mu_erf+=80.d0
  elseif (mu_erf .lt. 3200) then
   mu_erf+=160.0d0
  elseif (mu_erf .lt. 9600) then
   mu_erf+=320.d0
  else
   mu_erf+=640.d0
  endif
 endif
 call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)
 TOUCH mu_erf
 
end

