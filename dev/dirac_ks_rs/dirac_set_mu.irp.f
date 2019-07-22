program dirac_set_mu
  BEGIN_DOC
  ! Set mu_erf as a choosen value of mu
  END_DOC
 
 if (dirac_mu_over_kFmax .gt. 0.049 .and. dirac_mu_over_kFmax .lt. 0.051) then
  if (nucl_charge(1) .gt. 9.9d0 .and. nucl_charge(1) .lt. 10.1d0 ) then
  !!For Neon
   mu_erf = 1.3d0
  elseif (nucl_charge(1) .gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0 ) then
  !!For argon
   mu_erf = 2.45d0
  elseif (nucl_charge(1) .gt. 35.9d0 .and. nucl_charge(1) .lt. 36.1d0 ) then
  !!For Krypton
   mu_erf = 5.45d0
  elseif (nucl_charge(1) .gt. 53.9d0 .and. nucl_charge(1) .lt. 54.1d0 ) then
  !!For Xenon
   mu_erf = 9.6d0
  elseif (nucl_charge(1) .gt. 69.9d0 .and. nucl_charge(1) .lt. 70.1d0 ) then
  !!For Ytterbium
   mu_erf = 15.d0
  elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
  !!For Radon
   mu_erf = 23.d0
  elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
  !!For Uranium 
   mu_erf = 27.d0
  endif
 elseif (dirac_mu_over_kFmax .gt. 0.099 .and. dirac_mu_over_kFmax .lt. 0.101) then
  if (nucl_charge(1) .gt. 9.9d0 .and. nucl_charge(1) .lt. 10.1d0 ) then
  !!For Neon
   mu_erf = 2.6d0
  elseif (nucl_charge(1) .gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0 ) then
  !!For argon
   mu_erf = 4.9d0
  elseif (nucl_charge(1) .gt. 35.9d0 .and. nucl_charge(1) .lt. 36.1d0 ) then
  !!For Krypton
   mu_erf = 10.9d0
  elseif (nucl_charge(1) .gt. 53.9d0 .and. nucl_charge(1) .lt. 54.1d0 ) then
  !!For Xenon
   mu_erf = 19.2d0
  elseif (nucl_charge(1) .gt. 69.9d0 .and. nucl_charge(1) .lt. 70.1d0 ) then
  !!For Ytterbium
   mu_erf = 30.d0
  elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
  !!For Radon
   mu_erf = 46.d0
  elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
  !!For Uranium 
   mu_erf = 54.d0
  endif
 elseif (dirac_mu_over_kFmax .gt. 0.199 .and. dirac_mu_over_kFmax .lt. 0.201) then
  if (nucl_charge(1) .gt. 9.9d0 .and. nucl_charge(1) .lt. 10.1d0 ) then
  !!For Neon
   mu_erf = 5.2d0
  elseif (nucl_charge(1) .gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0 ) then
  !!For argon
   mu_erf = 9.8d0
  elseif (nucl_charge(1) .gt. 35.9d0 .and. nucl_charge(1) .lt. 36.1d0 ) then
  !!For Krypton
   mu_erf = 21.8d0
  elseif (nucl_charge(1) .gt. 53.9d0 .and. nucl_charge(1) .lt. 54.1d0 ) then
  !!For Xenon
   mu_erf = 38.4d0
  elseif (nucl_charge(1) .gt. 69.9d0 .and. nucl_charge(1) .lt. 70.1d0 ) then
  !!For Ytterbium
   mu_erf = 60.d0
  elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
  !!For Radon
   mu_erf = 92.d0
  elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
  !!For Uranium 
   mu_erf = 108.d0
  endif
 elseif (dirac_mu_over_kFmax .gt. 0.399 .and. dirac_mu_over_kFmax .lt. 0.401) then
  if (nucl_charge(1) .gt. 9.9d0 .and. nucl_charge(1) .lt. 10.1d0 ) then
  !!For Neon
   mu_erf = 10.4d0
  elseif (nucl_charge(1) .gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0 ) then
  !!For argon
   mu_erf = 19.6d0
  elseif (nucl_charge(1) .gt. 35.9d0 .and. nucl_charge(1) .lt. 36.1d0 ) then
  !!For Krypton
   mu_erf = 43.6d0
  elseif (nucl_charge(1) .gt. 53.9d0 .and. nucl_charge(1) .lt. 54.1d0 ) then
  !!For Xenon
   mu_erf = 76.8d0
  elseif (nucl_charge(1) .gt. 69.9d0 .and. nucl_charge(1) .lt. 70.1d0 ) then
  !!For Ytterbium
   mu_erf = 120.d0
  elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
  !!For Radon
   mu_erf = 184.d0
  elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
  !!For Uranium 
   mu_erf = 216.d0
  endif
 elseif (dirac_mu_over_kFmax .gt. 0.799 .and. dirac_mu_over_kFmax .lt. 0.801) then
  if (nucl_charge(1) .gt. 9.9d0 .and. nucl_charge(1) .lt. 10.1d0 ) then
  !!For Neon
   mu_erf = 20.8d0
  elseif (nucl_charge(1) .gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0 ) then
  !!For argon
   mu_erf = 39.2d0
  elseif (nucl_charge(1) .gt. 35.9d0 .and. nucl_charge(1) .lt. 36.1d0 ) then
  !!For Krypton
   mu_erf = 87.2d0
  elseif (nucl_charge(1) .gt. 53.9d0 .and. nucl_charge(1) .lt. 54.1d0 ) then
  !!For Xenon
   mu_erf = 153.6d0
  elseif (nucl_charge(1) .gt. 69.9d0 .and. nucl_charge(1) .lt. 70.1d0 ) then
  !!For Ytterbium
   mu_erf = 240.d0
  elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
  !!For Radon
   mu_erf = 368.d0
  elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
  !!For Uranium 
   mu_erf = 432.d0
  endif
 endif

 call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)
 TOUCH mu_erf

 end
