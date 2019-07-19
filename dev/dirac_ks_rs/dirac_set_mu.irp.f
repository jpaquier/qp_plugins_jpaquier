program dirac_set_mu
  BEGIN_DOC
  ! Set mu_erf as a choosen value of mu
  END_DOC

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
 call ezfio_set_ao_two_e_erf_ints_mu_erf(mu_erf)
 TOUCH mu_erf

 end
