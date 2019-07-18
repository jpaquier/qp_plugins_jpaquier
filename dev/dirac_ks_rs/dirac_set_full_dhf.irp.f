program dirac_set_full_dhf
  BEGIN_DOC
  ! Set dirac_dhf to full_dhf
  END_DOC

  call ezfio_set_dirac_mu_of_r_ints_dirac_dhf("full_dhf")
 TOUCH dirac_dhf

 end
