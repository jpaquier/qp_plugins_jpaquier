program dirac_set_local_dhf
  BEGIN_DOC
  ! Set dirac_dhf to local_dhf
  END_DOC

  call ezfio_set_dirac_mu_of_r_ints_dirac_dhf("local_dhf")
 TOUCH dirac_dhf

 end
