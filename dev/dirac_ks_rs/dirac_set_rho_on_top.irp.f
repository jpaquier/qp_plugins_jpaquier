program dirac_set_rho_on_top
  BEGIN_DOC
  ! Set dirac_rho as rho_on_top
  END_DOC

  call ezfio_set_dirac_dft_keywords_dirac_rho("rho_on_top")
 TOUCH dirac_rho

 end
