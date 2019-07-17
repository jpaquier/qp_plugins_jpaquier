program dirac_set_rho
  BEGIN_DOC
  ! Set dirac_rho as rho
  END_DOC

  call ezfio_set_dirac_dft_keywords_dirac_rho("rho")
 TOUCH dirac_rho

 end
