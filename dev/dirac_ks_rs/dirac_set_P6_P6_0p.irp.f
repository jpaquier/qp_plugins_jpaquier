program dirac_set_P6_P6_0p
  BEGIN_DOC
  ! Set dirac_rgga to P6_P6_0p
  END_DOC

  call ezfio_set_dirac_dft_utils_one_e_dirac_rgga("P6_P6_0p")
 TOUCH dirac_rgga

 end
