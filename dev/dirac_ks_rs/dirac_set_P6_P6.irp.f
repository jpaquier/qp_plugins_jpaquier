program dirac_set_P6_P6
  BEGIN_DOC
  ! Set dirac_rgga to P6_P6
  END_DOC

  call ezfio_set_dirac_dft_utils_one_e_dirac_rgga("P6_P6")
 TOUCH dirac_rgga

 end
