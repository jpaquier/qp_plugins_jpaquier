program dirac_set_P6_nr
  BEGIN_DOC
  ! Set dirac_rgga to P6_nr
  END_DOC

  call ezfio_set_dirac_dft_utils_one_e_dirac_rgga("P6_nr")
 TOUCH dirac_rgga

 end
