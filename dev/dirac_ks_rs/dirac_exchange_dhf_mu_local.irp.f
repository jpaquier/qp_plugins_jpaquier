program dirac_exchange_dhf_mu_local
  BEGIN_DOC
  ! produce the dirac energy
  END_DOC
   call run_exchange_dhf_mu_local
 end

 subroutine run_exchange_dhf_mu_local
 BEGIN_DOC
 ! Gives the energy for a given value of mu_erf
 END_DOC
 use bitmasks
 implicit none 
 integer :: i,length
 double precision :: r(3)
 double precision, allocatable :: dm(:)
 allocate(dm(N_states))
 !Choose Interaction
  r(1) = dirac_r_x
  r(2) = dirac_r_y
  r(3) = dirac_r_z
  call dirac_dm_dft_at_r(r,dm)
  if (dirac_interaction == "Coulomb") then
   print*,'**********'
   print*,'Short-range Coulomb interaction'   
   print*, 'dirac_HF_two_electron_C_Exchange_energy=', dirac_HF_two_electron_c_ex_energy_mu_of_r/dm(1)
   open (10, file='local_exchange_dhf_Z.dat',position ='append') 
   write(10,*) dirac_r_x, dirac_HF_two_electron_c_ex_energy_mu_of_r/dm(1)
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif
!if (elec_num == 2) then
  if (nucl_charge(1) == 2 ) then
  !!For helium
   if (dirac_r_x .lt. 0.5d0) then
    dirac_r_x += 0.05d0
   elseif (dirac_r_x .lt. 1.5d0) then
    dirac_r_x += 0.1d0
   else
    dirac_r_x += 0.2d0
   endif
  elseif (nucl_charge(1) == 10 ) then
  !!For Neon
   if (dirac_r_x .lt. 0.1d0) then
    dirac_r_x += 0.01d0
   elseif (dirac_r_x .lt. 0.3d0) then
    dirac_r_x += 0.02d0
   else
    dirac_r_x += 0.04d0
   endif
  elseif (nucl_charge(1) == 18 ) then
  !!!.gt. 17.9d0 .and. nucl_charge(1) .lt. 18.1d0) then
  !!For Argon
   if (dirac_r_x .lt. 0.07d0) then
    dirac_r_x += 0.007d0
   elseif (dirac_r_x .lt. 0.21d0) then
    dirac_r_x += 0.014d0
   else
    dirac_r_x += 0.028d0
   endif
  elseif (nucl_charge(1) == 36 ) then
  !!For Krypton
   if (dirac_r_x .lt. 0.035d0) then
    dirac_r_x += 0.0035d0
   elseif (dirac_r_x .lt. 0.105d0) then
    dirac_r_x += 0.007d0
   else
    dirac_r_x += 0.014d0
   endif
  elseif (nucl_charge(1) == 54 ) then
  !!For Xenon
   if (dirac_r_x .lt. 0.002d0) then
    dirac_r_x += 0.0004d0
   elseif (dirac_r_x .lt. 0.010d0) then
    dirac_r_x += 0.0010d0
   elseif (dirac_r_x .lt. 0.04d0) then
    dirac_r_x += 0.0025d0
   else
    dirac_r_x += 0.010d0
   endif
  elseif (nucl_charge(1) == 70 ) then
  !!For Ytterbium
   if (dirac_r_x .lt. 0.001d0) then
    dirac_r_x += 0.0002d0
   elseif (dirac_r_x .lt. 0.005d0) then
    dirac_r_x += 0.0005d0
   elseif (dirac_r_x .lt. 0.02d0) then
    dirac_r_x += 0.0020d0
   else
    dirac_r_x += 0.010d0
   endif
  elseif (nucl_charge(1) == 86 ) then
  !!For Radon
   if (dirac_r_x .lt. 0.0005d0) then
    dirac_r_x += 0.0001d0
   elseif (dirac_r_x .lt. 0.001d0) then
    dirac_r_x += 0.00025d0
   elseif (dirac_r_x .lt. 0.01d0) then
    dirac_r_x += 0.001d0
   else
    dirac_r_x += 0.005d0
   endif
  elseif (nucl_charge(1) == 92 ) then
  !!For Uranium
   if (dirac_r_x .lt. 0.0005d0) then
    dirac_r_x += 0.00008d0
   elseif (dirac_r_x .lt. 0.001d0) then
    dirac_r_x += 0.0002d0
   elseif (dirac_r_x .lt. 0.01d0) then
    dirac_r_x += 0.001d0
   else
    dirac_r_x += 0.005d0
   endif
  endif 
!endif
  call ezfio_set_dirac_ao_basis_dirac_r_x(dirac_r_x)
  TOUCH dirac_r_x
end

