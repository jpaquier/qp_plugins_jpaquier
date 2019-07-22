program dirac_exchange_rgga_mu_local
  BEGIN_DOC
  ! produce the dirac energy
  END_DOC
   call run_exchange_rgga_mu_local
 end

 subroutine run_exchange_rgga_mu_local
 BEGIN_DOC
 ! Gives the energy for a given value of mu_erf
 END_DOC
 use bitmasks
 implicit none 
 integer :: i
 double precision :: r(3),e_x,v_x
 double precision, allocatable :: dm(:),tr_dm_2(:), grad_dm(:,:), grad_dm_2(:), grad_dm_abs(:)
 double precision, allocatable :: grad_dm_on_top(:,:), grad_dm_on_top_2(:), grad_dm_on_top_abs(:) 
 allocate(dm(N_states),tr_dm_2(N_states),                                  &        
        grad_dm(3,N_states), grad_dm_2(N_states), grad_dm_abs(N_states),   &      
        grad_dm_on_top(3,N_states), grad_dm_on_top_2(N_states), grad_dm_on_top_abs(N_states))
 r(1) = dirac_r_x
 r(2) = dirac_r_y
 r(3) = dirac_r_z
 call dirac_dm_dft_at_r(r,dm)
 call dirac_tr_dm_2_dft_at_r(r,tr_dm_2)
 call dirac_grad_dm_dft_at_r(r,grad_dm,grad_dm_2,grad_dm_abs)
 call dirac_grad_dm_on_top_dft_at_r(r,grad_dm_on_top,grad_dm_on_top_2,grad_dm_on_top_abs)
 call dirac_ex_RGGA_sr(mu_erf,dm(1),tr_dm_2(1),grad_dm_on_top(1,1),grad_dm_on_top(2,1),grad_dm_on_top(3,1),grad_dm_2(1),grad_dm_on_top_2(1),e_x,v_x)
 !Choose interaction
 if (dirac_interaction == "Coulomb") then
  print*,'**********'
  print*,'Short-range Coulomb interaction'   
  print*, 'dirac_rgga_C_exchange_energy=', e_x/dm(1)
  open (10, file='local_exchange_rgga_Z.dat',position ='append') 
  write(10,*) dirac_r_x, e_x/dm(1)
 else
  print *,  'Unrecognized dirac_interaction : '//dirac_interaction
  stop 1
 endif
 if (nucl_charge(1) .gt. 1.9d0 .and. nucl_charge(1) .lt. 2.1d0 ) then
 !!For helium
  if (dirac_r_x .lt. 0.5d0) then
   dirac_r_x += 0.05d0
  elseif (dirac_r_x .lt. 1.5d0) then
   dirac_r_x += 0.1d0
  else
   dirac_r_x += 0.2d0
  endif
 elseif (nucl_charge(1) .gt. 85.9d0 .and. nucl_charge(1) .lt. 86.1d0 ) then
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
 elseif (nucl_charge(1) .gt. 91.9d0 .and. nucl_charge(1) .lt. 92.1d0 ) then
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
  call ezfio_set_dirac_ao_basis_dirac_r_x(dirac_r_x)
  TOUCH dirac_r_x
 end

