
subroutine FOBOCI_lmct_mlct_old_thr(iter,norm_total)
  use bitmasks
 implicit none
 integer, intent(in) :: iter
 double precision, intent(out) :: norm_total(N_states)
 integer :: i,j,k,l
 integer(bit_kind),allocatable :: unpaired_bitmask(:,:)
 integer, allocatable :: occ(:,:)
 integer       :: n_occ_alpha, n_occ_beta
 double precision :: norm_tmp(N_states)
 logical :: test_sym
 double precision :: thr,hij
 double precision, allocatable :: dressing_matrix(:,:)
 logical :: verbose,is_ok,is_ok_perturbative
 verbose = .True.
 thr = 1.d-12
 allocate(unpaired_bitmask(N_int,2))
 allocate (occ(N_int*bit_kind_size,2))
 do i = 1, N_int
  unpaired_bitmask(i,1) = unpaired_alpha_electrons(i)
  unpaired_bitmask(i,2) = unpaired_alpha_electrons(i)
 enddo
 norm_total = 0.d0
 call initialize_density_matrix_osoci
 call bitstring_to_list(inact_bitmask(1,1), occ(1,1), n_occ_beta, N_int)
 print*,''
 print*,''
 print*,'mulliken spin population analysis'
 accu =0.d0
 do i = 1, nucl_num
  accu += mulliken_spin_densities(i)
  print*,i,nucl_charge(i),mulliken_spin_densities(i)
 enddo
 print*,''
 print*,''
 print*,'DOING FIRST LMCT !!'
 print*,'Threshold_lmct = ',threshold_lmct
 integer(bit_kind) , allocatable :: zero_bitmask(:,:)
 integer(bit_kind) , allocatable :: psi_singles(:,:,:)
 logical :: lmct
 double precision, allocatable :: psi_singles_coef(:,:)
 logical :: exit_loop
 norm_total = 0.d0
 norm_tmp = 0.d0
 allocate( zero_bitmask(N_int,2) )
  do i = 1, n_inact_orb
!  cycle
   lmct = .True.
   integer :: i_hole_osoci
   i_hole_osoci = list_inact(i)
!  if(i_hole_osoci.ne.37)cycle
   print*,'--------------------------'
   ! First set the current generators to the one of restart
   call check_symetry(i_hole_osoci,thr,test_sym)
   if(.not.test_sym)cycle
   call set_generators_to_generators_restart
   call set_psi_det_to_generators
   print*,'i_hole_osoci = ',i_hole_osoci
   call create_restart_and_1h(i_hole_osoci)
   call set_generators_to_psi_det
   print*,'Passed set generators'
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)
   double precision :: e_pt2
   call is_a_good_candidate(threshold_lmct,is_ok,e_pt2,verbose,exit_loop,is_ok_perturbative)
   print*,'is_ok = ',is_ok
   if(is_ok)then
    if(.not.do_it_perturbative)then
     allocate(dressing_matrix(N_det_generators,N_det_generators))
     dressing_matrix = 0.d0
      do k = 1, N_det_generators
       do l = 1, N_det_generators
         call i_h_j(psi_det_generators(1,1,k),psi_det_generators(1,1,l),N_int,hkl)
         dressing_matrix(k,l) = hkl
       enddo
      enddo
      hkl = dressing_matrix(1,1)
      do k = 1, N_det_generators
        dressing_matrix(k,k) = dressing_matrix(k,k) - hkl
      enddo
      print*,'Naked matrix'
      do k = 1, N_det_generators
       write(*,'(100(F12.5,X))')dressing_matrix(k,:)
      enddo
     
      ! Do all the single excitations on top of the CAS and 1h determinants
      call set_bitmask_particl_as_input(reunion_of_bitmask)
      call set_bitmask_hole_as_input(reunion_of_bitmask)
      call all_single(e_pt2)
      call make_s2_eigenfunction_first_order
      threshold_davidson = 1.d-8
      soft_touch threshold_davidson davidson_criterion
      call diagonalize_ci
     double precision :: hkl
     call provide_matrix_dressing(dressing_matrix,n_det_generators,psi_det_generators)
     hkl = dressing_matrix(1,1)
     do k = 1, N_det_generators
       dressing_matrix(k,k) = dressing_matrix(k,k) - hkl
     enddo
     print*,'Dressed matrix'
     do k = 1, N_det_generators
      write(*,'(100(F12.5,X))')dressing_matrix(k,:)
     enddo
     deallocate(dressing_matrix)
     call set_intermediate_normalization_lmct_old(norm_tmp,i_hole_osoci)
     call update_density_matrix_osoci
     do k = 1, N_states
      print*,'norm_tmp = ',norm_tmp(k)
      norm_total(k) += norm_tmp(k)
     enddo
    else  
     if(.not. is_ok_perturbative)cycle
     call set_intermediate_normalization_lmct_old(norm_tmp,i_hole_osoci)
     call update_density_matrix_osoci
     do k = 1, N_states
      print*,'norm_tmp = ',norm_tmp(k)
      norm_total(k) += norm_tmp(k)
     enddo
    endif
   endif

 enddo

 if(.True.)then
  print*,''
  print*,'DOING THEN THE MLCT !!'
  print*,'Threshold_mlct = ',threshold_mlct
   lmct = .False.
   do i = 1, n_virt_orb
    integer :: i_particl_osoci
    i_particl_osoci = list_virt(i)
!   if(i_particl_osoci.ne.90)cycle
    print*,'--------------------------'
    ! First set the current generators to the one of restart
    call check_symetry(i_particl_osoci,thr,test_sym)
    if(.not.test_sym)cycle
    call set_generators_to_generators_restart
    call set_psi_det_to_generators
    print*,'i_particl_osoci= ',i_particl_osoci
    ! Initialize the bitmask to the restart ones
    call initialize_bitmask_to_restart_ones
    ! Impose that only the hole i_hole_osoci can be done
    call modify_bitmasks_for_particl(i_particl_osoci)
    call print_generators_bitmasks_holes
    ! Impose that only the active part can be reached 
    call set_bitmask_hole_as_input(unpaired_bitmask)
!!  call all_single_h_core
    call create_restart_and_1p(i_particl_osoci)
!!  ! Update the generators 
    call set_generators_to_psi_det
    call set_bitmask_particl_as_input(reunion_of_bitmask)
    call set_bitmask_hole_as_input(reunion_of_bitmask)
!!  ! so all the mono excitation on the new generators 
    call is_a_good_candidate(threshold_mlct,is_ok,e_pt2,verbose,exit_loop,is_ok_perturbative)
    print*,'is_ok = ',is_ok
    if(is_ok)then
     if(.not.do_it_perturbative)then
      allocate(dressing_matrix(N_det_generators,N_det_generators))
      dressing_matrix = 0.d0
      do k = 1, N_det_generators
       do l = 1, N_det_generators
         call i_h_j(psi_det_generators(1,1,k),psi_det_generators(1,1,l),N_int,hkl)
         dressing_matrix(k,l) = hkl
       enddo
      enddo
      call all_single(e_pt2)
      call make_s2_eigenfunction_first_order
      threshold_davidson = 1.d-8
      soft_touch threshold_davidson davidson_criterion
      call diagonalize_ci
      deallocate(dressing_matrix)
      call set_intermediate_normalization_mlct_old(norm_tmp,i_particl_osoci)
      call update_density_matrix_osoci
      do k = 1, N_states
       print*,'norm_tmp = ',norm_tmp(k)
       norm_total(k) += norm_tmp(k)
      enddo
     else
!     if(.not. is_ok_perturbative)cycle
      call set_intermediate_normalization_mlct_old(norm_tmp,i_particl_osoci)
      call update_density_matrix_osoci
      do k = 1, N_states
       print*,'norm_tmp = ',norm_tmp(k)
       norm_total(k) += norm_tmp(k)
      enddo
     endif
    else
     if(.not. is_ok_perturbative)cycle
     call set_intermediate_normalization_mlct_old(norm_tmp,i_hole_osoci)
     call update_density_matrix_osoci
     if(exit_loop)then
      call set_generators_to_generators_restart
      call set_psi_det_to_generators
      exit
     endif
    endif
!   call update_density_matrix_osoci
  enddo
 endif

   do k = 1, N_states
    print*, norm_generators_restart(k),norm_total(k)
    norm_total(k) += norm_generators_restart(k)
   enddo
   double precision :: accu(N_states)
   accu = 0.d0
   do k =1 , N_states
    do i = 1, mo_tot_num
!    print*, one_body_dm_mo_alpha_osoci(i,i,k) /norm_total(k), one_body_dm_mo_beta_osoci(i,i,k) /norm_total(k)
     accu(k) += (one_body_dm_mo_alpha_osoci(i,i,k) /norm_total(k) + one_body_dm_mo_beta_osoci(i,i,k) /norm_total(k)) 
    enddo
    print*,'accu = ',accu(k)
   enddo
end



