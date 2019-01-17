
use map_module

BEGIN_PROVIDER [ type(map_type), two_body_dm_ab_map ]
  implicit none
  BEGIN_DOC
  ! Map of the two body density matrix elements for the alpha/beta elements
  END_DOC
  integer(key_kind)              :: key_max
  integer(map_size_kind)         :: sze
  use map_module
  call bielec_integrals_index(mo_tot_num,mo_tot_num,mo_tot_num,mo_tot_num,key_max)
  sze = key_max
  call map_init(two_body_dm_ab_map,sze)
  print*, 'two_body_dm_ab_map initialized'
END_PROVIDER

subroutine insert_into_two_body_dm_ab_map(n_product,buffer_i, buffer_values, thr)
  use map_module
  implicit none
  
  BEGIN_DOC
  ! Create new entry into two_body_dm_ab_map, or accumulate in an existing entry
  END_DOC
  
  integer, intent(in)                :: n_product 
  integer(key_kind), intent(inout)   :: buffer_i(n_product)
  real(integral_kind), intent(inout) :: buffer_values(n_product)
  real(integral_kind), intent(in)    :: thr
  call map_update(two_body_dm_ab_map, buffer_i, buffer_values, n_product, thr)
end

double precision function get_two_body_dm_ab_map_element(i,j,k,l,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns one value of the wo body density matrix \rho_{ijkl}^{\alpha \beta} defined as :
  ! \rho_{ijkl}^{\alpha \beta  } = <\Psi|a^{\dagger}_{i\alpha} a^{\dagger}_{j\beta} a_{k\beta} a_{l\alpha}|\Psi>
  END_DOC
  PROVIDE two_body_dm_ab_map

  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx
  type(map_type), intent(inout)  :: map
  real(integral_kind)            :: tmp
  PROVIDE two_body_dm_in_map
  !DIR$ FORCEINLINE
  call bielec_integrals_index(i,j,k,l,idx)
  !DIR$ FORCEINLINE
  call map_get(two_body_dm_ab_map,idx,tmp)
  get_two_body_dm_ab_map_element = dble(tmp)
end

subroutine get_get_two_body_dm_ab_map_elements(j,k,l,sze,out_val,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple elements of the \rho_{ijkl}^{\alpha \beta }, all
  ! i for j,k,l fixed.
  END_DOC
  integer, intent(in)            :: j,k,l, sze
  double precision, intent(out)  :: out_val(sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i
  integer(key_kind)              :: hash(sze)
  real(integral_kind)            :: tmp_val(sze)
  PROVIDE two_body_dm_in_map
  
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,hash(i))
  enddo
  
  if (key_kind == 8) then
    call map_get_many(two_body_dm_ab_map, hash, out_val, sze)
  else
    call map_get_many(two_body_dm_ab_map, hash, tmp_val, sze)
    ! Conversion to double precision 
    do i=1,sze
      out_val(i) = dble(tmp_val(i))
    enddo
  endif
end

BEGIN_PROVIDER [ logical, two_body_dm_in_map ]
  implicit none

  BEGIN_DOC
  ! If True, the map of the two body density matrix alpha/beta is provided
  END_DOC

  two_body_dm_in_map = .True.
  call add_values_to_two_body_dm_map(full_ijkl_bitmask_4)
END_PROVIDER

subroutine add_values_to_two_body_dm_map(mask_ijkl)
  use bitmasks
  use map_module
  implicit none

  BEGIN_DOC
  ! Adds values to the map of two_body_dm according to some bitmask
  END_DOC

  integer(bit_kind), intent(in)  :: mask_ijkl(N_int,4)
  integer                        :: degree

  PROVIDE mo_coef psi_coef psi_det

  integer                        :: exc(0:2,2,2)
  integer                        :: h1,h2,p1,p2,s1,s2
  double precision               :: phase
  double precision               :: contrib
  integer(key_kind),allocatable  :: buffer_i(:)
  double precision ,allocatable  :: buffer_value(:)
  integer                        :: size_buffer
  integer                        :: n_elements
  integer                        :: occ(N_int*bit_kind_size,2)
  integer                        :: n_occ_ab(2)
  integer :: i,j,k,l,m
  
  size_buffer = min(mo_tot_num*mo_tot_num*mo_tot_num,16000000)

  allocate(buffer_i(size_buffer),buffer_value(size_buffer))

  n_elements = 0
  do i = 1, N_det ! i == |I>
   call bitstring_to_list_ab(psi_det(1,1,i), occ, n_occ_ab, N_int)
   do j = i+1, N_det ! j == <J|
    call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree,N_int)
    if(degree>2)cycle
    call get_excitation(psi_det(1,1,i),psi_det(1,1,j),exc,degree,phase,N_int)
    call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
    if(degree==2)then  ! case of the DOUBLE EXCITATIONS  ************************************

     if(s1==s2)cycle  ! Only the alpha/beta two body density matrix
     ! <J| a^{\dagger}_{p1 s1} a^{\dagger}_{p2 s2} a_{h2 s2} a_{h1 s1} |I> * c_I * c_J
     if(h1>p1)cycle
     if(h2>p2)cycle
!    if(s1.ne.1)cycle
     n_elements += 1
     contrib = psi_coef(i,1) * psi_coef(j,1) * phase
     buffer_value(n_elements) = contrib
     !DIR$ FORCEINLINE
     call mo_bielec_integrals_index(h1,h2,p1,p2,buffer_i(n_elements))
!    if (n_elements == size_buffer) then
!      call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
!          real(mo_integrals_threshold,integral_kind))
!      n_elements = 0
!    endif

    else ! case of the SINGLE EXCITATIONS  ***************************************************
     cycle

!    if(s1==1)then  ! Mono alpha : 
!     do k = 1, elec_beta_num  
!      m = occ(k,2)
!      n_elements += 1
!      buffer_value(n_elements) = contrib
!      ! <J| a^{\dagger}_{p1 \alpha} \hat{n}_{m \beta} a_{h1 \alpha} |I> * c_I * c_J
!      call mo_bielec_integrals_index(h1,m,p1,m,buffer_i(n_elements))
!      if (n_elements == size_buffer) then
!        call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
!            real(mo_integrals_threshold,integral_kind))
!        n_elements = 0
!      endif
!     enddo
!    else  ! Mono Beta : 
!     do k = 1, elec_alpha_num
!      m = occ(k,1)
!      n_elements += 1
!      buffer_value(n_elements) = contrib
!      ! <J| a^{\dagger}_{p1 \beta} \hat{n}_{m \alpha} a_{h1 \beta} |I> * c_I * c_J
!      call mo_bielec_integrals_index(h1,m,p1,m,buffer_i(n_elements))
!      if (n_elements == size_buffer) then
!        call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
!            real(mo_integrals_threshold,integral_kind))
!        n_elements = 0
!      endif
!     enddo
!    endif

    endif
   enddo
  enddo
  print*,'n_elements = ',n_elements
  call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  call map_merge(two_body_dm_ab_map)

  deallocate(buffer_i,buffer_value)

end

 BEGIN_PROVIDER [double precision, two_body_dm_ab_diag_inact, (n_inact_orb_allocate, n_inact_orb_allocate)]
&BEGIN_PROVIDER [double precision, two_body_dm_ab_diag_core, (n_core_orb_allocate, n_core_orb_allocate)]
&BEGIN_PROVIDER [double precision, two_body_dm_ab_diag_all, (mo_tot_num, mo_tot_num,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_diag_core_a_act_b, (n_core_orb_allocate,n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_diag_core_b_act_a, (n_core_orb_allocate,n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_diag_core_act, (n_core_orb_allocate,n_act_orb,N_states)]
 implicit none
 use bitmasks
 integer :: i,j,k,l,m
 integer                        :: occ(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab(2)
 integer                        :: occ_act(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab_act(2)
 integer                        :: occ_core(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab_core(2)
 double precision :: contrib(N_states)
 integer :: i_state
 BEGIN_DOC 
 ! two_body_dm_ab_diag_all(k,m) = <\Psi | n_(k\alpha) n_(m\beta) | \Psi>
 ! two_body_dm_ab_diag_act(k,m) is restricted to the active orbitals : 
 ! orbital k = list_act(k)
 ! two_body_dm_ab_diag_inact(k,m) is restricted to the inactive orbitals : 
 ! orbital k = list_inact(k)
 ! two_body_dm_ab_diag_core(k,m) is restricted to the core orbitals : 
 ! orbital k = list_core(k)
 ! two_body_dm_ab_diag_core_b_act_a(k,m) represents the core beta <-> active alpha part of the two body dm
 ! orbital k = list_core(k)
 ! orbital m = list_act(m)
 ! two_body_dm_ab_diag_core_a_act_b(k,m) represents the core alpha <-> active beta part of the two body dm
 ! orbital k = list_core(k)
 ! orbital m = list_act(m)
 ! two_body_dm_ab_diag_core_act(k,m) represents the core<->active part of the diagonal two body dm 
 ! when we traced on the spin 
 ! orbital k = list_core(k)
 ! orbital m = list_act(m)
 END_DOC
 integer(bit_kind) :: key_tmp_core(N_int,2)
 integer(bit_kind) :: key_tmp_act(N_int,2)

 two_body_dm_ab_diag_all = 0.d0
 two_body_dm_ab_diag_core = 0.d0
 two_body_dm_ab_diag_inact = 0.d0
 two_body_dm_diag_core_a_act_b = 0.d0
 two_body_dm_diag_core_b_act_a = 0.d0
 two_body_dm_diag_core_act = 0.d0
 
  do i = 1, N_det ! i == |I>
   ! Full diagonal part of the two body dm
   do i_state = 1, N_states
    contrib(i_state) = psi_coef(i,i_state)**2
   enddo
   call bitstring_to_list_ab(psi_det(1,1,i), occ, n_occ_ab, N_int)
   do j = 1, elec_beta_num  
    k = occ(j,2)
    do l = 1, elec_alpha_num
     m = occ(l,1)
     do i_state = 1, N_states
      two_body_dm_ab_diag_all(k,m,i_state) +=  contrib(i_state)
      two_body_dm_ab_diag_all(m,k,i_state) +=  contrib(i_state)
     enddo
    enddo
   enddo


   ! CORE PART of the diagonal part of the two body dm
   do j = 1, N_int
    key_tmp_core(j,1) = psi_det(j,1,i)
    key_tmp_core(j,2) = psi_det(j,2,i)
   enddo
   do j = 1, N_int
    key_tmp_core(j,1) = iand(key_tmp_core(j,1),core_bitmask(j,1))
    key_tmp_core(j,2) = iand(key_tmp_core(j,2),core_bitmask(j,1))
   enddo
   call bitstring_to_list_ab(key_tmp_core, occ_core, n_occ_ab_core, N_int)
   do j = 1,n_occ_ab_core(2)
    k = list_core_reverse(occ_core(j,2))
    do l = 1, n_occ_ab_core(1)
     m = list_core_reverse(occ_core(l,1))
     two_body_dm_ab_diag_core(k,m) +=  contrib(1)
     two_body_dm_ab_diag_core(m,k) +=  contrib(1)
    enddo
   enddo

   ! ACT<->CORE PART 
   ! alpha electron in active space 
   do j = 1,n_occ_ab_act(1)
    k = list_act_reverse(occ_act(j,1))
    ! beta electron in core space 
    do l = 1, n_occ_ab_core(2)
     m = list_core_reverse(occ_core(l,2))
     ! The fact that you have 1 * contrib and not 0.5 * contrib 
     ! takes into account the following symmetry : 
     ! 0.5 * <n_k n_m> + 0.5 * <n_m n_k>
     do i_state = 1, N_states
      two_body_dm_diag_core_b_act_a(m,k,i_state) += contrib(i_state) 
     enddo
    enddo
   enddo
   ! beta electron in active space 
   do j = 1,n_occ_ab_act(2)
    k = list_act_reverse(occ_act(j,2))
    ! alpha electron in core space 
    do l = 1, n_occ_ab_core(1)
     m = list_core_reverse(occ_core(l,1))
     ! The fact that you have 1 * contrib and not 0.5 * contrib 
     ! takes into account the following symmetry : 
     ! 0.5 * <n_k n_m> + 0.5 * <n_m n_k>
     do i_state = 1, N_states
      two_body_dm_diag_core_a_act_b(m,k,i_state) += contrib(i_state) 
     enddo
    enddo
   enddo
  enddo

 do i_state = 1, N_states
  do j = 1, n_core_orb 
   do l = 1, n_act_orb
    two_body_dm_diag_core_act(j,l,i_state) = two_body_dm_diag_core_b_act_a(j,l,i_state) + two_body_dm_diag_core_a_act_b(j,l,i_state)
   enddo
  enddo
 enddo
END_PROVIDER 


 BEGIN_PROVIDER [double precision, two_body_dm_ab_diag_act, (n_act_orb, n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_bb_diag_act, (n_act_orb, n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_aa_diag_act, (n_act_orb, n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_bb_diag_exchange_act, (n_act_orb, n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_aa_diag_exchange_act, (n_act_orb, n_act_orb,N_States)]
 implicit none
 use bitmasks
 integer :: i,j,k,l,m
 integer                        :: occ(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab(2)
 integer                        :: occ_act(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab_act(2)
 integer                        :: occ_core(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab_core(2)
 double precision :: contrib(N_states)
 BEGIN_DOC 
 ! two_body_dm_ab_diag_all(k,m) = <\Psi | n_(k\alpha) n_(m\beta) | \Psi>
 ! two_body_dm_ab_diag_act(k,m) is restricted to the active orbitals : 
 ! orbital k = list_act(k)
 ! two_body_dm_ab_diag_inact(k,m) is restricted to the inactive orbitals : 
 ! orbital k = list_inact(k)
 ! two_body_dm_ab_diag_core(k,m) is restricted to the core orbitals : 
 ! orbital k = list_core(k)
 ! two_body_dm_ab_diag_core_b_act_a(k,m) represents the core beta <-> active alpha part of the two body dm
 ! orbital k = list_core(k)
 ! orbital m = list_act(m)
 ! two_body_dm_ab_diag_core_a_act_b(k,m) represents the core alpha <-> active beta part of the two body dm
 ! orbital k = list_core(k)
 ! orbital m = list_act(m)
 ! two_body_dm_ab_diag_core_act(k,m) represents the core<->active part of the diagonal two body dm 
 ! when we traced on the spin 
 ! orbital k = list_core(k)
 ! orbital m = list_act(m)
 END_DOC
 integer(bit_kind) :: key_tmp_core(N_int,2)
 integer(bit_kind) :: key_tmp_act(N_int,2)

 two_body_dm_ab_diag_act = 0.d0
 two_body_dm_aa_diag_act = 0.d0
 two_body_dm_bb_diag_act = 0.d0
 two_body_dm_aa_diag_exchange_act = 0.d0
 two_body_dm_bb_diag_exchange_act = 0.d0
 
  integer :: i_state
 do i_state = 1, N_states
  do i = 1, N_det ! i == |I>
   contrib(i_state) = psi_coef(i,i_state)**2
   ! ACTIVE PART of the diagonal part of the two body dm
   do j = 1, N_int
    key_tmp_act(j,1) = psi_det(j,1,i)
    key_tmp_act(j,2) = psi_det(j,2,i)
   enddo
   do j = 1, N_int
    key_tmp_act(j,1) = iand(key_tmp_act(j,1),act_bitmask(j,1))
    key_tmp_act(j,2) = iand(key_tmp_act(j,2),act_bitmask(j,1))
   enddo
   call bitstring_to_list_ab(key_tmp_act, occ_act, n_occ_ab_act, N_int)
   do j = 1,n_occ_ab_act(2)
    k = list_act_reverse(occ_act(j,2))
     do l = 1, n_occ_ab_act(1)
      m = list_act_reverse(occ_act(l,1))
      two_body_dm_ab_diag_act(k,m,i_state) +=  contrib(i_state)
      two_body_dm_ab_diag_act(m,k,i_state) +=  contrib(i_state)
     enddo
   enddo
   contrib = contrib * 0.5d0 
   do j = 1,n_occ_ab_act(2)
    k = list_act_reverse(occ_act(j,2))
    do l = 1, n_occ_ab_act(2)
     m = list_act_reverse(occ_act(l,2))
     two_body_dm_bb_diag_act(k,m,i_state) +=  contrib(i_state)
     two_body_dm_bb_diag_act(m,k,i_state) +=  contrib(i_state)
     two_body_dm_bb_diag_exchange_act(k,m,i_state) -=   contrib(i_state)
     two_body_dm_bb_diag_exchange_act(m,k,i_state) -=   contrib(i_state)
    enddo
   enddo

   do j = 1,n_occ_ab_act(1)
    k = list_act_reverse(occ_act(j,1))
    do l = 1, n_occ_ab_act(1)
     m = list_act_reverse(occ_act(l,1))
     two_body_dm_aa_diag_act(k,m,i_state) += contrib(i_state)
     two_body_dm_aa_diag_act(m,k,i_state) += contrib(i_state)
     two_body_dm_aa_diag_exchange_act(k,m,i_state) -=  contrib(i_state)
     two_body_dm_aa_diag_exchange_act(m,k,i_state) -=  contrib(i_state)
    enddo
   enddo

  enddo

 enddo
END_PROVIDER 


 BEGIN_PROVIDER [double precision, two_body_dm_ab_big_array_act, (n_act_orb,n_act_orb,n_act_orb,n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_aa_big_array_act, (n_act_orb,n_act_orb,n_act_orb,n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_bb_big_array_act, (n_act_orb,n_act_orb,n_act_orb,n_act_orb,N_states)]
&BEGIN_PROVIDER [double precision, two_body_dm_ab_big_array_core_act, (n_core_orb_allocate,n_act_orb,n_act_orb,N_states)]
 implicit none
 use bitmasks
 integer :: i,j,k,l,m
 integer                        :: degree
 PROVIDE mo_coef psi_coef psi_det
 integer                        :: exc(0:2,2,2)
 integer                        :: h1,h2,p1,p2,s1,s2
 double precision               :: phase
 double precision               :: contrib(N_states)
 integer                        :: occ(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab(2)
 integer                        :: occ_core(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab_core(2)
 integer(bit_kind) :: key_tmp_i(N_int,2)
 integer(bit_kind) :: key_tmp_i_core(N_int,2)
 integer(bit_kind) :: key_tmp_j(N_int,2)
 two_body_dm_ab_big_array_act = 0.d0
 two_body_dm_ab_big_array_core_act = 0.d0
 two_body_dm_aa_big_array_act = 0.d0
 two_body_dm_bb_big_array_act = 0.d0
 integer :: i_state
 BEGIN_DOC
! two_body_dm_ab_big_array_act = Purely active part of the two body density matrix 
! two_body_dm_ab_big_array_act_core takes only into account the single excitation 
! within the active space that adds terms in the act <-> core two body dm 
! two_body_dm_ab_big_array_act_core(i,j,k)  = < a^\dagger_i n_k a_j > 
!                                              with i,j in the ACTIVE SPACE 
!                                              with k in the CORE SPACE 
! 
! The alpha-beta extra diagonal energy FOR WF DEFINED AS AN APPROXIMATION OF A CAS can be computed thanks to 
!    sum_{h1,p1,h2,p2} two_body_dm_ab_big_array_act(h1,p1,h2,p2) * (h1p1|h2p2)
! +  sum_{h1,p1,h2,p2} two_body_dm_ab_big_array_core_act(h1,p1,h2,p2) * (h1p1|h2p2)
 END_DOC

 do i = 1, N_det ! i == |I>
  ! active part of psi_det(i)
  do j = 1, N_int
   key_tmp_i(j,1) = psi_det(j,1,i)
   key_tmp_i(j,2) = psi_det(j,2,i)
   key_tmp_i_core(j,1) = psi_det(j,1,i)
   key_tmp_i_core(j,2) = psi_det(j,2,i)
  enddo
  do j = 1, N_int
   key_tmp_i(j,1) = iand(key_tmp_i(j,1),act_bitmask(j,1))
   key_tmp_i(j,2) = iand(key_tmp_i(j,2),act_bitmask(j,1))
  enddo
  do j = 1, N_int
   key_tmp_i_core(j,1) = iand(key_tmp_i_core(j,1),core_bitmask(j,1))
   key_tmp_i_core(j,2) = iand(key_tmp_i_core(j,2),core_bitmask(j,1))
  enddo
  call bitstring_to_list_ab(key_tmp_i_core, occ_core, n_occ_ab_core, N_int)
  call bitstring_to_list_ab(key_tmp_i, occ, n_occ_ab, N_int)
  do j = i+1, N_det ! j == <J|
   ! active part of psi_det(j)
   do k = 1, N_int
    key_tmp_j(k,1) = psi_det(k,1,j)
    key_tmp_j(k,2) = psi_det(k,2,j)
   enddo
   do k = 1, N_int
    key_tmp_j(k,1) = iand(key_tmp_j(k,1),act_bitmask(k,1))
    key_tmp_j(k,2) = iand(key_tmp_j(k,2),act_bitmask(k,1))
   enddo
   ! control if the two determinants are connected by 
   ! at most a double excitation WITHIN THE ACTIVE SPACE 
   call get_excitation_degree(key_tmp_i,key_tmp_j,degree,N_int)
   if(degree>2)cycle
   ! if it is the case, then compute the hamiltonian matrix element with the proper phase 
   call get_excitation(psi_det(1,1,i),psi_det(1,1,j),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   do i_state = 1, N_states
    contrib(i_state) =  psi_coef(i,i_state) * psi_coef(j,i_state) * phase
   enddo
   if(degree==2)then  ! case of the DOUBLE EXCITATIONS  ************************************
     h1 = list_act_reverse(h1)
     h2 = list_act_reverse(h2)
     p1 = list_act_reverse(p1)
     p2 = list_act_reverse(p2)
    if(s1==s2)then
     if(s1==1)then
      call insert_into_two_body_dm_big_array( two_body_dm_aa_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,h2,p2)
      call insert_into_two_body_dm_big_array( two_body_dm_aa_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,-contrib,h1,p2,h2,p1)
     else
      call insert_into_two_body_dm_big_array( two_body_dm_bb_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,h2,p2)
      call insert_into_two_body_dm_big_array( two_body_dm_bb_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,-contrib,h1,p2,h2,p1)
     endif
    else 
     ! <J| a^{\dagger}_{p1 s1} a^{\dagger}_{p2 s2} a_{h2 s2} a_{h1 s1} |I> * c_I * c_J
     call insert_into_two_body_dm_big_array( two_body_dm_ab_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,h2,p2)
    endif

   else if(degree==1)then! case of the SINGLE EXCITATIONS  ***************************************************
    h1 = list_act_reverse(h1)
    p1 = list_act_reverse(p1)
    
    if(s1==1)then  ! Mono alpha : 
     ! purely active part of the extra diagonal two body dm 
     do k = 1, n_occ_ab(2)
      m = list_act_reverse(occ(k,2))
      ! <J| a^{\dagger}_{p1 \alpha} \hat{n}_{m \beta} a_{h1 \alpha} |I> * c_I * c_J
      call insert_into_two_body_dm_big_array( two_body_dm_ab_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,m,m)
     enddo

     do k = 1, n_occ_ab(1)
      m = list_act_reverse(occ(k,1))
      ! <J| a^{\dagger}_{p1 \alpha} \hat{n}_{m \beta} a_{h1 \alpha} |I> * c_I * c_J
      call insert_into_two_body_dm_big_array( two_body_dm_aa_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,m,m)
      call insert_into_two_body_dm_big_array( two_body_dm_aa_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,-contrib,h1,m,p1,m)
     enddo
     
     
     ! core <-> active part of the extra diagonal two body dm 
     do k = 1, n_occ_ab_core(2)
      m = list_core_reverse(occ_core(k,2))
      ! <J| a^{\dagger}_{p1 \alpha} \hat{n}_{m \beta} a_{h1 \alpha} |I> * c_I * c_J
      do i_state = 1, N_states
       two_body_dm_ab_big_array_core_act(m,h1,p1,i_state) +=  contrib(i_state)
       two_body_dm_ab_big_array_core_act(m,p1,h1,i_state) +=  contrib(i_state)
      enddo
     enddo
    else  ! Mono Beta : 
     ! purely active part of the extra diagonal two body dm 
     do k = 1, n_occ_ab(1)
      m = list_act_reverse(occ(k,1))
      ! <J| a^{\dagger}_{p1 \beta} \hat{n}_{m \alpha} a_{h1 \beta} |I> * c_I * c_J
      call insert_into_two_body_dm_big_array(two_body_dm_ab_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,m,m)
     enddo
     
     do k = 1, n_occ_ab(2)
      m = list_act_reverse(occ(k,2))
      ! <J| a^{\dagger}_{p1 \beta} \hat{n}_{m \alpha} a_{h1 \beta} |I> * c_I * c_J
      call insert_into_two_body_dm_big_array(two_body_dm_bb_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,contrib,h1,p1,m,m)
      call insert_into_two_body_dm_big_array(two_body_dm_bb_big_array_act,n_act_orb,n_act_orb,n_act_orb,n_act_orb,-contrib,h1,m,p1,m)
     enddo
     
     ! core <-> active part of the extra diagonal two body dm 
     do k = 1, n_occ_ab_core(1)
      m = list_core_reverse(occ_core(k,1))
      ! <J| a^{\dagger}_{p1 \alpha} \hat{n}_{m \beta} a_{h1 \alpha} |I> * c_I * c_J
      do i_state = 1, N_states
       two_body_dm_ab_big_array_core_act(m,h1,p1,i_state) +=  contrib(i_state)
       two_body_dm_ab_big_array_core_act(m,p1,h1,i_state) +=  contrib(i_state) 
      enddo
     enddo
    endif

   endif
  enddo
 enddo
 print*,'Big array for density matrix provided !'

END_PROVIDER 

subroutine insert_into_two_body_dm_big_array(big_array,dim1,dim2,dim3,dim4,contrib,h1,p1,h2,p2)
 implicit none
 integer, intent(in) :: h1,p1,h2,p2
 integer, intent(in) :: dim1,dim2,dim3,dim4
 double precision, intent(inout) :: big_array(dim1,dim2,dim3,dim4,N_states)
 double precision :: contrib(N_states)
 integer :: i_state
 do i_state = 1, N_states
  ! Two spin symmetry
  big_array(h1,p1,h2,p2,i_state) +=  contrib(i_state)
  big_array(h2,p2,h1,p1,i_state) +=  contrib(i_state) 
  ! Hermicity : hole-particle symmetry
  big_array(p1,h1,p2,h2,i_state) +=  contrib(i_state)
  big_array(p2,h2,p1,h1,i_state) +=  contrib(i_state) 
 enddo

 
end
