!use map_module

!BEGIN_PROVIDER [ logical, dirac_ao_bielec_ints_erf_mu_of_r_in_map ]
! implicit none
! use f77_zmq
! use map_module
! BEGIN_DOC
! !  Map of Atomic ints
! !     i(r1) j(r2) 1/r12 k(r1) l(r2)
! END_DOC
! 
! integer                        :: i,j,k,l
! double precision               :: erf_mu_of_r_dirac_ao,cpu_1,cpu_2, wall_1, wall_2
! double precision               :: integral, wall_0
! include 'utils/constants.include.F'
! 
! ! For ints file
! integer(key_kind),allocatable  :: buffer_i(:)
! integer,parameter              :: size_buffer = 1024*64
! real(integral_kind),allocatable :: buffer_value(:)
! 
! integer                        :: n_ints, rc
! integer                        :: kk, m, j1, i1, lmax
! character*(64)                 :: fmt
! 
! integral = erf_mu_of_r_dirac_ao(1,1,1,1)
! double precision :: ints_matrix(dirac_ao_num,dirac_ao_num)
! call give_all_dirac_erf_mu_of_r_kl(1,1,ints_matrix)

! 
! double precision               :: map_mb
! PROVIDE read_dirac_ao_ints_mu_of_r io_dirac_ao_ints_mu_of_r
!!if (read_dirac_ao_ints_mu_of_r) then
!!  print*,'Reading the AO ERF mu of r ints'
!!    call map_load_from_disk(trim(ezfio_filename)//'/work/dirac_ao_ints_erf_mu_of_r',dirac_ao_ints_erf_mu_of_r_map)
!!    print*, 'AO ERF mu of r ints provided'
!!    dirac_ao_bielec_ints_erf_mu_of_r_in_map = .True.
!!    return
!!endif
! 
! print*, 'Providing the DIRAC AO ERF mu of r ints'
! call wall_time(wall_0)
! call wall_time(wall_1)
! call cpu_time(cpu_1)

! integer(ZMQ_PTR) :: zmq_to_qp_run_socket, zmq_socket_pull
! call new_parallel_job(zmq_to_qp_run_socket,zmq_socket_pull,'dirac_ao_ints_erf_mu_of_r')

! character(len=:), allocatable :: task
! allocate(character(len=dirac_ao_num*12) :: task)
! write(fmt,*) '(', dirac_ao_num, '(I5,X,I5,''|''))'
! do l=1,dirac_ao_num
!   write(task,fmt) (i,l, i=1,l)
!   integer, external :: add_task_to_taskserver
!   if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task)) == -1) then
!     stop 'Unable to add task to server'
!   endif
! enddo
! deallocate(task)
! 
! integer, external :: zmq_set_running
! if (zmq_set_running(zmq_to_qp_run_socket) == -1) then
!   print *,  irp_here, ': Failed in zmq_set_running'
! endif

! PROVIDE nproc
! !$OMP PARALLEL DEFAULT(shared) private(i) num_threads(nproc+1)
!     i = omp_get_thread_num()
!     if (i==0) then
!       call dirac_ao_bielec_ints_erf_mu_of_r_in_map_collector(zmq_socket_pull)
!     else
!       call dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave_inproc(i)
!     endif
! !$OMP END PARALLEL

! call end_parallel_job(zmq_to_qp_run_socket, zmq_socket_pull, 'dirac_ao_ints_erf_mu_of_r')


! print*, 'Sorting the map'
! call map_sort(dirac_ao_ints_erf_mu_of_r_map)
! call cpu_time(cpu_2)
! call wall_time(wall_2)
! integer(map_size_kind)         :: get_dirac_ao_erf_mu_of_r_map_size, dirac_ao_erf_mu_of_r_map_size
! dirac_ao_erf_mu_of_r_map_size = get_dirac_ao_erf_mu_of_r_map_size()
! 
! print*, 'DIRAC AO ERF mu of r ints provided:'
! print*, ' Size of DIRAC AO ERF mu of r map :         ', map_mb(dirac_ao_ints_erf_mu_of_r_map) ,'MB'
! print*, ' Number of DIRAC AO ERF mu of r ints :', dirac_ao_erf_mu_of_r_map_size
! print*, ' cpu  time :',cpu_2 - cpu_1, 's'
! print*, ' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1+tiny(1.d0)), ' )'
! 
! dirac_ao_bielec_ints_erf_mu_of_r_in_map = .True.

!!if (write_dirac_ao_ints_mu_of_r) then
!!  call ezfio_set_work_empty(.False.)
!!  call map_save_to_disk(trim(ezfio_filename)//'/work/dirac_ao_ints_erf_mu_of_r',dirac_ao_ints_erf_mu_of_r_map)
!!  call ezfio_set_mu_of_r_ints_io_dirac_ao_ints_mu_of_r("Read")
!!endif
! 
!END_PROVIDER
!

!!! AO Map
!!! ======
!
!BEGIN_PROVIDER [ type(map_type), dirac_ao_ints_erf_mu_of_r_map ]
! implicit none
! BEGIN_DOC
! ! AO ints
! END_DOC
! integer(key_kind)              :: key_max
! integer(map_size_kind)         :: sze
! call index_two_e_no_sym(dirac_ao_num,dirac_ao_num,dirac_ao_num,dirac_ao_num,dirac_ao_num,key_max)
! sze = key_max
! call map_init(dirac_ao_ints_erf_mu_of_r_map,sze)
! print*,  'AO map initialized : ', sze
!END_PROVIDER

!BEGIN_PROVIDER [ integer, dirac_ao_ints_erf_mu_of_r_cache_min ]
!&BEGIN_PROVIDER [ integer, dirac_ao_ints_erf_mu_of_r_cache_max ]
!implicit none
!BEGIN_DOC
!! Min and max values of the AOs for which the ints are in the cache
!END_DOC
!dirac_ao_ints_erf_mu_of_r_cache_min = max(1,dirac_ao_num - 63)
!dirac_ao_ints_erf_mu_of_r_cache_max = dirac_ao_num

!END_PROVIDER

!BEGIN_PROVIDER [ double precision, dirac_ao_ints_erf_mu_of_r_cache, (0:64*64*64*64) ]
! use map_module
!implicit none
!BEGIN_DOC
!! Cache of AO ints for fast access
!END_DOC
!PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map
!integer                        :: i,j,k,l,ii
!integer(key_kind)              :: idx1,idx2
!real(integral_kind)            :: integral1,integral2
!!$OMP PARALLEL DO PRIVATE (i,j,k,l,idx1,idx2,ii,integral1,integral2)
!do l=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
!  do k=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
!    do j=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
!      do i=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
!        !DIR$ FORCEINLINE
!        call index_two_e_no_sym(i,j,k,l,dirac_ao_num,idx1)
!        !DIR$ FORCEINLINE
!        call map_get(dirac_ao_ints_erf_mu_of_r_map,idx1,integral1)
!        ii = l-dirac_ao_ints_erf_mu_of_r_cache_min
!        ii = ior( ishft(ii,6), k-dirac_ao_ints_erf_mu_of_r_cache_min)
!        ii = ior( ishft(ii,6), j-dirac_ao_ints_erf_mu_of_r_cache_min)
!        ii = ior( ishft(ii,6), i-dirac_ao_ints_erf_mu_of_r_cache_min)
!        dirac_ao_ints_erf_mu_of_r_cache(ii) = integral1
!      enddo
!    enddo
!  enddo
!enddo
!!$OMP END PARALLEL DO
!END_PROVIDER


!double precision function get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,map) result(result)
! use map_module
! implicit none
! BEGIN_DOC
! ! Gets one AO bi-electronic integral from the AO map
! ! (i,k) = 1, (j,l) = 2
! END_DOC
! integer, intent(in)            :: i,j,k,l
! integer(key_kind)              :: idx1,idx2
! type(map_type), intent(inout)  :: map
! integer                        :: ii
! real(integral_kind)            :: tmp,tmp1,tmp2
! PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map dirac_ao_ints_erf_mu_of_r_cache_min
! !DIR$ FORCEINLINE
! if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < dirac_ao_integrals_threshold ) then
!  tmp = 0.d0
!!elseif ((i .le. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
!!    (i .le. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num) .or.  &
!!    (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
!!    (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
!!    (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
!!    (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
!!    (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
!!    (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num)) then
!! tmp = 0.d0
! else
!  !DIR$ FORCEINLINE
!  call index_two_e_no_sym(i,j,k,l,dirac_ao_num,idx1)
!  !DIR$ FORCEINLINE
!  call map_get(map,idx1,tmp1)
!  !DIR$ FORCEINLINE
!  call index_two_e_no_sym(j,i,l,k,dirac_ao_num,idx2)
!  !DIR$ FORCEINLINE
!  call map_get(map,idx2,tmp2)
!  tmp = 0.5d0 * (tmp1 + tmp2)
! endif
! result = tmp
!end

!subroutine get_dirac_ao_bielec_ints_erf_mu_of_r(j,k,l,sze,out_val)
! use map_module
! BEGIN_DOC
! ! Gets multiple AO bi-electronic integral from the AO map .
! ! All i are retrieved for j,k,l fixed.
! ! (j,l) = 1 ; (k,:) = 2
! END_DOC
! implicit none
! integer, intent(in)            :: j,k,l, sze
! real(integral_kind), intent(out) :: out_val(sze)
! 
! integer                        :: i
! integer(key_kind)              :: hash
! double precision               :: thresh
! PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map dirac_ao_ints_erf_mu_of_r_map
! thresh = dirac_ao_integrals_threshold
! 
! if (dirac_ao_overlap_abs(j,l) < thresh) then
!   out_val = 0.d0
!   return
! endif
! 
! double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
! do i=1,sze
!   out_val(i) = get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map) 
! enddo
!end

!subroutine get_dirac_ao_bielec_ints_erf_mu_of_r_non_zero(j,k,l,sze,out_val,out_val_index,non_zero_int)
! use map_module
! implicit none
! BEGIN_DOC
! ! Gets multiple DIRAC AO bi-electronic integral from the DIRAC AO map .
! ! All non-zero i are retrieved for j,k,l fixed.
! END_DOC
! integer, intent(in)            :: j,k,l, sze
! real(integral_kind), intent(out) :: out_val(sze)
! integer, intent(out)           :: out_val_index(sze),non_zero_int
! 
! integer                        :: i
! integer(key_kind)              :: hash1,hash2
! double precision               :: thresh,tmp1,tmp2
! PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map
! thresh = dirac_ao_integrals_threshold
! 
! non_zero_int = 0
! if (dirac_ao_overlap_abs(j,l) < thresh) then
!   out_val = 0.d0
!   return
! endif
!
! non_zero_int = 0
! do i=1,sze
!   integer, external :: dirac_ao_l4
!   double precision, external :: dirac_ao_bielec_integral_erf_mu_of_r
!   !DIR$ FORCEINLINE
!   if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < thresh) then
!     cycle
!   endif
!   call index_two_e_no_sym(i,j,k,l,dirac_ao_num,hash1)
!   call map_get(dirac_ao_ints_erf_mu_of_r_map, hash1,tmp1)
!   call index_two_e_no_sym(j,i,l,k,dirac_ao_num,hash2)
!   call map_get(dirac_ao_ints_erf_mu_of_r_map, hash2,tmp2)
!   if (dabs(tmp1) < thresh ) cycle
!   non_zero_int = non_zero_int+1
!   out_val_index(non_zero_int) = i
!   out_val(non_zero_int) = 0.5d0*(tmp1 + tmp2)
! enddo
!end


!function get_dirac_ao_erf_mu_of_r_map_size()
! implicit none
! integer (map_size_kind) :: get_dirac_ao_erf_mu_of_r_map_size
! BEGIN_DOC
! ! Returns the number of elements in the AO map
! END_DOC
! get_dirac_ao_erf_mu_of_r_map_size = dirac_ao_ints_erf_mu_of_r_map % n_elements
!end

!subroutine clear_dirac_ao_erf_mu_of_r_map
! implicit none
! BEGIN_DOC
! ! Frees the memory of the AO map
! END_DOC
! call map_deinit(dirac_ao_ints_erf_mu_of_r_map)
! FREE dirac_ao_ints_erf_mu_of_r_map
!end

