 subroutine  dirac_mu_of_r (r,mu_of_r)
 implicit none
 BEGIN_DOC
 ! input:
 ! * r(1) ==> r(1) = x, r(2) = y, r(3) = z
 ! output:
 ! * mu_of_r = value of the local range separation parameter
 !!!
 ! Warning, mu_of_r is defined for istate = 1
 !!!
 END_DOC
 double precision, intent(in)  :: r(3)
 double precision, intent(out) :: mu_of_r
 integer :: istate
 double precision :: dm(N_states),grad_dm(3,N_states),grad_dm_2(N_states),grad_dm_abs(N_states)
 call dirac_dm_dft_at_r(r,dm)
 call dirac_grad_dm_dft_at_r(r,grad_dm,grad_dm_2,grad_dm_abs)
!mu_of_r = 0.135d0 * grad_dm_abs(1) / dm(1)
 mu_of_r = 0.d0
 end

 BEGIN_PROVIDER [double precision, mu_of_r_for_ints_vector, (n_points_final_grid)]
 implicit none
 BEGIN_DOC
 ! value of mu(r) in each point in space
 END_DOC
 integer :: i, istate
 double precision :: mu_of_r
 double precision :: r(3)
 do i = 1, n_points_final_grid
  r(1) = final_grid_points(1,i)
  r(2) = final_grid_points(2,i)
  r(3) = final_grid_points(3,i)
  call dirac_mu_of_r (r,mu_of_r) 
  mu_of_r_for_ints_vector(i) = mu_of_r
 enddo 
 END_PROVIDER

 subroutine give_all_dirac_erf_mu_of_r_kl(k,l,ints)
 implicit none
 include 'utils/constants.include.F'
 integer, intent(in) :: k,l
 double precision, intent(out) :: ints(dirac_ao_num,dirac_ao_num)
 integer :: i_point,i,j
 double precision :: dirac_aos_array(dirac_ao_num)
 double precision :: ints_kl_of_r(n_points_final_grid),r(3),mu_of_r,NAI_pol_mult_erf_dirac_ao,tmp,NAI_pol_mult_erfc_dirac_ao
 double precision,allocatable :: v_vector(:)
 allocate(v_vector(n_points_final_grid))
 ints=0.d0
 if (dirac_dhf == "local_dhf") then
 !!! local_dhf uses mu_erf
  r(1) = dirac_r_x 
  r(2) = dirac_r_y
  r(3) = dirac_r_z
  call dirac_mu_of_r (r,mu_of_r)
  call give_all_dirac_aos_at_r(r,dirac_aos_array)
  do i = 1, dirac_ao_num
   do j = 1, dirac_ao_num
    ints(j,i) = NAI_pol_mult_erfc_dirac_ao(k,l,mu_erf,r) * dirac_aos_array(i) * dirac_aos_array(j) 
   enddo
  enddo
 else
  do i_point = 1, n_points_final_grid
   r(1) = final_grid_points(1,i_point)
   r(2) = final_grid_points(2,i_point)
   r(3) = final_grid_points(3,i_point)
   v_vector(i_point) = NAI_pol_mult_erf_dirac_ao(k,l,mu_of_r_for_ints_vector(i_point),r)
  enddo
  ! use DGEMM!!!
  do i_point = 1, n_points_final_grid
   do i = 1, dirac_ao_num
    do j = 1, dirac_ao_num
     ints(j,i) += v_vector(i_point) * dirac_aos_in_r_array(i,i_point) * dirac_aos_in_r_array(j,i_point) * final_weight_at_r_vector(i_point)
    enddo
   enddo
  enddo
 endif
 end
 
 subroutine compute_dirac_ao_ints_erf_mu_of_r_jl(j,l,n_ints,buffer_i,buffer_value)
 implicit none
 use map_module
 BEGIN_DOC
 !  Parallel client for AO ints
 END_DOC
 integer, intent(in)            :: j,l
 integer,intent(out)            :: n_ints
 integer(key_kind),intent(out)  :: buffer_i(dirac_ao_num*dirac_ao_num)
 real(integral_kind),intent(out) :: buffer_value(dirac_ao_num*dirac_ao_num)
 integer                        :: i,k
 double precision               :: cpu_1,cpu_2,wall_1, wall_2
 double precision               :: integral, wall_0
 double precision               :: thr
 integer                        :: kk, m, j1, i1
 thr = dirac_ao_integrals_threshold
 n_ints = 0
 double precision :: ints_matrix(dirac_ao_num,dirac_ao_num)
 call give_all_dirac_erf_mu_of_r_kl(j,l,ints_matrix)
 j1 = j+ishft(l*l-l,-1)
 do k = 1, dirac_ao_num           ! r1
  i1 = ishft(k*k-k,-1)
  do i = 1, dirac_ao_num
   i1 += 1
   integral = ints_matrix(k,i)  ! i,k : r1    j,l : r2
   if (abs(integral) < thr) then
    cycle
   endif
   n_ints += 1
  !!DIR$ FORCEINLINE
   call index_two_e_no_sym(i,j,k,l,dirac_ao_num,buffer_i(n_ints))
   buffer_value(n_ints) = integral
  enddo
 enddo
 end

 subroutine insert_into_dirac_ao_ints_erf_mu_of_r_map(n_ints,buffer_i, buffer_values)
  use map_module
  implicit none
  BEGIN_DOC
  ! Create new entry into AO map
  END_DOC
  
  integer, intent(in)                :: n_ints
  integer(key_kind), intent(inout)   :: buffer_i(n_ints)
  real(integral_kind), intent(inout) :: buffer_values(n_ints)
  
  call map_append(dirac_ao_ints_erf_mu_of_r_map, buffer_i, buffer_values, n_ints)
 end

 subroutine dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave_tcp(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
  ! Computes a buffer of ints. i is the ID of the current thread.
  END_DOC
  call dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave(0,i)
 end


 subroutine dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
  ! Computes a buffer of ints. i is the ID of the current thread.
  END_DOC
  call dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave(1,i)
 end


 subroutine dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave(thread,iproc)
  use map_module
  use f77_zmq
  implicit none
  BEGIN_DOC
 ! Computes a buffer of ints
  END_DOC
  integer, intent(in)            :: thread, iproc
  integer                        :: j,l,n_ints
  integer                        :: rc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)
  integer                        :: worker_id, task_id
  character*(512)                :: task
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket
  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push
  character*(64)                 :: state
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  integer, external :: connect_to_taskserver
  if (connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread) == -1) then
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    return
  endif
  zmq_socket_push      = new_zmq_push_socket(thread)
  allocate ( buffer_i(dirac_ao_num*dirac_ao_num), buffer_value(dirac_ao_num*dirac_ao_num) )
  do 
    integer, external :: get_task_from_taskserver
    if (get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task) == -1) then
      exit
    endif
    if (task_id == 0) exit
    read(task,*) j, l
    integer, external :: task_done_to_taskserver
    call compute_dirac_ao_ints_erf_mu_of_r_jl(j,  l, n_ints,buffer_i,buffer_value) 
    if (task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id) == -1) then
        stop 'Unable to send task_done'
    endif
    call push_integrals(zmq_socket_push, n_ints, buffer_i, buffer_value, task_id)
  enddo
  integer, external :: disconnect_from_taskserver
  if (disconnect_from_taskserver(zmq_to_qp_run_socket,worker_id) == -1) then
    continue
  endif
  deallocate( buffer_i, buffer_value )
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
 end


 subroutine dirac_ao_bielec_ints_erf_mu_of_r_in_map_collector(zmq_socket_pull)
  use map_module
  use f77_zmq
  implicit none
  BEGIN_DOC
  ! Collects results from the AO integral calculation
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  integer                        :: j,l,n_ints
  integer                        :: rc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer*8                      :: control, accu, sze
  integer                        :: task_id, more
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  sze = dirac_ao_num*dirac_ao_num
  allocate ( buffer_i(sze), buffer_value(sze) )
  accu = 0_8
  more = 1
  do while (more == 1)
    rc = f77_zmq_recv( zmq_socket_pull, n_ints, 4, 0)
    if (rc == -1) then
      n_ints = 0
      return
    endif
    if (rc /= 4) then
      print *, irp_here,  ': f77_zmq_recv( zmq_socket_pull, n_ints, 4, 0)'
      stop 'error'
    endif
    if (n_ints >= 0) then
      if (n_ints > sze) then
        deallocate (buffer_value, buffer_i)
        sze = n_ints
        allocate (buffer_value(sze), buffer_i(sze))
      endif
      rc = f77_zmq_recv( zmq_socket_pull, buffer_i, key_kind*n_ints, 0)
      if (rc /= key_kind*n_ints) then
        print *,  rc, key_kind, n_ints
        print *, irp_here,  ': f77_zmq_recv( zmq_socket_pull, buffer_i, key_kind*n_ints, 0)'
        stop 'error'
      endif
      rc = f77_zmq_recv( zmq_socket_pull, buffer_value, integral_kind*n_ints, 0)
      if (rc /= integral_kind*n_ints) then
        print *, irp_here,  ': f77_zmq_recv( zmq_socket_pull, buffer_value, integral_kind*n_ints, 0)'
        stop 'error'
      endif
      rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)
  IRP_IF ZMQ_PUSH
  IRP_ELSE
      rc = f77_zmq_send( zmq_socket_pull, 0, 4, 0)
      if (rc /= 4) then
        print *,  irp_here, ' : f77_zmq_send (zmq_socket_pull,...'
        stop 'error'
      endif
  IRP_ENDIF
      call insert_into_dirac_ao_ints_erf_mu_of_r_map(n_ints,buffer_i,buffer_value)
      accu += n_ints
      if (task_id /= 0) then
        integer, external :: zmq_delete_task
        if (zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more) == -1) then
          stop 'Unable to delete task'
        endif
      endif
    endif
  enddo
  deallocate( buffer_i, buffer_value )
  integer (map_size_kind) :: get_dirac_ao_erf_mu_of_r_map_size 
  control = get_dirac_ao_erf_mu_of_r_map_size(dirac_ao_ints_erf_mu_of_r_map)
  if (control /= accu) then
      print *, ''
      print *, irp_here
      print *, 'Control : ', control
      print *, 'Accu    : ', accu
      print *, 'Some ints were lost during the parallel computation.'
      print *, 'Try to reduce the number of threads.'
      stop
  endif
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
 end


 use map_module

 BEGIN_PROVIDER [ logical, dirac_ao_bielec_ints_erf_mu_of_r_in_map ]
  implicit none
  use f77_zmq
  use map_module
  BEGIN_DOC
  !  Map of Atomic ints
  !     i(r1) j(r2) 1/r12 k(r1) l(r2)
  END_DOC
  
  integer                        :: i,j,k,l
  double precision               :: erf_mu_of_r_dirac_ao,cpu_1,cpu_2, wall_1, wall_2
  double precision               :: integral, wall_0
  include 'utils/constants.include.F'
  
  ! For ints file
  integer(key_kind),allocatable  :: buffer_i(:)
  integer,parameter              :: size_buffer = 1024*64
  real(integral_kind),allocatable :: buffer_value(:)
  
  integer                        :: n_ints, rc
  integer                        :: kk, m, j1, i1, lmax
  character*(64)                 :: fmt
  
  integral = erf_mu_of_r_dirac_ao(1,1,1,1)
  double precision :: ints_matrix(dirac_ao_num,dirac_ao_num)
  call give_all_dirac_erf_mu_of_r_kl(1,1,ints_matrix)

  
  double precision               :: map_mb
  PROVIDE read_dirac_ao_ints_mu_of_r io_dirac_ao_ints_mu_of_r
 !if (read_dirac_ao_ints_mu_of_r) then
 !  print*,'Reading the AO ERF mu of r ints'
 !    call map_load_from_disk(trim(ezfio_filename)//'/work/dirac_ao_ints_erf_mu_of_r',dirac_ao_ints_erf_mu_of_r_map)
 !    print*, 'AO ERF mu of r ints provided'
 !    dirac_ao_bielec_ints_erf_mu_of_r_in_map = .True.
 !    return
 !endif
  
  print*, 'Providing the DIRAC AO ERF mu of r ints'
  call wall_time(wall_0)
  call wall_time(wall_1)
  call cpu_time(cpu_1)

  integer(ZMQ_PTR) :: zmq_to_qp_run_socket, zmq_socket_pull
  call new_parallel_job(zmq_to_qp_run_socket,zmq_socket_pull,'dirac_ao_ints_erf_mu_of_r')

  character(len=:), allocatable :: task
  allocate(character(len=dirac_ao_num*12) :: task)
  write(fmt,*) '(', dirac_ao_num, '(I5,X,I5,''|''))'
  do l=1,dirac_ao_num
    write(task,fmt) (i,l, i=1,l)
    integer, external :: add_task_to_taskserver
    if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task)) == -1) then
      stop 'Unable to add task to server'
    endif
  enddo
  deallocate(task)
  
  integer, external :: zmq_set_running
  if (zmq_set_running(zmq_to_qp_run_socket) == -1) then
    print *,  irp_here, ': Failed in zmq_set_running'
  endif
  PROVIDE nproc
  !$OMP PARALLEL DEFAULT(shared) private(i) num_threads(nproc+1)
      i = omp_get_thread_num()
      if (i==0) then
        call dirac_ao_bielec_ints_erf_mu_of_r_in_map_collector(zmq_socket_pull)
      else
        call dirac_ao_bielec_ints_erf_mu_of_r_in_map_slave_inproc(i)
      endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, zmq_socket_pull, 'dirac_ao_ints_erf_mu_of_r')
  print*, 'Sorting the map'
  call map_sort(dirac_ao_ints_erf_mu_of_r_map)
  call cpu_time(cpu_2)
  call wall_time(wall_2)
  integer(map_size_kind)         :: get_dirac_ao_erf_mu_of_r_map_size, dirac_ao_erf_mu_of_r_map_size
  dirac_ao_erf_mu_of_r_map_size = get_dirac_ao_erf_mu_of_r_map_size()
  print*, 'DIRAC AO ERF mu of r ints provided:'
  print*, ' Size of DIRAC AO ERF mu of r map :         ', map_mb(dirac_ao_ints_erf_mu_of_r_map) ,'MB'
  print*, ' Number of DIRAC AO ERF mu of r ints :', dirac_ao_erf_mu_of_r_map_size
  print*, ' cpu  time :',cpu_2 - cpu_1, 's'
  print*, ' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1+tiny(1.d0)), ' )'
  dirac_ao_bielec_ints_erf_mu_of_r_in_map = .True.
 !if (write_dirac_ao_ints_mu_of_r) then
 !  call ezfio_set_work_empty(.False.)
 !  call map_save_to_disk(trim(ezfio_filename)//'/work/dirac_ao_ints_erf_mu_of_r',dirac_ao_ints_erf_mu_of_r_map)
 !  call ezfio_set_mu_of_r_ints_io_dirac_ao_ints_mu_of_r("Read")
 !endif
  
 END_PROVIDER
 

 !! AO Map
 !! ======
 
 BEGIN_PROVIDER [ type(map_type), dirac_ao_ints_erf_mu_of_r_map ]
  implicit none
  BEGIN_DOC
  ! AO ints
  END_DOC
  integer(key_kind)              :: key_max
  integer(map_size_kind)         :: sze
  call index_two_e_no_sym(dirac_ao_num,dirac_ao_num,dirac_ao_num,dirac_ao_num,dirac_ao_num,key_max)
  sze = key_max
  call map_init(dirac_ao_ints_erf_mu_of_r_map,sze)
  print*,  'DIRAC AO map initialized : ', sze
 END_PROVIDER

 BEGIN_PROVIDER [ integer, dirac_ao_ints_erf_mu_of_r_cache_min ]
 &BEGIN_PROVIDER [ integer, dirac_ao_ints_erf_mu_of_r_cache_max ]
 implicit none
 BEGIN_DOC
 ! Min and max values of the AOs for which the ints are in the cache
 END_DOC
 dirac_ao_ints_erf_mu_of_r_cache_min = max(1,dirac_ao_num - 63)
 dirac_ao_ints_erf_mu_of_r_cache_max = dirac_ao_num

 END_PROVIDER

 BEGIN_PROVIDER [ double precision, dirac_ao_ints_erf_mu_of_r_cache, (0:64*64*64*64) ]
  use map_module
 implicit none
 BEGIN_DOC
 ! Cache of AO ints for fast access
 END_DOC
 PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map
 integer                        :: i,j,k,l,ii
 integer(key_kind)              :: idx1,idx2
 real(integral_kind)            :: integral1,integral2
 !$OMP PARALLEL DO PRIVATE (i,j,k,l,idx1,idx2,ii,integral1,integral2)
 do l=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
   do k=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
     do j=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
       do i=dirac_ao_ints_erf_mu_of_r_cache_min,dirac_ao_ints_erf_mu_of_r_cache_max
         !DIR$ FORCEINLINE
         call index_two_e_no_sym(i,j,k,l,dirac_ao_num,idx1)
         !DIR$ FORCEINLINE
         call map_get(dirac_ao_ints_erf_mu_of_r_map,idx1,integral1)
         ii = l-dirac_ao_ints_erf_mu_of_r_cache_min
         ii = ior( ishft(ii,6), k-dirac_ao_ints_erf_mu_of_r_cache_min)
         ii = ior( ishft(ii,6), j-dirac_ao_ints_erf_mu_of_r_cache_min)
         ii = ior( ishft(ii,6), i-dirac_ao_ints_erf_mu_of_r_cache_min)
         dirac_ao_ints_erf_mu_of_r_cache(ii) = integral1
       enddo
     enddo
   enddo
 enddo
 !$OMP END PARALLEL DO
 END_PROVIDER


 double precision function get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,map) result(result)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets one AO bi-electronic integral from the AO map
  ! (i,k) = 1, (j,l) = 2
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx1,idx2
  type(map_type), intent(inout)  :: map
  integer                        :: ii
  real(integral_kind)            :: tmp,tmp1,tmp2
  PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map dirac_ao_ints_erf_mu_of_r_cache_min
  !DIR$ FORCEINLINE
  if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < dirac_ao_integrals_threshold ) then
   tmp = 0.d0
  else
   !DIR$ FORCEINLINE
   call index_two_e_no_sym(i,j,k,l,dirac_ao_num,idx1)
   !DIR$ FORCEINLINE
   call map_get(map,idx1,tmp1)
   !DIR$ FORCEINLINE
   call index_two_e_no_sym(j,i,l,k,dirac_ao_num,idx2)
   !DIR$ FORCEINLINE
   call map_get(map,idx2,tmp2)
   tmp = 0.5d0 * (tmp1 + tmp2)
  endif
  result = tmp
 end
 
  double precision function get_dirac_ao_bielec_integral_erf_mu_of_r_1(i,j,k,l,map) result(result)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets one AO bi-electronic integral from the AO map
  ! (i,k) = 1, (j,l) = 2
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx1,idx2
  type(map_type), intent(inout)  :: map
  integer                        :: ii
  real(integral_kind)            :: tmp,tmp1,tmp2
  PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map dirac_ao_ints_erf_mu_of_r_cache_min
  !DIR$ FORCEINLINE
  if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < dirac_ao_integrals_threshold ) then
   tmp = 0.d0
  else
   !DIR$ FORCEINLINE
   call index_two_e_no_sym(i,j,k,l,dirac_ao_num,idx1)
   !DIR$ FORCEINLINE
   call map_get(map,idx1,tmp1)
   tmp = tmp1
  endif
  result = tmp
 end

 double precision function get_dirac_ao_bielec_integral_erf_mu_of_r_2(i,j,k,l,map) result(result)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets one AO bi-electronic integral from the AO map
  ! (i,k) = 1, (j,l) = 2
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx1,idx2
  type(map_type), intent(inout)  :: map
  integer                        :: ii
  real(integral_kind)            :: tmp,tmp1,tmp2
  PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map dirac_ao_ints_erf_mu_of_r_cache_min
  !DIR$ FORCEINLINE
  if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < dirac_ao_integrals_threshold ) then
   tmp = 0.d0
  else
   !DIR$ FORCEINLINE
   call index_two_e_no_sym(j,i,l,k,dirac_ao_num,idx2)
   !DIR$ FORCEINLINE
   call map_get(map,idx2,tmp2)
   tmp = tmp2
  endif
  result = tmp
 end


 subroutine get_dirac_ao_bielec_ints_erf_mu_of_r(j,k,l,sze,out_val)
  use map_module
  BEGIN_DOC
  ! Gets multiple AO bi-electronic integral from the AO map .
  ! All i are retrieved for j,k,l fixed.
  ! (j,l) = 1 ; (k,:) = 2
  END_DOC
  implicit none
  integer, intent(in)            :: j,k,l, sze
  real(integral_kind), intent(out) :: out_val(sze)
  
  integer                        :: i
  integer(key_kind)              :: hash
  double precision               :: thresh
  PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map dirac_ao_ints_erf_mu_of_r_map
  thresh = dirac_ao_integrals_threshold
  
  if (dirac_ao_overlap_abs(j,l) < thresh) then
    out_val = 0.d0
    return
  endif
  
  double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
  do i=1,sze
    out_val(i) = get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map) 
  enddo
 end

 subroutine get_dirac_ao_bielec_ints_erf_mu_of_r_non_zero(j,k,l,sze,out_val,out_val_index,non_zero_int)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets multiple DIRAC AO bi-electronic integral from the DIRAC AO map .
  ! All non-zero i are retrieved for j,k,l fixed.
  END_DOC
  integer, intent(in)            :: j,k,l, sze
  real(integral_kind), intent(out) :: out_val(sze)
  integer, intent(out)           :: out_val_index(sze),non_zero_int
  
  integer                        :: i
  integer(key_kind)              :: hash1,hash2
  double precision               :: thresh,tmp1,tmp2
  PROVIDE dirac_ao_bielec_ints_erf_mu_of_r_in_map
  thresh = dirac_ao_integrals_threshold
  
  non_zero_int = 0
  if (dirac_ao_overlap_abs(j,l) < thresh) then
    out_val = 0.d0
    return
  endif
 
  non_zero_int = 0
  do i=1,sze
    integer, external :: dirac_ao_l4
    double precision, external :: dirac_ao_bielec_integral_erf_mu_of_r
    !DIR$ FORCEINLINE
    if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < thresh) then
      cycle
    endif
    call index_two_e_no_sym(i,j,k,l,dirac_ao_num,hash1)
    call map_get(dirac_ao_ints_erf_mu_of_r_map, hash1,tmp1)
    call index_two_e_no_sym(j,i,l,k,dirac_ao_num,hash2)
    call map_get(dirac_ao_ints_erf_mu_of_r_map, hash2,tmp2)
    if (dabs(tmp1) < thresh ) cycle
    non_zero_int = non_zero_int+1
    out_val_index(non_zero_int) = i
    out_val(non_zero_int) = 0.5d0*(tmp1 + tmp2)
  enddo
 end


 function get_dirac_ao_erf_mu_of_r_map_size()
  implicit none
  integer (map_size_kind) :: get_dirac_ao_erf_mu_of_r_map_size
  BEGIN_DOC
  ! Returns the number of elements in the AO map
  END_DOC
  get_dirac_ao_erf_mu_of_r_map_size = dirac_ao_ints_erf_mu_of_r_map % n_elements
 end

 subroutine clear_dirac_ao_erf_mu_of_r_map
  implicit none
  BEGIN_DOC
  ! Frees the memory of the AO map
  END_DOC
  call map_deinit(dirac_ao_ints_erf_mu_of_r_map)
  FREE dirac_ao_ints_erf_mu_of_r_map
 end


!use map_module


!BEGIN_TEMPLATE

!subroutine dump_$dirac_ao_ints(filename)
! use map_module
! implicit none
! BEGIN_DOC
! ! Save to disk the $dirac_ao ints
! END_DOC
! character*(*), intent(in)      :: filename
! integer(cache_key_kind), pointer :: key(:)
! real(integral_kind), pointer   :: val(:)
! integer*8                      :: i,j, n
! call ezfio_set_work_empty(.False.)
! open(unit=66,file=filename,FORM='unformatted')
! write(66) integral_kind, key_kind
! write(66) $dirac_ao_ints_map%sorted, $dirac_ao_ints_map%map_size,    &
!     $dirac_ao_ints_map%n_elements
! do i=0_8,$dirac_ao_ints_map%map_size
!   write(66) $dirac_ao_ints_map%map(i)%sorted, $dirac_ao_ints_map%map(i)%map_size,&
!       $dirac_ao_ints_map%map(i)%n_elements
! enddo
! do i=0_8,$dirac_ao_ints_map%map_size
!   key => $dirac_ao_ints_map%map(i)%key
!   val => $dirac_ao_ints_map%map(i)%value
!   n = $dirac_ao_ints_map%map(i)%n_elements
!   write(66) (key(j), j=1,n), (val(j), j=1,n)
! enddo
! close(66)
!end

!IRP_IF COARRAY
!subroutine communicate_$dirac_ao_ints()
! use map_module
! implicit none
! BEGIN_DOC
! ! Communicate the $ao ints with co-array
! END_DOC
! integer(cache_key_kind), pointer :: key(:)
! real(integral_kind), pointer   :: val(:)
! integer*8                      :: i,j, k, nmax
! integer*8, save                :: n[*]
! integer                        :: copy_n

! real(integral_kind), allocatable            :: buffer_val(:)[:]
! integer(cache_key_kind), allocatable        :: buffer_key(:)[:]
! real(integral_kind), allocatable            :: copy_val(:)
! integer(key_kind), allocatable              :: copy_key(:)

! n = 0_8
! do i=0_8,$dirac_ao_ints_map%map_size
!   n = max(n,$dirac_ao_ints_map%map(i)%n_elements)
! enddo
! sync all
! nmax = 0_8
! do j=1,num_images()
!   nmax = max(nmax,n[j])
! enddo
! allocate( buffer_key(nmax)[*], buffer_val(nmax)[*])
! allocate( copy_key(nmax), copy_val(nmax))
! do i=0_8,$dirac_ao_ints_map%map_size
!   key => $dirac_ao_ints_map%map(i)%key
!   val => $dirac_ao_ints_map%map(i)%value
!   n = $dirac_ao_ints_map%map(i)%n_elements
!   do j=1,n
!     buffer_key(j) = key(j)
!     buffer_val(j) = val(j)
!   enddo
!   sync all
!   do j=1,num_images()
!     if (j /= this_image()) then
!       copy_n = n[j]
!       do k=1,copy_n
!         copy_val(k) = buffer_val(k)[j]
!         copy_key(k) = buffer_key(k)[j]
!         copy_key(k) = copy_key(k)+ishft(i,-map_shift)
!       enddo
!       call map_append($dirac_ao_ints_map, copy_key, copy_val, copy_n )
!     endif
!   enddo
!   sync all
! enddo
! deallocate( buffer_key, buffer_val, copy_val, copy_key)
!end
!IRP_ENDIF 


!integer function load_$dirac_ao_ints(filename)
! implicit none
! BEGIN_DOC
! ! Read from disk the $ao ints
! END_DOC
! character*(*), intent(in)      :: filename
! integer*8                      :: i
! integer(cache_key_kind), pointer :: key(:)
! real(integral_kind), pointer   :: val(:)
! integer                        :: iknd, kknd
! integer*8                      :: n, j
! load_$dirac_ao_ints = 1
! open(unit=66,file=filename,FORM='unformatted',STATUS='UNKNOWN')
! read(66,err=98,end=98) iknd, kknd
! if (iknd /= integral_kind) then
!   print *,  'Wrong ints kind in file :', iknd
!   stop 1
! endif
! if (kknd /= key_kind) then
!   print *,  'Wrong key kind in file :', kknd
!   stop 1
! endif
! read(66,err=98,end=98) $dirac_ao_ints_map%sorted, $dirac_ao_ints_map%map_size,&
!     $dirac_ao_ints_map%n_elements
! do i=0_8, $dirac_ao_ints_map%map_size
!   read(66,err=99,end=99) $dirac_ao_ints_map%map(i)%sorted,          &
!       $dirac_ao_ints_map%map(i)%map_size, $dirac_ao_ints_map%map(i)%n_elements
!   call cache_map_reallocate($dirac_ao_ints_map%map(i),$dirac_ao_ints_map%map(i)%map_size)
! enddo
! do i=0_8, $dirac_ao_ints_map%map_size
!   key => $dirac_ao_ints_map%map(i)%key
!   val => $dirac_ao_ints_map%map(i)%value
!   n = $dirac_ao_ints_map%map(i)%n_elements
!   read(66,err=99,end=99) (key(j), j=1,n), (val(j), j=1,n)
! enddo
! call map_sort($dirac_ao_ints_map)
! load_$dirac_ao_ints = 0
! return
! 99 continue
! call map_deinit($dirac_ao_ints_map)
! 98 continue
! stop 'Problem reading $dirac_ao_ints_map file in work/'
!end

!SUBST [ dirac_ao_ints_map, dirac_ao_ints, dirac_ao_num ]
!dirac_ao_ints_erf_mu_of_r_map ; dirac_ao_ints_erf_mu_of_r ; dirac_ao_num ;;
!END_TEMPLATE


