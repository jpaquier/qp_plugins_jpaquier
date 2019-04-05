use map_module

!subroutine dump_$dirac_ao_ints_erf_mu_of_r(filename)
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
! write(66) $dirac_ao_ints_erf_mu_of_r_map%sorted, $dirac_ao_ints_erf_mu_of_r_map%map_size,    &
!     $dirac_ao_ints_erf_mu_of_r_map%n_elements
! do i=0_8,$dirac_ao_ints_erf_mu_of_r_map%map_size
!   write(66) $dirac_ao_ints_erf_mu_of_r_map%map(i)%sorted, $dirac_ao_ints_erf_mu_of_r_map%map(i)%map_size,&
!       $dirac_ao_ints_erf_mu_of_r_map%map(i)%n_elements
! enddo
! do i=0_8,$dirac_ao_ints_erf_mu_of_r_map%map_size
!   key => $dirac_ao_ints_erf_mu_of_r_map%map(i)%key
!   val => $dirac_ao_ints_erf_mu_of_r_map%map(i)%value
!   n = $dirac_ao_ints_erf_mu_of_r_map%map(i)%n_elements
!   write(66) (key(j), j=1,n), (val(j), j=1,n)
! enddo
! close(66)
! 
!end

!IRP_IF COARRAY
!subroutine communicate_$dirac_ao_ints_erf_mu_of_r()
! use map_module
! implicit none
! BEGIN_DOC
! ! Communicate the $dirac_ao ints with co-array
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
! do i=0_8,$dirac_ao_ints_erf_mu_of_r_map%map_size
!   n = max(n,$dirac_ao_ints_erf_mu_of_r_map%map(i)%n_elements)
! enddo
! sync all
! nmax = 0_8
! do j=1,num_images()
!   nmax = max(nmax,n[j])
! enddo
! allocate( buffer_key(nmax)[*], buffer_val(nmax)[*])
! allocate( copy_key(nmax), copy_val(nmax))
! do i=0_8,$dirac_ao_ints_erf_mu_of_r_map%map_size
!   key => $dirac_ao_ints_erf_mu_of_r_map%map(i)%key
!   val => $dirac_ao_ints_erf_mu_of_r_map%map(i)%value
!   n = $dirac_ao_ints_erf_mu_of_r_map%map(i)%n_elements
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
!       call map_append($dirac_ao_ints_erf_mu_of_r_map, copy_key, copy_val, copy_n )
!     endif
!   enddo
!   sync all
! enddo
! deallocate( buffer_key, buffer_val, copy_val, copy_key)
! 
!end
!IRP_ENDIF 


!integer function load_$dirac_ao_ints_erf_mu_of_r(filename)
! implicit none
! BEGIN_DOC
! ! Read from disk the $dirac_ao ints
! END_DOC
! character*(*), intent(in)      :: filename
! integer*8                      :: i
! integer(cache_key_kind), pointer :: key(:)
! real(integral_kind), pointer   :: val(:)
! integer                        :: iknd, kknd
! integer*8                      :: n, j
! load_$dirac_ao_ints_erf_mu_of_r = 1
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
! read(66,err=98,end=98) $dirac_ao_ints_erf_mu_of_r_map%sorted, $dirac_ao_ints_erf_mu_of_r_map%map_size,&
!     $dirac_ao_ints_erf_mu_of_r_map%n_elements
! do i=0_8, $dirac_ao_ints_erf_mu_of_r_map%map_size
!   read(66,err=99,end=99) $dirac_ao_ints_erf_mu_of_r_map%map(i)%sorted,          &
!       $dirac_ao_ints_erf_mu_of_r_map%map(i)%map_size, $dirac_ao_ints_erf_mu_of_r_map%map(i)%n_elements
!   call cache_map_reallocate($dirac_ao_ints_erf_mu_of_r_map%map(i),$dirac_ao_ints_erf_mu_of_r_map%map(i)%map_size)
! enddo
! do i=0_8, $dirac_ao_ints_erf_mu_of_r_map%map_size
!   key => $dirac_ao_ints_erf_mu_of_r_map%map(i)%key
!   val => $dirac_ao_ints_erf_mu_of_r_map%map(i)%value
!   n = $dirac_ao_ints_erf_mu_of_r_map%map(i)%n_elements
!   read(66,err=99,end=99) (key(j), j=1,n), (val(j), j=1,n)
! enddo
! call map_sort($dirac_ao_ints_erf_mu_of_r_map)
! load_$dirac_ao_ints_erf_mu_of_r = 0
! return
! 99 continue
! call map_deinit($dirac_ao_ints_erf_mu_of_r_map)
! 98 continue
! stop 'Problem reading $dirac_ao_ints_erf_mu_of_r_map file in work/'
!end

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



