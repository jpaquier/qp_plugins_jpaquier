 BEGIN_PROVIDER [ character*(128), even_nucl_type, (nucl_num)]
  implicit none
  BEGIN_DOC
  !type of the nucleus present, to create the even-tempered basis set
  END_DOC
 
 END_PROVIDER

 BEGIN_PROVIDER [ double precision, even_large_expo_seed,(0:7,118) ]
 &BEGIN_PROVIDER [ double precision, even_large_expo_coef,(0:7,118) ]
 &BEGIN_PROVIDER [ Integer, even_large_expo_number ]
 implicit none
 BEGIN_DOC
 !even_large_expo :: exponents seed for each shell
 END_DOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 2-electrons even-tempered basis sets
!!! Ytterbium
!! S type function
! even_large_expo_seed(0,70) = 1.39601904d+07
! even_large_expo_coef(0,70) = 0.30d0
!! P type function
! even_large_expo_seed(1,70) = 3.39128349d+06
! even_large_expo_coef(1,70) = 0.25d0
!! D type function
! even_large_expo_seed(2,70) = 4.75587491d+03
! even_large_expo_coef(2,70) = 0.25d0
!!! Uranium
!! S type function
! even_large_expo_seed(0,92) = 5.58567332d+07
! even_large_expo_coef(0,92) = 0.295d0
!! P type function
! even_large_expo_seed(1,92) = 2.64721605d+07
! even_large_expo_coef(1,92) = 0.25d0
!! D type function
! even_large_expo_seed(2,92) = 3.34172062d+04
! even_large_expo_coef(2,92) = 0.25d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 4-electrons even-tempered basis sets
!!! Berylium
!! S type function
! even_large_expo_seed(0,4) = 6.40943779d+03
! even_large_expo_coef(0,4) = 0.25d0
!! P type function
! even_large_expo_seed(1,4) = 1.31800101d+01
! even_large_expo_coef(1,4) = 0.25d0
!! D type function
! even_large_expo_seed(2,4) = 2.71217265d-01
! even_large_expo_coef(2,4) = 0.25d0
!!! Neon
!! S type function
! even_large_expo_seed(0,10) = 2.63937466d+04
! even_large_expo_coef(0,10) = 0.30d0
!! P type function
! even_large_expo_seed(1,10) = 1.01356529d+02
! even_large_expo_coef(1,10) = 0.25d0
!! D type function
! even_large_expo_seed(2,10) = 2.16640868d+00
! even_large_expo_coef(2,10) = 0.25d0
!!! Argon
!! S type function
! even_large_expo_seed(0,18) = 2.16279421d+05
! even_large_expo_coef(0,18) = 0.26d0
!! P type function
! even_large_expo_seed(1,18) = 4.94782136d+02
! even_large_expo_coef(1,18) = 0.25d0
!! D type function
! even_large_expo_seed(2,18) = 1.13941141d+01
! even_large_expo_coef(2,18) = 0.25d0
!!! Krypton
!! S type function
! even_large_expo_seed(0,36) = 4.14098736d+06
! even_large_expo_coef(0,36) = 0.23d0
!! P type function
! even_large_expo_seed(1,36) = 9.01590791d+03
! even_large_expo_coef(1,36) = 0.25d0
!! D type function
! even_large_expo_seed(2,36) = 2.25939509d+02
! even_large_expo_coef(2,36) = 0.25d0
!!! Xenon
!! S type function
! even_large_expo_seed(0,54) = 9.04292743d+06
! even_large_expo_coef(0,54) = 0.23d0
!! P type function
! even_large_expo_seed(1,54) = 2.81773747d+05
! even_large_expo_coef(1,54) = 0.25d0
!! D type function
! even_large_expo_seed(2,54) = 1.95959651d+03
! even_large_expo_coef(2,54) = 0.25d0
!!! Ytterbium
!! S type function
! even_large_expo_seed(0,70) = 1.39601904d+07
! even_large_expo_coef(0,70) = 0.236d0
!! P type function
! even_large_expo_seed(1,70) = 3.39128349d+06
! even_large_expo_coef(1,70) = 0.25d0
!! D type function
! even_large_expo_seed(2,70) = 4.75587491d+03
! even_large_expo_coef(2,70) = 0.25d0
!!! Radon
!! S type function
! even_large_expo_seed(0,86) = 14679405.900000d0
! even_large_expo_coef(0,86) = 0.25d0
!! P type function
! even_large_expo_seed(1,86) = 1.72824122d+07
! even_large_expo_coef(1,86) = 0.25d0
!! D type function
! even_large_expo_seed(2,86) = 1253.856580d0
! even_large_expo_coef(2,86) = 0.25d0
!!! Uranium
!! S type function
!!even_large_expo_seed(0,92) = 5.58567332d+07
! even_large_expo_seed(0,92) = 1.46346866d+07
!!even_large_expo_seed(0,92) = 4.84272821d+06
! even_large_expo_coef(0,92) = 0.26d0
!! P type function
! even_large_expo_seed(1,92) = 2.64721605d+07
! even_large_expo_coef(1,92) = 0.25d0
!! D type function
! even_large_expo_seed(2,92) = 3.34172062d+04
! even_large_expo_coef(2,92) = 0.25d0
!!! Oganesson
!! S type function
! even_large_expo_seed(0,118) = 5.24543434d+07
! even_large_expo_coef(0,118) = 0.245d0
!! P type function
! even_large_expo_seed(1,118) = 4.56226785d+07
! even_large_expo_coef(1,118) = 0.25d0
!! D type function
! even_large_expo_seed(2,118) = 6.92742965d+03
! even_large_expo_coef(2,118) = 0.25d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 10-electrons even-tempered basis sets
!!! Neon
!! S type function
! even_large_expo_seed(0,10) = 2.63937466d+04
! even_large_expo_coef(0,10) = 0.30d0
!! P type function
! even_large_expo_seed(1,10) = 1.01356529d+02
! even_large_expo_coef(1,10) = 0.30d0
!! D type function
! even_large_expo_seed(2,10) = 2.16640868d+00
! even_large_expo_coef(2,10) = 0.25d0
!!! Argon
!! S type function
! even_large_expo_seed(0,18) = 2.16279421d+05
! even_large_expo_coef(0,18) = 0.29d0
!! P type function
! even_large_expo_seed(1,18) = 4.94782136d+02
! even_large_expo_coef(1,18) = 0.32d0
!! D type function
! even_large_expo_seed(2,18) = 1.13941141d+01
! even_large_expo_coef(2,18) = 0.25d0
!!! Krypton
!! S type function
! even_large_expo_seed(0,36) = 4.14098736d+06
! even_large_expo_coef(0,36) = 0.25d0
!! P type function
! even_large_expo_seed(1,36) = 9.01590791d+03
! even_large_expo_coef(1,36) = 0.25d0
!! D type function
! even_large_expo_seed(2,36) = 6.64569801d+01
! even_large_expo_coef(2,36) = 0.25d0
!!! Xenon
!! S type function
! even_large_expo_seed(0,54) = 9.04292743d+06
! even_large_expo_coef(0,54) = 0.255d0
!! P type function
! even_large_expo_seed(1,54) = 9.36856349d+03
! even_large_expo_coef(1,54) = 0.31d0
!! D type function
! even_large_expo_seed(2,54) = 1.95959651d+03
! even_large_expo_coef(2,54) = 0.25d0
!!! Ytterbium
!! S type function
! even_large_expo_seed(0,70) = 1.39601904d+07
! even_large_expo_coef(0,70) = 0.235d0
!! P type function
! even_large_expo_seed(1,70) = 2.61264417d+04
! even_large_expo_coef(1,70) = 0.32d0
!! D type function
! even_large_expo_seed(2,70) = 4.75587491d+03
! even_large_expo_coef(2,70) = 0.25d0
!!! Radon
!! S type function
! even_large_expo_seed(0,86) = 1.46794059d+07
! even_large_expo_coef(0,86) = 0.25d0
!! P type function
! even_large_expo_seed(1,86) = 5.70898829d+04
! even_large_expo_coef(1,86) = 0.30d0
!! D type function
! even_large_expo_seed(2,86) = 1.25385658d+03
! even_large_expo_coef(2,86) = 0.25d0
!!! Uranium
!! S type function
! even_large_expo_seed(0,92) = 1.46346866d+07
! even_large_expo_coef(0,92) = 0.26d0
! P type function
! even_large_expo_seed(1,92) = 1.22880210d+05
! even_large_expo_coef(1,92) = 0.28d0
!! D type function
! even_large_expo_seed(2,92) = 3.34172062d+04
! even_large_expo_coef(2,92) = 0.25d0
!!! Oganesson
!! S type function
! even_large_expo_seed(0,118) = 5.24543434d+07
! even_large_expo_coef(0,118) = 0.25d0
!! P type function
! even_large_expo_seed(1,118) = 4.39439154d+05
! even_large_expo_coef(1,118) = 0.25d0
!! D type function
! even_large_expo_seed(2,118) = 6.92742965d+03
! even_large_expo_coef(2,118) = 0.25d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 18-electrons even-tempered basis sets
!!! Argon
!! S type function
! even_large_expo_seed(0,18) = 2.16279421d+05
! even_large_expo_coef(0,18) = 0.30d0
!! P type function
! even_large_expo_seed(1,18) = 4.94782136d+02
! even_large_expo_coef(1,18) = 0.325d0
!! D type function
! even_large_expo_seed(2,18) = 1.13941141d+01
! even_large_expo_coef(2,18) = 0.25d0
!!! Krypton
!! S type function
! even_large_expo_seed(0,36) = 4.14098736d+06
!!even_large_expo_seed(0,36) = 4.88473543d+05
! even_large_expo_coef(0,36) = 0.25d0
!! P type function
! even_large_expo_seed(1,36) = 9.01590791d+03
!!even_large_expo_seed(1,36) = 1.73131083d+03
! even_large_expo_coef(1,36) = 0.30d0
!! D type function
! even_large_expo_seed(2,36) = 2.25939509d+02
!!even_large_expo_seed(2,36) = 6.64569801d+01
! even_large_expo_coef(2,36) = 0.25d0
!!! Xenon
!! S type function
! even_large_expo_seed(0,54) = 9.04292743d+06
! even_large_expo_coef(0,54) = 0.25d0
!! P type function
! even_large_expo_seed(1,54) = 9.36856349d+03
! even_large_expo_coef(1,54) = 0.36d0
!! D type function
! even_large_expo_seed(2,54) = 1.95959651d+03
! even_large_expo_coef(2,54) = 0.25d0
!!! Ytterbium
!! S type function
! even_large_expo_seed(0,70) = 1.39601904d+07
! even_large_expo_coef(0,70) = 0.26d0
!! P type function
! even_large_expo_seed(1,70) = 2.61264417d+04
! even_large_expo_coef(1,70) = 0.33d0
!! D type function
! even_large_expo_seed(2,70) = 4.75587491d+03
! even_large_expo_coef(2,70) = 0.25d0
!!! Radon
!! S type function
! even_large_expo_seed(0,86) = 1.46794059d+07
! even_large_expo_coef(0,86) = 0.30d0
!! P type function
! even_large_expo_seed(1,86) = 1.92412503d+05
! even_large_expo_coef(1,86) = 0.30d0
!! D type function
!!even_large_expo_seed(2,86) = 1.41613025d+04
! even_large_expo_seed(2,86) = 1.25385658d+03
! even_large_expo_coef(2,86) = 0.25d0
! even_large_expo_coef(2,86) = 0.25d0
 !! Uranium
 ! S type function
  even_large_expo_seed(0,92) = 1.46346866d+07
  even_large_expo_coef(0,92) = 0.29d0
! P type function
  even_large_expo_seed(1,92) = 1.22880210d+05
  even_large_expo_coef(1,92) = 0.29d0
 ! D type function
  even_large_expo_seed(2,92) = 4.95593590d+02
  even_large_expo_coef(2,92) = 0.25d0
!!! Oganesson
!! S type function
! even_large_expo_seed(0,118) = 5.24543434d+07
! even_large_expo_coef(0,118) = 0.25d0
!! P type function
! even_large_expo_seed(1,118) = 4.39439154d+05
! even_large_expo_coef(1,118) = 0.26d0
!! D type function
! even_large_expo_seed(2,118) = 6.92742965d+03
! even_large_expo_coef(2,118) = 0.24d0

 !Atomic number used in the on-the-fly even basis
  even_large_expo_number = 92
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_large_ao_prim_num, (even_large_ao_num) ]
 implicit none
  BEGIN_DOC
  !Number of large component primitives
  END_DOC
  even_large_ao_prim_num = 1
 END_PROVIDER

 BEGIN_PROVIDER [ integer, even_large_ao_prim_num_max ]
 implicit none
  BEGIN_DOC
  !max number of primitives of the large component
  END_DOC
  even_large_ao_prim_num_max = maxval(even_large_ao_prim_num)
 END_PROVIDER 

 BEGIN_PROVIDER [ integer, even_large_expo_shell_nucl_num, (0:7, nucl_num)]
 &BEGIN_PROVIDER [ integer, even_large_ao_num ]
 &BEGIN_PROVIDER [ integer, even_large_ao_nucl_num, (nucl_num)]
 &BEGIN_PROVIDER [ integer, even_large_ao_shell_nucl_num, (0:7,nucl_num)]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! even_large_expo_shell_nucl_num :: Number of l_type Aos per nucleus i
 ! even_large_ao_num :: Number of AOs of the large component
 ! even_large_ao_nucl_num :: Number of AOs per nucl
 ! even_large_ao_shell_nucl_num :: Number of AOs per shell per nucl
 END_DOC
 integer :: i,j,k,l_type
 even_large_expo_shell_nucl_num = 0
 even_large_ao_num = 0
 even_large_ao_nucl_num = 0
 even_large_ao_shell_nucl_num = 0
 do i = 1, nucl_num
 !do some test on the type of the nucleus i
   ! S type function
   even_large_expo_shell_nucl_num(0,i)+=12
   ! P type function
   even_large_expo_shell_nucl_num(1,i)+=8
   ! D type function
   even_large_expo_shell_nucl_num(2,i)+=3
   ! F type function
   even_large_expo_shell_nucl_num(3,i)+=0
   ! G type function
   even_large_expo_shell_nucl_num(4,i)+=0
   ! H type function
   even_large_expo_shell_nucl_num(5,i)+=0
   ! I type function
   even_large_expo_shell_nucl_num(6,i)+=0
   ! J type function
   even_large_expo_shell_nucl_num(7,i)+=0
 !enddo
  do l_type = 0,7
  even_large_ao_num += ((l_type+1)*(l_type+2)/2)*even_large_expo_shell_nucl_num(l_type,i)
  even_large_ao_nucl_num (i)+= ((l_type+1)*(l_type+2)/2)*even_large_expo_shell_nucl_num(l_type,i)
  even_large_ao_shell_nucl_num (l_type,i) += ((l_type+1)*(l_type+2)/2)*even_large_expo_shell_nucl_num(l_type,i)
  enddo
 enddo
 END_PROVIDER



 BEGIN_PROVIDER [ double precision, even_large_ao_expo,(even_large_ao_num) ]
 &BEGIN_PROVIDER [ double precision, even_large_ao_expo_ordered_transp,(even_large_ao_prim_num_max, even_large_ao_num) ]
 implicit none
 BEGIN_DOC
 !even_large_ao_expo :: exponents of the AOs
 !even_large_ao_expo_ordered_transp :: transposed ordered even_large_ao_expo
 END_DOC
 integer :: i,l,l_type,j,j_count,k,k_count
 even_large_ao_expo = 0 
 even_large_ao_expo_ordered_transp = 0
 k_count = 0
 do i = 1,nucl_num
 !do some test on the type of the nucleus i
  do l_type = 0,7
   do k = 1, even_large_expo_shell_nucl_num(l_type,i)
    do l =1, (l_type+1)*(l_type+2)/2
     k_count += 1
     even_large_ao_expo(k_count) = even_large_expo_seed(l_type,even_large_expo_number)*(even_large_expo_coef(l_type,even_large_expo_number)**(k-1))  
     if (k_count == 1 .or. even_large_ao_expo(k_count) /= even_large_ao_expo(k_count-1)) then 
      open (10, file='dirac_even_tempered_basis')
      if (l_type == 0) then
       write(10,'(A4, 3X, I0)'),"S",1
      elseif (l_type == 1) then
       write(10,'(A4, 3X, I0)'),"P",1 
      elseif (l_type == 2) then
       write(10,'(A4, 3X, I0)'),"D",1
      elseif (l_type == 3) then
       write(10,'(A4, 3X, I0)'),"F",1
      elseif (l_type == 4) then
       write(10,'(A4, 3X, I0)'),"G",1
      elseif (l_type == 5) then
       write(10,'(A4, 3X, I0)'),"H",1 
      elseif (l_type == 6) then
       write(10,'(A4, 3X, I0)'),"I",1
      elseif (l_type == 7) then
       write(10,'(A4, 3X, I0)'),"J",1
      endif
       write(10,'(5X, I0, 1X, F22.8,5X,F10.8)'),1, even_large_ao_expo(k_count),1.0000000
     endif
    enddo
   enddo
  enddo
 enddo
 do i = 1, even_large_ao_num
  even_large_ao_expo_ordered_transp(1,i) = even_large_ao_expo (i)
 enddo
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_large_ao_power, (even_large_ao_num,3)]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! even_large_ao_power ::  Powers of x, y and z for each |AO|
 END_DOC
 integer :: i,j,k,l,l_type,k_count,l_count,k_j
 even_large_ao_power = 0 
 k_count = 0 
 k_j = 0 
 do i = 1, nucl_num
  do l_type = 0,7
   do k = 1, even_large_expo_shell_nucl_num(l_type,i) 
    do j =1, l_type + 1
     if (j == 1) then
      k_count += 1
      k_j = k_count
      even_large_ao_power(k_count,1) = l_type
     else
      do l = 1, j
       k_count += 1
       even_large_ao_power(k_count,1) = even_large_ao_power(k_j,1) - j +1
       even_large_ao_power(k_count,2) = even_large_ao_power(k_j,2) + j -l
       even_large_ao_power(k_count,3) = even_large_ao_power(k_j,3) +l -1
      enddo
     endif
    enddo
   enddo
  enddo 
 enddo
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_large_ao_l, (even_large_ao_num) ]
 &BEGIN_PROVIDER [ integer, even_large_ao_l_max  ]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! :math:`l` value of the |AO|: :math`a+b+c` in :math:`x^a y^b z^c`
 END_DOC
 integer :: i
 do i=1,even_large_ao_num
   even_large_ao_l(i) = even_large_ao_power(i,1) + even_large_ao_power(i,2) + even_large_ao_power(i,3)
 enddo
 even_large_ao_l_max = maxval(even_large_ao_l)
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_large_ao_nucl, (even_large_ao_num)]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! even_large_ao_nucl :: Index of the nucleus on which the |AO| is centered
 END_DOC
 integer :: i,j,k
 integer, allocatable :: nucl_num_count, even_large_ao_nucl_tmp
 allocate(nucl_num_count,even_large_ao_nucl_tmp)
 nucl_num_count = 1
 even_large_ao_nucl_tmp = 0
 do j = 1, even_large_ao_num
  if (j .le. even_large_ao_nucl_num(nucl_num_count) + even_large_ao_nucl_tmp) then
   even_large_ao_nucl(j) = nucl_num_count
  else
   even_large_ao_nucl_tmp += even_large_ao_nucl_num(nucl_num_count)
   nucl_num_count += 1  
   even_large_ao_nucl(j) = nucl_num_count
  endif
 enddo
 deallocate(nucl_num_count,even_large_ao_nucl_tmp)
 END_PROVIDER

 BEGIN_PROVIDER [ integer, even_Nucl_N_Aos, (nucl_num)]
 &BEGIN_PROVIDER [ integer, even_N_AOs_max ]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! even_Nucl_N_Aos :: Number of |AOs| per nucl
 END_DOC
 integer :: i,j,k
  even_Nucl_N_Aos = 0
  do i = 1, even_large_ao_num
   even_Nucl_N_Aos(even_large_ao_nucl(i)) +=1
  enddo
  even_N_AOs_max = maxval(even_Nucl_N_Aos)
 END_PROVIDER
 
 BEGIN_PROVIDER [ integer, even_Nucl_Aos, (nucl_num,even_N_AOs_max)]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! even_Nucl_Aos :: List of |AOs| centered on each nucl
 END_DOC
 integer :: i,j,k
 integer, allocatable :: nucl_tmp(:)
 allocate(nucl_tmp(nucl_num))
 nucl_tmp = 0
 even_Nucl_Aos = 0
 do i = 1, even_large_ao_num
  nucl_tmp(even_large_ao_nucl(i))+=1
  even_Nucl_Aos(even_large_ao_nucl(i),nucl_tmp(even_large_ao_nucl(i))) = i
 enddo
 deallocate(nucl_tmp)
 END_PROVIDER

