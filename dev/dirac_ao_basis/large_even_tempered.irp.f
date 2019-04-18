 BEGIN_PROVIDER [ character*(128), even_nucl_type, (nucl_num)]
  implicit none
  BEGIN_DOC
  !type of the nucleus present, to create the even-tempered basis set
  END_DOC
 
 END_PROVIDER

 BEGIN_PROVIDER [ double precision, even_large_expo_seed,(0:7,118) ]
 &BEGIN_PROVIDER [ double precision, even_large_expo_coef,(0:7,118) ]
 implicit none
 BEGIN_DOC
 !even_large_expo :: exponents seed for each shell
 END_DOC
 !! Hydrogen
 ! S type function
  even_large_expo_seed(0,1) = 234.522333d0
  even_large_expo_coef(0,1) = 0.25d0
 ! P type function
  even_large_expo_seed(1,1) = 1.272284d0
!!! Radon
!! S type function
! even_large_expo_seed(0,86) = 5.68476598d+07
! even_large_expo_coef(0,86) = 0.25d0
!! P type function
! even_large_expo_seed(1,86) = 1.72824122d+07
! even_large_expo_coef(1,86) = 0.25d0
!! D type function
! even_large_expo_seed(2,86) = 1.41613025d+04
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
   even_large_expo_shell_nucl_num(0,i)+=10
   ! P type function
   even_large_expo_shell_nucl_num(1,i)+=6
   ! D type function
   even_large_expo_shell_nucl_num(2,i)+=1
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
    !even_large_ao_expo(k_count) = even_large_expo_seed(l_type,86)*(even_large_expo_coef(l_type,86)**(k-1))  
     even_large_ao_expo(k_count) = even_large_expo_seed(l_type,1)*(even_large_expo_coef(l_type,1)**(k-1))  
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

