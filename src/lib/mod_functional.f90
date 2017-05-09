! functional-fortran - Functional programming for modern Fortran
! Copyright (c) 2016-2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.

module mod_functional
use iso_fortran_env,only:i1 => int8,i2 => int16,i4 => int32,i8 => int64,&
                         r4 => real32,r8 => real64,r16 => real128
use mod_interfaces
implicit none

private

public :: arange,complement,empty,filter,foldl,foldr,foldt,head,init,&
          insert,intersection,iterfold,last,limit,map,reverse,set,sort,&
          split,subscript,tail,unfold,union

public :: operator(.complement.)
public :: operator(.head.)
public :: operator(.init.)
public :: operator(.intersection.)
public :: operator(.last.)
public :: operator(.reverse.)
public :: operator(.set.)
public :: operator(.sort.)
public :: operator(.tail.)
public :: operator(.union.)

interface arange
  module procedure :: arange_i1,arange_i2,arange_i4,arange_i8
  module procedure :: arange_r4,arange_r8,arange_r16
  module procedure :: arange_c4,arange_c8,arange_c16
endinterface arange

interface complement
  module procedure :: complement_i1,complement_i2,complement_i4,complement_i8
  module procedure :: complement_r4,complement_r8,complement_r16
  module procedure :: complement_c4,complement_c8,complement_c16
endinterface complement

interface operator(.complement.)
  module procedure :: complement_i1,complement_i2,complement_i4,complement_i8
  module procedure :: complement_r4,complement_r8,complement_r16
  module procedure :: complement_c4,complement_c8,complement_c16
endinterface

interface empty
  module procedure :: empty_i1,empty_i2,empty_i4,empty_i8
  module procedure :: empty_r4,empty_r8,empty_r16
  module procedure :: empty_c4,empty_c8,empty_c16
endinterface empty

interface filter
  module procedure :: filter_i1,filter_i2,filter_i4,filter_i8
  module procedure :: filter_r4,filter_r8,filter_r16
  module procedure :: filter_c4,filter_c8,filter_c16
endinterface filter

interface foldl
  module procedure :: foldl_i1,foldl_i2,foldl_i4,foldl_i8
  module procedure :: foldl_r4,foldl_r8,foldl_r16
  module procedure :: foldl_c4,foldl_c8,foldl_c16
endinterface foldl

interface foldr
  module procedure :: foldr_i1,foldr_i2,foldr_i4,foldr_i8
  module procedure :: foldr_r4,foldr_r8,foldr_r16
  module procedure :: foldr_c4,foldr_c8,foldr_c16
endinterface foldr

interface foldt
  module procedure :: foldt_i1,foldt_i2,foldt_i4,foldt_i8
  module procedure :: foldt_r4,foldt_r8,foldt_r16
  module procedure :: foldt_c4,foldt_c8,foldt_c16
endinterface foldt

interface head
  module procedure :: head_i1,head_i2,head_i4,head_i8
  module procedure :: head_r4,head_r8,head_r16
  module procedure :: head_c4,head_c8,head_c16
endinterface head

interface operator(.head.)
  module procedure :: head_i1,head_i2,head_i4,head_i8
  module procedure :: head_r4,head_r8,head_r16
  module procedure :: head_c4,head_c8,head_c16
endinterface

interface init
  module procedure :: init_i1,init_i2,init_i4,init_i8
  module procedure :: init_r4,init_r8,init_r16
  module procedure :: init_c4,init_c8,init_c16
endinterface init

interface operator(.init.)
  module procedure :: init_i1,init_i2,init_i4,init_i8
  module procedure :: init_r4,init_r8,init_r16
  module procedure :: init_c4,init_c8,init_c16
endinterface

interface insert
  module procedure :: insert_i1,insert_i2,insert_i4,insert_i8
  module procedure :: insert_r4,insert_r8,insert_r16
  module procedure :: insert_c4,insert_c8,insert_c16
endinterface insert

interface intersection
  module procedure :: intersection_i1,intersection_i2,intersection_i4,intersection_i8
  module procedure :: intersection_r4,intersection_r8,intersection_r16
  module procedure :: intersection_c4,intersection_c8,intersection_c16
endinterface intersection

interface operator(.intersection.)
  module procedure :: intersection_i1,intersection_i2,intersection_i4,intersection_i8
  module procedure :: intersection_r4,intersection_r8,intersection_r16
  module procedure :: intersection_c4,intersection_c8,intersection_c16
endinterface

interface iterfold
  module procedure :: iterfold_i1,iterfold_i2,iterfold_i4,iterfold_i8
  module procedure :: iterfold_r4,iterfold_r8,iterfold_r16
  module procedure :: iterfold_c4,iterfold_c8,iterfold_c16
endinterface iterfold

interface last
  module procedure :: last_i1,last_i2,last_i4,last_i8
  module procedure :: last_r4,last_r8,last_r16
  module procedure :: last_c4,last_c8,last_c16
endinterface last

interface operator(.last.)
  module procedure :: last_i1,last_i2,last_i4,last_i8
  module procedure :: last_r4,last_r8,last_r16
  module procedure :: last_c4,last_c8,last_c16
endinterface

interface limit
  module procedure :: limit_i1,limit_i2,limit_i4,limit_i8
  module procedure :: limit_r4,limit_r8,limit_r16
  module procedure :: limit_c4,limit_c8,limit_c16
endinterface limit

interface map
  module procedure :: map_i1,map_i2,map_i4,map_i8
  module procedure :: map_r4,map_r8,map_r16
  module procedure :: map_c4,map_c8,map_c16
endinterface map

interface reverse
  module procedure :: reverse_i1,reverse_i2,reverse_i4,reverse_i8
  module procedure :: reverse_r4,reverse_r8,reverse_r16
  module procedure :: reverse_c4,reverse_c8,reverse_c16
endinterface reverse

interface operator(.reverse.)
  module procedure :: reverse_i1,reverse_i2,reverse_i4,reverse_i8
  module procedure :: reverse_r4,reverse_r8,reverse_r16
  module procedure :: reverse_c4,reverse_c8,reverse_c16
endinterface

interface set
  module procedure :: set_i1,set_i2,set_i4,set_i8
  module procedure :: set_r4,set_r8,set_r16
  module procedure :: set_c4,set_c8,set_c16
endinterface set

interface operator(.set.)
  module procedure :: set_i1,set_i2,set_i4,set_i8
  module procedure :: set_r4,set_r8,set_r16
  module procedure :: set_c4,set_c8,set_c16
endinterface

interface sort
  module procedure :: sort_i1,sort_i2,sort_i4,sort_i8
  module procedure :: sort_r4,sort_r8,sort_r16
  module procedure :: sort_c4,sort_c8,sort_c16
endinterface sort

interface operator(.sort.)
  module procedure :: sort_i1,sort_i2,sort_i4,sort_i8
  module procedure :: sort_r4,sort_r8,sort_r16
  module procedure :: sort_c4,sort_c8,sort_c16
endinterface

interface split
  module procedure :: split_i1,split_i2,split_i4,split_i8
  module procedure :: split_r4,split_r8,split_r16
  module procedure :: split_c4,split_c8,split_c16
endinterface split

interface subscript
  module procedure :: subscript_i1,subscript_i2,subscript_i4,subscript_i8
  module procedure :: subscript_r4,subscript_r8,subscript_r16
  module procedure :: subscript_c4,subscript_c8,subscript_c16
endinterface subscript

interface tail
  module procedure :: tail_i1,tail_i2,tail_i4,tail_i8
  module procedure :: tail_r4,tail_r8,tail_r16
  module procedure :: tail_c4,tail_c8,tail_c16
endinterface tail

interface operator(.tail.)
  module procedure :: tail_i1,tail_i2,tail_i4,tail_i8
  module procedure :: tail_r4,tail_r8,tail_r16
  module procedure :: tail_c4,tail_c8,tail_c16
endinterface

interface unfold
  module procedure :: unfold_i1,unfold_i2,unfold_i4,unfold_i8
  module procedure :: unfold_r4,unfold_r8,unfold_r16
  module procedure :: unfold_c4,unfold_c8,unfold_c16
endinterface unfold

interface union
  module procedure :: union_i1,union_i2,union_i4,union_i8
  module procedure :: union_r4,union_r8,union_r16
  module procedure :: union_c4,union_c8,union_c16
endinterface union

interface operator(.union.)
  module procedure :: union_i1,union_i2,union_i4,union_i8
  module procedure :: union_r4,union_r8,union_r16
  module procedure :: union_c4,union_c8,union_c16
endinterface

interface operator(<)
  module procedure :: lt_c4,lt_c8,lt_c16
endinterface operator(<) 

interface operator(>=)
  module procedure :: ge_c4,ge_c8,ge_c16
endinterface operator(>=) 

contains


pure elemental logical function ge_c4(lhs,rhs) result(res)
  !! Private `>=` implementation for 4-byte complex numbers. 
  complex(kind=r4),intent(in) :: lhs,rhs
  res = abs(lhs) >= abs(rhs)
endfunction ge_c4

pure elemental logical function ge_c8(lhs,rhs) result(res)
  !! Private `>=` implementation for 8-byte complex numbers. 
  complex(kind=r8),intent(in) :: lhs,rhs
  res = abs(lhs) >= abs(rhs)
endfunction ge_c8

pure elemental logical function ge_c16(lhs,rhs) result(res)
  !! Private `>=` implementation for 16-byte complex numbers. 
  complex(kind=r16),intent(in) :: lhs,rhs
  res = abs(lhs) >= abs(rhs)
endfunction ge_c16

pure elemental logical function lt_c4(lhs,rhs) result(res)
  !! Private `<` implementation for 4-byte complex numbers. 
  complex(kind=r4),intent(in) :: lhs,rhs
  res = abs(lhs) < abs(rhs)
endfunction lt_c4

pure elemental logical function lt_c8(lhs,rhs) result(res)
  !! Private `<` implementation for 8-byte complex numbers. 
  complex(kind=r8),intent(in) :: lhs,rhs
  res = abs(lhs) < abs(rhs)
endfunction lt_c8

pure elemental logical function lt_c16(lhs,rhs) result(res)
  !! Private `<` implementation for 16-byte complex numbers. 
  complex(kind=r16),intent(in) :: lhs,rhs
  res = abs(lhs) < abs(rhs)
endfunction lt_c16


pure function arange_i1(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 1-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=i1),intent(in) :: start !! Start value of the array
  integer(kind=i1),intent(in) :: end !! End value of the array
  integer(kind=i1),intent(in),optional :: increment !! Array increment
  integer(kind=i1),dimension(:),allocatable :: arange
  integer(kind=i1) :: incr
  integer(kind=i1) :: i
  integer(kind=i1) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_i1


pure function arange_i2(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 2-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=i2),intent(in) :: start !! Start value of the array
  integer(kind=i2),intent(in) :: end !! End value of the array
  integer(kind=i2),intent(in),optional :: increment !! Array increment
  integer(kind=i2),dimension(:),allocatable :: arange
  integer(kind=i2) :: incr
  integer(kind=i2) :: i
  integer(kind=i2) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_i2


pure function arange_i4(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 4-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=i4),intent(in) :: start !! Start value of the array
  integer(kind=i4),intent(in) :: end !! End value of the array
  integer(kind=i4),intent(in),optional :: increment !! Array increment
  integer(kind=i4),dimension(:),allocatable :: arange
  integer(kind=i4) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_i4


pure function arange_i8(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 8-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=i8),intent(in) :: start !! Start value of the array
  integer(kind=i8),intent(in) :: end !! End value of the array
  integer(kind=i8),intent(in),optional :: increment !! Array increment
  integer(kind=i8),dimension(:),allocatable :: arange
  integer(kind=i8) :: incr
  integer(kind=i8) :: i
  integer(kind=i8) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_i8


pure function arange_r4(start,end,increment) result(arange)
  !! Returns an array of reals given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 4-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(kind=r4),intent(in) :: start !! Start value of the array
  real(kind=r4),intent(in) :: end !! End value of the array
  real(kind=r4),intent(in),optional :: increment !! Array increment
  real(kind=r4),dimension(:),allocatable :: arange
  real(kind=r4) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start+0.5*incr)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_r4


pure function arange_r8(start,end,increment) result(arange)
  !! Returns an array of reals given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 8-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(kind=r8),intent(in) :: start !! Start value of the array
  real(kind=r8),intent(in) :: end !! End value of the array
  real(kind=r8),intent(in),optional :: increment !! Array increment
  real(kind=r8),dimension(:),allocatable :: arange
  real(kind=r8) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start+0.5*incr)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_r8


pure function arange_r16(start,end,increment) result(arange)
  !! Returns an array of reals given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 16-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(kind=r16),intent(in) :: start !! Start value of the array
  real(kind=r16),intent(in) :: end !! End value of the array
  real(kind=r16),intent(in),optional :: increment !! Array increment
  real(kind=r16),dimension(:),allocatable :: arange
  real(kind=r16) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = (end-start+0.5*incr)/incr+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_r16


pure function arange_c4(start,end,increment) result(arange)
  !! Returns an array of complex reals given `start`, `end`, and 
  !! `increment` values. Increment defaults to (1,0) if not provided.
  !! Size of the resulting array is determined with real components of 
  !! `start`, `end`, and  `increment` values if `real(increment) /= 0`,
  !! and imaginary components otherwise.
  !! This specific procedure is for 4-byte complex reals.
  !! Oveloaded by generic procedure `arange`.
  complex(kind=r4),intent(in) :: start !! Start value of the array
  complex(kind=r4),intent(in) :: end !! End value of the array
  complex(kind=r4),intent(in),optional :: increment !! Array increment
  complex(kind=r4),dimension(:),allocatable :: arange
  complex(kind=r4) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = (1,0)
  endif
  if(real(incr) /= 0)then
    length = (real(end)-real(start)+0.5*real(incr))/real(incr)+1
  else
    length = (aimag(end)-aimag(start)+0.5*aimag(incr))/aimag(incr)+1
  endif
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = cmplx(real(start)+(i-1)*real(incr),&
                      aimag(start)+(i-1)*aimag(incr))
  enddo
endfunction arange_c4


pure function arange_c8(start,end,increment) result(arange)
  !! Returns an array of complex reals given `start`, `end`, and 
  !! `increment` values. Increment defaults to (1,0) if not provided.
  !! Size of the resulting array is determined with real components of 
  !! `start`, `end`, and  `increment` values if `real(increment) /= 0`,
  !! and imaginary components otherwise.
  !! This specific procedure is for 8-byte complex reals.
  !! Oveloaded by generic procedure `arange`.
  complex(kind=r8),intent(in) :: start !! Start value of the array
  complex(kind=r8),intent(in) :: end !! End value of the array
  complex(kind=r8),intent(in),optional :: increment !! Array increment
  complex(kind=r8),dimension(:),allocatable :: arange
  complex(kind=r8) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = (1,0)
  endif
  if(real(incr) /= 0)then
    length = (real(end)-real(start)+0.5*real(incr))/real(incr)+1
  else
    length = (aimag(end)-aimag(start)+0.5*aimag(incr))/aimag(incr)+1
  endif
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = cmplx(real(start)+(i-1)*real(incr),&
                      aimag(start)+(i-1)*aimag(incr))
  enddo
endfunction arange_c8


pure function arange_c16(start,end,increment) result(arange)
  !! Returns an array of complex reals given `start`, `end`, and 
  !! `increment` values. Increment defaults to (1,0) if not provided.
  !! Size of the resulting array is determined with real components of 
  !! `start`, `end`, and  `increment` values if `real(increment) /= 0`,
  !! and imaginary components otherwise.
  !! This specific procedure is for 16-byte complex reals.
  !! Oveloaded by generic procedure `arange`. 
  complex(kind=r16),intent(in) :: start !! Start value of the array
  complex(kind=r16),intent(in) :: end !! End value of the array
  complex(kind=r16),intent(in),optional :: increment !! Array increment
  complex(kind=r16),dimension(:),allocatable :: arange
  complex(kind=r16) :: incr
  integer(kind=i4) :: i
  integer(kind=i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = (1,0)
  endif
  if(real(incr) /= 0)then
    length = (real(end)-real(start)+0.5*real(incr))/real(incr)+1
  else
    length = (aimag(end)-aimag(start)+0.5*aimag(incr))/aimag(incr)+1
  endif
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = cmplx(real(start)+(i-1)*real(incr),&
                      aimag(start)+(i-1)*aimag(incr))
  enddo
endfunction arange_c16


pure function complement_i1(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=i1),dimension(:),intent(in) :: x !! First input array
  integer(kind=i1),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i1),dimension(:),allocatable :: complement
  integer(kind=i1),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i1,0_i1)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_i1


pure function complement_i2(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=i2),dimension(:),intent(in) :: x !! First input array
  integer(kind=i2),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i2),dimension(:),allocatable :: complement
  integer(kind=i2),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i2,0_i2)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_i2


pure function complement_i4(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=i4),dimension(:),intent(in) :: x !! First input array
  integer(kind=i4),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i4),dimension(:),allocatable :: complement
  integer(kind=i4),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i4,0_i4)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_i4


pure function complement_i8(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=i8),dimension(:),intent(in) :: x !! First input array
  integer(kind=i8),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i8),dimension(:),allocatable :: complement
  integer(kind=i8),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i8,0_i8)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_i8


pure function complement_r4(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(kind=r4),dimension(:),intent(in) :: x !! First input array
  real(kind=r4),dimension(:),intent(in) :: y !! Second input array
  real(kind=r4),dimension(:),allocatable :: complement
  real(kind=r4),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._r4,0._r4)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_r4


pure function complement_r8(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(kind=r8),dimension(:),intent(in) :: x !! First input array
  real(kind=r8),dimension(:),intent(in) :: y !! Second input array
  real(kind=r8),dimension(:),allocatable :: complement
  real(kind=r8),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._r4,0._r4)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_r8


pure function complement_r16(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(kind=r16),dimension(:),intent(in) :: x !! First input array
  real(kind=r16),dimension(:),intent(in) :: y !! Second input array
  real(kind=r16),dimension(:),allocatable :: complement
  real(kind=r16),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._r16,0._r16)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_r16


pure function complement_c4(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `complement`.
  complex(kind=r4),dimension(:),intent(in) :: x !! First input array
  complex(kind=r4),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r4),dimension(:),allocatable :: complement
  complex(kind=r4),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(cmplx(1._r4,0._r4),cmplx(0._r4,0._r4))
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_c4


pure function complement_c8(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `complement`.
  complex(kind=r8),dimension(:),intent(in) :: x !! First input array
  complex(kind=r8),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r8),dimension(:),allocatable :: complement
  complex(kind=r8),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(cmplx(1._r4,0._r4),cmplx(0._r4,0._r4))
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_c8


pure function complement_c16(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `complement`.
  complex(kind=r16),dimension(:),intent(in) :: x !! First input array
  complex(kind=r16),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r16),dimension(:),allocatable :: complement
  complex(kind=r16),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(cmplx(1._r16,0._r16),cmplx(0._r16,0._r16))
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_c16


pure function empty_i1(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(kind=i1),intent(in) :: a !! Input scalar
  integer(kind=i1),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_i1


pure function empty_i2(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(kind=i2),intent(in) :: a !! Input scalar
  integer(kind=i2),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_i2


pure function empty_i4(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(kind=i4),intent(in) :: a !! Input scalar
  integer(kind=i4),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_i4


pure function empty_i8(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(kind=i8),intent(in) :: a !! Input scalar
  integer(kind=i8),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_i8


pure function empty_r4(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `empty`.
  real(kind=r4),intent(in) :: a !! Input scalar
  real(kind=r4),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_r4


pure function empty_r8(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `empty`.
  real(kind=r8),intent(in) :: a !! Input scalar
  real(kind=r8),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_r8


pure function empty_r16(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `empty`.
  real(kind=r16),intent(in) :: a !! Input scalar
  real(kind=r16),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_r16


pure function empty_c4(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `empty`.
  complex(kind=r4),intent(in) :: a !! Input scalar
  complex(kind=r4),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_c4


pure function empty_c8(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `empty`.
  complex(kind=r8),intent(in) :: a !! Input scalar
  complex(kind=r8),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_c8


pure function empty_c16(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `empty`.
  complex(kind=r16),intent(in) :: a !! Input scalar
  complex(kind=r16),dimension(:),allocatable :: empty
  allocate(empty(0))
endfunction empty_c16


pure function filter_i1(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i1_logical) :: f !! Filtering function
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_i1


pure function filter_i2(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i2_logical) :: f !! Filtering function
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_i2


pure function filter_i4(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i4_logical) :: f !! Filtering function
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_i4


pure function filter_i8(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i8_logical) :: f !! Filtering function
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_i8


pure function filter_r4(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_r4_logical) :: f !! Filtering function
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_r4


pure function filter_r8(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_r8_logical) :: f !! Filtering function
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_r8


pure function filter_r16(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_r16_logical) :: f !! Filtering function
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_r16


pure function filter_c4(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_c4_logical) :: f !! Filtering function
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_c4


pure function filter_c8(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_c8_logical) :: f !! Filtering function
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_c8


pure function filter_c16(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_c16_logical) :: f !! Filtering function
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_c16


pure recursive integer(kind=i1) function foldl_i1(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i1) :: f !! Folding function
  integer(kind=i1),intent(in) :: start !! Accumulator start value
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_i1


pure recursive integer(kind=i2) function foldl_i2(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i2) :: f !! Folding function
  integer(kind=i2),intent(in) :: start !! Accumulator start value
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_i2


pure recursive integer(kind=i4) function foldl_i4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i4) :: f !! Folding function
  integer(kind=i4),intent(in) :: start !! Accumulator start value
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_i4


pure recursive integer(kind=i8) function foldl_i8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i8) :: f !! Folding function
  integer(kind=i8),intent(in) :: start !! Accumulator start value
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_i8


pure recursive real(kind=r4) function foldl_r4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_r4) :: f !! Folding function
  real(kind=r4),intent(in) :: start !! Accumulator start value
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_r4


pure recursive real(kind=r8) function foldl_r8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_r8) :: f !! Folding function
  real(kind=r8),intent(in) :: start !! Accumulator start value
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_r8


pure recursive real(kind=r16) function foldl_r16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_r16) :: f !! Folding function
  real(kind=r16),intent(in) :: start !! Accumulator start value
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_r16


pure recursive complex(kind=r4) function foldl_c4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_c4) :: f !! Folding function
  complex(kind=r4),intent(in) :: start !! Accumulator start value
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_c4


pure recursive complex(kind=r8) function foldl_c8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_c8) :: f !! Folding function
  complex(kind=r8),intent(in) :: start !! Accumulator start value
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_c8


pure recursive complex(kind=r16) function foldl_c16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_c16) :: f !! Folding function
  complex(kind=r16),intent(in) :: start !! Accumulator start value
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_c16


pure recursive integer(kind=i1) function foldr_i1(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i1) :: f !! Folding function
  integer(kind=i1),intent(in) :: start !! Accumulator start value
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_i1


pure recursive integer(kind=i2) function foldr_i2(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i2) :: f !! Folding function
  integer(kind=i2),intent(in) :: start !! Accumulator start value
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_i2


pure recursive integer(kind=i4) function foldr_i4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i4) :: f !! Folding function
  integer(kind=i4),intent(in) :: start !! Accumulator start value
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_i4


pure recursive integer(kind=i8) function foldr_i8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i8) :: f !! Folding function
  integer(kind=i8),intent(in) :: start !! Accumulator start value
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_i8


pure recursive real(kind=r4) function foldr_r4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_r4) :: f !! Folding function
  real(kind=r4),intent(in) :: start !! Accumulator start value
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_r4


pure recursive real(kind=r8) function foldr_r8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_r8) :: f !! Folding function
  real(kind=r8),intent(in) :: start !! Accumulator start value
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_r8


pure recursive real(kind=r16) function foldr_r16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_r16) :: f !! Folding function
  real(kind=r16),intent(in) :: start !! Accumulator start value
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_r16


pure recursive complex(kind=r4) function foldr_c4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_c4) :: f !! Folding function
  complex(kind=r4),intent(in) :: start !! Accumulator start value
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_c4


pure recursive complex(kind=r8) function foldr_c8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_c8) :: f !! Folding function
  complex(kind=r8),intent(in) :: start !! Accumulator start value
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_c8


pure recursive complex(kind=r16) function foldr_c16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_c16) :: f !! Folding function
  complex(kind=r16),intent(in) :: start !! Accumulator start value
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_c16


pure recursive integer(kind=i1) function foldt_i1(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i1) :: f !! Folding function
  integer(kind=i1),intent(in) :: start !! Accumulator start value
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_i1


pure recursive integer(kind=i2) function foldt_i2(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i2) :: f !! Folding function
  integer(kind=i2),intent(in) :: start !! Accumulator start value
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_i2


pure recursive integer(kind=i4) function foldt_i4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i4) :: f !! Folding function
  integer(kind=i4),intent(in) :: start !! Accumulator start value
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_i4


pure recursive integer(kind=i8) function foldt_i8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i8) :: f !! Folding function
  integer(kind=i8),intent(in) :: start !! Accumulator start value
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_i8


pure recursive real(kind=r4) function foldt_r4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_r4) :: f !! Folding function
  real(kind=r4),intent(in) :: start !! Accumulator start value
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_r4


pure recursive real(kind=r8) function foldt_r8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_r8) :: f !! Folding function
  real(kind=r8),intent(in) :: start !! Accumulator start value
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_r8


pure recursive real(kind=r16) function foldt_r16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_r16) :: f !! Folding function
  real(kind=r16),intent(in) :: start !! Accumulator start value
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_r16


pure recursive complex(kind=r4) function foldt_c4(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_c4) :: f !! Folding function
  complex(kind=r4),intent(in) :: start !! Accumulator start value
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_c4


pure recursive complex(kind=r8) function foldt_c8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_c8) :: f !! Folding function
  complex(kind=r8),intent(in) :: start !! Accumulator start value
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_c8


pure recursive complex(kind=r16) function foldt_c16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_c16) :: f !! Folding function
  complex(kind=r16),intent(in) :: start !! Accumulator start value
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_c16


pure integer(kind=i1) function head_i1(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_i1


pure integer(kind=i2) function head_i2(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_i2


pure integer(kind=i4) function head_i4(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_i4


pure integer(kind=i8) function head_i8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_i8


pure real(kind=r4) function head_r4(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `head`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_r4


pure real(kind=r8) function head_r8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `head`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_r8


pure real(kind=r16) function head_r16(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `head`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_r16


pure complex(kind=r4) function head_c4(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `head`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_c4


pure complex(kind=r8) function head_c8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `head`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_c8


pure complex(kind=r16) function head_c16(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `head`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_c16


pure function init_i1(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_i1


pure function init_i2(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_i2


pure function init_i4(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_i4


pure function init_i8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_i8


pure function init_r4(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `init`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_r4


pure function init_r8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `init`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_r8


pure function init_r16(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `init`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_r16


pure function init_c4(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `init`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_c4


pure function init_c8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `init`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_c8


pure function init_c16(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `init`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_c16


pure function insert_i1(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=i1),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_i1


pure function insert_i2(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=i2),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_i2


pure function insert_i4(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=i4),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_i4


pure function insert_i8(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=i8),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_i8


pure function insert_r4(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(kind=r4),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_r4


pure function insert_r8(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(kind=r8),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_r8


pure function insert_r16(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(kind=r16),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_r16


pure function insert_c4(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `insert`.
  complex(kind=r4),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_c4


pure function insert_c8(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `insert`.
  complex(kind=r8),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_c8


pure function insert_c16(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `insert`.
  complex(kind=r16),intent(in) :: elem !! Element to insert
  integer(kind=i4),intent(in) :: ind !! Index to insert element at
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_c16


pure function intersection_i1(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=i1),dimension(:),intent(in) :: x !! First input array
  integer(kind=i1),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i1),dimension(:),allocatable :: res
  integer(kind=i1),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i1)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_i1


pure function intersection_i2(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=i2),dimension(:),intent(in) :: x !! First input array
  integer(kind=i2),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i2),dimension(:),allocatable :: res
  integer(kind=i2),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i2)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_i2


pure function intersection_i4(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=i4),dimension(:),intent(in) :: x !! First input array
  integer(kind=i4),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i4),dimension(:),allocatable :: res
  integer(kind=i4),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i4)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_i4


pure function intersection_i8(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=i8),dimension(:),intent(in) :: x !! First input array
  integer(kind=i8),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i8),dimension(:),allocatable :: res
  integer(kind=i8),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i8)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_i8


pure function intersection_r4(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(kind=r4),dimension(:),intent(in) :: x !! First input array
  real(kind=r4),dimension(:),intent(in) :: y !! Second input array
  real(kind=r4),dimension(:),allocatable :: res
  real(kind=r4),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1._r4)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_r4


pure function intersection_r8(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(kind=r8),dimension(:),intent(in) :: x !! First input array
  real(kind=r8),dimension(:),intent(in) :: y !! Second input array
  real(kind=r8),dimension(:),allocatable :: res
  real(kind=r8),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1._r8)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_r8


pure function intersection_r16(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(kind=r16),dimension(:),intent(in) :: x !! First input array
  real(kind=r16),dimension(:),intent(in) :: y !! Second input array
  real(kind=r16),dimension(:),allocatable :: res
  real(kind=r16),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1._r16)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_r16


pure function intersection_c4(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `intersection`.
  complex(kind=r4),dimension(:),intent(in) :: x !! First input array
  complex(kind=r4),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r4),dimension(:),allocatable :: res
  complex(kind=r4),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(cmplx(1._r4,0._r4))
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_c4


pure function intersection_c8(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `intersection`.
  complex(kind=r8),dimension(:),intent(in) :: x !! First input array
  complex(kind=r8),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r8),dimension(:),allocatable :: res
  complex(kind=r8),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(cmplx(1._r8,0._r8))
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_c8


pure function intersection_c16(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `intersection`.
  complex(kind=r16),dimension(:),intent(in) :: x !! First input array
  complex(kind=r16),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r16),dimension(:),allocatable :: res
  complex(kind=r16),dimension(:),allocatable :: a,b
  integer(kind=i4) :: n
  a = set(x)
  b = set(y)
  res = empty(cmplx(1._r16,0._r16))
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_c16


pure integer(kind=i1) function iterfold_i1(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i1) :: f !! Folding function
  integer(kind=i1),intent(in) :: start !! Accumulator start value
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_i1


pure integer(kind=i2) function iterfold_i2(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i2) :: f !! Folding function
  integer(kind=i2),intent(in) :: start !! Accumulator start value
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_i2


pure integer(kind=i4) function iterfold_i4(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i4) :: f !! Folding function
  integer(kind=i4),intent(in) :: start !! Accumulator start value
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_i4


pure integer(kind=i8) function iterfold_i8(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i8) :: f !! Folding function
  integer(kind=i8),intent(in) :: start !! Accumulator start value
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_i8


pure real(kind=r4) function iterfold_r4(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_r4) :: f !! Folding function
  real(kind=r4),intent(in) :: start !! Accumulator start value
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_r4


pure real(kind=r8) function iterfold_r8(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_r8) :: f !! Folding function
  real(kind=r8),intent(in) :: start !! Accumulator start value
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_r8


pure real(kind=r16) function iterfold_r16(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_r16) :: f !! Folding function
  real(kind=r16),intent(in) :: start !! Accumulator start value
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_r16


pure complex(kind=r4) function iterfold_c4(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_c4) :: f !! Folding function
  complex(kind=r4),intent(in) :: start !! Accumulator start value
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_c4


pure complex(kind=r8) function iterfold_c8(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_c8) :: f !! Folding function
  complex(kind=r8),intent(in) :: start !! Accumulator start value
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_c8


pure complex(kind=r16) function iterfold_c16(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_c16) :: f !! Folding function
  complex(kind=r16),intent(in) :: start !! Accumulator start value
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_c16


pure integer(kind=i1) function last_i1(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_i1


pure integer(kind=i2) function last_i2(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_i2


pure integer(kind=i4) function last_i4(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_i4


pure integer(kind=i8) function last_i8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_i8


pure real(kind=r4) function last_r4(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `last`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_r4


pure real(kind=r8) function last_r8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `last`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_r8


pure real(kind=r16) function last_r16(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `last`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_r16


pure complex(kind=r4) function last_c4(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `last`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_c4


pure complex(kind=r8) function last_c8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `last`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_c8


pure complex(kind=r16) function last_c16(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `last`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_c16


pure elemental integer(kind=i1) function limit_i1(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=i1),intent(in) :: x !! Input scalar
  integer(kind=i1),intent(in) :: a !! First limit
  integer(kind=i1),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_i1


pure elemental integer(kind=i2) function limit_i2(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=i2),intent(in) :: x !! Input scalar
  integer(kind=i2),intent(in) :: a !! First limit
  integer(kind=i2),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_i2


pure elemental integer(kind=i4) function limit_i4(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=i4),intent(in) :: x !! Input scalar
  integer(kind=i4),intent(in) :: a !! First limit
  integer(kind=i4),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_i4


pure elemental integer(kind=i8) function limit_i8(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=i8),intent(in) :: x !! Input scalar
  integer(kind=i8),intent(in) :: a !! First limit
  integer(kind=i8),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_i8


pure elemental real(kind=r4) function limit_r4(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(kind=r4),intent(in) :: x !! Input scalar
  real(kind=r4),intent(in) :: a !! First limit
  real(kind=r4),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_r4


pure elemental real(kind=r8) function limit_r8(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(kind=r8),intent(in) :: x !! Input scalar
  real(kind=r8),intent(in) :: a !! First limit
  real(kind=r8),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_r8


pure elemental real(kind=r16) function limit_r16(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(kind=r16),intent(in) :: x !! Input scalar
  real(kind=r16),intent(in) :: a !! First limit
  real(kind=r16),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_r16


pure elemental complex(kind=r4) function limit_c4(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`,
  !! for Re and Im components each.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `limit`.
  complex(kind=r4),intent(in) :: x !! Input scalar
  complex(kind=r4),intent(in) :: a !! First limit
  complex(kind=r4),intent(in) :: b !! Second limit
  limit = cmplx(min(max(real(x),min(real(a),real(b))),max(real(a),real(b))),&
    min(max(aimag(x),min(aimag(a),aimag(b))),max(aimag(a),aimag(b))))
endfunction limit_c4


pure elemental complex(kind=r8) function limit_c8(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`,
  !! for Re and Im components each.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `limit`.
  complex(kind=r8),intent(in) :: x !! Input scalar
  complex(kind=r8),intent(in) :: a !! First limit
  complex(kind=r8),intent(in) :: b !! Second limit
  limit = cmplx(min(max(real(x),min(real(a),real(b))),max(real(a),real(b))),&
    min(max(aimag(x),min(aimag(a),aimag(b))),max(aimag(a),aimag(b))))
endfunction limit_c8


pure elemental complex(kind=r16) function limit_c16(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`,
  !! for Re and Im components each.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `limit`.
  complex(kind=r16),intent(in) :: x !! Input scalar
  complex(kind=r16),intent(in) :: a !! First limit
  complex(kind=r16),intent(in) :: b !! Second limit
  limit = cmplx(min(max(real(x),min(real(a),real(b))),max(real(a),real(b))),&
    min(max(aimag(x),min(aimag(a),aimag(b))),max(aimag(a),aimag(b))))
endfunction limit_c16


pure function map_i1(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i1) :: f !! Mapping function
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_i1


pure function map_i2(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i2) :: f !! Mapping function
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_i2


pure function map_i4(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i4) :: f !! Mapping function
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_i4


pure function map_i8(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i8) :: f !! Mapping function
  integer(kind=i8),dimension(:),intent(in) :: x
  integer(kind=i8),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_i8


pure function map_r4(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_r4) :: f !! Mapping function
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_r4


pure function map_r8(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_r8) :: f !! Mapping function
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_r8


pure function map_r16(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_r16) :: f !! Mapping function
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_r16


pure function map_c4(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_c4) :: f !! Mapping function
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_c4


pure function map_c8(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_c8) :: f !! Mapping function
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_c8


pure function map_c16(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_c16) :: f !! Mapping function
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(size(x)) :: map
  integer(kind=i4) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_c16


pure function reverse_i1(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_i1
 

pure function reverse_i2(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_i2


pure function reverse_i4(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_i4


pure function reverse_i8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_i8


pure function reverse_r4(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_r4


pure function reverse_r8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_r8


pure function reverse_r16(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_r16


pure function reverse_c4(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `reverse`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_c4


pure function reverse_c8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `reverse`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_c8


pure function reverse_c16(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `reverse`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_c16


pure recursive function set_i1(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_i1


pure recursive function set_i2(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_i2

pure recursive function set_i4(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_i4


pure recursive function set_i8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_i8


pure recursive function set_r4(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `set`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_r4


pure recursive function set_r8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `set`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_r8


pure recursive function set_r16(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `set`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_r16


pure recursive function set_c4(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `set`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_c4


pure recursive function set_c8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `set`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_c8


pure recursive function set_c16(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `set`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_c16


pure recursive function sort_i1(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(size(x)) :: res
  integer(kind=i1),dimension(size(x)-1) :: rest
  integer(kind=i1) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_i1


pure recursive function sort_i2(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! using binary search tree pivot.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(size(x)) :: res
  integer(kind=i2),dimension(size(x)-1) :: rest
  integer(kind=i2) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_i2


pure recursive function sort_i4(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(size(x)) :: res
  integer(kind=i4),dimension(size(x)-1) :: rest
  integer(kind=i4) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_i4


pure recursive function sort_i8(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! using binary search tree pivot.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(size(x)) :: res
  integer(kind=i8),dimension(size(x)-1) :: rest
  integer(kind=i8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_i8


pure recursive function sort_r4(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(size(x)) :: res
  real(kind=r4),dimension(size(x)-1) :: rest
  real(kind=r4) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_r4


pure recursive function sort_r8(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(size(x)) :: res
  real(kind=r8),dimension(size(x)-1) :: rest
  real(kind=r8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_r8


pure recursive function sort_r16(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(size(x)) :: res
  real(kind=r16),dimension(size(x)-1) :: rest
  real(kind=r16) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_r16


pure recursive function sort_c4(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `sort`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(size(x)) :: res
  complex(kind=r4),dimension(size(x)-1) :: rest
  complex(kind=r4) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_c4


pure recursive function sort_c8(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `sort`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(size(x)) :: res
  complex(kind=r8),dimension(size(x)-1) :: rest
  complex(kind=r8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_c8


pure recursive function sort_c16(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `sort`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(size(x)) :: res
  complex(kind=r16),dimension(size(x)-1) :: rest
  complex(kind=r16) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_c16


pure function split_i1(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  integer(kind=i1),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_i1


pure function split_i2(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  integer(kind=i2),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_i2


pure function split_i4(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  integer(kind=i4),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_i4


pure function split_i8(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  integer(kind=i8),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_i8


pure function split_r4(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `split`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  real(kind=r4),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_r4


pure function split_r8(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `split`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  real(kind=r8),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_r8


pure function split_r16(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `split`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  real(kind=r16),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_r16


pure function split_c4(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `split`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  complex(kind=r4),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_c4


pure function split_c8(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `split`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  complex(kind=r8),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_c8


pure function split_c16(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `split`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),intent(in) :: section !! Array section to return
  complex(kind=r16),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_c16


pure function subscript_i1(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=i1),dimension(:),allocatable :: subscript
  integer(kind=i1),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_i1


pure function subscript_i2(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=i2),dimension(:),allocatable :: subscript
  integer(kind=i2),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_i2


pure function subscript_i4(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=i4),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_i4


pure function subscript_i8(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=i8),dimension(:),allocatable :: subscript
  integer(kind=i8),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_i8


pure function subscript_r4(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  real(kind=r4),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_r4


pure function subscript_r8(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  real(kind=r8),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_r8


pure function subscript_r16(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  real(kind=r16),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_r16


pure function subscript_c4(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `subscript`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  complex(kind=r4),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_c4


pure function subscript_c8(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `subscript`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  complex(kind=r8),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_c8


pure function subscript_c16(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `subscript`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(:),intent(in) :: ind !! Indices to subscript
  complex(kind=r16),dimension(:),allocatable :: subscript
  integer(kind=i4),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_c16


pure function tail_i1(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=i1),dimension(:),intent(in) :: x !! Input array
  integer(kind=i1),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_i1


pure function tail_i2(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=i2),dimension(:),intent(in) :: x !! Input array
  integer(kind=i2),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_i2


pure function tail_i4(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=i4),dimension(:),intent(in) :: x !! Input array
  integer(kind=i4),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_i4


pure function tail_i8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=i8),dimension(:),intent(in) :: x !! Input array
  integer(kind=i8),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_i8


pure function tail_r4(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(kind=r4),dimension(:),intent(in) :: x !! Input array
  real(kind=r4),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_r4


pure function tail_r8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(kind=r8),dimension(:),intent(in) :: x !! Input array
  real(kind=r8),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_r8


pure function tail_r16(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(kind=r16),dimension(:),intent(in) :: x !! Input array
  real(kind=r16),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_r16


pure function tail_c4(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `tail`.
  complex(kind=r4),dimension(:),intent(in) :: x !! Input array
  complex(kind=r4),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_c4


pure function tail_c8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `tail`.
  complex(kind=r8),dimension(:),intent(in) :: x !! Input array
  complex(kind=r8),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_c8


pure function tail_c16(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `tail`.
  complex(kind=r16),dimension(:),intent(in) :: x !! Input array
  complex(kind=r16),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_c16


pure recursive function unfold_i1(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i1) :: f !! Unfolding function
  integer(kind=i1),dimension(:),intent(in) :: x !! Start value
  integer(kind=i1),intent(in) :: len !! Array length to return
  integer(kind=i1),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_i1


pure recursive function unfold_i2(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i2) :: f !! Unfolding function
  integer(kind=i2),dimension(:),intent(in) :: x !! Start value
  integer(kind=i2),intent(in) :: len !! Array length to return
  integer(kind=i2),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_i2


pure recursive function unfold_i4(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i4) :: f !! Unfolding function
  integer(kind=i4),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  integer(kind=i4),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_i4


pure recursive function unfold_i8(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i8) :: f !! Unfolding function
  integer(kind=i8),dimension(:),intent(in) :: x !! Start value
  integer(kind=i8),intent(in) :: len !! Array length to return
  integer(kind=i8),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_i8


pure recursive function unfold_r4(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_r4) :: f !! Unfolding function
  real(kind=r4),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  real(kind=r4),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_r4


pure recursive function unfold_r8(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_r8) :: f !! Unfolding function
  real(kind=r8),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  real(kind=r8),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_r8


pure recursive function unfold_r16(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_r16) :: f !! Unfolding function
  real(kind=r16),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  real(kind=r16),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_r16


pure recursive function unfold_c4(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_c4) :: f !! Unfolding function
  complex(kind=r4),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  complex(kind=r4),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_c4


pure recursive function unfold_c8(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_c8) :: f !! Unfolding function
  complex(kind=r8),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  complex(kind=r8),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_c8


pure recursive function unfold_c16(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_c16) :: f !! Unfolding function
  complex(kind=r16),dimension(:),intent(in) :: x !! Start value
  integer(kind=i4),intent(in) :: len !! Array length to return
  complex(kind=r16),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_c16


pure function union_i1(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=i1),dimension(:),intent(in) :: x !! First input array
  integer(kind=i1),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i1),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_i1


pure function union_i2(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=i2),dimension(:),intent(in) :: x !! First input array
  integer(kind=i2),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i2),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_i2


pure function union_i4(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=i4),dimension(:),intent(in) :: x !! First input array
  integer(kind=i4),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i4),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_i4


pure function union_i8(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=i8),dimension(:),intent(in) :: x !! First input array
  integer(kind=i8),dimension(:),intent(in) :: y !! Second input array
  integer(kind=i8),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_i8


pure function union_r4(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `union`.
  real(kind=r4),dimension(:),intent(in) :: x !! First input array
  real(kind=r4),dimension(:),intent(in) :: y !! Second input array
  real(kind=r4),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_r4


pure function union_r8(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `union`.
  real(kind=r8),dimension(:),intent(in) :: x !! First input array
  real(kind=r8),dimension(:),intent(in) :: y !! Second input array
  real(kind=r8),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_r8


pure function union_r16(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `union`.
  real(kind=r16),dimension(:),intent(in) :: x !! First input array
  real(kind=r16),dimension(:),intent(in) :: y !! Second input array
  real(kind=r16),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_r16


pure function union_c4(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `union`.
  complex(kind=r4),dimension(:),intent(in) :: x !! First input array
  complex(kind=r4),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r4),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_c4


pure function union_c8(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `union`.
  complex(kind=r8),dimension(:),intent(in) :: x !! First input array
  complex(kind=r8),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r8),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_c8


pure function union_c16(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `union`.
  complex(kind=r16),dimension(:),intent(in) :: x !! First input array
  complex(kind=r16),dimension(:),intent(in) :: y !! Second input array
  complex(kind=r16),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_c16

endmodule mod_functional
