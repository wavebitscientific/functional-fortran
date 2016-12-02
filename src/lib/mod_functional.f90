! functional-fortran - Functional programming for modern Fortran
! Copyright (c) 2016, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.

module mod_functional
use iso_fortran_env,only:int8,int16,int32,int64,real32,real32,real64,real128
use mod_interfaces
implicit none

private

public :: arange,complement,filter,foldl,foldr,foldt,head,init,insert,&
          intersection,iterfold,last,limit,map,reverse,set,sort,split,&
          subscript,tail,unfold,union

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
  module procedure :: arange_int8,arange_int16,arange_int32,arange_int64
  module procedure :: arange_real32,arange_real64,arange_real128
endinterface arange

interface complement
  module procedure :: complement_int8,complement_int16,complement_int32,complement_int64
  module procedure :: complement_real32,complement_real64,complement_real128
endinterface complement

interface operator(.complement.)
  module procedure :: complement_int8,complement_int16,complement_int32,complement_int64
  module procedure :: complement_real32,complement_real64,complement_real128
endinterface

interface filter
  module procedure :: filter_int8,filter_int16,filter_int32,filter_int64
  module procedure :: filter_real32,filter_real64,filter_real128
endinterface filter

interface foldl
  module procedure :: foldl_int8,foldl_int16,foldl_int32,foldl_int64
  module procedure :: foldl_real32,foldl_real64,foldl_real128
endinterface foldl

interface foldr
  module procedure :: foldr_int8,foldr_int16,foldr_int32,foldr_int64
  module procedure :: foldr_real32,foldr_real64,foldr_real128
endinterface foldr

interface foldt
  module procedure :: foldt_int8,foldt_int16,foldt_int32,foldt_int64
  module procedure :: foldt_real32,foldt_real64,foldt_real128
endinterface foldt

interface head
  module procedure :: head_int8,head_int16,head_int32,head_int64
  module procedure :: head_real32,head_real64,head_real128
endinterface head

interface operator(.head.)
  module procedure :: head_int8,head_int16,head_int32,head_int64
  module procedure :: head_real32,head_real64,head_real128
endinterface

interface init
  module procedure :: init_int8,init_int16,init_int32,init_int64
  module procedure :: init_real32,init_real64,init_real128
endinterface init

interface operator(.init.)
  module procedure :: init_int8,init_int16,init_int32,init_int64
  module procedure :: init_real32,init_real64,init_real128
endinterface

interface insert
  module procedure :: insert_int8,insert_int16,insert_int32,insert_int64
  module procedure :: insert_real32,insert_real64,insert_real128
endinterface insert

interface intersection
  module procedure :: intersection_int8,intersection_int16,intersection_int32,intersection_int64
  module procedure :: intersection_real32,intersection_real64,intersection_real128
endinterface intersection

interface operator(.intersection.)
  module procedure :: intersection_int8,intersection_int16,intersection_int32,intersection_int64
  module procedure :: intersection_real32,intersection_real64,intersection_real128
endinterface

interface iterfold
  module procedure :: iterfold_int8,iterfold_int16,iterfold_int32,iterfold_int64
  module procedure :: iterfold_real32,iterfold_real64,iterfold_real128
endinterface iterfold

interface last
  module procedure :: last_int8,last_int16,last_int32,last_int64
  module procedure :: last_real32,last_real64,last_real128
endinterface last

interface operator(.last.)
  module procedure :: last_int8,last_int16,last_int32,last_int64
  module procedure :: last_real32,last_real64,last_real128
endinterface

interface limit
  module procedure :: limit_int8,limit_int16,limit_int32,limit_int64
  module procedure :: limit_real32,limit_real64,limit_real128
endinterface limit

interface map
  module procedure :: map_int8,map_int16,map_int32,map_int64
  module procedure :: map_real32,map_real64,map_real128
endinterface map

interface reverse
  module procedure :: reverse_int8,reverse_int16,reverse_int32,reverse_int64
  module procedure :: reverse_real32,reverse_real64,reverse_real128
endinterface reverse

interface operator(.reverse.)
  module procedure :: reverse_int8,reverse_int16,reverse_int32,reverse_int64
  module procedure :: reverse_real32,reverse_real64,reverse_real128
endinterface

interface set
  module procedure :: set_int8,set_int16,set_int32,set_int64
  module procedure :: set_real32,set_real64,set_real128
endinterface set

interface operator(.set.)
  module procedure :: set_int8,set_int16,set_int32,set_int64
  module procedure :: set_real32,set_real64,set_real128
endinterface

interface sort
  module procedure :: sort_int8,sort_int16,sort_int32,sort_int64
  module procedure :: sort_real32,sort_real64,sort_real128
endinterface sort

interface operator(.sort.)
  module procedure :: sort_int8,sort_int16,sort_int32,sort_int64
  module procedure :: sort_real32,sort_real64,sort_real128
endinterface

interface split
  module procedure :: split_int8,split_int16,split_int32,split_int64
  module procedure :: split_real32,split_real64,split_real128
endinterface split

interface subscript
  module procedure :: subscript_int8,subscript_int16,subscript_int32,subscript_int64
  module procedure :: subscript_real32,subscript_real64,subscript_real128
endinterface subscript

interface tail
  module procedure :: tail_int8,tail_int16,tail_int32,tail_int64
  module procedure :: tail_real32,tail_real64,tail_real128
endinterface tail

interface operator(.tail.)
  module procedure :: tail_int8,tail_int16,tail_int32,tail_int64
  module procedure :: tail_real32,tail_real64,tail_real128
endinterface

interface unfold
  module procedure :: unfold_int8,unfold_int16,unfold_int32,unfold_int64
  module procedure :: unfold_real32,unfold_real64,unfold_real128
endinterface unfold

interface union
  module procedure :: union_int8,union_int16,union_int32,union_int64
  module procedure :: union_real32,union_real64,union_real128
endinterface union

interface operator(.union.)
  module procedure :: union_int8,union_int16,union_int32,union_int64
  module procedure :: union_real32,union_real64,union_real128
endinterface

contains

pure function arange_int8(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 1-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=int8),intent(in) :: start !! Start value of the array
  integer(kind=int8),intent(in) :: end !! End value of the array
  integer(kind=int8),intent(in),optional :: increment !! Array increment
  integer(kind=int8),dimension(:),allocatable :: arange
  integer(kind=int8) :: incr
  integer(kind=int8) :: i
  integer(kind=int8) :: length
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
endfunction arange_int8


pure function arange_int16(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 2-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=int16),intent(in) :: start !! Start value of the array
  integer(kind=int16),intent(in) :: end !! End value of the array
  integer(kind=int16),intent(in),optional :: increment !! Array increment
  integer(kind=int16),dimension(:),allocatable :: arange
  integer(kind=int16) :: incr
  integer(kind=int16) :: i
  integer(kind=int16) :: length
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
endfunction arange_int16


pure function arange_int32(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 4-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=int32),intent(in) :: start !! Start value of the array
  integer(kind=int32),intent(in) :: end !! End value of the array
  integer(kind=int32),intent(in),optional :: increment !! Array increment
  integer(kind=int32),dimension(:),allocatable :: arange
  integer(kind=int32) :: incr
  integer(kind=int32) :: i
  integer(kind=int32) :: length
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
endfunction arange_int32


pure function arange_int64(start,end,increment) result(arange)
  !! Returns an array of integers given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 8-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(kind=int64),intent(in) :: start !! Start value of the array
  integer(kind=int64),intent(in) :: end !! End value of the array
  integer(kind=int64),intent(in),optional :: increment !! Array increment
  integer(kind=int64),dimension(:),allocatable :: arange
  integer(kind=int64) :: incr
  integer(kind=int64) :: i
  integer(kind=int64) :: length
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
endfunction arange_int64


pure function arange_real32(start,end,increment) result(arange)
  !! Returns an array of reals given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 4-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(kind=real32),intent(in) :: start !! Start value of the array
  real(kind=real32),intent(in) :: end !! End value of the array
  real(kind=real32),intent(in),optional :: increment !! Array increment
  real(kind=real32),dimension(:),allocatable :: arange
  real(kind=real32) :: incr
  integer(kind=int32) :: i
  integer(kind=int32) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = int((end-start)/incr)+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_real32


pure function arange_real64(start,end,increment) result(arange)
  !! Returns an array of reals given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 8-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(kind=real64),intent(in) :: start !! Start value of the array
  real(kind=real64),intent(in) :: end !! End value of the array
  real(kind=real64),intent(in),optional :: increment !! Array increment
  real(kind=real64),dimension(:),allocatable :: arange
  real(kind=real64) :: incr
  integer(kind=int32) :: i
  integer(kind=int32) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = int((end-start)/incr)+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_real64


pure function arange_real128(start,end,increment) result(arange)
  !! Returns an array of reals given `start`, `end`, and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 16-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(kind=real128),intent(in) :: start !! Start value of the array
  real(kind=real128),intent(in) :: end !! End value of the array
  real(kind=real128),intent(in),optional :: increment !! Array increment
  real(kind=real128),dimension(:),allocatable :: arange
  real(kind=real128) :: incr
  integer(kind=int32) :: i
  integer(kind=int32) :: length
  if(present(increment))then
    incr = increment
  else
    incr = 1
  endif
  length = int((end-start)/incr)+1
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = start+(i-1)*incr
  enddo
endfunction arange_real128


pure function complement_int8(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=int8),dimension(:),intent(in) :: x !! First input array
  integer(kind=int8),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int8),dimension(:),allocatable :: complement
  integer(kind=int8),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_int8,0_int8)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_int8


pure function complement_int16(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=int16),dimension(:),intent(in) :: x !! First input array
  integer(kind=int16),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int16),dimension(:),allocatable :: complement
  integer(kind=int16),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_int16,0_int16)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_int16


pure function complement_int32(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=int32),dimension(:),intent(in) :: x !! First input array
  integer(kind=int32),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int32),dimension(:),allocatable :: complement
  integer(kind=int32),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_int32,0_int32)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_int32


pure function complement_int64(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(kind=int64),dimension(:),intent(in) :: x !! First input array
  integer(kind=int64),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int64),dimension(:),allocatable :: complement
  integer(kind=int64),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_int64,0_int64)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_int64


pure function complement_real32(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(kind=real32),dimension(:),intent(in) :: x !! First input array
  real(kind=real32),dimension(:),intent(in) :: y !! Second input array
  real(kind=real32),dimension(:),allocatable :: complement
  real(kind=real32),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._real32,0._real32)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_real32


pure function complement_real64(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(kind=real64),dimension(:),intent(in) :: x !! First input array
  real(kind=real64),dimension(:),intent(in) :: y !! Second input array
  real(kind=real64),dimension(:),allocatable :: complement
  real(kind=real64),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._real32,0._real32)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_real64


pure function complement_real128(x,y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(kind=real128),dimension(:),intent(in) :: x !! First input array
  real(kind=real128),dimension(:),intent(in) :: y !! Second input array
  real(kind=real128),dimension(:),allocatable :: complement
  real(kind=real128),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._real128,0._real128)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement,a(n)]
  enddo
endfunction complement_real128


pure function filter_int8(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_int8_logical) :: f !! Filtering function
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_int8


pure function filter_int16(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_int16_logical) :: f !! Filtering function
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_int16


pure function filter_int32(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_int32_logical) :: f !! Filtering function
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_int32


pure function filter_int64(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_int64_logical) :: f !! Filtering function
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int64),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_int64


pure function filter_real32(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_real32_logical) :: f !! Filtering function
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  real(kind=real32),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_real32


pure function filter_real64(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_real64_logical) :: f !! Filtering function
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  real(kind=real64),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_real64


pure function filter_real128(f,x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_real128_logical) :: f !! Filtering function
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  real(kind=real128),dimension(:),allocatable :: filter
  logical,dimension(:),allocatable :: f_x
  integer :: i
  allocate(f_x(size(x)))
  do concurrent(i = 1:size(x))
    f_x(i) = f(x(i))
  enddo
  filter = pack(x,f_x)
endfunction filter_real128


pure recursive integer(kind=int8) function foldl_int8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_int8) :: f !! Folding function
  integer(kind=int8),intent(in) :: start !! Accumulator start value
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_int8


pure recursive integer(kind=int16) function foldl_int16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_int16) :: f !! Folding function
  integer(kind=int16),intent(in) :: start !! Accumulator start value
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_int16


pure recursive integer(kind=int32) function foldl_int32(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_int32) :: f !! Folding function
  integer(kind=int32),intent(in) :: start !! Accumulator start value
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_int32


pure recursive integer(kind=int64) function foldl_int64(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_int64) :: f !! Folding function
  integer(kind=int64),intent(in) :: start !! Accumulator start value
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_int64


pure recursive real(kind=real32) function foldl_real32(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_real32) :: f !! Folding function
  real(kind=real32),intent(in) :: start !! Accumulator start value
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_real32


pure recursive real(kind=real64) function foldl_real64(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_real64) :: f !! Folding function
  real(kind=real64),intent(in) :: start !! Accumulator start value
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_real64


pure recursive real(kind=real128) function foldl_real128(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty, the 
  !! result is `start`; else we recurse immediately, making the new 
  !! initial value the result of combining the old initial value 
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_real128) :: f !! Folding function
  real(kind=real128),intent(in) :: start !! Accumulator start value
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f,f(start,x(1)),x(2:))
  endif
endfunction foldl_real128


pure recursive integer(kind=int8) function foldr_int8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_int8) :: f !! Folding function
  integer(kind=int8),intent(in) :: start !! Accumulator start value
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_int8


pure recursive integer(kind=int16) function foldr_int16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_int16) :: f !! Folding function
  integer(kind=int16),intent(in) :: start !! Accumulator start value
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_int16


pure recursive integer(kind=int32) function foldr_int32(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_int32) :: f !! Folding function
  integer(kind=int32),intent(in) :: start !! Accumulator start value
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_int32


pure recursive integer(kind=int64) function foldr_int64(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_int64) :: f !! Folding function
  integer(kind=int64),intent(in) :: start !! Accumulator start value
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_int64


pure recursive real(kind=real32) function foldr_real32(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_real32) :: f !! Folding function
  real(kind=real32),intent(in) :: start !! Accumulator start value
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_real32


pure recursive real(kind=real64) function foldr_real64(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_real64) :: f !! Folding function
  real(kind=real64),intent(in) :: start !! Accumulator start value
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_real64


pure recursive real(kind=real128) function foldr_real128(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty, the 
  !! result is `start`; else apply `f` to the first element and the 
  !! result of folding the rest.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_real128) :: f !! Folding function
  real(kind=real128),intent(in) :: start !! Accumulator start value
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1),foldr(f,start,x(2:)))
  endif
endfunction foldr_real128


pure recursive integer(kind=int8) function foldt_int8(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_int8) :: f !! Folding function
  integer(kind=int8),intent(in) :: start !! Accumulator start value
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_int8


pure recursive integer(kind=int16) function foldt_int16(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_int16) :: f !! Folding function
  integer(kind=int16),intent(in) :: start !! Accumulator start value
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_int16


pure recursive integer(kind=int32) function foldt_int32(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_int32) :: f !! Folding function
  integer(kind=int32),intent(in) :: start !! Accumulator start value
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_int32


pure recursive integer(kind=int64) function foldt_int64(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_int64) :: f !! Folding function
  integer(kind=int64),intent(in) :: start !! Accumulator start value
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_int64


pure recursive real(kind=real32) function foldt_real32(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_real32) :: f !! Folding function
  real(kind=real32),intent(in) :: start !! Accumulator start value
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_real32


pure recursive real(kind=real64) function foldt_real64(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_real64) :: f !! Folding function
  real(kind=real64),intent(in) :: start !! Accumulator start value
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_real64


pure recursive real(kind=real128) function foldt_real128(f,start,x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold, splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_real128) :: f !! Folding function
  real(kind=real128),intent(in) :: start !! Accumulator start value
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start,x(1))
  else
    res = foldt(f,foldt(f,start,split(x,1)),split(x,2))
  endif
endfunction foldt_real128


pure integer(kind=int8) function head_int8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_int8


pure integer(kind=int16) function head_int16(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_int16


pure integer(kind=int32) function head_int32(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_int32


pure integer(kind=int64) function head_int64(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_int64


pure real(kind=real32) function head_real32(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `head`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_real32

pure complex(kind=real32) function head_complex32(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `head`.
  complex(kind=real32),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_complex32



pure real(kind=real64) function head_real64(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `head`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_real64


pure real(kind=real128) function head_real128(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `head`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  head = x(1)
endfunction head_real128


pure function init_int8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_int8


pure function init_int16(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_int16


pure function init_int32(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_int32


pure function init_int64(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int64),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_int64


pure function init_real32(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `init`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  real(kind=real32),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_real32


pure function init_real64(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `init`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  real(kind=real64),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_real64


pure function init_real128(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `init`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  real(kind=real128),dimension(size(x)-1) :: init
  init = x(:size(x)-1)
endfunction init_real128


pure function insert_int8(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=int8),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_int8


pure function insert_int16(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=int16),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_int16


pure function insert_int32(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=int32),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_int32


pure function insert_int64(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(kind=int64),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int64),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_int64


pure function insert_real32(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(kind=real32),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  real(kind=real32),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_real32


pure function insert_real64(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(kind=real64),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  real(kind=real64),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_real64


pure function insert_real128(elem,ind,x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(kind=real128),intent(in) :: elem !! Element to insert
  integer(kind=int32),intent(in) :: ind !! Index to insert element at
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  real(kind=real128),dimension(size(x)+1) :: insert
  insert = [x(:limit(ind,1,size(x)+1)-1),elem,x(limit(ind,1,size(x)+1):)]
endfunction insert_real128


pure function intersection_int8(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=int8),dimension(:),intent(in) :: x !! First input array
  integer(kind=int8),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int8),dimension(:),allocatable :: res
  integer(kind=int8),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1_int8,0_int8)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_int8


pure function intersection_int16(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=int16),dimension(:),intent(in) :: x !! First input array
  integer(kind=int16),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int16),dimension(:),allocatable :: res
  integer(kind=int16),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1_int16,0_int16)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_int16


pure function intersection_int32(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=int32),dimension(:),intent(in) :: x !! First input array
  integer(kind=int32),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int32),dimension(:),allocatable :: res
  integer(kind=int32),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1_int32,0_int32)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_int32


pure function intersection_int64(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(kind=int64),dimension(:),intent(in) :: x !! First input array
  integer(kind=int64),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int64),dimension(:),allocatable :: res
  integer(kind=int64),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1_int64,0_int64)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_int64


pure function intersection_real32(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(kind=real32),dimension(:),intent(in) :: x !! First input array
  real(kind=real32),dimension(:),intent(in) :: y !! Second input array
  real(kind=real32),dimension(:),allocatable :: res
  real(kind=real32),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1._real32,0._real32)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_real32


pure function intersection_real64(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(kind=real64),dimension(:),intent(in) :: x !! First input array
  real(kind=real64),dimension(:),intent(in) :: y !! Second input array
  real(kind=real64),dimension(:),allocatable :: res
  real(kind=real64),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1._real64,0._real64)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_real64


pure function intersection_real128(x,y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(kind=real128),dimension(:),intent(in) :: x !! First input array
  real(kind=real128),dimension(:),intent(in) :: y !! Second input array
  real(kind=real128),dimension(:),allocatable :: res
  real(kind=real128),dimension(:),allocatable :: a,b
  integer(kind=int32) :: n
  a = set(x)
  b = set(y)
  res = arange(1._real128,0._real128)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res,b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res,a(n)]
    enddo
  endif
endfunction intersection_real128


pure integer(kind=int8) function last_int8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_int8


pure integer(kind=int16) function last_int16(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_int16


pure integer(kind=int32) function last_int32(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_int32


pure integer(kind=int64) function last_int64(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_int64


pure real(kind=real32) function last_real32(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `last`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_real32


pure real(kind=real64) function last_real64(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `last`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_real64


pure real(kind=real128) function last_real128(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `last`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  last = x(size(x))
endfunction last_real128


pure elemental integer(kind=int8) function limit_int8(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=int8),intent(in) :: x !! Input scalar
  integer(kind=int8),intent(in) :: a !! First limit
  integer(kind=int8),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_int8


pure elemental integer(kind=int16) function limit_int16(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=int16),intent(in) :: x !! Input scalar
  integer(kind=int16),intent(in) :: a !! First limit
  integer(kind=int16),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_int16


pure elemental integer(kind=int32) function limit_int32(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=int32),intent(in) :: x !! Input scalar
  integer(kind=int32),intent(in) :: a !! First limit
  integer(kind=int32),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_int32


pure elemental integer(kind=int64) function limit_int64(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(kind=int64),intent(in) :: x !! Input scalar
  integer(kind=int64),intent(in) :: a !! First limit
  integer(kind=int64),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_int64


pure elemental real(kind=real32) function limit_real32(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(kind=real32),intent(in) :: x !! Input scalar
  real(kind=real32),intent(in) :: a !! First limit
  real(kind=real32),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_real32


pure elemental real(kind=real64) function limit_real64(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(kind=real64),intent(in) :: x !! Input scalar
  real(kind=real64),intent(in) :: a !! First limit
  real(kind=real64),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_real64


pure elemental real(kind=real128) function limit_real128(x,a,b) result(limit)
  !! Returns `x` if `min(a,b) <= x .and. x <= max(a,b)`, 
  !! `min(a,b) if `x < min(a,b)` and `max(a,b) if `x < max(a,b)`
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(kind=real128),intent(in) :: x !! Input scalar
  real(kind=real128),intent(in) :: a !! First limit
  real(kind=real128),intent(in) :: b !! Second limit
  limit = min(max(x,min(a,b)),max(a,b)) 
endfunction limit_real128


pure function map_int8(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_int8) :: f !! Mapping function
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_int8


pure function map_int16(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_int16) :: f !! Mapping function
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_int16


pure function map_int32(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_int32) :: f !! Mapping function
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_int32


pure function map_int64(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_int64) :: f !! Mapping function
  integer(kind=int64),dimension(:),intent(in) :: x
  integer(kind=int64),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_int64


pure function map_real32(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_real32) :: f !! Mapping function
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  real(kind=real32),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_real32


pure function map_real64(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_real64) :: f !! Mapping function
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  real(kind=real64),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_real64


pure function map_real128(f,x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_real128) :: f !! Mapping function
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  real(kind=real128),dimension(size(x)) :: map
  integer(kind=int32) :: i
  do concurrent(i = 1:size(x))
    map(i) = f(x(i))
  enddo
endfunction map_real128


pure integer(kind=int8) function iterfold_int8(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_int8) :: f
  integer(kind=int8),intent(in) :: start
  integer(kind=int8),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_int8


pure integer(kind=int16) function iterfold_int16(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_int16) :: f
  integer(kind=int16),intent(in) :: start
  integer(kind=int16),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_int16


pure integer(kind=int32) function iterfold_int32(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_int32) :: f
  integer(kind=int32),intent(in) :: start
  integer(kind=int32),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_int32


pure integer(kind=int64) function iterfold_int64(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_int64) :: f
  integer(kind=int64),intent(in) :: start
  integer(kind=int64),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_int64


pure real(kind=real32) function iterfold_real32(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_real32) :: f
  real(kind=real32),intent(in) :: start
  real(kind=real32),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_real32


pure real(kind=real64) function iterfold_real64(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_real64) :: f
  real(kind=real64),intent(in) :: start
  real(kind=real64),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_real64


pure real(kind=real128) function iterfold_real128(f,start,x) result(iterfold)
  !! Reduces input array `x` using input function `f(x,y)`.
  !! Initial value is `start`, if given, and zero otherwise.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_real128) :: f
  real(kind=real128),intent(in) :: start
  real(kind=real128),dimension(:),intent(in) :: x
  integer :: i
  iterfold = start
  do i = 1,size(x)
    iterfold = f(iterfold,x(i))
  enddo
endfunction iterfold_real128


pure function reverse_int8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_int8
 

pure function reverse_int16(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_int16


pure function reverse_int32(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_int32


pure function reverse_int64(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int64),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_int64


pure function reverse_real32(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  real(kind=real32),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_real32


pure function reverse_real64(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  real(kind=real64),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_real64


pure function reverse_real128(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  real(kind=real128),dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
endfunction reverse_real128

 
pure recursive function sort_int8(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=int8),dimension(:),intent(in) :: x
  integer(kind=int8),dimension(size(x)) :: res
  integer(kind=int8),dimension(size(x)-1) :: rest
  integer(kind=int8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_int8


pure recursive function sort_int16(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! using binary search tree pivot.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=int16),dimension(:),intent(in) :: x
  integer(kind=int16),dimension(size(x)) :: res
  integer(kind=int16),dimension(size(x)-1) :: rest
  integer(kind=int16) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_int16


pure recursive function sort_int32(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=int32),dimension(:),intent(in) :: x
  integer(kind=int32),dimension(size(x)) :: res
  integer(kind=int32),dimension(size(x)-1) :: rest
  integer(kind=int32) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_int32


pure recursive function sort_int64(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! using binary search tree pivot.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(kind=int64),dimension(:),intent(in) :: x
  integer(kind=int64),dimension(size(x)) :: res
  integer(kind=int64),dimension(size(x)-1) :: rest
  integer(kind=int64) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_int64


pure recursive function sort_real32(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(kind=real32),dimension(:),intent(in) :: x
  real(kind=real32),dimension(size(x)) :: res
  real(kind=real32),dimension(size(x)-1) :: rest
  real(kind=real32) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_real32


pure recursive function sort_real64(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(kind=real64),dimension(:),intent(in) :: x
  real(kind=real64),dimension(size(x)) :: res
  real(kind=real64),dimension(size(x)-1) :: rest
  real(kind=real64) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_real64


pure recursive function sort_real128(x) result(res)
  !! Recursive quicksort using binary tree pivot. 
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(kind=real128),dimension(:),intent(in) :: x
  real(kind=real128),dimension(size(x)) :: res
  real(kind=real128),dimension(size(x)-1) :: rest
  real(kind=real128) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x,2))
    rest = [split(x,1),tail(split(x,2))]
    res = [sort(pack(rest,rest < pivot)),pivot,&
           sort(pack(rest,rest >= pivot))]
  else
    res = x
  endif
endfunction sort_real128


pure recursive function set_int8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=int8),dimension(:),intent(in) :: x
  integer(kind=int8),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_int8


pure recursive function set_int16(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=int16),dimension(:),intent(in) :: x
  integer(kind=int16),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_int16

pure recursive function set_int32(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=int32),dimension(:),intent(in) :: x
  integer(kind=int32),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_int32


pure recursive function set_int64(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(kind=int64),dimension(:),intent(in) :: x
  integer(kind=int64),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_int64


pure recursive function set_real32(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `set`.
  real(kind=real32),dimension(:),intent(in) :: x
  real(kind=real32),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_real32


pure recursive function set_real64(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `set`.
  real(kind=real64),dimension(:),intent(in) :: x
  real(kind=real64),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_real64


pure recursive function set_real128(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `set`.
  real(kind=real128),dimension(:),intent(in) :: x
  real(kind=real128),dimension(:),allocatable :: res
  if(size(x) > 1)then
    res = [x(1),set(pack(x(2:),.not. x(2:) == x(1)))]
  else
    res = x
  endif
endfunction set_real128


pure function split_int8(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  integer(kind=int8),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_int8


pure function split_int16(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  integer(kind=int16),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_int16


pure function split_int32(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  integer(kind=int32),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_int32


pure function split_int64(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  integer(kind=int64),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_int64


pure function split_real32(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `split`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  real(kind=real32),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_real32


pure function split_real64(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `split`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  real(kind=real64),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_real64


pure function split_real128(x,section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`, `split(x,1)` 
  !! returns and empty array, and `split(x,2)` returns `x(1)`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `split`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),intent(in) :: section !! Array section to return
  real(kind=real128),dimension(:),allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
endfunction split_real128


pure function subscript_int8(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int8),dimension(:),allocatable :: subscript
  integer(kind=int8),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_int8


pure function subscript_int16(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int16),dimension(:),allocatable :: subscript
  integer(kind=int16),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_int16


pure function subscript_int32(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int32),dimension(:),allocatable :: subscript
  integer(kind=int32),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_int32


pure function subscript_int64(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int64),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int64),dimension(:),allocatable :: subscript
  integer(kind=int64),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_int64


pure function subscript_real32(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int32),dimension(:),allocatable :: subscript
  integer(kind=int32),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_real32


pure function subscript_real64(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int32),dimension(:),allocatable :: subscript
  integer(kind=int32),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_real64


pure function subscript_real128(x,ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(:),intent(in) :: ind !! Indices to subscript
  integer(kind=int32),dimension(:),allocatable :: subscript
  integer(kind=int32),dimension(:),allocatable :: indices
  integer :: i
  indices = pack(ind,ind > 0 .and. ind < size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
endfunction subscript_real128


pure function tail_int8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=int8),dimension(:),intent(in) :: x !! Input array
  integer(kind=int8),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_int8


pure function tail_int16(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=int16),dimension(:),intent(in) :: x !! Input array
  integer(kind=int16),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_int16


pure function tail_int32(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=int32),dimension(:),intent(in) :: x !! Input array
  integer(kind=int32),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_int32


pure function tail_int64(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(kind=int64),dimension(:),intent(in) :: x !! Input array
  integer(kind=int64),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_int64


pure function tail_real32(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(kind=real32),dimension(:),intent(in) :: x !! Input array
  real(kind=real32),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_real32


pure function tail_real64(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(kind=real64),dimension(:),intent(in) :: x !! Input array
  real(kind=real64),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_real64


pure function tail_real128(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(kind=real128),dimension(:),intent(in) :: x !! Input array
  real(kind=real128),dimension(size(x)-1) :: tail
  tail = x(2:)
endfunction tail_real128


pure recursive function unfold_int8(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_int8) :: f !! Unfolding function
  integer(kind=int8),dimension(:),intent(in) :: x !! Start value
  integer(kind=int8),intent(in) :: len !! Array length to return
  integer(kind=int8),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_int8


pure recursive function unfold_int16(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_int16) :: f !! Unfolding function
  integer(kind=int16),dimension(:),intent(in) :: x !! Start value
  integer(kind=int16),intent(in) :: len !! Array length to return
  integer(kind=int16),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_int16


pure recursive function unfold_int32(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_int32) :: f !! Unfolding function
  integer(kind=int32),dimension(:),intent(in) :: x !! Start value
  integer(kind=int32),intent(in) :: len !! Array length to return
  integer(kind=int32),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_int32


pure recursive function unfold_int64(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_int64) :: f !! Unfolding function
  integer(kind=int64),dimension(:),intent(in) :: x !! Start value
  integer(kind=int64),intent(in) :: len !! Array length to return
  integer(kind=int64),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_int64


pure recursive function unfold_real32(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_real32) :: f !! Unfolding function
  real(kind=real32),dimension(:),intent(in) :: x !! Start value
  integer(kind=int32),intent(in) :: len !! Array length to return
  real(kind=real32),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_real32


pure recursive function unfold_real64(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_real64) :: f !! Unfolding function
  real(kind=real64),dimension(:),intent(in) :: x !! Start value
  integer(kind=int32),intent(in) :: len !! Array length to return
  real(kind=real64),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_real64


pure recursive function unfold_real128(f,x,len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_real128) :: f !! Unfolding function
  real(kind=real128),dimension(:),intent(in) :: x !! Start value
  integer(kind=int32),intent(in) :: len !! Array length to return
  real(kind=real128),dimension(:),allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f,[x,f(last(x))],len)
  endif
endfunction unfold_real128


pure function union_int8(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=int8),dimension(:),intent(in) :: x !! First input array
  integer(kind=int8),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int8),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_int8


pure function union_int16(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=int16),dimension(:),intent(in) :: x !! First input array
  integer(kind=int16),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int16),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_int16


pure function union_int32(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=int32),dimension(:),intent(in) :: x !! First input array
  integer(kind=int32),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int32),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_int32


pure function union_int64(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(kind=int64),dimension(:),intent(in) :: x !! First input array
  integer(kind=int64),dimension(:),intent(in) :: y !! Second input array
  integer(kind=int64),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_int64


pure function union_real32(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `union`.
  real(kind=real32),dimension(:),intent(in) :: x !! First input array
  real(kind=real32),dimension(:),intent(in) :: y !! Second input array
  real(kind=real32),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_real32


pure function union_real64(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `union`.
  real(kind=real64),dimension(:),intent(in) :: x !! First input array
  real(kind=real64),dimension(:),intent(in) :: y !! Second input array
  real(kind=real64),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_real64


pure function union_real128(x,y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `union`.
  real(kind=real128),dimension(:),intent(in) :: x !! First input array
  real(kind=real128),dimension(:),intent(in) :: y !! Second input array
  real(kind=real128),dimension(:),allocatable :: union
  union = set([x,y])
endfunction union_real128

endmodule mod_functional
