! functional-fortran - Functional programming for modern Fortran
! Copyright (c) 2016, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.

module mod_interfaces
use iso_fortran_env,only:int8,int16,int32,int64,real32,real32,real64,real128
implicit none

private

public :: f_int8,f_int16,f_int32,f_int64
public :: f_real32,f_real64,f_real128
public :: f_array_int8,f_array_int16,f_array_int32,f_array_int64
public :: f_array_real32,f_array_real64,f_array_real128
public :: f2_int8,f2_int16,f2_int32,f2_int64
public :: f2_real32,f2_real64,f2_real128
public :: f_int8_logical,f_int16_logical,f_int32_logical,f_int64_logical
public :: f_real32_logical,f_real64_logical,f_real128_logical

interface

  pure integer(kind=int8) function f_int8(x)
    !! f :: int8 -> int8
    import :: int8
    integer(kind=int8),intent(in) :: x
  endfunction f_int8

  pure integer(kind=int16) function f_int16(x)
    !! f :: int16 -> int16
    import :: int16
    integer(kind=int16),intent(in) :: x
  endfunction f_int16

  pure integer(kind=int32) function f_int32(x)
    !! f :: int32 -> int32
    import :: int32
    integer(kind=int32),intent(in) :: x
  endfunction f_int32

  pure integer(kind=int64) function f_int64(x)
    !! f :: int64 -> int64
    import :: int64
    integer(kind=int64),intent(in) :: x
  endfunction f_int64

  pure real(kind=real32) function f_real32(x)
    !! f :: real32 -> real32
    import :: real32
    real(kind=real32),intent(in) :: x
  endfunction f_real32

  pure real(kind=real64) function f_real64(x)
    !! f :: real64 -> real64
    import :: real64
    real(kind=real64),intent(in) :: x
  endfunction f_real64

  pure real(kind=real128) function f_real128(x)
    !! f :: real128 -> real128
    import :: real128
    real(kind=real128),intent(in) :: x
  endfunction f_real128

  pure function f_array_int8(x) result(f)
    !! f :: [int8] -> [int8]
    import :: int8
    integer(kind=int8),dimension(:),intent(in) :: x
    integer(kind=int8),dimension(:),allocatable :: f
  endfunction f_array_int8

  pure function f_array_int16(x) result(f)
    !! f :: [int16] -> [int16]
    import :: int16
    integer(kind=int16),dimension(:),intent(in) :: x
    integer(kind=int16),dimension(:),allocatable :: f
  endfunction f_array_int16

  pure function f_array_int32(x) result(f)
    !! f :: [int32] -> [int32]
    import :: int32
    integer(kind=int32),dimension(:),intent(in) :: x
    integer(kind=int32),dimension(:),allocatable :: f
  endfunction f_array_int32

  pure function f_array_int64(x) result(f)
    !! f :: [int64] -> [int64]
    import :: int64
    integer(kind=int64),dimension(:),intent(in) :: x
    integer(kind=int64),dimension(:),allocatable :: f
  endfunction f_array_int64

  pure function f_array_real32(x) result(f)
    !! f :: [real32] -> [real32]
    import :: real32
    real(kind=real32),dimension(:),intent(in) :: x
    real(kind=real32),dimension(:),allocatable :: f
  endfunction f_array_real32

  pure function f_array_real64(x) result(f)
    !! f :: [real64] -> [real64]
    import :: real64
    real(kind=real64),dimension(:),intent(in) :: x
    real(kind=real64),dimension(:),allocatable :: f
  endfunction f_array_real64

  pure function f_array_real128(x) result(f)
    !! f :: [real128] -> [real128]
    import :: real128
    real(kind=real128),dimension(:),intent(in) :: x
    real(kind=real128),dimension(:),allocatable :: f
  endfunction f_array_real128

  pure integer(kind=int8) function f2_int8(x,y)
    !! f :: int8 int8 -> int8
    import :: int8
    integer(kind=int8),intent(in) :: x,y
  endfunction f2_int8

  pure integer(kind=int16) function f2_int16(x,y)
    !! f :: int16 int16 -> int16
    import :: int16
    integer(kind=int16),intent(in) :: x,y
  endfunction f2_int16

  pure integer(kind=int32) function f2_int32(x,y)
    !! f :: int32 int32 -> int32
    import :: int32
    integer(kind=int32),intent(in) :: x,y
  endfunction f2_int32

  pure integer(kind=int64) function f2_int64(x,y)
    !! f :: int64 int64 -> int64
    import :: int64
    integer(kind=int64),intent(in) :: x,y
  endfunction f2_int64

  pure real(kind=real32) function f2_real32(x,y)
    !! f :: real32 real32 -> real32
    import :: real32
    real(kind=real32),intent(in) :: x,y
  endfunction f2_real32

  pure real(kind=real64) function f2_real64(x,y)
    !! f :: real64 real64 -> real64
    import :: real64
    real(kind=real64),intent(in) :: x,y
  endfunction f2_real64

  pure real(kind=real128) function f2_real128(x,y)
    !! f :: real128 real128 -> real128
    import :: real128
    real(kind=real128),intent(in) :: x,y
  endfunction f2_real128

  pure logical function f_int8_logical(x)
    !! f :: int8 -> logical
    import :: int8
    integer(kind=int8),intent(in) :: x
  endfunction f_int8_logical

  pure logical function f_int16_logical(x)
    !! f :: int16 -> logical
    import :: int16
    integer(kind=int16),intent(in) :: x
  endfunction f_int16_logical

  pure logical function f_int32_logical(x)
    !! f :: int32 -> logical
    import :: int32
    integer(kind=int32),intent(in) :: x
  endfunction f_int32_logical

  pure logical function f_int64_logical(x)
    !! f :: int64 -> logical
    import :: int64
    integer(kind=int64),intent(in) :: x
  endfunction f_int64_logical

  pure logical function f_real32_logical(x)
    !! f :: real32 -> logical
    import :: real32
    real(kind=real32),intent(in) :: x
  endfunction f_real32_logical

  pure logical function f_real64_logical(x)
    !! f :: real64 -> logical
    import :: real64
    real(kind=real64),intent(in) :: x
  endfunction f_real64_logical

  pure logical function f_real128_logical(x)
    !! f :: real128 -> logical
    import :: real128
    real(kind=real128),intent(in) :: x
  endfunction f_real128_logical

endinterface

endmodule mod_interfaces
