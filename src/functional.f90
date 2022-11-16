module functional

use iso_fortran_env, only:i1 => int8, i2 => int16, i4 => int32, i8 => int64, &
                          r4 => real32, r8 => real64, r16 => real128

implicit none

private

public :: arange, arrstr, complement, empty, filter, foldl, foldr, foldt, head,&
          init, insert, intersection, iterfold, last, limit, map, reverse, set,&
          sort, split, strarr, subscript, tail, unfold, union

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
  module procedure :: arange_i1, arange_i2, arange_i4, arange_i8
  module procedure :: arange_r4, arange_r8, arange_r16
  module procedure :: arange_c4, arange_c8, arange_c16
end interface arange

interface complement
  module procedure :: complement_i1, complement_i2, complement_i4, complement_i8
  module procedure :: complement_r4, complement_r8, complement_r16
  module procedure :: complement_c4, complement_c8, complement_c16
  module procedure :: complement_char
end interface complement

interface operator(.complement.)
  module procedure :: complement_i1, complement_i2, complement_i4, complement_i8
  module procedure :: complement_r4, complement_r8, complement_r16
  module procedure :: complement_c4, complement_c8, complement_c16
  module procedure :: complement_char
end interface

interface empty
  module procedure :: empty_i1, empty_i2, empty_i4, empty_i8
  module procedure :: empty_r4, empty_r8, empty_r16
  module procedure :: empty_c4, empty_c8, empty_c16
  module procedure :: empty_char
end interface empty

interface filter
  module procedure :: filter_i1, filter_i2, filter_i4, filter_i8
  module procedure :: filter_r4, filter_r8, filter_r16
  module procedure :: filter_c4, filter_c8, filter_c16
end interface filter

interface foldl
  module procedure :: foldl_i1, foldl_i2, foldl_i4, foldl_i8
  module procedure :: foldl_r4, foldl_r8, foldl_r16
  module procedure :: foldl_c4, foldl_c8, foldl_c16
end interface foldl

interface foldr
  module procedure :: foldr_i1, foldr_i2, foldr_i4, foldr_i8
  module procedure :: foldr_r4, foldr_r8, foldr_r16
  module procedure :: foldr_c4, foldr_c8, foldr_c16
end interface foldr

interface foldt
  module procedure :: foldt_i1, foldt_i2, foldt_i4, foldt_i8
  module procedure :: foldt_r4, foldt_r8, foldt_r16
  module procedure :: foldt_c4, foldt_c8, foldt_c16
end interface foldt

interface head
  module procedure :: head_i1, head_i2, head_i4, head_i8
  module procedure :: head_r4, head_r8, head_r16
  module procedure :: head_c4, head_c8, head_c16
  module procedure :: head_char
end interface head

interface operator(.head.)
  module procedure :: head_i1, head_i2, head_i4, head_i8
  module procedure :: head_r4, head_r8, head_r16
  module procedure :: head_c4, head_c8, head_c16
  module procedure :: head_char
end interface

interface init
  module procedure :: init_i1, init_i2, init_i4, init_i8
  module procedure :: init_r4, init_r8, init_r16
  module procedure :: init_c4, init_c8, init_c16
  module procedure :: init_char
end interface init

interface operator(.init.)
  module procedure :: init_i1, init_i2, init_i4, init_i8
  module procedure :: init_r4, init_r8, init_r16
  module procedure :: init_c4, init_c8, init_c16
  module procedure :: init_char
end interface

interface insert
  module procedure :: insert_i1, insert_i2, insert_i4, insert_i8
  module procedure :: insert_r4, insert_r8, insert_r16
  module procedure :: insert_c4, insert_c8, insert_c16
  module procedure :: insert_char
end interface insert

interface intersection
  module procedure :: intersection_i1, intersection_i2, intersection_i4, intersection_i8
  module procedure :: intersection_r4, intersection_r8, intersection_r16
  module procedure :: intersection_c4, intersection_c8, intersection_c16
  module procedure :: intersection_char
end interface intersection

interface operator(.intersection.)
  module procedure :: intersection_i1, intersection_i2, intersection_i4, intersection_i8
  module procedure :: intersection_r4, intersection_r8, intersection_r16
  module procedure :: intersection_c4, intersection_c8, intersection_c16
  module procedure :: intersection_char
end interface

interface iterfold
  module procedure :: iterfold_i1, iterfold_i2, iterfold_i4, iterfold_i8
  module procedure :: iterfold_r4, iterfold_r8, iterfold_r16
  module procedure :: iterfold_c4, iterfold_c8, iterfold_c16
end interface iterfold

interface last
  module procedure :: last_i1, last_i2, last_i4, last_i8
  module procedure :: last_r4, last_r8, last_r16
  module procedure :: last_c4, last_c8, last_c16
  module procedure :: last_char
end interface last

interface operator(.last.)
  module procedure :: last_i1, last_i2, last_i4, last_i8
  module procedure :: last_r4, last_r8, last_r16
  module procedure :: last_c4, last_c8, last_c16
  module procedure :: last_char
end interface

interface limit
  module procedure :: limit_i1, limit_i2, limit_i4, limit_i8
  module procedure :: limit_r4, limit_r8, limit_r16
  module procedure :: limit_c4, limit_c8, limit_c16
end interface limit

interface map
  module procedure :: map_i1, map_i2, map_i4, map_i8
  module procedure :: map_r4, map_r8, map_r16
  module procedure :: map_c4, map_c8, map_c16
end interface map

interface reverse
  module procedure :: reverse_i1, reverse_i2, reverse_i4, reverse_i8
  module procedure :: reverse_r4, reverse_r8, reverse_r16
  module procedure :: reverse_c4, reverse_c8, reverse_c16
  module procedure :: reverse_char
end interface reverse

interface operator(.reverse.)
  module procedure :: reverse_i1, reverse_i2, reverse_i4, reverse_i8
  module procedure :: reverse_r4, reverse_r8, reverse_r16
  module procedure :: reverse_c4, reverse_c8, reverse_c16
  module procedure :: reverse_char
end interface

interface set
  module procedure :: set_i1, set_i2, set_i4, set_i8
  module procedure :: set_r4, set_r8, set_r16
  module procedure :: set_c4, set_c8, set_c16
  module procedure :: set_char
end interface set

interface operator(.set.)
  module procedure :: set_i1, set_i2, set_i4, set_i8
  module procedure :: set_r4, set_r8, set_r16
  module procedure :: set_c4, set_c8, set_c16
  module procedure :: set_char
end interface

interface sort
  module procedure :: sort_i1, sort_i2, sort_i4, sort_i8
  module procedure :: sort_r4, sort_r8, sort_r16
  module procedure :: sort_c4, sort_c8, sort_c16
  module procedure :: sort_char
end interface sort

interface operator(.sort.)
  module procedure :: sort_i1, sort_i2, sort_i4, sort_i8
  module procedure :: sort_r4, sort_r8, sort_r16
  module procedure :: sort_c4, sort_c8, sort_c16
  module procedure :: sort_char
end interface

interface split
  module procedure :: split_i1, split_i2, split_i4, split_i8
  module procedure :: split_r4, split_r8, split_r16
  module procedure :: split_c4, split_c8, split_c16
  module procedure :: split_char
end interface split

interface subscript
  module procedure :: subscript_i1, subscript_i2, subscript_i4, subscript_i8
  module procedure :: subscript_r4, subscript_r8, subscript_r16
  module procedure :: subscript_c4, subscript_c8, subscript_c16
end interface subscript

interface tail
  module procedure :: tail_i1, tail_i2, tail_i4, tail_i8
  module procedure :: tail_r4, tail_r8, tail_r16
  module procedure :: tail_c4, tail_c8, tail_c16
  module procedure :: tail_char
end interface tail

interface operator(.tail.)
  module procedure :: tail_i1, tail_i2, tail_i4, tail_i8
  module procedure :: tail_r4, tail_r8, tail_r16
  module procedure :: tail_c4, tail_c8, tail_c16
  module procedure :: tail_char
end interface

interface unfold
  module procedure :: unfold_i1, unfold_i2, unfold_i4, unfold_i8
  module procedure :: unfold_r4, unfold_r8, unfold_r16
  module procedure :: unfold_c4, unfold_c8, unfold_c16
end interface unfold

interface union
  module procedure :: union_i1, union_i2, union_i4, union_i8
  module procedure :: union_r4, union_r8, union_r16
  module procedure :: union_c4, union_c8, union_c16
  module procedure :: union_char
end interface union

interface operator(.union.)
  module procedure :: union_i1, union_i2, union_i4, union_i8
  module procedure :: union_r4, union_r8, union_r16
  module procedure :: union_c4, union_c8, union_c16
  module procedure :: union_char
end interface

interface operator(<)
  module procedure :: lt_c4, lt_c8, lt_c16
end interface operator(<)

interface operator(>=)
  module procedure :: ge_c4, ge_c8, ge_c16
end interface operator(>=)


interface

  pure integer(i1) function f_i1(x)
    !! f :: i1 -> i1
    import :: i1
    integer(i1), intent(in) :: x
  end function f_i1

  pure integer(i2) function f_i2(x)
    !! f :: i2 -> i2
    import :: i2
    integer(i2), intent(in) :: x
  end function f_i2

  pure integer(i4) function f_i4(x)
    !! f :: i4 -> i4
    import :: i4
    integer(i4), intent(in) :: x
  end function f_i4

  pure integer(i8) function f_i8(x)
    !! f :: i8 -> i8
    import :: i8
    integer(i8), intent(in) :: x
  end function f_i8

  pure real(r4) function f_r4(x)
    !! f :: r4 -> r4
    import :: r4
    real(r4), intent(in) :: x
  end function f_r4

  pure real(r8) function f_r8(x)
    !! f :: r8 -> r8
    import :: r8
    real(r8), intent(in) :: x
  end function f_r8

  pure real(r16) function f_r16(x)
    !! f :: r16 -> r16
    import :: r16
    real(r16), intent(in) :: x
  end function f_r16

  pure complex(r4) function f_c4(x)
    !! f :: c4 -> c4
    import :: r4
    complex(r4), intent(in) :: x
  end function f_c4

  pure complex(r8) function f_c8(x)
    !! f :: c8 -> c8
    import :: r8
    complex(r8), intent(in) :: x
  end function f_c8

  pure complex(r16) function f_c16(x)
    !! f :: c16 -> c16
    import :: r16
    complex(r16), intent(in) :: x
  end function f_c16

  pure function f_array_i1(x) result(f)
    !! f :: [i1] -> [i1]
    import :: i1
    integer(i1), dimension(:), intent(in) :: x
    integer(i1), dimension(:), allocatable :: f
  end function f_array_i1

  pure function f_array_i2(x) result(f)
    !! f :: [i2] -> [i2]
    import :: i2
    integer(i2), dimension(:), intent(in) :: x
    integer(i2), dimension(:), allocatable :: f
  end function f_array_i2

  pure function f_array_i4(x) result(f)
    !! f :: [i4] -> [i4]
    import :: i4
    integer(i4), dimension(:), intent(in) :: x
    integer(i4), dimension(:), allocatable :: f
  end function f_array_i4

  pure function f_array_i8(x) result(f)
    !! f :: [i8] -> [i8]
    import :: i8
    integer(i8), dimension(:), intent(in) :: x
    integer(i8), dimension(:), allocatable :: f
  end function f_array_i8

  pure function f_array_r4(x) result(f)
    !! f :: [r4] -> [r4]
    import :: r4
    real(r4), dimension(:), intent(in) :: x
    real(r4), dimension(:), allocatable :: f
  end function f_array_r4

  pure function f_array_r8(x) result(f)
    !! f :: [r8] -> [r8]
    import :: r8
    real(r8), dimension(:), intent(in) :: x
    real(r8), dimension(:), allocatable :: f
  end function f_array_r8

  pure function f_array_r16(x) result(f)
    !! f :: [r16] -> [r16]
    import :: r16
    real(r16), dimension(:), intent(in) :: x
    real(r16), dimension(:), allocatable :: f
  end function f_array_r16

  pure function f_array_c4(x) result(f)
    !! f :: [c4] -> [c4]
    import :: r4
    complex(r4), dimension(:), intent(in) :: x
    complex(r4), dimension(:), allocatable :: f
  end function f_array_c4

  pure function f_array_c8(x) result(f)
    !! f :: [c8] -> [c8]
    import :: r8
    complex(r8), dimension(:), intent(in) :: x
    complex(r8), dimension(:), allocatable :: f
  end function f_array_c8

  pure function f_array_c16(x) result(f)
    !! f :: [c16] -> [c16]
    import :: r16
    complex(r16), dimension(:), intent(in) :: x
    complex(r16), dimension(:), allocatable :: f
  end function f_array_c16

  pure integer(i1) function f2_i1(x, y)
    !! f :: i1 i1 -> i1
    import :: i1
    integer(i1), intent(in) :: x, y
  end function f2_i1

  pure integer(i2) function f2_i2(x, y)
    !! f :: i2 i2 -> i2
    import :: i2
    integer(i2), intent(in) :: x, y
  end function f2_i2

  pure integer(i4) function f2_i4(x, y)
    !! f :: i4 i4 -> i4
    import :: i4
    integer(i4), intent(in) :: x, y
  end function f2_i4

  pure integer(i8) function f2_i8(x, y)
    !! f :: i8 i8 -> i8
    import :: i8
    integer(i8), intent(in) :: x, y
  end function f2_i8

  pure real(r4) function f2_r4(x, y)
    !! f :: r4 r4 -> r4
    import :: r4
    real(r4), intent(in) :: x, y
  end function f2_r4

  pure real(r8) function f2_r8(x, y)
    !! f :: r8 r8 -> r8
    import :: r8
    real(r8), intent(in) :: x, y
  end function f2_r8

  pure real(r16) function f2_r16(x, y)
    !! f :: r16 r16 -> r16
    import :: r16
    real(r16), intent(in) :: x, y
  end function f2_r16

  pure complex(r4) function f2_c4(x, y)
    !! f :: c4 c4 -> c4
    import :: r4
    complex(r4), intent(in) :: x, y
  end function f2_c4

  pure complex(r8) function f2_c8(x, y)
    !! f :: c8 c8 -> c8
    import :: r8
    complex(r8), intent(in) :: x, y
  end function f2_c8

  pure complex(r16) function f2_c16(x, y)
    !! f :: c16 c16 -> c16
    import :: r16
    complex(r16), intent(in) :: x, y
  end function f2_c16

  pure logical function f_i1_logical(x)
    !! f :: i1 -> logical
    import :: i1
    integer(i1), intent(in) :: x
  end function f_i1_logical

  pure logical function f_i2_logical(x)
    !! f :: i2 -> logical
    import :: i2
    integer(i2), intent(in) :: x
  end function f_i2_logical

  pure logical function f_i4_logical(x)
    !! f :: i4 -> logical
    import :: i4
    integer(i4), intent(in) :: x
  end function f_i4_logical

  pure logical function f_i8_logical(x)
    !! f :: i8 -> logical
    import :: i8
    integer(i8), intent(in) :: x
  end function f_i8_logical

  pure logical function f_r4_logical(x)
    !! f :: r4 -> logical
    import :: r4
    real(r4), intent(in) :: x
  end function f_r4_logical

  pure logical function f_r8_logical(x)
    !! f :: r8 -> logical
    import :: r8
    real(r8), intent(in) :: x
  end function f_r8_logical

  pure logical function f_r16_logical(x)
    !! f :: r16 -> logical
    import :: r16
    real(r16), intent(in) :: x
  end function f_r16_logical

  pure logical function f_c4_logical(x)
    !! f :: c4 -> logical
    import :: r4
    complex(r4), intent(in) :: x
  end function f_c4_logical

  pure logical function f_c8_logical(x)
    !! f :: c8 -> logical
    import :: r8
    complex(r8), intent(in) :: x
  end function f_c8_logical

  pure logical function f_c16_logical(x)
    !! f :: c16 -> logical
    import :: r16
    complex(r16), intent(in) :: x
  end function f_c16_logical

  pure logical function f_char_logical(x)
    !! f :: character -> logical
    character(len=1), intent(in) :: x
  end function f_char_logical

end interface


contains


pure elemental logical function ge_c4(lhs, rhs) result(res)
  !! Private `>=` implementation for 4-byte complex numbers.
  complex(r4), intent(in) :: lhs, rhs
  res = abs(lhs) >= abs(rhs)
end function ge_c4


pure elemental logical function ge_c8(lhs, rhs) result(res)
  !! Private `>=` implementation for 8-byte complex numbers.
  complex(r8), intent(in) :: lhs, rhs
  res = abs(lhs) >= abs(rhs)
end function ge_c8


pure elemental logical function ge_c16(lhs, rhs) result(res)
  !! Private `>=` implementation for 16-byte complex numbers.
  complex(r16), intent(in) :: lhs, rhs
  res = abs(lhs) >= abs(rhs)
end function ge_c16


pure elemental logical function lt_c4(lhs, rhs) result(res)
  !! Private `<` implementation for 4-byte complex numbers.
  complex(r4), intent(in) :: lhs, rhs
  res = abs(lhs) < abs(rhs)
end function lt_c4


pure elemental logical function lt_c8(lhs, rhs) result(res)
  !! Private `<` implementation for 8-byte complex numbers.
  complex(r8), intent(in) :: lhs, rhs
  res = abs(lhs) < abs(rhs)
end function lt_c8


pure elemental logical function lt_c16(lhs, rhs) result(res)
  !! Private `<` implementation for 16-byte complex numbers.
  complex(r16), intent(in) :: lhs, rhs
  res = abs(lhs) < abs(rhs)
end function lt_c16


pure function arange_i1(start, end, increment) result(arange)
  !! Returns an array of integers given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 1-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(i1), intent(in) :: start !! Start value of the array
  integer(i1), intent(in) :: end !! End value of the array
  integer(i1), intent(in), optional :: increment !! Array increment
  integer(i1), dimension(:), allocatable :: arange
  integer(i1) :: incr
  integer(i1) :: i
  integer(i1) :: length
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
end function arange_i1


pure function arange_i2(start, end, increment) result(arange)
  !! Returns an array of integers given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 2-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(i2), intent(in) :: start !! Start value of the array
  integer(i2), intent(in) :: end !! End value of the array
  integer(i2), intent(in), optional :: increment !! Array increment
  integer(i2), dimension(:), allocatable :: arange
  integer(i2) :: incr
  integer(i2) :: i
  integer(i2) :: length
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
end function arange_i2


pure function arange_i4(start, end, increment) result(arange)
  !! Returns an array of integers given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 4-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(i4), intent(in) :: start !! Start value of the array
  integer(i4), intent(in) :: end !! End value of the array
  integer(i4), intent(in), optional :: increment !! Array increment
  integer(i4), dimension(:), allocatable :: arange
  integer(i4) :: incr
  integer(i4) :: i
  integer(i4) :: length
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
end function arange_i4


pure function arange_i8(start, end, increment) result(arange)
  !! Returns an array of integers given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 8-byte integers.
  !! Oveloaded by generic procedure `arange`.
  integer(i8), intent(in) :: start !! Start value of the array
  integer(i8), intent(in) :: end !! End value of the array
  integer(i8), intent(in), optional :: increment !! Array increment
  integer(i8), dimension(:), allocatable :: arange
  integer(i8) :: incr
  integer(i8) :: i
  integer(i8) :: length
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
end function arange_i8


pure function arange_r4(start, end, increment) result(arange)
  !! Returns an array of reals given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 4-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(r4), intent(in) :: start !! Start value of the array
  real(r4), intent(in) :: end !! End value of the array
  real(r4), intent(in), optional :: increment !! Array increment
  real(r4), dimension(:), allocatable :: arange
  real(r4) :: incr
  integer(i4) :: i
  integer(i4) :: length
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
end function arange_r4


pure function arange_r8(start, end, increment) result(arange)
  !! Returns an array of reals given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 8-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(r8), intent(in) :: start !! Start value of the array
  real(r8), intent(in) :: end !! End value of the array
  real(r8), intent(in), optional :: increment !! Array increment
  real(r8), dimension(:), allocatable :: arange
  real(r8) :: incr
  integer(i4) :: i
  integer(i4) :: length
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
end function arange_r8


pure function arange_r16(start, end, increment) result(arange)
  !! Returns an array of reals given `start`,  `end`,  and `increment` values.
  !! Increment defaults to 1 if not provided.
  !! This specific procedure is for 16-byte reals.
  !! Oveloaded by generic procedure `arange`.
  real(r16), intent(in) :: start !! Start value of the array
  real(r16), intent(in) :: end !! End value of the array
  real(r16), intent(in), optional :: increment !! Array increment
  real(r16), dimension(:), allocatable :: arange
  real(r16) :: incr
  integer(i4) :: i
  integer(i4) :: length
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
end function arange_r16


pure function arange_c4(start, end, increment) result(arange)
  !! Returns an array of complex reals given `start`,  `end`,  and
  !! `increment` values. Increment defaults to (1, 0) if not provided.
  !! Size of the resulting array is determined with real components of
  !! `start`,  `end`,  and  `increment` values if `real(increment) /= 0`,
  !! and imaginary components otherwise.
  !! This specific procedure is for 4-byte complex reals.
  !! Oveloaded by generic procedure `arange`.
  complex(r4), intent(in) :: start !! Start value of the array
  complex(r4), intent(in) :: end !! End value of the array
  complex(r4), intent(in), optional :: increment !! Array increment
  complex(r4), dimension(:), allocatable :: arange
  complex(r4) :: incr
  integer(i4) :: i
  integer(i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = (1, 0)
  endif
  if(real(incr) /= 0)then
    length = (real(end)-real(start)+0.5*real(incr))/real(incr)+1
  else
    length = (aimag(end)-aimag(start)+0.5*aimag(incr))/aimag(incr)+1
  endif
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = cmplx(real(start)+(i-1)*real(incr), &
                      aimag(start)+(i-1)*aimag(incr))
  enddo
end function arange_c4


pure function arange_c8(start, end, increment) result(arange)
  !! Returns an array of complex reals given `start`,  `end`,  and
  !! `increment` values. Increment defaults to (1, 0) if not provided.
  !! Size of the resulting array is determined with real components of
  !! `start`,  `end`,  and  `increment` values if `real(increment) /= 0`,
  !! and imaginary components otherwise.
  !! This specific procedure is for 8-byte complex reals.
  !! Oveloaded by generic procedure `arange`.
  complex(r8), intent(in) :: start !! Start value of the array
  complex(r8), intent(in) :: end !! End value of the array
  complex(r8), intent(in), optional :: increment !! Array increment
  complex(r8), dimension(:), allocatable :: arange
  complex(r8) :: incr
  integer(i4) :: i
  integer(i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = (1, 0)
  endif
  if(real(incr) /= 0)then
    length = (real(end)-real(start)+0.5*real(incr))/real(incr)+1
  else
    length = (aimag(end)-aimag(start)+0.5*aimag(incr))/aimag(incr)+1
  endif
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = cmplx(real(start)+(i-1)*real(incr), &
                      aimag(start)+(i-1)*aimag(incr))
  enddo
end function arange_c8


pure function arange_c16(start, end, increment) result(arange)
  !! Returns an array of complex reals given `start`,  `end`,  and
  !! `increment` values. Increment defaults to (1, 0) if not provided.
  !! Size of the resulting array is determined with real components of
  !! `start`,  `end`,  and  `increment` values if `real(increment) /= 0`,
  !! and imaginary components otherwise.
  !! This specific procedure is for 16-byte complex reals.
  !! Oveloaded by generic procedure `arange`.
  complex(r16), intent(in) :: start !! Start value of the array
  complex(r16), intent(in) :: end !! End value of the array
  complex(r16), intent(in), optional :: increment !! Array increment
  complex(r16), dimension(:), allocatable :: arange
  complex(r16) :: incr
  integer(i4) :: i
  integer(i4) :: length
  if(present(increment))then
    incr = increment
  else
    incr = (1, 0)
  endif
  if(real(incr) /= 0)then
    length = (real(end)-real(start)+0.5*real(incr))/real(incr)+1
  else
    length = (aimag(end)-aimag(start)+0.5*aimag(incr))/aimag(incr)+1
  endif
  allocate(arange(length))
  do concurrent(i = 1:length)
    arange(i) = cmplx(real(start)+(i-1)*real(incr), &
                      aimag(start)+(i-1)*aimag(incr))
  enddo
end function arange_c16


pure function arrstr(array) result(string)
  !! Returns a string given an array of len=1 characters.
  character(len=1), dimension(:), intent(in) :: array !! Input array
  character(len=:), allocatable :: string
  integer :: n
  allocate(character(len=size(array)) :: string)
  do concurrent(n = 1:size(array))
    string(n:n) = array(n)
  enddo
end function arrstr


pure function complement_i1(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(i1), dimension(:), intent(in) :: x !! First input array
  integer(i1), dimension(:), intent(in) :: y !! Second input array
  integer(i1), dimension(:), allocatable :: complement
  integer(i1), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i1, 0_i1)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_i1


pure function complement_i2(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(i2), dimension(:), intent(in) :: x !! First input array
  integer(i2), dimension(:), intent(in) :: y !! Second input array
  integer(i2), dimension(:), allocatable :: complement
  integer(i2), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i2, 0_i2)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_i2


pure function complement_i4(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(i4), dimension(:), intent(in) :: x !! First input array
  integer(i4), dimension(:), intent(in) :: y !! Second input array
  integer(i4), dimension(:), allocatable :: complement
  integer(i4), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i4, 0_i4)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_i4


pure function complement_i8(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `complement`.
  integer(i8), dimension(:), intent(in) :: x !! First input array
  integer(i8), dimension(:), intent(in) :: y !! Second input array
  integer(i8), dimension(:), allocatable :: complement
  integer(i8), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1_i8, 0_i8)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_i8


pure function complement_r4(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(r4), dimension(:), intent(in) :: x !! First input array
  real(r4), dimension(:), intent(in) :: y !! Second input array
  real(r4), dimension(:), allocatable :: complement
  real(r4), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._r4, 0._r4)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_r4


pure function complement_r8(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(r8), dimension(:), intent(in) :: x !! First input array
  real(r8), dimension(:), intent(in) :: y !! Second input array
  real(r8), dimension(:), allocatable :: complement
  real(r8), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._r4, 0._r4)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_r8


pure function complement_r16(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `complement`.
  real(r16), dimension(:), intent(in) :: x !! First input array
  real(r16), dimension(:), intent(in) :: y !! Second input array
  real(r16), dimension(:), allocatable :: complement
  real(r16), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(1._r16, 0._r16)
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_r16


pure function complement_c4(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `complement`.
  complex(r4), dimension(:), intent(in) :: x !! First input array
  complex(r4), dimension(:), intent(in) :: y !! Second input array
  complex(r4), dimension(:), allocatable :: complement
  complex(r4), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(cmplx(1._r4, 0._r4), cmplx(0._r4, 0._r4))
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_c4


pure function complement_c8(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `complement`.
  complex(r8), dimension(:), intent(in) :: x !! First input array
  complex(r8), dimension(:), intent(in) :: y !! Second input array
  complex(r8), dimension(:), allocatable :: complement
  complex(r8), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(cmplx(1._r4, 0._r4), cmplx(0._r4, 0._r4))
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_c8


pure function complement_c16(x, y) result(complement)
  !! Returns a set complement of two arrays.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `complement`.
  complex(r16), dimension(:), intent(in) :: x !! First input array
  complex(r16), dimension(:), intent(in) :: y !! Second input array
  complex(r16), dimension(:), allocatable :: complement
  complex(r16), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = arange(cmplx(1._r16, 0._r16), cmplx(0._r16, 0._r16))
  do concurrent (n = 1:size(a))
    if(.not. any(b == a(n)))complement = [complement, a(n)]
  enddo
end function complement_c16


pure function complement_char(x, y) result(complement)
  !! Returns a set complement of two character strings.
  !! Overloaded by generic procedure `complement`.
  character(len=*), intent(in) :: x !! First input array
  character(len=*), intent(in) :: y !! Second input array
  character(len=:), allocatable :: complement
  character(len=:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  complement = ''
  do concurrent (n = 1:len(a))
    if (scan(a(n:n), b) == 0) complement = complement // a(n:n)
  enddo
end function complement_char


pure function empty_i1(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(i1), intent(in) :: a !! Input scalar
  integer(i1), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_i1


pure function empty_i2(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(i2), intent(in) :: a !! Input scalar
  integer(i2), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_i2


pure function empty_i4(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(i4), intent(in) :: a !! Input scalar
  integer(i4), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_i4


pure function empty_i8(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `empty`.
  integer(i8), intent(in) :: a !! Input scalar
  integer(i8), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_i8


pure function empty_r4(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `empty`.
  real(r4), intent(in) :: a !! Input scalar
  real(r4), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_r4


pure function empty_r8(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `empty`.
  real(r8), intent(in) :: a !! Input scalar
  real(r8), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_r8


pure function empty_r16(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `empty`.
  real(r16), intent(in) :: a !! Input scalar
  real(r16), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_r16


pure function empty_c4(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `empty`.
  complex(r4), intent(in) :: a !! Input scalar
  complex(r4), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_c4


pure function empty_c8(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `empty`.
  complex(r8), intent(in) :: a !! Input scalar
  complex(r8), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_c8


pure function empty_c16(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `empty`.
  complex(r16), intent(in) :: a !! Input scalar
  complex(r16), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_c16


pure function empty_char(a) result(empty)
  !! Returns an allocated array of length `0`,
  !! and type and kind same as that of scalar `a`.
  !! This specific procedure is for len=1 character.
  !! Overloaded by generic procedure `empty`.
  character(len=1), intent(in) :: a !! Input scalar
  character(len=1), dimension(:), allocatable :: empty
  allocate(empty(0))
end function empty_char


pure function filter_i1(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i1_logical) :: f !! Filtering function
  integer(i1), intent(in) :: x(:) !! Input array
  integer(i1), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_i1


pure function filter_i2(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i2_logical) :: f !! Filtering function
  integer(i2), intent(in) :: x(:) !! Input array
  integer(i2), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_i2


pure function filter_i4(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i4_logical) :: f !! Filtering function
  integer(i4), intent(in) :: x(:) !! Input array
  integer(i4), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_i4


pure function filter_i8(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `filter`.
  procedure(f_i8_logical) :: f !! Filtering function
  integer(i8), intent(in) :: x(:) !! Input array
  integer(i8), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_i8


pure function filter_r4(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_r4_logical) :: f !! Filtering function
  real(r4), intent(in) :: x(:) !! Input array
  real(r4), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_r4


pure function filter_r8(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_r8_logical) :: f !! Filtering function
  real(r8), intent(in) :: x(:) !! Input array
  real(r8), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_r8


pure function filter_r16(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_r16_logical) :: f !! Filtering function
  real(r16), intent(in) :: x(:) !! Input array
  real(r16), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_r16


pure function filter_c4(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_c4_logical) :: f !! Filtering function
  complex(r4), intent(in) :: x(:) !! Input array
  complex(r4), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_c4


pure function filter_c8(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_c8_logical) :: f !! Filtering function
  complex(r8), intent(in) :: x(:) !! Input array
  complex(r8), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_c8


pure function filter_c16(f, x) result(filter)
  !! Returns a subset of `x` for which `f(x) == .true.`
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `filter`.
  procedure(f_c16_logical) :: f !! Filtering function
  complex(r16), intent(in) :: x(:) !! Input array
  complex(r16), allocatable :: filter(:)
  integer :: i
  filter = pack(x, [(f(x(i)), i = 1, size(x))])
end function filter_c16


pure recursive integer(i1) function foldl_i1(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i1) :: f !! Folding function
  integer(i1), intent(in) :: start !! Accumulator start value
  integer(i1), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_i1


pure recursive integer(i2) function foldl_i2(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i2) :: f !! Folding function
  integer(i2), intent(in) :: start !! Accumulator start value
  integer(i2), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_i2


pure recursive integer(i4) function foldl_i4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i4) :: f !! Folding function
  integer(i4), intent(in) :: start !! Accumulator start value
  integer(i4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_i4


pure recursive integer(i8) function foldl_i8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_i8) :: f !! Folding function
  integer(i8), intent(in) :: start !! Accumulator start value
  integer(i8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_i8


pure recursive real(r4) function foldl_r4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_r4) :: f !! Folding function
  real(r4), intent(in) :: start !! Accumulator start value
  real(r4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_r4


pure recursive real(r8) function foldl_r8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_r8) :: f !! Folding function
  real(r8), intent(in) :: start !! Accumulator start value
  real(r8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_r8


pure recursive real(r16) function foldl_r16(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_r16) :: f !! Folding function
  real(r16), intent(in) :: start !! Accumulator start value
  real(r16), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_r16


pure recursive complex(r4) function foldl_c4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_c4) :: f !! Folding function
  complex(r4), intent(in) :: start !! Accumulator start value
  complex(r4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_c4


pure recursive complex(r8) function foldl_c8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_c8) :: f !! Folding function
  complex(r8), intent(in) :: start !! Accumulator start value
  complex(r8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_c8


pure recursive complex(r16) function foldl_c16(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's left fold. If the array is empty,  the
  !! result is `start`; else we recurse immediately,  making the new
  !! initial value the result of combining the old initial value
  !! with the first element of `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldl`.
  procedure(f2_c16) :: f !! Folding function
  complex(r16), intent(in) :: start !! Accumulator start value
  complex(r16), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = foldl(f, f(start, x(1)), x(2:))
  endif
end function foldl_c16


pure recursive integer(i1) function foldr_i1(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i1) :: f !! Folding function
  integer(i1), intent(in) :: start !! Accumulator start value
  integer(i1), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_i1


pure recursive integer(i2) function foldr_i2(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i2) :: f !! Folding function
  integer(i2), intent(in) :: start !! Accumulator start value
  integer(i2), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_i2


pure recursive integer(i4) function foldr_i4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i4) :: f !! Folding function
  integer(i4), intent(in) :: start !! Accumulator start value
  integer(i4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_i4


pure recursive integer(i8) function foldr_i8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_i8) :: f !! Folding function
  integer(i8), intent(in) :: start !! Accumulator start value
  integer(i8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_i8


pure recursive real(r4) function foldr_r4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_r4) :: f !! Folding function
  real(r4), intent(in) :: start !! Accumulator start value
  real(r4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_r4


pure recursive real(r8) function foldr_r8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_r8) :: f !! Folding function
  real(r8), intent(in) :: start !! Accumulator start value
  real(r8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_r8


pure recursive real(r16) function foldr_r16(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_r16) :: f !! Folding function
  real(r16), intent(in) :: start !! Accumulator start value
  real(r16), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_r16


pure recursive complex(r4) function foldr_c4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_c4) :: f !! Folding function
  complex(r4), intent(in) :: start !! Accumulator start value
  complex(r4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_c4


pure recursive complex(r8) function foldr_c8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_c8) :: f !! Folding function
  complex(r8), intent(in) :: start !! Accumulator start value
  complex(r8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_c8


pure recursive complex(r16) function foldr_c16(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`.
  !! Equivalent to haskell's right fold. If the list is empty,  the
  !! result is `start`; else apply `f` to the first element and the
  !! result of folding the rest.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `foldr`.
  procedure(f2_c16) :: f !! Folding function
  complex(r16), intent(in) :: start !! Accumulator start value
  complex(r16), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  else
    res = f(x(1), foldr(f, start, x(2:)))
  endif
end function foldr_c16


pure recursive integer(i1) function foldt_i1(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i1) :: f !! Folding function
  integer(i1), intent(in) :: start !! Accumulator start value
  integer(i1), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_i1


pure recursive integer(i2) function foldt_i2(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i2) :: f !! Folding function
  integer(i2), intent(in) :: start !! Accumulator start value
  integer(i2), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_i2


pure recursive integer(i4) function foldt_i4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i4) :: f !! Folding function
  integer(i4), intent(in) :: start !! Accumulator start value
  integer(i4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_i4


pure recursive integer(i8) function foldt_i8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_i8) :: f !! Folding function
  integer(i8), intent(in) :: start !! Accumulator start value
  integer(i8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_i8


pure recursive real(r4) function foldt_r4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_r4) :: f !! Folding function
  real(r4), intent(in) :: start !! Accumulator start value
  real(r4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_r4


pure recursive real(r8) function foldt_r8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_r8) :: f !! Folding function
  real(r8), intent(in) :: start !! Accumulator start value
  real(r8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_r8


pure recursive real(r16) function foldt_r16(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_r16) :: f !! Folding function
  real(r16), intent(in) :: start !! Accumulator start value
  real(r16), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_r16


pure recursive complex(r4) function foldt_c4(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_c4) :: f !! Folding function
  complex(r4), intent(in) :: start !! Accumulator start value
  complex(r4), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_c4


pure recursive complex(r8) function foldt_c8(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_c8) :: f !! Folding function
  complex(r8), intent(in) :: start !! Accumulator start value
  complex(r8), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_c8


pure recursive complex(r16) function foldt_c16(f, start, x) result(res)
  !! Applies function `f` recursively along elements of array `x`
  !! using a tree-like fold,  splitting the array into two and repeating
  !! until we deplete the array.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `foldt`.
  procedure(f2_c16) :: f !! Folding function
  complex(r16), intent(in) :: start !! Accumulator start value
  complex(r16), dimension(:), intent(in) :: x !! Input array
  if(size(x) < 1)then
    res = start
  elseif(size(x) == 1)then
    res = f(start, x(1))
  else
    res = foldt(f, foldt(f, start, split(x, 1)), split(x, 2))
  endif
end function foldt_c16


pure integer(i1) function head_i1(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_i1


pure integer(i2) function head_i2(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_i2


pure integer(i4) function head_i4(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_i4


pure integer(i8) function head_i8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `head`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_i8


pure real(r4) function head_r4(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `head`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_r4


pure real(r8) function head_r8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `head`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_r8


pure real(r16) function head_r16(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `head`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_r16


pure complex(r4) function head_c4(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `head`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_c4


pure complex(r8) function head_c8(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `head`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_c8


pure complex(r16) function head_c16(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `head`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  head = x(1)
end function head_c16


pure character(len=1) function head_char(x) result(head)
  !! Returns the first element of array `x`.
  !! This specific procedure is for character strings.
  !! Overloaded by generic procedure `head`.
  character(len=*), intent(in) :: x !! Input array
  head = x(1:1)
end function head_char


pure function init_i1(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_i1


pure function init_i2(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_i2


pure function init_i4(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_i4


pure function init_i8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `init`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_i8


pure function init_r4(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `init`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_r4


pure function init_r8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `init`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_r8


pure function init_r16(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `init`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_r16


pure function init_c4(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `init`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_c4


pure function init_c8(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `init`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_c8


pure function init_c16(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `init`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(size(x)-1) :: init
  init = x(:size(x)-1)
end function init_c16


pure function init_char(x) result(init)
  !! Returns all elements of `x` but the last.
  !! This specific procedure is for character string.
  !! Overloaded by generic procedure `init`.
  character(len=*), intent(in) :: x !! Input array
  character(len=len(x)-1) :: init
  init = x(:len(x)-1)
end function init_char


pure function insert_i1(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(i1), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_i1


pure function insert_i2(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(i2), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_i2


pure function insert_i4(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(i4), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_i4


pure function insert_i8(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `insert`.
  integer(i8), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_i8


pure function insert_r4(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(r4), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_r4


pure function insert_r8(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(r8), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_r8


pure function insert_r16(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `insert`.
  real(r16), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_r16


pure function insert_c4(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `insert`.
  complex(r4), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_c4


pure function insert_c8(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `insert`.
  complex(r8), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_c8


pure function insert_c16(elem, ind, x) result(insert)
  !! Inserts `elem` into index `ind` of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `insert`.
  complex(r16), intent(in) :: elem !! Element to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(size(x)+1) :: insert
  insert = [x(:limit(ind, 1, size(x)+1)-1), elem, x(limit(ind, 1, size(x)+1):)]
end function insert_c16


pure function insert_char(elem, ind, x) result(insert)
  !! Inserts character string `elem` into 
  !! index `ind` of character string `x`.
  !! Overloaded by generic procedure `insert`.
  character(len=*), intent(in) :: elem !! Character string to insert
  integer(i4), intent(in) :: ind !! Index to insert element at
  character(len=*), intent(in) :: x !! Input array
  character(len=len(elem)+len(x)) :: insert
  insert = x(:ind-1) // elem // x(ind:)
end function insert_char


pure function intersection_i1(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(i1), dimension(:), intent(in) :: x !! First input array
  integer(i1), dimension(:), intent(in) :: y !! Second input array
  integer(i1), dimension(:), allocatable :: res
  integer(i1), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i1)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_i1


pure function intersection_i2(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(i2), dimension(:), intent(in) :: x !! First input array
  integer(i2), dimension(:), intent(in) :: y !! Second input array
  integer(i2), dimension(:), allocatable :: res
  integer(i2), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i2)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_i2


pure function intersection_i4(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(i4), dimension(:), intent(in) :: x !! First input array
  integer(i4), dimension(:), intent(in) :: y !! Second input array
  integer(i4), dimension(:), allocatable :: res
  integer(i4), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i4)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_i4


pure function intersection_i8(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `intersection`.
  integer(i8), dimension(:), intent(in) :: x !! First input array
  integer(i8), dimension(:), intent(in) :: y !! Second input array
  integer(i8), dimension(:), allocatable :: res
  integer(i8), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1_i8)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_i8


pure function intersection_r4(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(r4), dimension(:), intent(in) :: x !! First input array
  real(r4), dimension(:), intent(in) :: y !! Second input array
  real(r4), dimension(:), allocatable :: res
  real(r4), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1._r4)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_r4


pure function intersection_r8(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(r8), dimension(:), intent(in) :: x !! First input array
  real(r8), dimension(:), intent(in) :: y !! Second input array
  real(r8), dimension(:), allocatable :: res
  real(r8), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1._r8)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_r8


pure function intersection_r16(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `intersection`.
  real(r16), dimension(:), intent(in) :: x !! First input array
  real(r16), dimension(:), intent(in) :: y !! Second input array
  real(r16), dimension(:), allocatable :: res
  real(r16), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(1._r16)
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_r16


pure function intersection_c4(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `intersection`.
  complex(r4), dimension(:), intent(in) :: x !! First input array
  complex(r4), dimension(:), intent(in) :: y !! Second input array
  complex(r4), dimension(:), allocatable :: res
  complex(r4), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(cmplx(1._r4, 0._r4))
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_c4


pure function intersection_c8(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `intersection`.
  complex(r8), dimension(:), intent(in) :: x !! First input array
  complex(r8), dimension(:), intent(in) :: y !! Second input array
  complex(r8), dimension(:), allocatable :: res
  complex(r8), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(cmplx(1._r8, 0._r8))
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_c8


pure function intersection_c16(x, y) result(res)
  !! Returns a set intersection of two arrays.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `intersection`.
  complex(r16), dimension(:), intent(in) :: x !! First input array
  complex(r16), dimension(:), intent(in) :: y !! Second input array
  complex(r16), dimension(:), allocatable :: res
  complex(r16), dimension(:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = empty(cmplx(1._r16, 0._r16))
  if(size(a) > size(b))then
    do concurrent (n = 1:size(b))
      if(any(a == b(n)))res = [res, b(n)]
    enddo
  else
    do concurrent (n = 1:size(a))
      if(any(b == a(n)))res = [res, a(n)]
    enddo
  endif
end function intersection_c16


pure function intersection_char(x, y) result(res)
  !! Returns a set intersection of two character strings.
  !! Overloaded by generic procedure `intersection`.
  character(len=*), intent(in) :: x !! First input array
  character(len=*), intent(in) :: y !! Second input array
  character(len=:), allocatable :: res
  character(len=:), allocatable :: a, b
  integer(i4) :: n
  a = set(x)
  b = set(y)
  res = ''
  if (len(a) > len(b)) then
    do concurrent (n = 1:len(b))
      if (scan(b(n:n), a) > 0) res = res // b(n:n)
    end do
  else
    do concurrent (n = 1:len(a))
      if (scan(a(n:n), b) > 0) res = res // a(n:n)
    end do
  end if
end function intersection_char


pure integer(i1) function iterfold_i1(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i1) :: f !! Folding function
  integer(i1), intent(in) :: start !! Accumulator start value
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_i1


pure integer(i2) function iterfold_i2(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i2) :: f !! Folding function
  integer(i2), intent(in) :: start !! Accumulator start value
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_i2


pure integer(i4) function iterfold_i4(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i4) :: f !! Folding function
  integer(i4), intent(in) :: start !! Accumulator start value
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_i4


pure integer(i8) function iterfold_i8(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_i8) :: f !! Folding function
  integer(i8), intent(in) :: start !! Accumulator start value
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_i8


pure real(r4) function iterfold_r4(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_r4) :: f !! Folding function
  real(r4), intent(in) :: start !! Accumulator start value
  real(r4), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_r4


pure real(r8) function iterfold_r8(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_r8) :: f !! Folding function
  real(r8), intent(in) :: start !! Accumulator start value
  real(r8), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_r8


pure real(r16) function iterfold_r16(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_r16) :: f !! Folding function
  real(r16), intent(in) :: start !! Accumulator start value
  real(r16), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_r16


pure complex(r4) function iterfold_c4(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_c4) :: f !! Folding function
  complex(r4), intent(in) :: start !! Accumulator start value
  complex(r4), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_c4


pure complex(r8) function iterfold_c8(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_c8) :: f !! Folding function
  complex(r8), intent(in) :: start !! Accumulator start value
  complex(r8), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_c8


pure complex(r16) function iterfold_c16(f, start, x) result(iterfold)
  !! Reduces input array `x` using input function `f(x, y)`.
  !! Initial value is `start`,  if given,  and zero otherwise.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `iterfold`.
  procedure(f2_c16) :: f !! Folding function
  complex(r16), intent(in) :: start !! Accumulator start value
  complex(r16), dimension(:), intent(in) :: x !! Input array
  integer :: i
  iterfold = start
  do i = 1, size(x)
    iterfold = f(iterfold, x(i))
  enddo
end function iterfold_c16


pure integer(i1) function last_i1(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_i1


pure integer(i2) function last_i2(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_i2


pure integer(i4) function last_i4(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_i4


pure integer(i8) function last_i8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `last`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_i8


pure real(r4) function last_r4(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `last`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_r4


pure real(r8) function last_r8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `last`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_r8


pure real(r16) function last_r16(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `last`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_r16


pure complex(r4) function last_c4(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `last`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_c4


pure complex(r8) function last_c8(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `last`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_c8


pure complex(r16) function last_c16(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `last`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  last = x(size(x))
end function last_c16


pure character(len=1) function last_char(x) result(last)
  !! Returns the last element of array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `last`.
  character(len=*), intent(in) :: x !! Input array
  last = x(len(x):len(x))
end function last_char


pure elemental integer(i1) function limit_i1(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(i1), intent(in) :: x !! Input scalar
  integer(i1), intent(in) :: a !! First limit
  integer(i1), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_i1


pure elemental integer(i2) function limit_i2(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(i2), intent(in) :: x !! Input scalar
  integer(i2), intent(in) :: a !! First limit
  integer(i2), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_i2


pure elemental integer(i4) function limit_i4(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(i4), intent(in) :: x !! Input scalar
  integer(i4), intent(in) :: a !! First limit
  integer(i4), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_i4


pure elemental integer(i8) function limit_i8(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `limit`.
  integer(i8), intent(in) :: x !! Input scalar
  integer(i8), intent(in) :: a !! First limit
  integer(i8), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_i8


pure elemental real(r4) function limit_r4(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(r4), intent(in) :: x !! Input scalar
  real(r4), intent(in) :: a !! First limit
  real(r4), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_r4


pure elemental real(r8) function limit_r8(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(r8), intent(in) :: x !! Input scalar
  real(r8), intent(in) :: a !! First limit
  real(r8), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_r8


pure elemental real(r16) function limit_r16(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `limit`.
  real(r16), intent(in) :: x !! Input scalar
  real(r16), intent(in) :: a !! First limit
  real(r16), intent(in) :: b !! Second limit
  limit = min(max(x, min(a, b)), max(a, b))
end function limit_r16


pure elemental complex(r4) function limit_c4(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`,
  !! for Re and Im components each.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `limit`.
  complex(r4), intent(in) :: x !! Input scalar
  complex(r4), intent(in) :: a !! First limit
  complex(r4), intent(in) :: b !! Second limit
  limit = cmplx(min(max(real(x), min(real(a), real(b))), max(real(a), real(b))), &
    min(max(aimag(x), min(aimag(a), aimag(b))), max(aimag(a), aimag(b))))
end function limit_c4


pure elemental complex(r8) function limit_c8(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`,
  !! for Re and Im components each.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `limit`.
  complex(r8), intent(in) :: x !! Input scalar
  complex(r8), intent(in) :: a !! First limit
  complex(r8), intent(in) :: b !! Second limit
  limit = cmplx(min(max(real(x), min(real(a), real(b))), max(real(a), real(b))), &
    min(max(aimag(x), min(aimag(a), aimag(b))), max(aimag(a), aimag(b))))
end function limit_c8


pure elemental complex(r16) function limit_c16(x, a, b) result(limit)
  !! Returns `x` if `min(a, b) <= x .and. x <= max(a, b)`,
  !! `min(a, b) if `x < min(a, b)` and `max(a, b) if `x < max(a, b)`,
  !! for Re and Im components each.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `limit`.
  complex(r16), intent(in) :: x !! Input scalar
  complex(r16), intent(in) :: a !! First limit
  complex(r16), intent(in) :: b !! Second limit
  limit = cmplx(min(max(real(x), min(real(a), real(b))), max(real(a), real(b))), &
    min(max(aimag(x), min(aimag(a), aimag(b))), max(aimag(a), aimag(b))))
end function limit_c16


pure function map_i1(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i1) :: f !! Mapping function
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_i1


pure function map_i2(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i2) :: f !! Mapping function
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_i2


pure function map_i4(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i4) :: f !! Mapping function
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_i4


pure function map_i8(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `map`.
  procedure(f_i8) :: f !! Mapping function
  integer(i8), dimension(:), intent(in) :: x
  integer(i8), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_i8


pure function map_r4(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_r4) :: f !! Mapping function
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_r4


pure function map_r8(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_r8) :: f !! Mapping function
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_r8


pure function map_r16(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_r16) :: f !! Mapping function
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_r16


pure function map_c4(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_c4) :: f !! Mapping function
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_c4


pure function map_c8(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_c8) :: f !! Mapping function
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_c8


pure function map_c16(f, x) result(map)
  !! Returns `f(x)` given input function `f` and array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `map`.
  procedure(f_c16) :: f !! Mapping function
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(size(x)) :: map
  integer(i4) :: i
  map = [(f(x(i)), i = 1, size(x))]
end function map_c16


pure function reverse_i1(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_i1


pure function reverse_i2(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_i2


pure function reverse_i4(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_i4


pure function reverse_i8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `reverse`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_i8


pure function reverse_r4(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_r4


pure function reverse_r8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_r8


pure function reverse_r16(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `reverse`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_r16


pure function reverse_c4(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `reverse`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_c4


pure function reverse_c8(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `reverse`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_c8


pure function reverse_c16(x) result(reverse)
  !! Returns the array `x` in reverse order.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `reverse`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(size(x)) :: reverse
  reverse = x(size(x):1:-1)
end function reverse_c16


pure function reverse_char(x) result(res)
  !! Returns the character string `x` in reverse order.
  !! Overloaded by generic procedure `reverse`.
  character(len=*), intent(in) :: x !! Input array
  character(len=len(x)) :: res
  res = arrstr(achar(reverse(iachar(strarr(x)))))
end function reverse_char


pure recursive function set_i1(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_i1


pure recursive function set_i2(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_i2

pure recursive function set_i4(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_i4


pure recursive function set_i8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `set`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_i8


pure recursive function set_r4(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `set`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_r4


pure recursive function set_r8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `set`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_r8


pure recursive function set_r16(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `set`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_r16


pure recursive function set_c4(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `set`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_c4


pure recursive function set_c8(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `set`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_c8


pure recursive function set_c16(x) result(res)
  !! Returns a set given array `x`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `set`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(:), allocatable :: res
  if(size(x) > 1)then
    res = [x(1), set(pack(x(2:), .not. x(2:) == x(1)))]
  else
    res = x
  endif
end function set_c16


pure recursive function set_char(x) result(res)
  !! Returns a set given character string `x`.
  !! Overloaded by generic procedure `set`.
  character(len=*), intent(in) :: x !! Input character string
  character(len=:), allocatable :: res
  res = arrstr(achar(set(iachar(strarr(x)))))
end function set_char


pure recursive function sort_i1(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(size(x)) :: res
  integer(i1), dimension(size(x)-1) :: rest
  integer(i1) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_i1


pure recursive function sort_i2(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! using binary search tree pivot.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(size(x)) :: res
  integer(i2), dimension(size(x)-1) :: rest
  integer(i2) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_i2


pure recursive function sort_i4(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(size(x)) :: res
  integer(i4), dimension(size(x)-1) :: rest
  integer(i4) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_i4


pure recursive function sort_i8(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! using binary search tree pivot.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `sort`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(size(x)) :: res
  integer(i8), dimension(size(x)-1) :: rest
  integer(i8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_i8


pure recursive function sort_r4(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(size(x)) :: res
  real(r4), dimension(size(x)-1) :: rest
  real(r4) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_r4


pure recursive function sort_r8(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(size(x)) :: res
  real(r8), dimension(size(x)-1) :: rest
  real(r8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_r8


pure recursive function sort_r16(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `sort`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(size(x)) :: res
  real(r16), dimension(size(x)-1) :: rest
  real(r16) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_r16


pure recursive function sort_c4(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `sort`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(size(x)) :: res
  complex(r4), dimension(size(x)-1) :: rest
  complex(r4) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_c4


pure recursive function sort_c8(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `sort`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(size(x)) :: res
  complex(r8), dimension(size(x)-1) :: rest
  complex(r8) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_c8


pure recursive function sort_c16(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `sort`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(size(x)) :: res
  complex(r16), dimension(size(x)-1) :: rest
  complex(r16) :: pivot
  if(size(x) > 1)then
    pivot = head(split(x, 2))
    rest = [split(x, 1), tail(split(x, 2))]
    res = [sort(pack(rest, rest < pivot)), pivot, &
           sort(pack(rest, rest >= pivot))]
  else
    res = x
  endif
end function sort_c16


pure function sort_char(x) result(res)
  !! Recursive quicksort using binary tree pivot.
  !! This specific procedure is for character strings.
  !! Overloaded by generic procedure `sort`.
  character(len=*), intent(in) :: x !! Input array
  character(len=len(x)) :: res
  res = arrstr(achar(sort(iachar(strarr(x)))))
end function sort_char


pure function split_i1(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  integer(i1), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_i1


pure function split_i2(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  integer(i2), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_i2


pure function split_i4(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  integer(i4), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_i4


pure function split_i8(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `split`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  integer(i8), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_i8


pure function split_r4(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `split`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  real(r4), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_r4


pure function split_r8(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `split`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  real(r8), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_r8


pure function split_r16(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `split`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  real(r16), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_r16


pure function split_c4(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `split`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  complex(r4), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_c4


pure function split_c8(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `split`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  complex(r8), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_c8


pure function split_c16(x, section) result(split)
  !! Returns the first half of the array `x` if `section == 1`,
  !! the second half of the array `x` if `section == 2`,
  !! and an empty array otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `split`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  complex(r16), dimension(:), allocatable :: split
  if(section == 1)then
    split = x(1:size(x)/2)
  elseif(section == 2)then
    split = x(size(x)/2+1:)
  endif
end function split_c16


pure function split_char(x, section) result(split)
  !! Returns the first half of the character string `x` 
  !! if `section == 1`, the second half of  `x` if `section == 2`,
  !! and an empty string otherwise. If `size(x) == 1`,  `split(x, 1)`
  !! returns and empty array,  and `split(x, 2)` returns `x(1)`.
  !! Overloaded by generic procedure `split`.
  character(len=*), intent(in) :: x !! Input array
  integer(i4), intent(in) :: section !! Array section to return
  character(len=:), allocatable :: split
  if (section == 1) then
    split = x(1:len(x) / 2)
  else if (section == 2) then
    split = x(len(x) / 2 + 1:)
  else
    split = ''
  end if
end function split_char


pure function strarr(string) result(array)
  !! Returns an array of len=1 characters given a string.
  character(len=*), intent(in) :: string !! Input string
  character(len=1), dimension(:), allocatable :: array
  integer :: n
  allocate(array(len(string)))
  do concurrent(n = 1:len(string))
    array(n) = string(n:n)
  enddo
end function strarr


pure function subscript_i1(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(:), intent(in) :: ind !! Indices to subscript
  integer(i1), dimension(:), allocatable :: subscript
  integer(i1), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_i1


pure function subscript_i2(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(:), intent(in) :: ind !! Indices to subscript
  integer(i2), dimension(:), allocatable :: subscript
  integer(i2), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_i2


pure function subscript_i4(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  integer(i4), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_i4


pure function subscript_i8(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `subscript`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(:), intent(in) :: ind !! Indices to subscript
  integer(i8), dimension(:), allocatable :: subscript
  integer(i8), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_i8


pure function subscript_r4(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  real(r4), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_r4


pure function subscript_r8(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  real(r8), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_r8


pure function subscript_r16(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `subscript`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  real(r16), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_r16


pure function subscript_c4(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `subscript`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  complex(r4), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_c4


pure function subscript_c8(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `subscript`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  complex(r8), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_c8


pure function subscript_c16(x, ind) result(subscript)
  !! Subscripts the array `x` along indices `ind`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `subscript`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(:), intent(in) :: ind !! Indices to subscript
  complex(r16), dimension(:), allocatable :: subscript
  integer(i4), dimension(:), allocatable :: indices
  integer :: i
  indices = pack(ind, ind > 0 .and. ind <= size(x))
  allocate(subscript(size(indices)))
  do concurrent(i = 1:size(indices))
    subscript(i) = x(indices(i))
  enddo
end function subscript_c16


pure function tail_i1(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(i1), dimension(:), intent(in) :: x !! Input array
  integer(i1), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_i1


pure function tail_i2(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(i2), dimension(:), intent(in) :: x !! Input array
  integer(i2), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_i2


pure function tail_i4(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(i4), dimension(:), intent(in) :: x !! Input array
  integer(i4), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_i4


pure function tail_i8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `tail`.
  integer(i8), dimension(:), intent(in) :: x !! Input array
  integer(i8), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_i8


pure function tail_r4(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(r4), dimension(:), intent(in) :: x !! Input array
  real(r4), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_r4


pure function tail_r8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(r8), dimension(:), intent(in) :: x !! Input array
  real(r8), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_r8


pure function tail_r16(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `tail`.
  real(r16), dimension(:), intent(in) :: x !! Input array
  real(r16), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_r16


pure function tail_c4(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `tail`.
  complex(r4), dimension(:), intent(in) :: x !! Input array
  complex(r4), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_c4


pure function tail_c8(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `tail`.
  complex(r8), dimension(:), intent(in) :: x !! Input array
  complex(r8), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_c8


pure function tail_c16(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `tail`.
  complex(r16), dimension(:), intent(in) :: x !! Input array
  complex(r16), dimension(size(x)-1) :: tail
  tail = x(2:)
end function tail_c16


pure function tail_char(x) result(tail)
  !! Returns all elements of `x` but the first.
  !! This specific procedure is for character strings.
  !! Overloaded by generic procedure `tail`.
  character(len=*), intent(in) :: x !! Input array
  character(len=len(x)-1) :: tail
  tail = x(2:)
end function tail_char


pure recursive function unfold_i1(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i1) :: f !! Unfolding function
  integer(i1), dimension(:), intent(in) :: x !! Start value
  integer(i1), intent(in) :: len !! Array length to return
  integer(i1), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_i1


pure recursive function unfold_i2(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i2) :: f !! Unfolding function
  integer(i2), dimension(:), intent(in) :: x !! Start value
  integer(i2), intent(in) :: len !! Array length to return
  integer(i2), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_i2


pure recursive function unfold_i4(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i4) :: f !! Unfolding function
  integer(i4), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  integer(i4), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_i4


pure recursive function unfold_i8(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_i8) :: f !! Unfolding function
  integer(i8), dimension(:), intent(in) :: x !! Start value
  integer(i8), intent(in) :: len !! Array length to return
  integer(i8), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_i8


pure recursive function unfold_r4(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_r4) :: f !! Unfolding function
  real(r4), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  real(r4), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_r4


pure recursive function unfold_r8(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_r8) :: f !! Unfolding function
  real(r8), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  real(r8), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_r8


pure recursive function unfold_r16(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_r16) :: f !! Unfolding function
  real(r16), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  real(r16), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_r16


pure recursive function unfold_c4(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_c4) :: f !! Unfolding function
  complex(r4), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  complex(r4), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_c4


pure recursive function unfold_c8(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_c8) :: f !! Unfolding function
  complex(r8), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  complex(r8), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_c8


pure recursive function unfold_c16(f, x, len) result(res)
  !! Generates an array of length `len` by unfolding starting
  !! array `x` using input function `f`.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `unfold`.
  procedure(f_c16) :: f !! Unfolding function
  complex(r16), dimension(:), intent(in) :: x !! Start value
  integer(i4), intent(in) :: len !! Array length to return
  complex(r16), dimension(:), allocatable :: res
  if(size(x) >= len)then
    res = x
  else
    res = unfold(f, [x, f(last(x))], len)
  endif
end function unfold_c16


pure function union_i1(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 1-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(i1), dimension(:), intent(in) :: x !! First input array
  integer(i1), dimension(:), intent(in) :: y !! Second input array
  integer(i1), dimension(:), allocatable :: union
  union = set([x, y])
end function union_i1


pure function union_i2(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 2-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(i2), dimension(:), intent(in) :: x !! First input array
  integer(i2), dimension(:), intent(in) :: y !! Second input array
  integer(i2), dimension(:), allocatable :: union
  union = set([x, y])
end function union_i2


pure function union_i4(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(i4), dimension(:), intent(in) :: x !! First input array
  integer(i4), dimension(:), intent(in) :: y !! Second input array
  integer(i4), dimension(:), allocatable :: union
  union = set([x, y])
end function union_i4


pure function union_i8(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte integers.
  !! Overloaded by generic procedure `union`.
  integer(i8), dimension(:), intent(in) :: x !! First input array
  integer(i8), dimension(:), intent(in) :: y !! Second input array
  integer(i8), dimension(:), allocatable :: union
  union = set([x, y])
end function union_i8


pure function union_r4(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte reals.
  !! Overloaded by generic procedure `union`.
  real(r4), dimension(:), intent(in) :: x !! First input array
  real(r4), dimension(:), intent(in) :: y !! Second input array
  real(r4), dimension(:), allocatable :: union
  union = set([x, y])
end function union_r4


pure function union_r8(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte reals.
  !! Overloaded by generic procedure `union`.
  real(r8), dimension(:), intent(in) :: x !! First input array
  real(r8), dimension(:), intent(in) :: y !! Second input array
  real(r8), dimension(:), allocatable :: union
  union = set([x, y])
end function union_r8


pure function union_r16(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 16-byte reals.
  !! Overloaded by generic procedure `union`.
  real(r16), dimension(:), intent(in) :: x !! First input array
  real(r16), dimension(:), intent(in) :: y !! Second input array
  real(r16), dimension(:), allocatable :: union
  union = set([x, y])
end function union_r16


pure function union_c4(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 4-byte complex reals.
  !! Overloaded by generic procedure `union`.
  complex(r4), dimension(:), intent(in) :: x !! First input array
  complex(r4), dimension(:), intent(in) :: y !! Second input array
  complex(r4), dimension(:), allocatable :: union
  union = set([x, y])
end function union_c4


pure function union_c8(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 8-byte complex reals.
  !! Overloaded by generic procedure `union`.
  complex(r8), dimension(:), intent(in) :: x !! First input array
  complex(r8), dimension(:), intent(in) :: y !! Second input array
  complex(r8), dimension(:), allocatable :: union
  union = set([x, y])
end function union_c8


pure function union_c16(x, y) result(union)
  !! Returns a union of two arrays.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `union`.
  complex(r16), dimension(:), intent(in) :: x !! First input array
  complex(r16), dimension(:), intent(in) :: y !! Second input array
  complex(r16), dimension(:), allocatable :: union
  union = set([x, y])
end function union_c16


pure function union_char(x, y) result(union)
  !! Returns a union of two character strings.
  !! This specific procedure is for 16-byte complex reals.
  !! Overloaded by generic procedure `union`.
  character(len=*), intent(in) :: x !! First input array
  character(len=*), intent(in) :: y !! Second input array
  character(len=:), allocatable :: union
  union = set(x // y)
end function union_char

end module functional
