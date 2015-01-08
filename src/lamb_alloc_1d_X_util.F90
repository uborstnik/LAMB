!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012, 2013, 2014 Urban Borstnik.
!
! The LAMB library is free software: you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation, either version 2 of the
! License, or (at your option) any later version.
!
! LAMB is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with LAMB.  If not, see <http://www.gnu.org/licenses/>.
!
! If you want to redistribute modifications, please note that derived
! work must not be called LAMB, Lamb, lamb, libLAMB, libLamb, liblamb
! nor any other case variation.  Details are found in the README &
! COPYING files.  If they are missing, get the official version at the
! http://lamb.borstnik.net/ website.
!
! We ask you to cite the published articles on this software.  You can
! find a list in the README file in the main directory.
!
!******************************************************************************

!> \brief Memory utilities for ALLOCATABLE rank-1 types (here _X).

MODULE lamb_alloc_1d_X_util
!MAKE: d s c z i l b w

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_constants
  use lamb_1d_X_util

  IMPLICIT NONE

  PRIVATE
  public :: lamb_alloc_1d_X_ensure_size
  public :: lamb_alloc_1d_X_ensure_size_i8,&
            lamb_alloc_1d_X_ensure_size_i4
  public :: lamb_alloc_1d_X_copy

  interface lamb_alloc_1d_X_ensure_size
     module procedure lamb_alloc_1d_X_ensure_size_i8,&
          lamb_alloc_1d_X_ensure_size_i4
  end interface  lamb_alloc_1d_X_ensure_size

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'alloc_1d_X_util'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

! *****************************************************************************
!> \brief Resizes an array to at least the requested size.
!>
!> \param[in,out] alloc_1d_X  Low-level array to resize
!> \param[in] n Resize array to at least this size.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_alloc_1d_X_ensure_size_i8 (alloc_1d_X, n, error)
    doubleprecision, dimension(:), allocatable :: alloc_1d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_8), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_alloc_1d_X_ensure_size_i4', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    doubleprecision, dimension(:), allocatable :: new_allocatable
    integer(kind=int_8) :: new_n, old_n

    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)

    if (allocated (alloc_1d_X)) then
       old_n = int(size(alloc_1d_X),kind=int_8)
       if (old_n .LT. n) then
          new_n = max(int(real(old_n)*scale_factor,kind=int_8), n)
          allocate (new_allocatable(old_n), stat=stat)
          call lamb_memory_check (stat, "new_allocatable", old_n, error)
          call lamb_1d_X_copy(alloc_1d_X, new_allocatable, int(old_n,kind=int_big))
          deallocate (alloc_1d_X, stat=stat)
          call lamb_memory_check (stat, "alloc_1d_X", error)
          allocate (alloc_1d_X(new_n), stat=stat)
          call lamb_memory_check (stat, "alloc_1d_X", n, error)
          call lamb_1d_X_copy(new_allocatable, alloc_1d_X, int(old_n,kind=int_big))
          deallocate (new_allocatable, stat=stat)
          call lamb_memory_check (stat, "new_allocatable", error)
       endif
    else
       allocate (alloc_1d_X(n), stat=stat)
       call lamb_memory_check (stat, "alloc_1d_X", n, error)
    endif

    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_alloc_1d_X_ensure_size_i8


! *****************************************************************************
!> \brief Resizes an array to at least the requested size.
!>
!> \param[in,out] alloc_1d_X  Low-level array to resize
!> \param[in] n Resize array to at least this size.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_alloc_1d_X_ensure_size_i4 (alloc_1d_X, n, error)
    doubleprecision, dimension(:), allocatable :: alloc_1d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_4), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_alloc_1d_X_ensure_size_i4', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    doubleprecision, dimension(:), pointer :: new_allocatable
    integer(kind=int_4) :: new_n, old_n

    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)

    if (allocated (alloc_1d_X)) then
       old_n = int(size(alloc_1d_X),kind=int_4)
       if (old_n .LT. n) then
          new_n = max(int(real(old_n)*scale_factor,kind=int_4), n)
          allocate (new_allocatable(new_n), stat=stat)
          call lamb_memory_check (stat, "alloc_1d_X", n, error)
          call lamb_1d_X_copy(alloc_1d_X, new_allocatable, int(old_n,kind=int_big))
          deallocate (alloc_1d_X, stat=stat)
          call lamb_memory_check (stat, "alloc_1d_X", error)
          allocate (alloc_1d_X(new_n), stat=stat)
          call lamb_memory_check (stat, "alloc_1d_X", n, error)
          call lamb_1d_X_copy(new_allocatable, alloc_1d_X, int(old_n,kind=int_big))
          deallocate (new_allocatable, stat=stat)
          call lamb_memory_check (stat, "new_allocatable", error)
       endif
    else
       allocate (alloc_1d_X(n), stat=stat)
       call lamb_memory_check (stat, "alloc_1d_X", n, error)
    endif
    
    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_alloc_1d_X_ensure_size_i4

  subroutine lamb_alloc_1d_X_copy(src, dst, n, error)
    doubleprecision, dimension(:) :: src
    doubleprecision, dimension(:), allocatable, intent(inout) :: dst
    integer(kind=int_big), intent(in), optional :: n
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_alloc_1d_X_copy', &
         routineP = moduleN//':'//routineN
    integer :: error_handle
    integer(kind=int_big) :: new_n
    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)
    if (present (n)) then
       new_n = n
    else
       new_n = size(src,kind=int_big)
    endif
    call lamb_alloc_1d_X_ensure_size (dst, new_n, error)
    call lamb_1d_X_copy (src, dst, new_n)
    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_alloc_1d_X_copy

  !pure subroutine low_copy(src, dst, n)
  !  doubleprecision, dimension(*), intent(in) :: src
  !  doubleprecision, dimension(*), intent(out) :: dst
  !  integer(kind=int_big), intent(in) :: n
  !  dst(1:n) = src(1:n)
  !end subroutine low_copy

END MODULE lamb_alloc_1d_X_util
