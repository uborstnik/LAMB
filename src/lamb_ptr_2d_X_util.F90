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

!> \brief Utilities functions for rank-2 POINTER types (here _X).

MODULE lamb_ptr_2d_X_util
!MAKE: d s c z i l b w

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_constants

  IMPLICIT NONE

  PRIVATE

  public :: lamb_ptr_2d_X_ensure_size
  public :: lamb_ptr_2d_X_ensure_size_i8,&
            lamb_ptr_2d_X_ensure_size_i4

  interface lamb_ptr_2d_X_ensure_size
     module procedure lamb_ptr_2d_X_ensure_size_i8,&
          lamb_ptr_2d_X_ensure_size_i4
  end interface  lamb_ptr_2d_X_ensure_size

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'ptr_2d_X_util'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

! *****************************************************************************
!> \brief Resizes an array to at least the requested size.
!>
!> \param[in,out] array_2d_X  Low-level array to resize
!> \param[in] n Resize array to at least this size.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_ptr_2d_X_ensure_size_i8 (array_2d_X, n1, n2, error)
    doubleprecision, dimension(:,:), POINTERINOUT :: array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_8), intent(in) :: n1, n2

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_ptr_2d_X_ensure_size_i4', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    doubleprecision, dimension(:,:), pointer :: new_array
    integer(kind=int_8) :: new_n1, new_n2, old_n1, old_n2

    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)

    if (associated (array_2d_X)) then
       old_n1 = int(size(array_2d_X,1),int_8)
       old_n2 = int(size(array_2d_X,2),int_8)
       if (old_n1 .NE. n1 .or. old_n2 .LT. n2) then
          new_n1 = n1
          new_n2 = max(int(real(old_n2)*scale_factor,kind=int_8), n2)
          allocate (new_array(new_n1, new_n2), stat=stat)
          call lamb_memory_check (stat, "new_array",&
               int(new_n1,kind=int_big)*int(new_n2,kind=int_big), error)
          call low_copy(array_2d_X, new_array(1:old_n1,1:old_n2),&
               int(old_n1,kind=int_big), int(old_n2,kind=int_big))
          deallocate (array_2d_X, stat=stat)
          call lamb_memory_check (stat, "array_2d_X", error)
          array_2d_X => new_array
       endif
    else
       allocate (array_2d_X(n1,n2), stat=stat)
       call lamb_memory_check (stat, "array_2d_X",&
            int(n1,kind=int_big)*int(n2,kind=int_big), error)
    endif
    
    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_ptr_2d_X_ensure_size_i8

! *****************************************************************************
!> \brief Resizes an array to at least the requested size.
!>
!> \param[in,out] array_2d_X  Low-level array to resize
!> \param[in] n Resize array to at least this size.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_ptr_2d_X_ensure_size_i4 (array_2d_X, n1, n2, error)
    doubleprecision, dimension(:,:), POINTERINOUT :: array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_4), intent(in) :: n1, n2

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_ptr_2d_X_ensure_size_i4', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    doubleprecision, dimension(:,:), pointer :: new_array
    integer(kind=int_4) :: new_n1, new_n2, old_n1, old_n2

    if (careful_mod) &
         CALL lamb_error_set (error, error_handle, routineN)

    if (associated (array_2d_X)) then
       old_n1 = int(size(array_2d_X,1),int_4)
       old_n2 = int(size(array_2d_X,2),int_4)
       if (old_n1 .NE. n1 .or. old_n2 .LT. n2) then
          new_n1 = n1
          new_n2 = max(int(real(old_n2)*scale_factor,kind=int_4), n2)
          allocate (new_array(new_n1, new_n2), stat=stat)
          call lamb_memory_check (stat, "new_array",&
               int(new_n1,kind=int_big)*int(new_n2,kind=int_big), error)
          call low_copy(array_2d_X, new_array(1:old_n1,1:old_n2),&
               int(old_n1,kind=int_big), int(old_n2,kind=int_big))
          deallocate (array_2d_X, stat=stat)
          call lamb_memory_check (stat, "array_2d_X", error)
          array_2d_X => new_array
       endif
    else
       allocate (array_2d_X(n1,n2), stat=stat)
       call lamb_memory_check (stat, "array_2d_X",&
            int(n1,kind=int_big)*int(n2,kind=int_big), error)
    endif
    
    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_ptr_2d_X_ensure_size_i4

  pure subroutine low_copy(src, dst, n1, n2)
    doubleprecision, dimension(n1,*), intent(in) :: src
    doubleprecision, dimension(n1,*), intent(out) :: dst
    integer(kind=int_big), intent(in) :: n1, n2
    dst(:,1:n2) = src(:,1:n2)
  end subroutine low_copy

END MODULE lamb_ptr_2d_X_util
