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

!> \brief Utilities for sector objects.

!MAKE s d c z
MODULE lamb_sector_X_util

#include "lamb_defs.h"

  use lamb_error
  use lamb_data_types
  use lamb_data_X_types
  use lamb_data_X_methods

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: sector_X_resize_stores

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'sector_X_util'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS
  subroutine sector_X_resize_stores(data_stores, new_size, error)
    type(lamb_data_X_type), dimension(:), allocatable, intent(inout) :: data_stores
    integer, intent(in) :: new_size
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'sector_X_resize_stores', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, old_size, i
    type(lamb_data_X_type), dimension(:), allocatable :: tmp_stores
    CALL lamb_error_set (error, error_handle, routineN)
    old_size = size(data_stores)
    if (new_size > old_size) then
       allocate(tmp_stores(old_size), stat=stat)
       call lamb_memory_check(stat, "tmp_stores", old_size, error)
       tmp_stores(1:old_size) = data_stores(1:old_size)
       deallocate (data_stores, stat=stat)
       call lamb_memory_check(stat, "data_stores", error)
       allocate(data_stores(new_size), stat=stat)
       call lamb_memory_check(stat, "data_stores", new_size, error)
       data_stores(1:old_size) = tmp_stores(1:old_size)
       do i = old_size+1, new_size
          call lamb_data_X_create(data_stores(i), error=error)
       enddo
    elseif (new_size < old_size) then
       do i = new_size+1, old_size
          call lamb_data_X_destroy(data_stores(i), error=error)
       enddo
       allocate(tmp_stores(new_size), stat=stat)
       call lamb_memory_check(stat, "tmp_stores", new_size, error)
       tmp_stores(1:new_size) = data_stores(1:new_size)
       deallocate (data_stores, stat=stat)
       call lamb_memory_check(stat, "data_stores", error)
       allocate(data_stores(new_size), stat=stat)
       call lamb_memory_check(stat, "data_stores", new_size, error)
       data_stores(1:new_size) = tmp_stores(1:new_size)
    endif
    CALL lamb_error_stop (error, error_handle)
  end subroutine sector_X_resize_stores

END MODULE lamb_sector_X_util
