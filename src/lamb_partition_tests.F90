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

!> \brief Tests for Cartesian space partitions.

MODULE lamb_partition_tests

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_partition_operations
  use lamb_partition_types
  use lamb_space_types
  use lamb_space_methods

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_partition_test


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'partition_tests'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_partition_test(error)
    type(lamb_error_type), intent(inout) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_test', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    type(lamb_partition_type), pointer :: partition
    type(lamb_space_type), pointer :: space
    integer :: i
    integer(kind=cube_k), dimension(5) :: cube_counts
    data cube_counts/1, 2, 8, 12, 60/

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_space_new_unit_box (space, error)

    do i = 1, size(cube_counts)
       call lamb_partition_calculate (partition, space, cube_counts(i), error)
       call lamb_partition_verify (partition, error)
       write(*,*)"Cube count:", cube_counts(i)
       call lamb_partition_print (partition, error)
       call lamb_partition_release (partition, error)
    enddo

    call lamb_space_release (space, error)

    CALL lamb_error_stop (error, error_handle)

  end subroutine lamb_partition_test

END MODULE lamb_partition_tests
