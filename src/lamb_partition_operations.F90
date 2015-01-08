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

!> \brief Operations on Cartesian space partitions.

MODULE lamb_partition_operations

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_partition_types
  use lamb_space_types
  

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_partition_calculate


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'partition_operations'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  !> Generates a partition for a relatively dense matrix.
  subroutine lamb_partition_calculate(partition, space, n_cubes, error)
    type(lamb_partition_type), POINTEROUT :: partition
    type(lamb_space_type), POINTERIN :: space
    integer(kind=cube_k), intent(in) :: n_cubes
    type(lamb_error_type), intent(inout) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_partition_calculate', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, dim
    real :: a, b, c
    integer, dimension(3) :: dims
    integer :: i, p, n, product, tgt
    integer(kind=dimcube_k), dimension(3) :: ds

    CALL lamb_error_set (error, error_handle, routineN)

    n = int(n_cubes)
    ! Distributes the factors among the three dimensions.
    dims(:) = 1
    p = 1
    product = 1
    i = 2
    tgt = n
    do while (product < n)
       if (mod(tgt,i) .eq. 0) then
          dims(p) = dims(p) * i
          product = product * i
          p = mod(p,3)+1
          if (product >= n) then
             exit
          end if
          tgt = tgt / i
       else
          i = i + 1
       endif
    end do
    do i = 1, 3
       call lamb_assert (dims(i), "LE", int(HUGE(0_cube_k)),&
            lamb_failure_level, lamb_internal_error, &
            "Cube kind too small to hold calculated cube counts.", &
            routineN, __LINE__, error=error)
    enddo
    ds = int(dims, kind=dimcube_k)
    call lamb_partition_new (partition, space, ds, error=error)
    call lamb_error_stop (error, error_handle)
  end subroutine lamb_partition_calculate

END MODULE lamb_partition_operations
