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

!> \brief Methods for Cartesian space partition.

MODULE lamb_partition_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_partition_types
  use lamb_space_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_partition_get_n_cubes
  PUBLIC :: lamb_partition_get_space
  public :: lamb_partition_which_cube
  public :: lamb_partition_cube_convert

  interface lamb_partition_which_cube
     module procedure lamb_partition_which_cube1
  end interface lamb_partition_which_cube

  interface lamb_partition_cube_convert
     module procedure lamb_partition_cube_to_cube_i
  end interface lamb_partition_cube_convert

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'partition_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  pure function lamb_partition_get_n_cubes(partition) result (n_cubes)
    type(lamb_partition_type), intent(in) :: partition
    integer(kind=cube_k) :: n_cubes
    n_cubes = int(partition%ncubes(1),kind=cube_k) &
         * int(partition%ncubes(2),kind=cube_k) &
         * int(partition%ncubes(3),kind=cube_k)
  end function lamb_partition_get_n_cubes

  function lamb_partition_get_space(partition) result (space)
    type(lamb_partition_type), intent(in) :: partition
    type(lamb_space_type), pointer :: space
    space => partition%space
  end function lamb_partition_get_space

  !pure function lamb_partition_which_cube1(partition, coordinates) result (cube)
  function lamb_partition_which_cube1(partition, coordinates) result (cube_i)
    type(lamb_partition_type), intent(in) :: partition
    real(kind=space_k), dimension(3), intent(in) :: coordinates
    integer(kind=cube_k) :: cube_i
    integer(kind=dimcube_k), dimension(3) :: cube
    integer(kind=int_1) :: dim
    ! These coordinates should be wrapped correctly!
    forall (dim = 1:3)
       cube(dim) = mod(int(coordinates(dim) * partition%multiplier(dim), kind=dimcube_k),partition%ncubes(dim))
    end forall
    cube_i = lamb_partition_cube_convert(partition, cube)
  end function lamb_partition_which_cube1

  pure function lamb_partition_cube_to_cube_i(partition, cube) result (cube_i)
    type(lamb_partition_type), intent(in) :: partition
    integer(kind=dimcube_k), dimension(3), intent(in) :: cube
    integer(kind=cube_k) :: cube_i
    cube_i = 1_cube_k + &
         int(cube(1),kind=cube_k)&
         + int(cube(2),kind=cube_k)*int(partition%ncubes(1),kind=cube_k) &
         + int(cube(3),kind=cube_k)*int(partition%ncubes(1),kind=cube_k) * &
         int(partition%ncubes(2),kind=cube_k)
  end function lamb_partition_cube_to_cube_i


END MODULE lamb_partition_methods
