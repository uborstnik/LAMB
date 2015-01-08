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

!> \brief Methods for the plexus class.

MODULE lamb_plexus_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_partition_types
  use lamb_plexus_types
  use lamb_space_types

  IMPLICIT NONE

  PRIVATE

  public :: lamb_plexus_get_partition
  public :: lamb_plexus_tuple_get_proc
  public :: lamb_plexus_get_sector_i

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'plexus_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  function lamb_plexus_get_partition(plexus, error) result (partition)
    TYPE(lamb_plexus_type), intent(in) :: plexus
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_partition_type), POINTER :: partition

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_get_partition', &
         routineP = moduleN//':'//routineN

    partition => plexus%partition
  end function lamb_plexus_get_partition

  !subroutine lamb_plexus_get_home(plexus, subsets, sector, p, error)
  !  type(lamb_plexus_type), intent(in) :: plexus
  !  integer(kind=subset_k), dimension(:), intent(in) :: subsets
  !  integer(kind=sector_k), intent(out) :: sector
  !  integer(kind=proc_k), intent(out), optional :: p
  !  type(lamb_error_type), intent(inout) :: error
  !  CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_get_home', &
  !       routineP = moduleN//':'//routineN
  !  integer :: error_handle
  !  call lamb_error_set (error, error_handle, routineN)
  !  sector = plexus%map(subsets(1),subsets(2))
  !  p = sector
  !  call lamb_error_stop (error, error_handle)
  !end subroutine lamb_plexus_get_home

  function lamb_plexus_tuple_get_proc(plexus, subsets, local_index, error)&
       result(p)
    type(lamb_plexus_type), intent(in) :: plexus
    integer(kind=subset_k), dimension(:), intent(in) :: subsets
    integer(kind=locatm_k), dimension(:), intent(in) :: local_index
    type(lamb_error_type), intent(inout) :: error
    integer(kind=proc_k) :: p
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_tuple_get_p', &
         routineP = moduleN//':'//routineN
    integer :: error_handle
    if (careful_mod) &
         call lamb_error_set (error, error_handle, routineN)
    !map realms are sectors
    p = plexus%realm_map(subsets(1),subsets(2))
    if (careful_mod) &
         call lamb_error_stop (error, error_handle)
  end function lamb_plexus_tuple_get_proc


  !> \brief Finds the local sector index for a subset tuple.
  !> \retval Local sector index or 0 if not found.
  !!!scale Searching is not efficient.
  function lamb_plexus_get_sector_i(plexus, subsets, error)&
       result(sector_i)
    type(lamb_plexus_type), intent(in) :: plexus
    integer(kind=subset_k), dimension(:), intent(in) :: subsets
    type(lamb_error_type), intent(inout) :: error
    integer(kind=cube_k) :: sector_i
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_get_sector_i', &
         routineP = moduleN//':'//routineN
    integer :: error_handle
    integer(kind=cube_k) :: i
    if (careful_mod) &
         call lamb_error_set (error, error_handle, routineN)
    sector_i = 0
    do i = 1_cube_k, plexus%my_sectors_size
       if (all(plexus%my_sectors(:,i) == subsets(:))) then
          sector_i = i
          exit
       endif
    enddo
    if (careful_mod) &
         call lamb_error_stop (error, error_handle)
  end function lamb_plexus_get_sector_i


END MODULE lamb_plexus_methods
