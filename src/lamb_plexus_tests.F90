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

!> \brief Functions to test the plexus.

MODULE lamb_plexus_tests

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_partition_operations
  use lamb_partition_types
  use lamb_plexus_operations
  use lamb_plexus_types
  use lamb_space_types
  use lamb_space_methods
  

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_plexus_test


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'plexus_tests'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  subroutine lamb_plexus_test(mp_env, error)
    type(lamb_error_type), intent(inout) :: error
    type(lamb_mp_env_type), optional, pointerin :: mp_env

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_plexus_test', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    type(lamb_space_type), pointer :: space
    type(lamb_partition_type) :: partition
    type(lamb_plexus_type), pointer :: plexus
    integer :: i
    type(lamb_mp_env_type), pointer :: mp

    integer(kind=proc_k), dimension(5) :: p_counts
    data p_counts/1, 3, 6, 15, 28/

    CALL lamb_error_set (error, error_handle, routineN)

    if (present(mp_env)) then
       mp => mp_env
    else
       call lamb_mp_env_new (mp, error=error)
    endif
    call lamb_mp_env_hold(mp, error)

    call lamb_space_new_unit_box (space, error)

    call lamb_plexus_calculate (plexus, space, mp_env, error)

    call lamb_plexus_verify (plexus, error)
    call lamb_plexus_release (plexus, error)

    if (.false.) then
       write(*,*)"Forcing process counts."
       do i = 1, size(p_counts)
          write(*,*)"Using processes:", p_counts(i)
          
          call lamb_plexus_calculate (plexus, space, mp_env, &
               error, force_n_procs=p_counts(i))
          
          call lamb_plexus_verify (plexus, error)
          call lamb_plexus_release (plexus, error)

       enddo
    endif

    call lamb_space_release (space, error)

    call lamb_mp_env_release (mp, error)

    CALL lamb_error_stop (error, error_handle)

  end subroutine lamb_plexus_test

END MODULE lamb_plexus_tests
