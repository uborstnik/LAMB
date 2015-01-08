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

!> \brief Tests for LAMB data storage and operations.

PROGRAM lamb_tests

#include "lamb_defs.h"

  use lamb_data_d_tests
  USE lamb_error
  use lamb_error_tests
  use lamb_kinds
  use lamb_mp_env_types

  use lamb_matrix_d_tests
  use lamb_multiplication
  use lamb_partition_tests
  use lamb_plexus_tests

  IMPLICIT NONE

  CHARACTER(LEN=*), PARAMETER :: moduleN = 'lamb_test'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb



  type(lamb_error_type) :: error
  integer :: error_handle
  type(lamb_mp_env_type), pointer :: mp_env

  if (.false.) &
       call lamb_error_test_all()

  CALL lamb_error_set (error, error_handle, "lamb_tests")

  write(*,*)"Testing data_d modules."
  call lamb_data_d_test (error)
  write(*,*)"Testing the partition modules."
  call lamb_partition_test (error)

  call lamb_mp_env_new (mp_env, error=error)

  write(*,*)"Testing the plexus modules."
  call lamb_plexus_test (mp_env, error)
  write(*,*)"Testing the matrix_d modules."
  call lamb_matrix_d_test (mp_env, error)

  call lamb_multiply_test (mp_env, error)

  call lamb_mp_env_release (mp_env, error)

  write(*,*)"All tests are done!"

  CALL lamb_error_stop (error, error_handle)

END PROGRAM lamb_tests
