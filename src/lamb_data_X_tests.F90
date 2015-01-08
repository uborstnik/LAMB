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

!> \brief Tests for data objects.

!MAKE s d c z
MODULE lamb_data_X_tests

#include "lamb_defs.h"

  use lamb_data_X_types
  USE lamb_error
  use lamb_kinds

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_data_X_test

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'lamb_data_X_tests'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: data_X_id


CONTAINS

! *****************************************************************************
!> \brief Tests the lamb_data_X module.
!>
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_test (error)
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_test', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    type(lamb_data_X_type), pointer :: d

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_data_X_new (d, error=error)
    call lamb_data_X_hold (d, error=error)
    call lamb_data_X_verify (d, error=error)
    call lamb_data_X_release (d, error=error)
    call lamb_data_X_release (d, error=error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_data_X_test

END MODULE lamb_data_X_tests
