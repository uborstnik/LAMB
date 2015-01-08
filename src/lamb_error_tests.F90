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

!> \brief Routines to test the data types, subroutines, and functions.

MODULE lamb_error_tests

#include "lamb_defs.h"

  USE lamb_error

  IMPLICIT NONE

  PRIVATE

  public :: lamb_error_test_all

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'error_tests'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS

  subroutine lamb_error_test_all()
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_error_test_all', &
         routineP = moduleN//':'//routineN
    type(lamb_error_type) :: error
    integer :: error_handle
    logical :: e

    call error_basic()
    call error_test_default()
    call error_test_assert()
  end subroutine lamb_error_test_all

  subroutine error_basic()
    CHARACTER(LEN=*), PARAMETER :: routineN = 'error_basic', &
         routineP = moduleN//':'//routineN
    type(lamb_error_type) :: error
    integer :: error_handle
    logical :: e

    write(*,*)"Testing basic set/stop"
    call lamb_error_set(error, error_handle, routineN)    
    call lamb_error_stop(error, error_handle)

  end subroutine error_basic

  subroutine error_test_default()
    CHARACTER(LEN=*), PARAMETER :: routineN = 'error_test_default', &
         routineP = moduleN//':'//routineN
    type(lamb_error_type) :: error
    integer :: error_handle, level
    logical :: e
    write(*,*)"Testing default test."
    call lamb_error_set(error, error_handle, routineN)
    e = lamb_error_not_ok (error)
    write(*,*)"Error happened?", e
    e = lamb_error_not_ok (error, level)
    write(*,*)"Error at level", level, "."
    call lamb_error_stop(error, error_handle)
  end subroutine error_test_default

  subroutine error_test_assert()
    CHARACTER(LEN=*), PARAMETER :: routineN = 'error_test_assert', &
         routineP = moduleN//':'//routineN
    type(lamb_error_type) :: error
    integer :: error_handle, level
    logical :: e
    call lamb_error_set(error, error_handle, routineN)
    write(*,*)"Testing error at note level"
    call lamb_assert (.FALSE., lamb_note_level, lamb_internal_error, routineN,&
         "Note level error.", __LINE__, error)
    write(*,*)"Error happened?", e
    e = lamb_error_not_ok (error, level)
    write(*,*)"Error at level", level, "."
    if (.false.) then
       call lamb_assert (.FALSE., &
            lamb_failure_level, lamb_internal_error, routineN,&
            "Failure level error.", __LINE__, error)
       write(*,*)"Error happened?", e
       e = lamb_error_not_ok (error, level)
       write(*,*)"Error at level", level, "."
    endif
    write(*,*)"Testing die level of lamb_failure_level+1 and error at failure level"
    call lamb_error_set_die_level(error, lamb_failure_level+1)
    call lamb_assert (.FALSE., &
         lamb_failure_level, lamb_internal_error, routineN,&
         "Failure level error.", __LINE__, error)
    write(*,*)"Error happened?", e
    e = lamb_error_not_ok (error, level)
    write(*,*)"Error at level", level, "."
    call lamb_error_set_die_level(error)
    
    call lamb_error_stop(error, error_handle)
  end subroutine error_test_assert
  

END MODULE lamb_error_tests
