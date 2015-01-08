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

!> \brief Object encapsulation for rank-2 arrays of type _X.

MODULE lamb_array_2d_X_types
!MAKE: d s c z i l b w

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_array_2d_X_type
  PUBLIC :: lamb_array_2d_X_type_p

  PUBLIC :: lamb_array_2d_X_create
  PUBLIC :: lamb_array_2d_X_destroy

  PUBLIC :: lamb_array_2d_X_new
  PUBLIC :: lamb_array_2d_X_hold
  PUBLIC :: lamb_array_2d_X_release

  PUBLIC :: lamb_array_2d_X_valid

  PUBLIC :: lamb_array_2d_X_verify


  interface lamb_array_2d_X_create
     module procedure lamb_array_2d_X_create_fresh_i8, &
          lamb_array_2d_X_create_fresh_i4, &
          lamb_array_2d_X_create_gift
  end interface lamb_array_2d_X_create
  public :: lamb_array_2d_X_create_fresh_i8, &
          lamb_array_2d_X_create_fresh_i4, &
          lamb_array_2d_X_create_gift

  interface lamb_array_2d_X_new
     module procedure lamb_array_2d_X_new_fresh_i8, &
          lamb_array_2d_X_new_fresh_i4, &
          lamb_array_2d_X_new_gift
  end interface lamb_array_2d_X_new
  public :: lamb_array_2d_X_new_fresh_i8, &
          lamb_array_2d_X_new_fresh_i4, &
          lamb_array_2d_X_new_gift


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'array_2d_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: array_2d_X_id


  TYPE lamb_array_2d_X_type
     INTEGER :: refcount
     INTEGER :: id
     doubleprecision, dimension(:,:), pointer :: d
     integer :: n
  END TYPE lamb_array_2d_X_type

  TYPE lamb_array_2d_X_type_p
     TYPE(lamb_array_2d_X_type), POINTER :: p
  END TYPE lamb_array_2d_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out]array_2d_X  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_array_2d_X_create_fresh_i8 (array_2d_X, n, error)
    TYPE(lamb_array_2d_X_type), INTENT(OUT)::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_8), dimension(2), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

   array_2d_X%id = 0
   array_2d_X%refcount = 0

   allocate (array_2d_X%d(n(1), n(2)), stat=stat)
   call lamb_memory_check (stat, "array_2d_X", n(1)*n(2), error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_create_fresh_i8

  SUBROUTINE lamb_array_2d_X_create_fresh_i4 (array_2d_X, n, error)
    TYPE(lamb_array_2d_X_type), INTENT(OUT)::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_4), dimension(2), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

   array_2d_X%id = 0
   array_2d_X%refcount = 0

   allocate (array_2d_X%d(n(1), n(2)), stat=stat)
   call lamb_memory_check (stat, "array_2d_X", n(1)*n(2), error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_create_fresh_i4

  SUBROUTINE lamb_array_2d_X_create_gift (array_2d_X, data, error)
    TYPE(lamb_array_2d_X_type), INTENT(OUT)::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    doubleprecision, dimension(:,:), pointer :: data

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    array_2d_X%id = 0
    array_2d_X%refcount = 0

    if (careful_mod) then
       call lamb_assert (associated (data),&
            lamb_fatal_level, lamb_wrong_args,&
            "Passed data pointer is not associated.",&
            routineN, __LINE__, error=error)
    endif
    array_2d_X%d => data
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_create_gift


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out]array_2d_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_array_2d_X_destroy (array_2d_X, error)
    TYPE(lamb_array_2d_X_type), INTENT(INOUT) ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    deallocate (array_2d_X%d, stat=stat)
    call lamb_memory_check (stat, "array_2d_X", error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out]array_2d_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_array_2d_X_new_fresh_i8 (array_2d_X, n, error)
    TYPE(lamb_array_2d_X_type), POINTEROUT ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_8), dimension(2), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (array_2d_X, stat=stat)
    CALL lamb_memory_check (stat, "array_2d_X", -1, error)

    CALL lamb_array_2d_X_create (array_2d_X, n, error)
   array_2d_X%refcount = 1
   array_2d_X%id =array_2d_X_id
   array_2d_X_id =array_2d_X_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_new_fresh_i8

  SUBROUTINE lamb_array_2d_X_new_fresh_i4 (array_2d_X, n, error)
    TYPE(lamb_array_2d_X_type), POINTEROUT ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_4), dimension(2), intent(in) :: n

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (array_2d_X, stat=stat)
    CALL lamb_memory_check (stat, "array_2d_X", -1, error)

    CALL lamb_array_2d_X_create (array_2d_X, n, error)
   array_2d_X%refcount = 1
   array_2d_X%id =array_2d_X_id
   array_2d_X_id =array_2d_X_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_new_fresh_i4

  SUBROUTINE lamb_array_2d_X_new_gift (array_2d_X, data, error)
    TYPE(lamb_array_2d_X_type), POINTEROUT ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    doubleprecision, dimension(:,:), pointer :: data

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (array_2d_X, stat=stat)
    CALL lamb_memory_check (stat, "array_2d_X", -1, error)

    CALL lamb_array_2d_X_create (array_2d_X, data, error)
   array_2d_X%refcount = 1
   array_2d_X%id =array_2d_X_id
   array_2d_X_id =array_2d_X_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_new_gift


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out]array_2d_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_array_2d_X_hold (array_2d_X, error)
    TYPE(lamb_array_2d_X_type), POINTERINOUT ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

   array_2d_X%refcount =array_2d_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out]array_2d_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_array_2d_X_release (array_2d_X, error)
    TYPE(lamb_array_2d_X_type), POINTERINOUT ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

   array_2d_X%refcount =array_2d_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (array_2d_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (array_2d_X%refcount <= 0) THEN
       CALL lamb_array_2d_X_destroy (array_2d_X, error)
       DEALLOCATE (array_2d_X, stat=stat)
       CALL lamb_memory_check (stat, "array_2d_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in]array_2d_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_array_2d_X_valid (array_2d_X, error) RESULT (valid)
    TYPE(lamb_array_2d_X_type), POINTERIN ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (array_2d_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_array_2d_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in]array_2d_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_array_2d_X_verify (array_2d_X, error)
    TYPE(lamb_array_2d_X_type), INTENT(IN) ::array_2d_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_array_2d_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_array_2d_X_verify

END MODULE lamb_array_2d_X_types
