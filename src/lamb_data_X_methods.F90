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

!> \brief Methods for data objects.

!MAKE s d c z
MODULE lamb_data_X_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_data_X_types
  use lamb_ptr_1d_X_util

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_data_X_set_len, lamb_data_X_get_len
  public :: lamb_data_X_ensure_length
  public :: lamb_data_X_add_data
  public :: lamb_data_X_pto_pointer

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'data_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS

! *****************************************************************************
!> \brief Defines the (used) length of the data area.
!>
!> Sets up an object for use.
!> \param[in,out] data_X  Object to create
!> \param[in] length  (optional) Set data to this length.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_set_len (data_X, length, error)
    TYPE(lamb_data_X_type), INTENT(INOUT):: data_X
    integer(kind=dataptr_k), intent(in) :: length
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_set_len', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    if (careful_mod) then
       CALL lamb_error_set (error, error_handle, routineN)
       call lamb_assert (int(length,kind=int_big), 'LE', &
            int(size(data_X%d),kind=int_big),&
            lamb_failure_level, lamb_wrong_args,&
            "Requested data length exceeds allocated data size.",&
            routineN, __LINE__, error)
    endif

    data_X%data_len = length

    if (careful_mod) then
       CALL lamb_error_stop (error, error_handle)
    endif
  END SUBROUTINE lamb_data_X_set_len

! *****************************************************************************
!> \brief Gets the used length of data in a data store.
!>
!> \param[in] data_X  Object to query
!> \result length  (optional) Set data to this length.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_data_X_get_len (data_X, error) result (length)
    TYPE(lamb_data_X_type), INTENT(IN):: data_X
    integer(kind=dataptr_k) :: length
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_get_len', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    if (careful_mod) then
       CALL lamb_error_set (error, error_handle, routineN)
    endif

    length = data_X%data_len

    if (careful_mod) then
       CALL lamb_error_stop (error, error_handle)
    endif
  END FUNCTION lamb_data_X_get_len


! *****************************************************************************
!> \brief Defines the (used) length of the data area.
!>
!> Sets up an object for use.
!> \param[in,out] data_X  Object to create
!> \param[in] length  (optional) Set data to this length.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_ensure_length (data_X, length, error)
    TYPE(lamb_data_X_type), INTENT(INOUT):: data_X
    integer(kind=dataptr_k), intent(in) :: length
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_ensure_length', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    if (careful_mod) then
       CALL lamb_error_set (error, error_handle, routineN)
       call lamb_assert(data_X%mine,&
            lamb_warning_level, lamb_caller_error,&
            "Attempting to resize foreign pointer.",&
            routineN, __LINE__, error)
    endif

    call lamb_ptr_1d_X_ensure_size (data_X%d, length, error)
    call lamb_data_X_set_len (data_X, length, error)

    if (careful_mod) then
       CALL lamb_error_stop (error, error_handle)
    endif
  END SUBROUTINE lamb_data_X_ensure_length


! *****************************************************************************
!> \brief Adds data to the end of the current data area.
!>
!> Sets up an object for use.
!> \param[in,out] data_X  Object to create
!> \param[in] data_length  Amount of data to add
!> \param[in] data         Data to add
!> \param[out] added_pos   Starting position of where the data was added
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_data_X_add_data (data_X, data_length, data, added_pos, error)
    TYPE(lamb_data_X_type), INTENT(INOUT):: data_X
    integer(kind=dataptr_k), intent(in) :: data_length
    doubleprecision, dimension(*), intent(in) :: data
    integer(kind=dataptr_k), intent(out) :: added_pos
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_add_data', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=dataptr_k) :: new_size

    if (careful_mod) then
       CALL lamb_error_set (error, error_handle, routineN)
    endif

    added_pos = lamb_data_X_get_len (data_X, error)
    new_size = added_pos + data_length
    added_pos = added_pos + 1_dataptr_k
    call lamb_ptr_1d_X_ensure_size (data_X%d, new_size, error)
    call lamb_data_X_set_len (data_X, data_length, error)

    data_X%d(added_pos:new_size) = data(1:data_length)

    if (careful_mod) then
       CALL lamb_error_stop (error, error_handle)
    endif
  END SUBROUTINE lamb_data_X_add_data

  SUBROUTINE lamb_data_X_pto_pointer (data_X, ptr, error)
    TYPE(lamb_data_X_type), INTENT(INOUT):: data_X
    doubleprecision, dimension(:), POINTERIN :: ptr
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_data_X_pto_pointer',&
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    CALL lamb_error_set (error, error_handle, routineN)

    if (data_X%mine) then
       deallocate (data_X%d, stat=stat)
       call lamb_memory_check (stat, "data_X%d", error)
       data_X%mine = .FALSE.
    endif
    data_X%d => ptr
    data_X%data_len = size(ptr,kind=dataptr_k)

    CALL lamb_error_stop (error, error_handle)
  end SUBROUTINE lamb_data_X_pto_pointer

END MODULE lamb_data_X_methods
