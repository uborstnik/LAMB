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

!> \brief Methods for MPI windows.

!MAKE s d c z i l b w
MODULE lamb_mp_window_X_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_mpi_kinds
  use lamb_mpi_util
  use lamb_mp_window_X_types

  IMPLICIT NONE

#include "mpif.h"

  PRIVATE

  PUBLIC :: lamb_mp_window_X_fence


  PUBLIC :: lamb_mp_window_X_lock
  PUBLIC :: lamb_mp_window_X_unlock

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'mp_window_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS


! *****************************************************************************
!> \brief Locks a window.
!>
!> \param[in,out] mp_window_X  Lock using this window object
!> \param[in] remote           Lock on this remote node.
!> \param[in] shared           (optional) Access is shared. Default is
!>                             exclusive. Toggles between MPI_LOCK_EXCLUSIVE
!>                             and MPI_LOCK_SHARED for the MPI_Win_lock
!>                             exclusivity argument.
!> \param[in] nocheck          (optional) Do not check for others' locks.
!>                             Default is to check. Possibly sets the
!>                             MPI_Win_lock assertion argument to
!>                             MPI_MODE_NOCHECK.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_lock (mp_window_X, remote_node, shared, nocheck, error)
    TYPE(lamb_mp_window_X_type), INTENT(INOUT):: mp_window_X
    integer(kind=proc_k), intent(in) :: remote_node
    logical, intent(in), optional :: shared, nocheck
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_lock', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(the_mpi_integer_kind) :: ierr, exclusivity, assertion, remote_rank

    CALL lamb_error_set (error, error_handle, routineN)

    exclusivity = MPI_LOCK_EXCLUSIVE
    IF (PRESENT (shared)) THEN
       IF (shared) exclusivity = MPI_LOCK_SHARED
    ENDIF
    assertion = 0_the_mpi_integer_kind
    IF (PRESENT (nocheck)) THEN
       IF (nocheck) assertion = MPI_MODE_NOCHECK
    ENDIF

    remote_rank = int(remote_node, kind=the_mpi_integer_kind)
    CALL MPI_WIN_LOCK (exclusivity, remote_rank, assertion, mp_window_X%win_id, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Error locking window.")

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_lock

! *****************************************************************************
!> \brief Unlocks a window.
!>
!> \param[in,out] mp_window_X  Unlock using this window object
!> \param[in] remote           Lock on this remote node.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_unlock (mp_window_X, remote_node,  error)
    TYPE(lamb_mp_window_X_type), INTENT(INOUT):: mp_window_X
    integer(kind=proc_k), intent(in) :: remote_node
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_unlock', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(the_mpi_integer_kind) :: ierr, remote_rank

    CALL lamb_error_set (error, error_handle, routineN)

    remote_rank = int(remote_node, kind=the_mpi_integer_kind)
    CALL MPI_WIN_UNLOCK (remote_rank, mp_window_X%win_id, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Error unlocking window.")

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_unlock

! *****************************************************************************
!> \brief Creates a window epoch
!>
!> \param[in,out] mp_window_X Creates epoch in this window.
!> \param[in] nothing_stored  (optional) Asserts that there were no local
!>                            window updates since last sync. Default is
!>                            .FALSE..
!> \param[in] no_put_follows  (optional) Asserts that nobody will put or
!>                            accumulate data in local data of window until
!>                            next fence. Default is .FALSE..
!> \param[in] no_pre_access   (optional) Asserts that no RMA access calls
!>                            were made since the last fence. All procesess
!>                            must specify the same flag. Default is .FALSE..
!> \param[in] no_post_access  (optional) Asserts that no RMA access will be
!>                            issued until the next fence. All processess must
!>                            specify the same flag. Default is .FALSE..
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_mp_window_X_fence (mp_window_X, nothing_stored, no_put_follows,&
       no_pre_access, no_post_access, error)
    TYPE(lamb_mp_window_X_type), INTENT(INOUT):: mp_window_X
    logical, intent(in), optional :: nothing_stored, no_put_follows,&
         no_pre_access, no_post_access
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_mp_window_X_fence', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle
    integer(the_mpi_integer_kind) :: ierr, assertion

    CALL lamb_error_set (error, error_handle, routineN)

    assertion = 0_the_mpi_integer_kind
    IF (PRESENT (nothing_stored)) THEN
       IF (nothing_stored) assertion = assertion + MPI_MODE_NOSTORE
    ENDIF
    IF (PRESENT (no_put_follows)) THEN
       IF (no_put_follows) assertion = assertion + MPI_MODE_NOPUT
    ENDIF
    IF (PRESENT (no_pre_access)) THEN
       IF (no_pre_access) assertion = assertion + MPI_MODE_NOPRECEDE
    ENDIF
    IF (PRESENT (no_post_access)) THEN
       IF (no_post_access) assertion = assertion + MPI_MODE_NOSUCCEED
    ENDIF
    CALL MPI_WIN_FENCE (assertion, mp_window_X%win_id, ierr)
    call lamb_mpi_check_error (ierr,&
         lamb_here(moduleN, routineN, __LINE__),&
         lamb_failure_level, error,&
         "Error fencing window")

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_mp_window_X_fence


END MODULE lamb_mp_window_X_methods
