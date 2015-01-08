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

!> \brief Methods for communication schedules.

MODULE lamb_sched_methods

#include "lamb_defs.h"

  use lamb_constants
  use lamb_kinds
  USE lamb_error
  use lamb_alloc_1d_util
  use lamb_alloc_2d_util
  use lamb_sched_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_sched_ensure_size
  public :: lamb_sched_add_entry
  public :: lamb_sched_print


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'sched_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS


! *****************************************************************************
!> \brief Resizes the array.s
!>
!> \param[in,out] sched  Object for which to resize backing arrayrs
!> \param[in] new_size        How large the new arrays should be.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_sched_ensure_size (sched, new_size, error)
    TYPE(lamb_sched_type), INTENT(INOUT):: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer, intent(in) :: new_size

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_ensure_size', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, n_desc, n_pairs

    if (new_size > sched%n_steps) then
       CALL lamb_error_set (error, error_handle, routineN)
       n_pairs = size(sched%pairs, 1)
       n_desc = size(sched%what, 1)
       call lamb_alloc_1d_ensure_size (sched%actions, new_size, error)
       call lamb_alloc_1d_ensure_size (sched%eras, new_size, error)
       call lamb_alloc_2d_ensure_size (sched%pairs, n_pairs, new_size, error)
       call lamb_alloc_2d_ensure_size (sched%what, n_desc, new_size, error)
    
       CALL lamb_error_stop (error, error_handle)
    endif
  END SUBROUTINE lamb_sched_ensure_size


  SUBROUTINE lamb_sched_add_entry (sched, pair, action, what, era, error)
    TYPE(lamb_sched_type), INTENT(INOUT):: sched
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer, dimension(:), intent(in) :: pair
    integer, dimension(:), intent(in) :: what
    integer, intent(in) :: era
    integer, intent(in) :: action

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_add_entry', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, n, n1

    CALL lamb_error_set (error, error_handle, routineN)
    n = sched%n_steps
    n1 = sched%n_steps + 1
    call lamb_sched_ensure_size (sched, n1, error)
    sched%n_steps = n1

    sched%actions(n1) = lamb_sched_action_end
    sched%eras(n1) = -1
    sched%pairs(:,n1) = -1
    sched%what(:,n1) = -1

    sched%actions(n) = action
    sched%eras(n) = era
    sched%pairs(:,n) = pair
    sched%what(:,n) = what
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_sched_add_entry

  subroutine lamb_sched_print (sched, error)
    TYPE(lamb_sched_type), INTENT(IN):: sched
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_sched_add_entry', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: i, last_era
    character(3) :: action
    character(def_str_len) :: fmt_str

    CALL lamb_error_set (error, error_handle, routineN)
    write(*,*)'schedule'
    write(fmt_str, '(A,I4,A)')&
         '( 1X, I4, "->", I4,1X,A3, ',&
         size(sched%what,1),&
         '(I4))'
    last_era = -1
    do i = 1, sched%n_steps
       select case( sched%actions(i))
       case (lamb_sched_action_end)
          action = 'END'
       case (lamb_sched_action_get)
          action = 'GET'
       case (lamb_sched_action_put)
          action = 'PUT'
       case (lamb_sched_action_snd)
          action = 'SND'
       case (lamb_sched_action_rcv)
          action = 'RCV'
       end select
       if (sched%eras(i) /= last_era) then
          write(*,'( 1X,"-- Era", I4 ," --" )')sched%eras(i)
          last_era = sched%eras(i)
       end if
       write(*,fmt_str)&
            sched%pairs(1,i), sched%pairs(2,i),&
            action, sched%what(:,i)
    enddo
    CALL lamb_error_stop (error, error_handle)    
  end subroutine lamb_sched_print

END MODULE lamb_sched_methods
