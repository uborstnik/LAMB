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

!> \brief Methods for the set class.

MODULE lamb_set_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_set_types

  IMPLICIT NONE

  PRIVATE

  public :: lamb_set_get_n_elements
  public :: lamb_set_update_coordinates
  public :: lamb_set_get_coordinates

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'set_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

  pure function lamb_set_get_n_elements(set) result (n_elements)
    type(lamb_set_type), intent(in) :: set
    integer(allatm_k) :: n_elements
    n_elements = set%n
  end function lamb_set_get_n_elements

  subroutine lamb_set_update_coordinates(set, all_coordinates, error)
    type(lamb_set_type), intent(inout) :: set
    real(kind=space_k), dimension(:,:), intent(in) :: all_coordinates
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_update_coordinates', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    set%coordinates(:,:) = all_coordinates(:,:)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_set_update_coordinates

  function lamb_set_get_coordinates(set, element, error) result (coordinates)
    type(lamb_set_type), intent(in) :: set
    integer(kind=allatm_k), intent(in) :: element
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    real(kind=space_k), dimension(3) :: coordinates

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_set_get_coordinates', &
         routineP = moduleN//':'//routineN

    integer :: error_handle

    if (careful_mod) then
       CALL lamb_error_set (error, error_handle, routineN)
       call lamb_assert(element, "LE", lamb_set_get_n_elements(set),&
            lamb_failure_level, lamb_wrong_args,&
            "Element number out of bounds.",&
            routineN, __LINE__, error)
    end if

    coordinates(:) = set%coordinates(:,element)

    if (careful_mod) &
         CALL lamb_error_stop (error, error_handle)
  END function lamb_set_get_coordinates
  

END MODULE lamb_set_methods
