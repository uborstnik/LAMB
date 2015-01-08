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

!> \brief A class to decribe Cartesian space.

MODULE lamb_space_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_space_type
  PUBLIC :: lamb_space_type_p

  PUBLIC :: lamb_space_create
  PUBLIC :: lamb_space_destroy

  PUBLIC :: lamb_space_new
  PUBLIC :: lamb_space_hold
  PUBLIC :: lamb_space_release

  PUBLIC :: lamb_space_valid

  PUBLIC :: lamb_space_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'space_types'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: space_id


  !> Description of space
  !> \par Inspired by CP2K
  TYPE lamb_space_type
     INTEGER :: refcount
     INTEGER :: id
     logical :: orthorhombic
     real(kind=space_k), dimension(3,3) :: h, h_inv
     integer, dimension(3) :: s
  END TYPE lamb_space_type

  TYPE lamb_space_type_p
     TYPE(lamb_space_type), POINTER :: p
  END TYPE lamb_space_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] space  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_space_create (space, h, error)
    TYPE(lamb_space_type), INTENT(OUT):: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    real(kind=space_k), dimension(:,:), intent(in) :: h

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    space%id = 0
    space%refcount = 0

    space%orthorhombic = is_orthorhombic (h)
    space%h(:,:) = h
    space%h_inv = real(inv_3x3(real(h, kind=dp)), kind=space_k)
    space%s = 1
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_space_create


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] space  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_space_destroy (space, error)
    TYPE(lamb_space_type), INTENT(INOUT) :: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_space_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] space  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_space_new (space, h, error)
    TYPE(lamb_space_type), POINTEROUT :: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    real(kind=space_k), dimension(:,:), intent(in) :: h

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (space, stat=stat)
    CALL lamb_memory_check (stat, "space", -1, error)

    CALL lamb_space_create (space, h, error)
    space%refcount = 1
    space%id = space_id
    space_id = space_id + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_space_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] space  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_space_hold (space, error)
    TYPE(lamb_space_type), POINTERINOUT :: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    space%refcount = space%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_space_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] space  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_space_release (space, error)
    TYPE(lamb_space_type), POINTERINOUT :: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    space%refcount = space%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (space%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            "Reference count below 0.", routineN, __LINE__, error)
    ENDIF
    IF (space%refcount <= 0) THEN
       CALL lamb_space_destroy (space, error)
       DEALLOCATE (space, stat=stat)
       CALL lamb_memory_check (stat, "space", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_space_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] space  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_space_valid (space, error) RESULT (valid)
    TYPE(lamb_space_type), POINTERIN :: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (space)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_space_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] space  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_space_verify (space, error)
    TYPE(lamb_space_type), INTENT(IN) :: space
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_space_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_space_verify

! *****************************************************************************
!> \brief   Determines whether a cell is orthorhombic.
!> \author UB
!> \date 2012-010-23
! *****************************************************************************
  pure logical function is_orthorhombic (mat)
    real(kind=space_k), dimension(3,3), intent(in) :: mat
    is_orthorhombic = all (mat(2:3,1) == 0.0) .and. &
         mat(1,2) == 0.0 .and. mat(3,2) == 0.0 .and. &
         all (mat(1:2,3) == 0.0)
  end function is_orthorhombic

! *****************************************************************************
!> \brief   Returns the determinant of a 3x3 matrix.
!> \author  MK
!> \date    13.03.2004
!> \version 1.0
! *****************************************************************************
  PURE FUNCTION det_3x3(a) RESULT(det_a)
    REAL(KIND=dp), DIMENSION(3, 3), &
      INTENT(IN)                             :: a
    REAL(KIND=dp)                            :: det_a

    det_a = a(1,1)*(a(2,2)*a(3,3) - a(2,3)*a(3,2)) +&
            a(1,2)*(a(2,3)*a(3,1) - a(2,1)*a(3,3)) +&
            a(1,3)*(a(2,1)*a(3,2) - a(2,2)*a(3,1))

  END FUNCTION det_3x3

! *****************************************************************************
!> \brief   Returns the inverse of a 3x3 matrix.
!> \author  MK
!> \date    13.03.2004
!> \version 1.0
! ***************************************************************************
  pure FUNCTION inv_3x3(a) RESULT(a_inv)

    REAL(KIND=dp), DIMENSION(3, 3), &
      INTENT(IN)                             :: a
    REAL(KIND=dp), DIMENSION(3, 3)           :: a_inv

    REAL(KIND=dp)                            :: det_a

    det_a = 1.0_dp/det_3x3(a)

    a_inv(1,1) = (a(2,2)*a(3,3) - a(3,2)*a(2,3))*det_a
    a_inv(2,1) = (a(2,3)*a(3,1) - a(3,3)*a(2,1))*det_a
    a_inv(3,1) = (a(2,1)*a(3,2) - a(3,1)*a(2,2))*det_a

    a_inv(1,2) = (a(1,3)*a(3,2) - a(3,3)*a(1,2))*det_a
    a_inv(2,2) = (a(1,1)*a(3,3) - a(3,1)*a(1,3))*det_a
    a_inv(3,2) = (a(1,2)*a(3,1) - a(3,2)*a(1,1))*det_a

    a_inv(1,3) = (a(1,2)*a(2,3) - a(2,2)*a(1,3))*det_a
    a_inv(2,3) = (a(1,3)*a(2,1) - a(2,3)*a(1,1))*det_a
    a_inv(3,3) = (a(1,1)*a(2,2) - a(2,1)*a(1,2))*det_a

  END FUNCTION inv_3x3


END MODULE lamb_space_types
