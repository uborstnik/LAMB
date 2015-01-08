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

!> \brief A market is an object that is used to exchange goods (serialized matrix sectors) among processes.

!MAKE s d c z
MODULE lamb_market_X_types

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_distribution_methods
  use lamb_goods_types
  use lamb_goods_X_types
  use lamb_goods_X_methods
  use lamb_matrix_X_types
  use lamb_matrix_X_methods
  use lamb_mp_env_types
  use lamb_mp_types
  use lamb_mpi_kinds
  use lamb_plexus_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_market_X_type
  PUBLIC :: lamb_market_X_type_p

  PUBLIC :: lamb_market_X_create
  PUBLIC :: lamb_market_X_copy
  PUBLIC :: lamb_market_X_destroy

  PUBLIC :: lamb_market_X_new
  PUBLIC :: lamb_market_X_hold
  PUBLIC :: lamb_market_X_release

  PUBLIC :: lamb_market_X_valid

  PUBLIC :: lamb_market_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'market_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: market_X_id


  TYPE lamb_market_X_type
     INTEGER :: refcount
     INTEGER :: id
     type(lamb_goods_X_type), dimension(:), allocatable :: goods
     integer(kind=int_big), dimension(lamb_goods_num) :: largest_goods
     type(lamb_plexus_type), pointer :: plexus
     type(lamb_mp_env_type), pointer :: mp_env
     !> Basically the max. number of sectors any node has for this matrix.
     integer :: most_goods
     integer :: recv_n
     type(lamb_goods_X_type), dimension(1) :: recv_buffers
     !integer(kind=the_mpi_integer_kind), dimension(2) :: recv_req
     type(lamb_mp_op_req_type), dimension(2) :: recv_req
     ! The fields are: (who, which_good, which field).
     integer, dimension(:), pointer :: listen_request
     !integer(kind=the_mpi_integer_kind) :: listen_req
     type(lamb_mp_op_req_type) :: listen_req
     !integer(kind=the_mpi_integer_kind), dimension(2) :: sending_req
     type(lamb_mp_op_req_type), dimension(2) :: sending_req
  END TYPE lamb_market_X_type

  TYPE lamb_market_X_type_p
     TYPE(lamb_market_X_type), POINTER :: p
  END TYPE lamb_market_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] market_X  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_create (market_X, matrix, error)
    TYPE(lamb_market_X_type), INTENT(OUT):: market_X
    type(lamb_matrix_X_type), intent(in) :: matrix
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: n_sectors, i_sector, i
    integer(kind=int_big), dimension(lamb_goods_num) :: goods_sizes, max_goods_sizes
    integer(kind=int_big), dimension(lamb_goods_num+1) :: xchg_input, xchg_output
    integer :: max_num_goods

    CALL lamb_error_set (error, error_handle, routineN)

    market_X%id = market_X_id
    market_X_id = market_X_id + 1
    market_X%refcount = 0

    market_X%mp_env => lamb_distribution_get_mp_env(lamb_matrix_X_get_distribution(matrix))
    call lamb_mp_env_hold(market_X%mp_env, error)

    market_X%plexus => lamb_distribution_get_plexus_p(lamb_matrix_X_get_distribution(matrix))
    call lamb_plexus_hold(market_X%plexus, error)

    n_sectors = lamb_matrix_X_get_n_sectors(matrix, error)
    allocate(market_X%goods(n_sectors), stat=stat)
    call lamb_memory_check(stat, "market_X%goods", n_sectors, error)

    do i_sector = 1, n_sectors
       call lamb_goods_X_create(market_X%goods(i_sector), matrix%sectors(i_sector), error)
    enddo

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] old_market_X       Object to copy from
!> \param[out] market_X  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_copy (old_market_X, market_X, error)
    TYPE(lamb_market_X_type), INTENT(IN):: old_market_X
    TYPE(lamb_market_X_type), INTENT(OUT):: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    market_X = old_market_X
    market_X%refcount = 0
    market_X%id = 0
    STOP routineN//" The copy method is not implemented."

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] market_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_destroy (market_X, error)
    TYPE(lamb_market_X_type), INTENT(INOUT) :: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: n_sectors, i_sector

    CALL lamb_error_set (error, error_handle, routineN)

    n_sectors = size(market_X%goods)
    do i_sector = 1, n_sectors
       call lamb_goods_X_destroy(market_X%goods(i_sector), error)
    enddo

    call lamb_mp_env_release(market_X%mp_env, error)
    call lamb_plexus_release(market_X%plexus, error)

    deallocate(market_X%goods, stat=stat)
    call lamb_memory_check(stat, "matrix_X%goods", error)
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] market_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_new (market_X, matrix, error)
    TYPE(lamb_market_X_type), POINTEROUT :: market_X
    type(lamb_matrix_X_type), intent(in) :: matrix
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (market_X, stat=stat)
    CALL lamb_memory_check (stat, "market_X", -1, error)

    CALL lamb_market_X_create (market_X, matrix, error)
    market_X%refcount = 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] market_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_hold (market_X, error)
    TYPE(lamb_market_X_type), POINTERINOUT :: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    market_X%refcount = market_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] market_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_release (market_X, error)
    TYPE(lamb_market_X_type), POINTERINOUT :: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    market_X%refcount = market_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (market_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (market_X%refcount <= 0) THEN
       CALL lamb_market_X_destroy (market_X, error)
       DEALLOCATE (market_X, stat=stat)
       CALL lamb_memory_check (stat, "market_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] market_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_market_X_valid (market_X, error) RESULT (valid)
    TYPE(lamb_market_X_type), POINTERIN :: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (market_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_market_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] market_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_verify (market_X, error)
    TYPE(lamb_market_X_type), INTENT(IN) :: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_verify


END MODULE lamb_market_X_types
