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

!> \brief Goods are serialized sectors. Defines the goods types and basic methods.

!MAKE s d c z
MODULE lamb_goods_X_types

#include "lamb_defs.h"

  use lamb_kinds
  USE lamb_error
  use lamb_data_X_types
  use lamb_data_X_methods
  use lamb_mp_env_types
  use lamb_mp_window_l_types
  use lamb_mp_window_X_types
  use lamb_sector_X_types
  use lamb_1d_X_util

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_goods_X_type
  PUBLIC :: lamb_goods_X_type_p

  PUBLIC :: lamb_goods_X_create
  PUBLIC :: lamb_goods_X_copy
  PUBLIC :: lamb_goods_X_destroy

  PUBLIC :: lamb_goods_X_new
  PUBLIC :: lamb_goods_X_hold
  PUBLIC :: lamb_goods_X_release

  PUBLIC :: lamb_goods_X_valid

  PUBLIC :: lamb_goods_X_verify


  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'goods_X'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: goods_X_id

  integer, parameter, public :: lamb_comm_slot_index_offset = 1
  integer, parameter, public :: lamb_comm_slot_index_size =   2
  integer, parameter, public :: lamb_comm_slot_data_offset  = 3
  integer, parameter, public :: lamb_comm_slot_data_size  =   4
  integer, parameter, public :: lamb_comm_slot_row_p_offset = 5
  integer, parameter, public :: lamb_comm_slot_row_p_size   = 6
  integer, parameter, public :: lamb_comm_slot_col_i_offset = 7
  integer, parameter, public :: lamb_comm_slot_col_i_size   = 8
  integer, parameter, public :: lamb_comm_slot_extents_offset = 9
  integer, parameter, public :: lamb_comm_slot_extents_size   = 10
  integer, parameter, public :: lamb_comm_slot_flags_offset = 11
  integer, parameter, public :: lamb_comm_slot_flags_size   = 12
  integer, parameter, public :: lamb_comm_slot_size         = 12

  TYPE lamb_goods_X_type
     INTEGER :: refcount
     INTEGER :: id
     !integer(kind=locatm_k), dimension(:), pointer :: index
     !integer(kind=dataptr_k), dimension(:), pointer :: extents
     integer(kind=int_8), dimension(:), pointer :: index
     doubleprecision, dimension(:), pointer :: data
     type(lamb_mp_window_l_type) :: index_win
     type(lamb_mp_window_X_type) :: data_win
  END TYPE lamb_goods_X_type

  TYPE lamb_goods_X_type_p
     TYPE(lamb_goods_X_type), POINTER :: p
  END TYPE lamb_goods_X_type_p

CONTAINS


! *****************************************************************************
!> \brief Creates and initializes an object.
!>
!> Sets up an object for use.
!> \param[in,out] goods_X  Object to create
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_create (goods_X, sector, error)
    TYPE(lamb_goods_X_type), INTENT(OUT):: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_sector_X_type), intent(IN) :: sector

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_create', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=int_big) :: n_words_index, size_data,&
         n_words_rows, n_words_cols, n_words_extents, n_words_flags, offset, a, b, i, s
    integer :: scale_factor
    integer :: word_size_m1, word_size, index_kind
    logical, parameter :: debug = .false.

    CALL lamb_error_set (error, error_handle, routineN)

    goods_X%id = goods_X_id
    goods_X_id = goods_X_id + 1
    goods_X%refcount = 0

    word_size = BIT_SIZE(int(0,kind=int_big))/8
    word_size_m1 = word_size - 1
    index_kind = kind(goods_X%index)

    call lamb_assert(size(sector%data_stores), "EQ", 1,&
         lamb_failure_level, lamb_internal_error,&
         "Sector has too many data stores.",&
         routineN, __LINE__, error)
    size_data = lamb_data_X_get_len(sector%data_stores(1), error)
    allocate(goods_X%data(size_data), stat=stat)
    call lamb_memory_check(stat, "goods_X%data", int(size_data,kind=int_big),&
         error)
    call lamb_1d_X_copy(sector%data_stores(1)%d,&
         goods_X%data, size_data)

    call lamb_assert(kind(sector%index%row_p), "EQ", blkn_k,&
         lamb_failure_level, lamb_internal_error,&
         "Wrong data type assumed.", routineN, __LINE__, error)
    ! In these calculations, we first calculate the size in
    ! bytes. This is then converted to the number of words.
    n_words_rows = int(sector%index%n_rows, kind=int_big) &
         * int(BIT_SIZE(int(0,kind=blkn_k)) / 8, kind=int_big)
    n_words_rows = byte_to_word_count(n_words_rows)
    call lamb_assert(kind(sector%index%col_i), "EQ", locatm_k,&
         lamb_failure_level, lamb_internal_error,&
         "Wrong data type assumed.", routineN, __LINE__, error)
    n_words_cols = int(sector%index%n_idx_blks, kind=int_big) &
         * int(BIT_SIZE(int(0,kind=locatm_k)) / 8, kind=int_big)
    n_words_cols = byte_to_word_count(n_words_cols)
    n_words_extents = int(sector%index%n_idx_blks, kind=int_big)*2_int_big &
         * int(BIT_SIZE(int(0,kind=dataptr_k)) / 8, kind=int_big)
    n_words_extents = byte_to_word_count(n_words_extents)
    n_words_flags = int(sector%index%n_idx_blks, kind=int_big)
    n_words_flags = byte_to_word_count(n_words_flags)

    n_words_index = int(lamb_comm_slot_size, kind=int_big)
    n_words_index = n_words_index + n_words_rows + n_words_cols + n_words_extents + n_words_flags

    if (debug) then
       write(*,*)routineN//' n_words_index', n_words_index
    endif

    allocate(goods_X%index(n_words_index), stat=stat)
    call lamb_memory_check(stat, "goods_X%index", n_words_index, error)

    goods_X%index(lamb_comm_slot_data_offset) = 1_int_big
    goods_X%index(lamb_comm_slot_data_size) = size_data

    !offset = lamb_comm_slot_size + 1_int_big
    offset = 1_int_big
    goods_X%index(lamb_comm_slot_index_offset) = offset
    goods_X%index(lamb_comm_slot_index_size) = n_words_index
    !offset = offset + n_words_index
    offset = offset + lamb_comm_slot_size

    goods_X%index(lamb_comm_slot_row_p_offset) = offset
    goods_X%index(lamb_comm_slot_row_p_size) = sector%index%n_rows
    !goods_X%index(lamb_comm_slot_row_p_size) = n_words_rows
    offset = offset + n_words_rows

    goods_X%index(lamb_comm_slot_col_i_offset) = offset
    goods_X%index(lamb_comm_slot_col_i_size) = sector%index%n_idx_blks
    !goods_X%index(lamb_comm_slot_col_i_size) = n_words_cols
    offset = offset + n_words_cols

    goods_X%index(lamb_comm_slot_extents_offset) = offset
    goods_X%index(lamb_comm_slot_extents_size) = sector%index%n_idx_blks
    !goods_X%index(lamb_comm_slot_extents_size) = n_words_extents
    offset = offset + n_words_extents

    goods_X%index(lamb_comm_slot_flags_offset) = offset
    goods_X%index(lamb_comm_slot_flags_size) = sector%index%n_idx_blks
    !goods_X%index(lamb_comm_slot_flags_size) = n_words_flags
    offset = offset + n_words_flags

    if (debug) then
       write(*,*)size(goods_X%index)
       do i = 1, lamb_comm_slot_size
          write(*,*)goods_X%index(i)
       end do
       write(*,*)goods_X%index(1:lamb_comm_slot_size)
       write(*,'(2(1X,I16))')goods_X%index(1:lamb_comm_slot_size)
       write(*,*)'Copying...'
    endif


    ! Now copy the index data
    a = int(goods_X%index(lamb_comm_slot_row_p_offset),kind=int_big)
    s = n_words_rows !goods_X%index(lamb_comm_slot_row_p_size)
    b =  a + s - 1_int_big
    goods_X%index(b) = int(0,kind=kind(goods_X%index))
    goods_X%index(a:b) = TRANSFER(sector%index%row_p, goods_X%index, int(s))

    a = int(goods_X%index(lamb_comm_slot_col_i_offset),kind=int_big)
    s = n_words_cols !goods_X%index(lamb_comm_slot_col_i_size)
    b =  a + s - 1_int_big
    goods_X%index(b) = int(0,kind=kind(goods_X%index))
    goods_X%index(a:b) = TRANSFER(sector%index%col_i, goods_X%index, int(s))

    a = int(goods_X%index(lamb_comm_slot_extents_offset),kind=int_big)
    s = n_words_extents !goods_X%index(lamb_comm_slot_extents_size)
    b =  a + s - 1_int_big
    goods_X%index(b) = int(0,kind=kind(goods_X%index))
    !goods_X%index(a:b) = TRANSFER(sector%index%blk_p, goods_X%index, s)
    scale_factor = int(BIT_SIZE(goods_X%index)) / int(BIT_SIZE(int(0,kind=dataptr_k)))
    call lamb_assert(scale_factor == 1 .or. scale_factor==2,&
         lamb_internal_error, lamb_unimplemented,&
         "Types are not compatible.", routineN, __LINE__, error)
    do i = 1_int_big, int(sector%index%n_idx_blks,kind=int_big)
       goods_X%index(a + (i-1_int_big)*2_int_big/scale_factor:) = &
            transfer(sector%index%blk_info(i)%extent(:), goods_X%index,&
            2/scale_factor)
    enddo

    !!! FLAGS not copied
    a = int(goods_X%index(lamb_comm_slot_flags_offset),kind=int_big)
    s = n_words_flags !goods_X%index(lamb_comm_slot_flags_size)
    b =  a + s - 1_int_big
    goods_X%index(a:b) = int(0,kind=kind(goods_X%index))

    CALL lamb_error_stop (error, error_handle)
  contains
    function byte_to_word_count (n_bytes) result (n_words) !IGNORE
      integer(kind=int_big), intent(in) :: n_bytes
      integer(kind=int_big) :: n_words
      n_words  = (n_bytes+int(word_size-1,kind=int_big))/int(word_size,kind=int_big)
      n_words = max(1_int_big, n_words)
    end function byte_to_word_count
  END SUBROUTINE lamb_goods_X_create


! *****************************************************************************
!> \brief Copies an object shallowly.
!>
!> \param[in] old_goods_X       Object to copy from
!> \param[out] goods_X  Object to copy to
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_copy (old_goods_X, goods_X, error)
    TYPE(lamb_goods_X_type), INTENT(IN):: old_goods_X
    TYPE(lamb_goods_X_type), INTENT(OUT):: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_copy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    goods_X = old_goods_X
    goods_X%refcount = 0
    goods_X%id = 0
    STOP routineN//" The copy method is not implemented."

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_copy


! *****************************************************************************
!> \brief Destroys an object.
!>
!> Destroys an object.  Releases allocations held by it.
!> \param[in,out] goods_X  Object to destroy.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_destroy (goods_X, error)
    TYPE(lamb_goods_X_type), INTENT(INOUT) :: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_destroy', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    if (associated (goods_X%index)) then
       deallocate (goods_X%index, stat=stat)
       call lamb_memory_check(stat, "goods_X%index", error)
    endif
    if (associated (goods_X%data)) then
       deallocate (goods_X%data, stat=stat)
       call lamb_memory_check(stat, "goods_X%data", error)
    endif
    
    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_destroy


! *****************************************************************************
!> \brief Allocates and creates a new object.
!>
!> \param[in,out] goods_X  Object to allocate.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_new (goods_X, sector, error)
    TYPE(lamb_goods_X_type), POINTEROUT :: goods_X
    type(lamb_sector_X_type), intent(IN) :: sector
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_new', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    ALLOCATE (goods_X, stat=stat)
    CALL lamb_memory_check (stat, "goods_X", -1, error)

    CALL lamb_goods_X_create (goods_X, sector, error)
    goods_X%refcount = 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_new


! *****************************************************************************
!> \brief Registers a new reference to an object.
!>
!> \param[in,out] goods_X  Registers reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_hold (goods_X, error)
    TYPE(lamb_goods_X_type), POINTERINOUT :: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_hold', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    goods_X%refcount = goods_X%refcount + 1

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_hold


! *****************************************************************************
!> \brief Deregisters a reference to an object and possible releases it.
!>
!> \param[in,out] goods_X  Deregisters reference to this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_release (goods_X, error)
    TYPE(lamb_goods_X_type), POINTERINOUT :: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_release', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    goods_X%refcount = goods_X%refcount - 1
    IF (careful_mod) THEN
       CALL lamb_assert (goods_X%refcount, "GE", 0,&
            lamb_fatal_level, lamb_caller_error,&
            routineN, "Reference count below 0.", __LINE__, error)
    ENDIF
    IF (goods_X%refcount <= 0) THEN
       CALL lamb_goods_X_destroy (goods_X, error)
       DEALLOCATE (goods_X, stat=stat)
       CALL lamb_memory_check (stat, "goods_X", error)
    ENDIF

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_release


! *****************************************************************************
!> \brief Checks whether the object is valid.
!>
!> \param[in] goods_X  Checks validity of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_goods_X_valid (goods_X, error) RESULT (valid)
    TYPE(lamb_goods_X_type), POINTERIN :: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    LOGICAL :: valid

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_valid', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    valid = ASSOCIATED (goods_X)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_goods_X_valid


! *****************************************************************************
!> \brief Verifies consistency of an object.
!>
!> \param[in] goods_X  Checks consistency of this object.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_verify (goods_X, error)
    TYPE(lamb_goods_X_type), INTENT(IN) :: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_verify', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_verify


END MODULE lamb_goods_X_types
