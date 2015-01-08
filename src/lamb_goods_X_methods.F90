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

!> \brief Methods for the goods class.

!MAKE s d c z
MODULE lamb_goods_X_methods

#include "lamb_defs.h"

  use lamb_kinds
  USE lamb_error
  use lamb_1d_X_util
  use lamb_alloc_1d_util
  use lamb_data_X_types
  use lamb_data_X_methods
  use lamb_goods_X_types
  use lamb_goods_types
  use lamb_index_methods
  use lamb_mult_sector_X_types
  use lamb_mp_env_types
  use lamb_mp_window_types
  use lamb_ptr_1d_util
  use lamb_sector_X_types

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_goods_X_open
  PUBLIC :: lamb_goods_X_close
  PUBLIC :: lamb_goods_X_get_size

  public :: lamb_goods_X_unpack

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'goods_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS


! *****************************************************************************
!> \brief Opens access to some goods.
!>
!> \param[in,out] goods_X  Open access to these goods.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_open (goods_X, mp_env, error)
    TYPE(lamb_goods_X_type), INTENT(INOUT):: goods_X
    type(lamb_mp_env_type), intent(in) :: mp_env
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_open', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    if (.false.) then
       call lamb_mp_window_create(goods_X%index_win, mp_env, goods_X%index, error)
       call lamb_mp_window_create(goods_X%data_win, mp_env, goods_X%data, error)
    endif

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_open


! *****************************************************************************
!> \brief Closes access to some goods.
!>
!> \param[in,out] goods_X  Close access to these goods.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_close (goods_X, error)
    TYPE(lamb_goods_X_type), INTENT(INOUT):: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_close', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    if (.false.) then
       call lamb_mp_window_destroy(goods_X%index_win, error)
       call lamb_mp_window_destroy(goods_X%data_win, error)
    endif

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_close


! *****************************************************************************
!> \brief Gets the size of goods.
!>
!> \param[in] goods_X    Gets the size of these goods.
!> \param[in,out] error  Error object.
!> \return               The sizes of the goods.
!> \author UB
! *****************************************************************************
  FUNCTION lamb_goods_X_get_size (goods_X, error) RESULT (sizes)
    TYPE(lamb_goods_X_type), INTENT(IN):: goods_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=int_big), dimension(lamb_goods_num) :: sizes

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_get_size', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat

    CALL lamb_error_set (error, error_handle, routineN)

    sizes(1) = size(goods_X%index, kind=int_big)
    sizes(2) = size(goods_X%data, kind=int_big)

    CALL lamb_error_stop (error, error_handle)
  END FUNCTION lamb_goods_X_get_size


! *****************************************************************************
!> \brief Unpacks goods into a sector.
!>
!> \param[in] goods_X    Packed goods
!> \param[in,out] sector Sector into which goods are unpacked.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_goods_X_unpack (goods_X, mult_sector, error)
    TYPE(lamb_goods_X_type), INTENT(IN):: goods_X
    type(lamb_mult_sector_X_type), intent(INOUT) :: mult_sector
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_goods_X_unpack', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer(kind=int_big) :: n_words_index, size_data,&
         n_words_rows, n_words_cols, n_words_extents, n_words_flags,&
         offset, a, b, i, s
    integer :: scale_factor
    integer :: word_size_m1, word_size, index_kind
    logical, parameter :: debug = debug_mod
    integer :: n_rows
    integer(kind=blkn_k) :: n_idx_blks

    CALL lamb_error_set (error, error_handle, routineN)

    word_size = BIT_SIZE(int(0,kind=int_big))/8
    word_size_m1 = word_size - 1
    index_kind = kind(goods_X%index)

    n_idx_blks = goods_X%index(lamb_comm_slot_extents_size)

    ! Point data
    size_data = goods_X%index(lamb_comm_slot_data_size)
    call lamb_data_X_pto_pointer(mult_sector%data, goods_X%data, error)

    ! Point index
    a = goods_X%index(lamb_comm_slot_row_p_offset)
    s = goods_X%index(lamb_comm_slot_row_p_size)
    call point_into_array(mult_sector%row_p,&
         goods_X%index, a, s)

    a = goods_X%index(lamb_comm_slot_col_i_offset)
    s = goods_X%index(lamb_comm_slot_col_i_size)
    call point_into_array(mult_sector%col_i,&
         goods_X%index, a, s)

    a = goods_X%index(lamb_comm_slot_extents_offset)
    s = goods_X%index(lamb_comm_slot_extents_size)
    call point_into_array(mult_sector%extents,&
         goods_X%index, a, s*2)

    a = goods_X%index(lamb_comm_slot_flags_offset)
    s = goods_X%index(lamb_comm_slot_flags_size)
    call point_into_array(mult_sector%bits,&
         goods_X%index, a, s)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_goods_X_unpack

END MODULE lamb_goods_X_methods
