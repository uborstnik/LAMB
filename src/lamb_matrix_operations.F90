!******************************************************************************
!
! This source code is part of the LAMB library.
!
! Written by Urban Borstnik.
!
! Inspired by CP2K and DBCSR.
!
! Copyright (C) 2012, 2013 Urban Borstnik.
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

!> \brief Whole-matrix matrix operations
!>
!> The functions in this file implement operations on the whole matrix.

MODULE lamb_matrix_operations

#include "lamb_defs.h"

  use lamb_constants
  use lamb_data_types
  use lamb_data_d_types
  use lamb_distribution_methods
  USE lamb_error
  use lamb_index_types
  use lamb_index_methods
  use lamb_kinds
  use lamb_matrix_d_types
  use lamb_matrix_methods
  use lamb_mp_env_types
  use lamb_mp_env_methods
  use lamb_mp_methods
  use lamb_mp_l_methods
  use lamb_plexus_types
  use lamb_plexus_methods
  use lamb_set_partition_types
  use lamb_set_partition_methods
  use lamb_sector_types
  use lamb_sector_methods

  IMPLICIT NONE

  PRIVATE

  public :: internal_distribute

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'matrix_operations'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

CONTAINS

!>\brief Distributes blocks into a matrix on the correct process.
!>
!> The blocks are given as separate arrays.
!>\param[in,out] matrix   The matrix for which blocks should be distributed.
!>\param[in] n            The number of blocks.
!>\param[in] subset_idx   The subset index of the blocks.
!>\param[in] local_idx    The subset-local index of the blocks.
!>\param[in] datastore_i  The datastore in which the blocks' data resides.
!>\param[in] blkdata_extents  The extents within a datastore of the blocks' data.
!>\param[in] data_areas       The datastores.
!>\param[in,out] error        LAMB error
  subroutine internal_distribute(matrix, n, subset_idx, local_idx, &
       datastore_i, blkdata_extents, data_areas, error)
    type(lamb_matrix_d_type), intent(inout) :: matrix
    integer(kind=allatm_k), intent(in) :: n ! big because there might be a lot here.
    integer(kind=subset_k), dimension(:,:), intent(in) :: subset_idx
    integer(kind=locatm_k), dimension(:,:), intent(in) :: local_idx
    integer(kind=datastore_k), dimension(:), intent(in) :: datastore_i
    integer(kind=dataptr_k), dimension(:,:), intent(in) :: blkdata_extents
    type(lamb_data_d_type), dimension(:), intent(in) :: data_areas
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'internal_distribute', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    type(lamb_mp_env_type), pointer :: mp_env
    integer(kind=int_big) :: nblocks, offset, end_offset
    integer(kind=allatm_k) :: blk
    integer(kind=proc_k) :: p, n_proc
    type(lamb_set_partition_type), pointer :: set_partition1, set_partition2
    type(lamb_plexus_type), pointer :: plexus
    integer(kind=proc_k), dimension(n) :: home_proc
    integer(kind=int_big), dimension(:,:), allocatable :: send_count, recv_count,&
         send_offsets, recv_offsets, work_offsets
    integer(kind=int_big), dimension(2) :: total_send_count, total_recv_count
    real(kind=dp), dimension(:), allocatable :: send_data_buffer, recv_data_buffer
    integer(kind=int_big), dimension(:,:), allocatable :: send_index_buffer, recv_index_buffer
    integer(kind=int_big), parameter :: vector_size = 6

    logical, parameter :: debug = debug_mod

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok (error, prev_level)
    mp_env => lamb_distribution_get_mp_env (matrix%distribution)
    plexus => matrix%distribution%plexus !!!

    ! Determine how much data must be exchanged.
    n_proc = lamb_mp_env_get_n_procs(mp_env)
    allocate(send_count(2,0:n_proc-1), stat=stat)
    call lamb_memory_check(stat, 'send_count', n_proc*2, error)
    allocate(recv_count(2,0:n_proc-1), stat=stat)
    call lamb_memory_check(stat, 'recv_count', n_proc*2, error)

    ! These are indexed differently to avoid array temporaries when
    ! passed to MPI.
    allocate(send_offsets(0:n_proc-1,2), stat=stat)
    call lamb_memory_check(stat, 'send_offsets', n_proc*2, error)
    allocate(recv_offsets(0:n_proc-1,2), stat=stat)
    call lamb_memory_check(stat, 'recv_offsets', n_proc*2, error)
    allocate(work_offsets(0:n_proc-1,2), stat=stat)
    call lamb_memory_check(stat, 'work_offsets', n_proc*2, error)

    ! First count how much we have to send to whom.
    send_count(:,:) = 0_int_big
    do blk = 1_allatm_k, n
       home_proc(blk) = lamb_plexus_tuple_get_proc(plexus, subset_idx(:,blk),&
            local_idx(:,blk), error)
       send_count(imeta,home_proc(blk)) =&
       send_count(imeta,home_proc(blk)) + 1_int_big

       send_count(idata,home_proc(blk)) =&
       send_count(idata,home_proc(blk)) + &
            int(blkdata_extents(2,blk)&
               -blkdata_extents(1,blk)&
               +1_dataptr_k,kind=int_big)
    enddo

    total_send_count(imeta) = sum(send_count(imeta,:))
    total_send_count(idata) = sum(send_count(idata,:))
    if (debug) then
       write(*,*)'send_count',send_count
       write(*,*)'total send',total_send_count
    endif
    ! Then exchange the send counts to know how much we receieve from others.
    call lamb_mp_l_alltoall(mp_env, int(2,kind=int_big), send_count,&
         recv_count, error)
    total_recv_count(imeta) = sum(recv_count(imeta,:))
    total_recv_count(idata) = sum(recv_count(idata,:))
    if (debug) then
       write(*,*)'recv_count',recv_count
       write(*,*)'total recv',total_recv_count
    endif

    ! Calculate offsets
    send_offsets(0,:) = 0_int_big
    recv_offsets(0,:) = 0_int_big
    do p = 1_proc_k, n_proc-1_proc_k
       send_offsets(p,:) = send_offsets(p-1_proc_k,:) + send_count(:, p-1_proc_k)
       recv_offsets(p,:) = recv_offsets(p-1_proc_k,:) + recv_count(:, p-1_proc_k)
    enddo
    work_offsets(:,:) = 1_int_big + send_offsets(:,:)

    allocate (send_index_buffer(vector_size,total_send_count(imeta)), stat=stat)
    call lamb_memory_check(stat, 'send_index_buffer',&
         vector_size*total_send_count(imeta), error)
    allocate (send_data_buffer(total_send_count(idata)), stat=stat)
    call lamb_memory_check(stat, 'send_data_buffer',&
         total_send_count(idata), error)

    allocate (recv_index_buffer(vector_size,total_recv_count(imeta)), stat=stat)
    call lamb_memory_check(stat, 'recv_index_buffer',&
         vector_size*total_recv_count(imeta), error)
    allocate (recv_data_buffer(total_recv_count(idata)), stat=stat)
    call lamb_memory_check(stat, 'recv_data_buffer',&
         total_recv_count(idata), error)

    ! Now copy the data to be sent.
    do blk = 1_allatm_k, n
       !home_proc(blk) = lamb_plexus_tuple_get_proc(plexus, subset_idx(:,blk),&
       !     local_idx(:,blk), error)
       p = home_proc(blk)
       offset = work_offsets(p,imeta)
       send_index_buffer(1:2, offset) = int(subset_idx(:,blk), kind=int_big)
       send_index_buffer(3:4, offset) = int(local_idx(:,blk), kind=int_big)
       ! Transform the extents to offsets relative to each
       ! process. The work_offsets keeps track of the size of each
       ! process's bin while the send_offset is removes the offset of
       ! the process's part of the buffer.
       send_index_buffer(5, offset) = work_offsets(p,idata) - send_offsets(p,idata)
       send_index_buffer(6, offset) = work_offsets(p,idata) -send_offsets(p,idata) + &
            int(blkdata_extents(2,blk) - blkdata_extents(1,blk),kind=int_big)

       work_offsets(p,imeta) = work_offsets(p,imeta) + 1_int_big

       ! Copy the data block.
       send_data_buffer(&
            work_offsets(p,idata) : &
            work_offsets(p,idata) + int( &
                blkdata_extents(2,blk)-blkdata_extents(1,blk),kind=int_big)) &
           = &
           data_areas(datastore_i(blk))%d(&
           blkdata_extents(1,blk) : blkdata_extents(2,blk))

       work_offsets(p,idata) = work_offsets(p,idata) &
            + blkdata_extents(2,blk) - blkdata_extents(1,blk) &
            + 1_int_big
    enddo

    if (debug) then
       write(*,*)'work_offsets', work_offsets
       write(*,*)'send_offsets', send_offsets
      
    endif

    send_count(imeta,:) = send_count(imeta,:) * vector_size
    send_offsets(:,imeta) = send_offsets(:,imeta) * vector_size
    call lamb_mp_l_alltoall_v(mp_env,&
         send_count(imeta,:), send_offsets(:,imeta), send_index_buffer,&
         vector_size*recv_count(imeta,:),&
         vector_size*recv_offsets(:,imeta), recv_index_buffer, error)

    call lamb_mp_alltoall_v(mp_env,&
         send_count(idata,:), send_offsets(:,idata), send_data_buffer,&
         recv_count(idata,:), recv_offsets(:,idata), recv_data_buffer, error)

    ! Offset the data offsets according to the offset of the processor.
    do p = 0_proc_k, n_proc-1_proc_k
       recv_index_buffer(5:6,&
            1_int_big+recv_offsets(p,imeta):recv_offsets(p,imeta)+recv_count(imeta,p)) &
            = &
            recv_index_buffer(5:6,&
            1_int_big+recv_offsets(p,imeta):recv_offsets(p,imeta)+recv_count(imeta,p)) &
            + recv_offsets(p,idata)
    enddo
    if (total_recv_count(imeta) > 0) then
       call lamb_assert (recv_index_buffer(6,total_recv_count(imeta)), "EQ",&
            total_recv_count(idata),&
            lamb_failure_level, lamb_internal_error,&
            "Miscalculation of data offsets.",&
            routineP, __LINE__, error)
    endif

    do blk = 1, total_recv_count(imeta)
       if (recv_index_buffer(3,blk) > matrix%distribution%set_partitions(1)%p%n_subset_elements(&
            recv_index_buffer(1,blk))) write(*,*)1,recv_index_buffer(3,blk),recv_index_buffer(1,blk)
       if (recv_index_buffer(4,blk) > matrix%distribution%set_partitions(2)%p%n_subset_elements(&
            recv_index_buffer(2,blk))) write(*,*)2,recv_index_buffer(4,blk),recv_index_buffer(2,blk)
    enddo

    ! Re-arrange the local data into sectors.
    call fill_local_sectors(matrix, total_recv_count(imeta),&
         int(recv_index_buffer(1:2,:), kind=subset_k),&
         int(recv_index_buffer(3:4,:), kind=locatm_k),&
         int(recv_index_buffer(5:6,:), kind=dataptr_k),&
         recv_data_buffer, error)

    deallocate(send_offsets, stat=stat)
    call lamb_memory_check(stat, 'send_offsets', error)
    deallocate(recv_offsets, stat=stat)
    call lamb_memory_check(stat, 'recv_offsets', error)
    deallocate(work_offsets, stat=stat)
    call lamb_memory_check(stat, 'work_offsets', error)

    deallocate(send_count, stat=stat)
    call lamb_memory_check(stat, 'send_count', error)
    deallocate(recv_count, stat=stat)
    call lamb_memory_check(stat, 'recv_count', error)

    call lamb_error_stop (error, error_handle)
  end subroutine internal_distribute

  !> \brief Build index and data areas for local sectors.
  !> \param[in,out] matrix   Matrix for which to generate the index and data areas of its local sectors.
  !> \param[in] n            Number of blocks to add into the local sectors.
!>\param[in] subset_idx   The subset index of the blocks.
!>\param[in] local_idx    The subset-local index of the blocks.
!>\param[in] data_areas       The datastores.
!>\param[in,out] error        LAMB error
  subroutine fill_local_sectors(matrix, n, subset_idx, local_idx, &
       blkdata_extents, data_area, error)
    type(lamb_matrix_d_type), intent(inout) :: matrix
    integer(kind=allatm_k), intent(in) :: n ! big because there might be a lot here.
    integer(kind=subset_k), dimension(:,:), intent(in) :: subset_idx
    integer(kind=locatm_k), dimension(:,:), intent(in) :: local_idx
    !integer(kind=datastore_k), dimension(:), intent(in) :: datastore_i
    integer(kind=dataptr_k), dimension(:,:), intent(in) :: blkdata_extents
    !type(lamb_data_d_type), dimension(:), intent(in) :: data_areas
    real(kind=dp), dimension(:), intent(in) :: data_area
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'fill_local_sectors', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle, stat, prev_level
    integer(kind=dataptr_k) :: extent_len

    type(lamb_plexus_type), pointer :: plexus
    integer(kind=allatm_k) :: blk
    integer(kind=cube_k) :: sector_i, n_sectors
    type(lamb_blk_info_type) :: block_info
    integer :: i

    logical, parameter :: debug = debug_mod

    call lamb_error_set (error, error_handle, routineN)

    plexus => lamb_distribution_get_plexus_p(matrix%distribution)

    call lamb_matrix_update (matrix, error)
    n_sectors = lamb_dist_get_n_local_sectors(matrix%distribution, error)
    do sector_i = 1_cube_k, n_sectors
       call lamb_index_set_fmt_coor(matrix%sectors(sector_i)%index, error)
    enddo
    ! Adds each block in turn to its appropriate sector.
    do blk = 1_allatm_k, n
       ! Calculate how much data there is to add.
       extent_len = blkdata_extents(2,blk) - blkdata_extents(1,blk) + 1_dataptr_k
       call lamb_index_blk_info_pack(block_info,&
            int(1, kind=datastore_k),&
            blkdata_extents(1:2,blk),&
            .false., .false., .false.)
       ! Now adds the index and data to the appropriate sector.
       sector_i = lamb_plexus_get_sector_i(plexus, subset_idx(:,blk), error)
       if (careful_mod) then
          if (int(sector_i) <= 0) then
             write(*,*)routineN//' sector not found for subsets',subset_idx(:,blk)
          endif
          call lamb_assert (int(sector_i), 'GT', 0,&
               lamb_warning_level, lamb_internal_error,&
               "Sector not found",&
               routineN, __LINE__, error=error)
          do i = 1, size(local_idx, 1)
             call lamb_assert(int(matrix%sectors(sector_i)%subsets(i)%p%n),&
                  'EQ',&
                  int(matrix%distribution%set_partitions(i)%p%n_subset_elements(subset_idx(i,blk))),&
                  lamb_warning_level, lamb_internal_error,&
                  "Subset size does not match declaration.",&
                  routineN, __LINE__, error)
             call lamb_assert (int(local_idx(i,blk)), "LE", &
                  int(matrix%sectors(sector_i)%subsets(i)%p%n),&
                  lamb_warning_level, lamb_internal_error,&
                  "Local atom index too high for subset size.",&
                  routineN, __LINE__, error)
             if (local_idx(i,blk) > matrix%sectors(sector_i)%subsets(i)%p%n) &
                  write(*,*)'for atom',blk,sector_i,':',&
                  subset_idx(:,blk),local_idx(:,blk)
          enddo
       endif
       call lamb_sector_add_new_block(matrix%sectors(sector_i),&
            local_idx(:,blk), block_info, extent_len,&
            data_area(blkdata_extents(1,blk):blkdata_extents(2,blk)),&
            error)
    enddo

    ! Convert the index to standard CSR.
    do sector_i = 1_cube_k, n_sectors
       call lamb_index_convert_to_csr(matrix%sectors(sector_i)%index, error)
       write(*,*)routineN//' sector',sector_i,'size=',matrix%sectors(sector_i)%n_blks
    enddo

    call lamb_error_stop (error, error_handle)
  end subroutine fill_local_sectors

END MODULE lamb_matrix_operations
