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

!MAKE s d c z

!> \brief LAMB tests for specific matrix types (_X).
!> \author Urban Borstnik

MODULE lamb_matrix_X_tests

#include "lamb_defs.h"

  use lamb_data_X_types
  use lamb_distribution_types
  use lamb_distribution_methods
  USE lamb_error
  use lamb_io_operations
  use lamb_kinds
  use lamb_matrix_X_types
  use lamb_matrix_X_methods
  use lamb_meta_types
  use lamb_mp_env_types
  use lamb_partition_types
  use lamb_partition_methods
  use lamb_plexus_types
  use lamb_plexus_methods
  use lamb_plexus_operations
  use lamb_relation_types
  use lamb_sector_X_types
  use lamb_sector_X_methods
  use lamb_set_types
  use lamb_set_methods
  use lamb_set_partition_types
  use lamb_subset_types
  use lamb_space_types
  use lamb_space_operations

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_matrix_X_test

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'matrix_X_tests'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS

! *****************************************************************************
!> \brief Creates a random LAMB matrix.
!>
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_create_random (matrix_X, name, plexus, error)
    TYPE(lamb_matrix_X_type), INTENT(OUT) :: matrix_X
    character(len=*) :: name
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_plexus_type), pointerin :: plexus

    INTEGER :: error_handle, stat, i
    integer, SAVE :: creation_counter = 0
    type(lamb_meta_type) :: meta
    type(lamb_relation_type), pointer :: relation
    type(lamb_set_type), pointer :: atom_set
    type(lamb_set_type_p), dimension(2) :: sets
    integer(kind=allatm_k) :: n_atoms
    integer(kind=allatm_k), parameter :: max_atoms = 100
    real :: tmp_real0
    real, dimension(:), allocatable :: tmp_real1
    integer(kind=blksz_k), dimension(:), allocatable :: blk_sizes
    type(lamb_set_partition_type_p), dimension(2) :: set_partitions
    type(lamb_set_partition_type), pointer :: set_partition
    type(lamb_partition_type), pointer :: partition
    type(lamb_distribution_type) :: distribution
    real(kind=space_k), dimension(:,:), allocatable :: coordinates
    type(lamb_space_type), pointer :: space

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_create_random', &
         routineP = moduleN//':'//routineN

    CALL lamb_error_set (error, error_handle, routineN)

    ! Total number of atoms.
    call random_number (tmp_real0)
    n_atoms = int(tmp_real0 * real(max_atoms), kind=allatm_k)
    write(*,*)routineN//" Using n_atoms", n_atoms
    allocate (tmp_real1(n_atoms), stat=stat)
    call lamb_memory_check(stat, "tmp_real1", int(n_atoms,kind=int_big), error)
    call random_number(tmp_real1)
    allocate (blk_sizes(n_atoms), stat=stat)
    call lamb_memory_check(stat, "blk_sizes", int(n_atoms,kind=int_big), error)
    ! Four different block sizes
    blk_sizes = int((tmp_real1*real(4)), kind=blksz_k)
    call lamb_set_new (atom_set, int(n_atoms,kind=allatm_k), blk_sizes, error)
    deallocate (blk_sizes, stat=stat)
    call lamb_memory_check(stat, "blk_sizes", error)
    allocate (coordinates(3,n_atoms), stat=stat)
    call lamb_memory_check(stat, "coordinates", 3*n_atoms, error)
    partition => lamb_plexus_get_partition (plexus, error)
    space => lamb_partition_get_space(partition)
    call lamb_space_make_random_points(space, n_atoms, coordinates, error)
    call lamb_set_update_coordinates(atom_set, coordinates, error)
    deallocate (coordinates, stat=stat)
    call lamb_memory_check(stat, "coordinates", error)
    sets(1)%p => atom_set
    sets(2)%p => atom_set
    call lamb_relation_new (relation, sets, lamb_rel_no_symmetry, error)
    call lamb_set_release (atom_set, error)
    call lamb_meta_create (meta, name, relation, error)
    call lamb_relation_release (relation, error)

    call lamb_set_partition_new (set_partition, atom_set, partition, error)
    set_partitions(1)%p => set_partition
    set_partitions(2)%p => set_partition
    call lamb_distribution_create (distribution, plexus, set_partitions, error)
    call lamb_set_partition_release (set_partition, error)

    call lamb_distribution_update (distribution, error)

    call lamb_matrix_X_create (matrix_X, meta, distribution, error)

    call lamb_distribution_destroy (distribution, error)

    call lamb_meta_destroy (meta, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_create_random


  SUBROUTINE lamb_matrix_X_fill_random (matrix_X, error)
    TYPE(lamb_matrix_X_type), INTENT(INOUT) :: matrix_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    INTEGER :: error_handle, stat

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_fill_random', &
         routineP = moduleN//':'//routineN

    integer(kind=cube_k) :: sector_i, n_sectors

    CALL lamb_error_set (error, error_handle, routineN)

    n_sectors = lamb_dist_get_n_local_sectors (&
         lamb_matrix_X_get_distribution(matrix_X), error)
    call lamb_assert(int(n_sectors), "EQ", size(matrix_X%sectors),&
         lamb_failure_level, lamb_internal_error,&
         "Local sector count not equal to sector list size.",&
         routineN, __LINE__, error=error)
    do sector_i = 1_cube_k, n_sectors
       call fill_sector(matrix_X%sectors(sector_i), error)
    enddo

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_fill_random

  subroutine fill_sector(sector, error)
    type(lamb_sector_X_type), intent(inout) :: sector
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    INTEGER :: error_handle, stat

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_fill_random', &
         routineP = moduleN//':'//routineN

    integer(kind=locatm_k) :: n_a, n_b, i, j
    type(lamb_subset_type), pointer :: subset_a, subset_b
    real, parameter :: fill = 0.7
    real :: coin_flip
    integer(kind=locatm_k), dimension(:,:), allocatable :: block_list
    integer(kind=dataptr_k), dimension(:,:), allocatable :: extents
    type(lamb_data_X_type) :: my_data
    doubleprecision, dimension(:), pointer :: data_direct
    integer(kind=int_big) :: row_size, col_size
    integer(kind=dataptr_k) :: blk_p, blksize
    integer(kind=blkn_k) :: nblks

    CALL lamb_error_set (error, error_handle, routineN)
    subset_a => sector%subsets(1)%p
    subset_b => sector%subsets(2)%p
    n_a = subset_a%n
    n_b = subset_b%n

    row_size = sum(int(subset_a%blksizes(1:n_a),kind=int_big))
    col_size = sum(int(subset_b%blksizes(1:n_b),kind=int_big))

    call lamb_data_X_create(my_data,&
         presize=int(row_size*col_size,kind=dataptr_k), error=error)
    data_direct => my_data%d
    allocate (block_list(2,n_a*n_b), stat=stat)
    call lamb_memory_check(stat, "block_list", int(2*n_a*n_b,kind=int_big), error)
    allocate (extents(2,n_a*n_b), stat=stat)
    call lamb_memory_check(stat, "extents", int(2*n_a*n_b,kind=int_big), error)

    nblks = 0_blkn_k
    blk_p = 0_dataptr_k
    do i = 1, n_a
       do j = 1, n_b
          call random_number(coin_flip)
          if (coin_flip < fill) then
             nblks = nblks + 1_blkn_k
             block_list(1,nblks) = i
             block_list(2,nblks) = j
             blksize = int(subset_a%blksizes(i),kind=dataptr_k)*&
                       int(subset_b%blksizes(j),kind=dataptr_k)
             extents(1:2,nblks) = (/ blk_p, blk_p+blksize-1_dataptr_k /)
             call random_number(data_direct(blk_p+1_dataptr_k:blk_p+blksize))
             blk_p = blk_p + blksize
          endif
       enddo
    enddo
    my_data%data_len = blk_p
    call lamb_sector_X_shove(sector, nblks, block_list, extents, my_data, error)

    CALL lamb_error_stop (error, error_handle)
  end subroutine fill_sector


! *****************************************************************************
!> \brief Tests LAMB matrix routines.
!>
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_matrix_X_test (mp_env, error)
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_mp_env_type), pointerin :: mp_env

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_matrix_X_test', &
         routineP = moduleN//':'//routineN

    TYPE(lamb_matrix_X_type) :: matrix_X, matrix_X_read
    INTEGER :: error_handle
    type(lamb_plexus_type), pointer :: plexus
    TYPE(lamb_matrix_X_type), pointer :: matrix_read2
    logical, parameter :: debug = .true.

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_plexus_create_simple (plexus, mp_env, error)

    call lamb_matrix_X_create_random(matrix_X, "test", plexus, error)
    call lamb_matrix_X_create_random(matrix_X_read, "read_test", plexus, error)

    call lamb_plexus_release (plexus, error)

    call lamb_matrix_X_update (matrix_X, error)

    call lamb_matrix_X_fill_random (matrix_X, error)

    call lamb_matrix_X_destroy (matrix_X, error)
    call lamb_load_generic (matrix_X_read, "test.f1", error)
    call lamb_matrix_X_destroy (matrix_X_read, error)

    !call lamb_load_separate (matrix_read2, "test_dist", mp_env, error)
    if (debug) then
       write(*,*)routineN//' Loading DBCSR test'
    endif
    call lamb_load_dbcsr (matrix_read2, "small.dbcsr", mp_env, error)
    call lamb_matrix_X_release (matrix_read2, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_matrix_X_test

END MODULE lamb_matrix_X_tests
