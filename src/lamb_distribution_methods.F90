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

!> \brief Methods for the distribution class.
MODULE lamb_distribution_methods

#include "lamb_defs.h"

  use lamb_distribution_types
  USE lamb_error
  use lamb_kinds
  use lamb_mp_env_types
  use lamb_plexus_types
  use lamb_plexus_methods
  use lamb_set_partition_types
  use lamb_set_partition_methods

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_distribution_update
  public :: lamb_dist_get_n_local_sectors
  public :: lamb_dist_get_my_sectors_p
  public :: lamb_distribution_get_mp_env
  public :: lamb_distribution_get_plexus_p

  public :: lamb_dist_localize_index

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'distribution_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb


CONTAINS

  !> \brief Updates distribution according to any changes
  !>(currently [repartitions the set into subsets](@ref lamb_set_partition_methods::lamb_set_partition_update)).
  !> \param[in,out] distribution  Updates this distribution.
  !> \param[in,out] error  LAMB error.
  SUBROUTINE lamb_distribution_update (distribution, error)
    TYPE(lamb_distribution_type), INTENT(INOUT):: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_distribution_update', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle

    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_set_partition_update (distribution%set_partitions(1)%p, error)
    if (distribution%set_partitions(1)%p%id /= distribution%set_partitions(2)%p%id) then
       call lamb_set_partition_update (distribution%set_partitions(2)%p, error)
    endif

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_distribution_update

  !> \brief Returns the number of sectors stored by the local process.
  !> \param[in] distribution  Distribution used to get the local sectors via its plexus.
  !> \param[in,out] error         LAMB error.
  !> \return n_sectors    Number of sectors stored by the local process.
  function lamb_dist_get_n_local_sectors (distribution, error) result (n_sectors)
    TYPE(lamb_distribution_type), INTENT(IN) :: distribution
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    integer(kind=cube_k) :: n_sectors
  
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_dist_get_n_local_sectors', &
         routineP = moduleN//':'//routineN
  
    INTEGER :: error_handle, stat
  
    CALL lamb_error_set (error, error_handle, routineN)
    n_sectors = distribution%plexus%my_sectors_size
    CALL lamb_error_stop (error, error_handle)
  END function lamb_dist_get_n_local_sectors


  !subroutine lamb_dist_get_local_sectors (distribution, error)
  !  TYPE(lamb_distribution_type), INTENT(IN) :: distribution
  !  TYPE(lamb_error_type), INTENT(INOUT) :: error
  !
  !  CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_dist_get_local_sectors', &
  !       routineP = moduleN//':'//routineN
  !
  !  INTEGER :: error_handle, stat
  !
  !  CALL lamb_error_set (error, error_handle, routineN)
  !  stop routineP//'NYI'
  !  CALL lamb_error_stop (error, error_handle)
  !END SUBROUTINE lamb_dist_get_local_sectors

  !> \brief Returns the list of sectors stored by the local process.
  !> \param[in] distribution  Distribution used to get the local sectors via its plexus.
  !> \param[in,out] error         LAMB error.
  !> \return sectors_p Pointer to array describing the local sectors
  !>                   with the format (tuple of subsets, sector_rank).
  function lamb_dist_get_my_sectors_p(distribution, error) result (sectors_p)
    type(lamb_distribution_type), pointerin :: distribution
    type(lamb_error_type), intent(inout) :: error
    integer(kind=subset_k), dimension(:,:), pointer :: sectors_p
    sectors_p => distribution%plexus%my_sectors(:,1:distribution%plexus%my_sectors_size)
  end function lamb_dist_get_my_sectors_p


  !> \brief Returns the message-passing environment on which this distribution is based.
  !> \param[in] distribution  Returns message-passing environment of this distribution.
  !> \return mp_env  The message-passig environment.
  function lamb_distribution_get_mp_env (distribution) result (mp_env)
    type(lamb_distribution_type), intent(in) :: distribution
    type(lamb_mp_env_type), pointer :: mp_env
    mp_env => distribution%plexus%mp_env
  end function lamb_distribution_get_mp_env

  !> \brief Returns the plexus on which this distribution is based.
  !> \param[in] distribution  Returns the plexus of this distribution
  !> \return plexus_p   Pointor to a plexus.
  function lamb_distribution_get_plexus_p (distribution) result (plexus_p)
    type(lamb_distribution_type), intent(in) :: distribution
    type(lamb_plexus_type), pointer :: plexus_p
    plexus_p => distribution%plexus
  end function lamb_distribution_get_plexus_p

  !> \brief Converts global indices of particle tuples to localized indices.
  !>
  !> Returns tuples of subset id and their position in the subset for
  !> a list of given particle tuples. For example, given a single
  !> (atom1, atom2) tuple, this subroutine finds the corresponding
  !> (subset1, subset2) and (local_index1, local_index2) tuples.
  !>
  !> The dimension of the first rank of the global_idx, subset_idx, and
  !> local_idx arrays must match; for a standard matrix the dimension is 2.
  !>
  !> \see \ref lamb_set_part_methdos::lamb_set_part_find_subsets.
  !>
  !> \param[in] dist  This distribution is used to find the local indices.
  !> \param[in] n     The number of particles tuples in the array.
  !> \param[in] global_idx  The global indices of the particle tuples.
  !> \param[out] subset_idx  The subsets of the particles.
  !> \param[out] local_idx  The subset-local index of the particles.
  !> \param[in,out] error  LAMB error
  subroutine lamb_dist_localize_index(dist, n, global_idx,&
       subset_idx, local_idx, error)
    TYPE(lamb_distribution_type), intent(in) :: dist
    integer(kind=allatm_k), intent(in) :: n
    integer(kind=allatm_k), dimension(:,:), intent(in) :: global_idx
    integer(kind=subset_k), dimension(:,:), intent(out) :: subset_idx
    integer(kind=locatm_k), dimension(:,:), intent(out) :: local_idx
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_dist_localize_index', &
         routineP = moduleN//':'//routineN

    type(lamb_plexus_type), pointer :: plexus
    integer :: error_handle, prev_level
    integer :: dim, d

    call lamb_error_set (error, error_handle, routineN)
    call lamb_error_make_ok (error, prev_level)

    dim = size(global_idx,1)
    call lamb_assert(dim, "EQ", size(subset_idx,1),&
         lamb_failure_level, lamb_wrong_args,&
         "Dimension mismatch.", routineN, __LINE__, error)
    call lamb_assert(dim, "EQ", size(local_idx,1),&
         lamb_failure_level, lamb_wrong_args,&
         "Dimension mismatch.", routineN, __LINE__, error)

    call lamb_assert(int(size(global_idx,2),kind=int_big), "EQ", n,&
         lamb_failure_level, lamb_wrong_args,&
         "Index size mismatch.", routineN, __LINE__, error)
    call lamb_assert(int(size(subset_idx,2),kind=int_big), "EQ", n,&
         lamb_failure_level, lamb_wrong_args,&
         "Index size mismatch.", routineN, __LINE__, error)
    call lamb_assert(int(size(local_idx,2),kind=int_big), "EQ", n,&
         lamb_failure_level, lamb_wrong_args,&
         "Index size mismatch.", routineN, __LINE__, error)

    do d = 1, dim
       call lamb_set_part_find_subsets(dist%set_partitions(d)%p,&
            global_idx(d,:),&
            subset_idx(d,:), local_idx(d,:), error)
    enddo

    call lamb_error_stop (error, error_handle)
  end subroutine lamb_dist_localize_index

END MODULE lamb_distribution_methods
