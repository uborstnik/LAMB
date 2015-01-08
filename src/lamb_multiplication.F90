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

!> \brief The LAMB matrix multiplication functions.

MODULE lamb_multiplication

#include "lamb_defs.h"

  use lamb_constants
  use lamb_data_types
  use lamb_data_d_types
  use lamb_distribution_types
  use lamb_distribution_methods
  USE lamb_error
  use lamb_goods_d_types
  use lamb_goods_methods
  use lamb_index_types
  use lamb_index_methods
  use lamb_io_operations
  use lamb_kinds
  use lamb_market_types
  use lamb_market_methods
  use lamb_matrix_d_types
  use lamb_matrix_types
  use lamb_matrix_methods
  use lamb_mp_env_types
  use lamb_mp_env_methods
  use lamb_mp_methods
  use lamb_mp_l_methods
  use lamb_mult_sector_d_types
  use lamb_plexus_types
  use lamb_plexus_methods
  use lamb_plexus_operations
  use lamb_sched_types
  use lamb_set_partition_types
  use lamb_set_partition_methods
  use lamb_sector_types
  use lamb_sector_methods

  IMPLICIT NONE

  PRIVATE

  public :: lamb_multiply, lamb_multiply_test

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'multiplication'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = .FALSE.

CONTAINS

  subroutine lamb_multiply_test(mp_env, error)
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    type(lamb_mp_env_type), pointerin :: mp_env

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_multiply_test', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, old_handle
    TYPE(lamb_matrix_d_type), pointer :: matrix_a
    type(lamb_matrix_d_type) ::  matrix_b, matrix_c
    logical, parameter :: debug = .true.

    CALL lamb_error_set (error, error_handle, routineN)

    if (debug) then
       write(*,*)
       write(*,*)routineN//" Running DBCSR multiplication test"
    endif
    call lamb_load_dbcsr (matrix_a, "small.dbcsr", mp_env, error)
    call lamb_matrix_copy (matrix_a, matrix_b, error)
    call lamb_matrix_copy (matrix_a, matrix_c, error)

    call lamb_multiply(matrix_a, matrix_b, matrix_c, error)

    call lamb_matrix_release (matrix_a, error)
    call lamb_matrix_destroy (matrix_b, error)
    call lamb_matrix_destroy (matrix_c, error)

    CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_multiply_test

  subroutine lamb_multiply(matrix_a, matrix_b, matrix_c, error)
    type(lamb_matrix_d_type), intent(in) :: matrix_a, matrix_b
    type(lamb_matrix_d_type), intent(inout) :: matrix_c
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_multiply', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle
    type(lamb_distribution_type) :: dist_a, dist_b, dist_c
    integer :: n_sectors_a, n_sectors_b, i_sector_a, i_sector_b, i_sector_c
    type(lamb_plexus_type), pointer :: plexus_c
    integer(kind=cube_k), dimension(:,:), pointer :: sectors_a, sectors_b, sectors_c
    integer(kind=subset_k) :: subset_m, subset_n, subset_k
    type(lamb_sched_type) :: sched
    type(lamb_market_d_type) :: market_a, market_b
    type(lamb_mult_sector_d_type) :: mult_sector_a, mult_sector_b

    logical, parameter :: debug = debug_mod

    call lamb_error_set (error, error_handle, routineN)

    dist_a = lamb_matrix_get_distribution (matrix_a)
    dist_b = lamb_matrix_get_distribution (matrix_b)
    call lamb_assert (dist_a%id, "EQ", dist_b%id, &
         lamb_warning_level, lamb_wrong_args,&
         "A, B distributions do not match.",&
         routineN, __LINE__, error)
    dist_c = lamb_matrix_get_distribution (matrix_c)
    call lamb_assert (dist_a%id, "EQ", dist_c%id, &
         lamb_warning_level, lamb_wrong_args,&
         "A, C distributions do not match.",&
         routineN, __LINE__, error)
    plexus_c => lamb_distribution_get_plexus_p(dist_c)

    call lamb_plexus_create_sched(plexus_c, sched, error)

    call lamb_mult_sector_d_create(mult_sector_a, error)
    call lamb_mult_sector_d_create(mult_sector_b, error)

    call lamb_market_create(market_a, matrix_a, error)
    call lamb_market_create(market_b, matrix_b, error)
    call lamb_market_open(market_a, error)
    call lamb_market_open(market_b, error)

    call service_schedule(sched, matrix_a, matrix_b, matrix_c,&
         market_a, market_b, mult_sector_a, mult_sector_b, error)

    call lamb_market_close(market_a, error)
    call lamb_market_close(market_b, error)

    call lamb_mult_sector_d_destroy(mult_sector_a, error)
    call lamb_mult_sector_d_destroy(mult_sector_b, error)

    call lamb_mp_env_barrier(market_a%mp_env, error)

    call lamb_market_destroy(market_a, error)
    call lamb_market_destroy(market_b, error)

    !STOP routineN//' do something with the schedule'
    call lamb_sched_destroy (sched, error)

    n_sectors_a = lamb_matrix_get_n_sectors(matrix_a, error)
    n_sectors_b = lamb_matrix_get_n_sectors(matrix_b, error)
    write(*,*)routineN//' n_sectors', n_sectors_a, n_sectors_b, lamb_dist_get_n_local_sectors(dist_c, error)
    write(*,*)routineN//' n_sectors', size(matrix_a%sectors), size(matrix_c%sectors), size(matrix_c%sectors)
    do i_sector_a = 1_cube_k, n_sectors_a
       subset_m = matrix_a%sectors(i_sector_a)%subsets(1)%p%pos
       subset_k = matrix_a%sectors(i_sector_a)%subsets(2)%p%pos
       do i_sector_b = 1_cube_k, n_sectors_b
          subset_n = matrix_b%sectors(i_sector_b)%subsets(2)%p%pos
          if (debug) then
             write(*,'(A,"(",2(I5),")?(",2(I5),")")')routineN//' Subset mult',&
                  subset_m, subset_k, &
                  matrix_b%sectors(i_sector_b)%subsets(1)%p%pos, subset_n
          endif
          if (subset_k == matrix_b%sectors(i_sector_b)%subsets(1)%p%pos) then
             i_sector_c = lamb_plexus_get_sector_i(plexus_c, (/ subset_m, subset_n /), error)
             if (debug) then
                write(*,*)routineN//' sector idxs', i_sector_a, i_sector_b, i_sector_c
             endif
             if (i_sector_c > 0) then
                call sector_multiply (matrix_a%sectors(i_sector_a),&
                     matrix_b%sectors(i_sector_b), matrix_c%sectors(i_sector_c), error)
             endif
          endif
       enddo
    enddo

    call lamb_error_stop (error, error_handle)
  end subroutine lamb_multiply

  subroutine sector_multiply(sector_a, sector_b, sector_c, error)
    type(lamb_sector_d_type), intent(in) :: sector_a, sector_b
    type(lamb_sector_d_type), intent(inout) :: sector_c
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'sector_multiply', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle

    logical, parameter :: debug = debug_mod

    call lamb_error_set (error, error_handle, routineN)

    if (debug) then
       write(*,'(A,"(",2(I5),")?(",2(I5),")")')routineN//' Subset mult',&
            sector_a%subsets(1)%p%pos, sector_a%subsets(2)%p%pos,  &
            sector_b%subsets(1)%p%pos, sector_b%subsets(2)%p%pos
    endif

    call lamb_error_stop (error, error_handle)
  end subroutine sector_multiply

  subroutine service_schedule(schedule, matrix_a, matrix_b, matrix_c,&
       market_a, market_b, mult_sector_a, mult_sector_b, error)
    type(lamb_sched_type), intent(in) :: schedule
    type(lamb_matrix_d_type), intent(in) :: matrix_a, matrix_b
    type(lamb_matrix_d_type), intent(inout) :: matrix_c
    type(lamb_market_d_type), intent(inout) :: market_a, market_b
    type(lamb_mult_sector_d_type), intent(inout) :: mult_sector_a, mult_sector_b
    type(lamb_error_type), intent(inout) :: error
    CHARACTER(LEN=*), PARAMETER :: routineN = 'service_schedule', &
         routineP = moduleN//':'//routineN
    INTEGER :: error_handle

    integer :: step, era, prev_era, what_size, n_serviced
    logical :: redo, ready, wait_for_a_delivery, wait_for_b_delivery
    character(len=3) :: action
    integer(kind=proc_k) :: src, dst
    integer, dimension(16) :: what
    logical, parameter :: debug = debug_mod
    integer, dimension(16) :: req_data

    call lamb_error_set (error, error_handle, routineN)

    what_size = size(schedule%what,1)
    if (debug) then
       write(*,*)routineN//' what size=',what_size
    endif
    call lamb_assert(what_size, "LE", size(what),&
         lamb_fatal_level, lamb_internal_error,&
         "WHAT description size is too small.",&
         routineN, __LINE__, error)

    prev_era = 0
    step = 1
    wait_for_a_delivery = .false.
    wait_for_b_delivery = .false.
    do while (step < schedule%n_steps)
       era = schedule%eras(step)
       if (debug) &
            write(*,*)routineN//' step',step,'era',era
       if (era < 0) EXIT
       call lamb_market_service(market_a, n_serviced, error)
       call lamb_market_service(market_b, n_serviced, error)
       if (wait_for_a_delivery .and. wait_for_b_delivery) then
          ! Waits for goods to be received.
          call lamb_market_check_delivery(market_a, ready, error)
          if (ready) then
             if (debug) write(*,*)routineN//' a delivery is ready'
             call lamb_market_check_delivery(market_b, ready, error)
             if (ready .and. debug) write(*,*)routineN//' b delivery is ready'
          endif
          if (ready) then
             if (debug) write(*,*)'ready!'
             call lamb_market_accept(market_a, error)
             call lamb_market_accept(market_b, error)
             call lamb_goods_unpack(market_a%recv_buffers(1), mult_sector_a, error)
             call lamb_goods_unpack(market_b%recv_buffers(1), mult_sector_b, error)
             if (debug) &
                  write(*,*)'do local stuff'
             wait_for_a_delivery = .false.
             wait_for_b_delivery = .false.
          endif
       endif
       if (.not. (wait_for_a_delivery .and. wait_for_b_delivery)) then
          what(1:what_size) = schedule%what(:,step)
          src = schedule%pairs(1,step)
          dst = schedule%pairs(2,step)
          if (debug) &
               write(*,*)routineN//' step',step,':',src,"->",dst,':',what(1:what_size)
          req_data(1:2) = what(2:what_size)
          req_data(3) = step * 2
          select case (what(1))
          case(1)
             if (.not. wait_for_a_delivery) then
                  call lamb_market_request(market_a, src, req_data(1:3), error)
                  step = step + 1
                  prev_era = era
               endif
          case(2)
             if (.not. wait_for_b_delivery) then
                call lamb_market_request(market_b, src, req_data(1:3), error)
                step = step + 1
                prev_era = era
             end if
          case default
             call lamb_error_throw(error, lamb_warning_level, lamb_internal_error,&
                  "Invalid schedule descriptor",&
                  HERE)
          end select
       end if
    end do
    call lamb_mp_env_barrier(market_a%mp_env, error)
    call lamb_mp_env_barrier(market_b%mp_env, error)
    call lamb_error_stop (error, error_handle)
  end subroutine service_schedule

END MODULE lamb_multiplication
