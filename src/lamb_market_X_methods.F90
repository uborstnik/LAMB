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

!> \brief Methods for the market class.


!MAKE s d c z
MODULE lamb_market_X_methods

#include "lamb_defs.h"

  USE lamb_error
  use lamb_kinds
  use lamb_goods_types
  use lamb_goods_X_types
  use lamb_goods_X_methods
  use lamb_market_X_types
  use lamb_matrix_types
  use lamb_matrix_X_types
  use lamb_matrix_X_methods
  use lamb_mp_env_types
  use lamb_mp_methods
  use lamb_mp_X_methods
  use lamb_mp_mem_util
  use lamb_mp_types
  use lamb_mpi_kinds
  use lamb_mpi_util
  use lamb_plexus_types
  use lamb_plexus_methods

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lamb_market_X_open
  PUBLIC :: lamb_market_X_close

  public :: lamb_market_X_service
  public :: lamb_market_X_request
  public :: lamb_market_X_accept
  public :: lamb_market_X_check_delivery

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: moduleN = 'market_X_methods'

  LOGICAL, PARAMETER :: careful_mod = careful_lamb
  LOGICAL, PARAMETER :: debug_mod = debug_lamb

  INTEGER, PRIVATE, SAVE :: market_X_id

  integer, parameter, private :: request_size = 3


CONTAINS


! *****************************************************************************
!> \brief Open the market for access.
!>
!> \param[in,out] market_X  Opens this market.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_open (market_X, error)
    TYPE(lamb_market_X_type), INTENT(INOUT):: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_open', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: n_sectors, i_sector

    integer(kind=int_big), dimension(lamb_goods_num) :: goods_sizes, max_goods_sizes
    integer(kind=int_big), dimension(lamb_goods_num+1) :: xchg_input, xchg_output
    integer :: i, max_num_goods
    integer(kind=the_mpi_integer_kind) :: tag
    logical, parameter :: debug = debug_mod


    CALL lamb_error_set (error, error_handle, routineN)

    n_sectors = size(market_X%goods)
    max_goods_sizes = 0_int_big
    do i_sector = 1, n_sectors
       call lamb_goods_X_open(market_X%goods(i_sector), market_X%mp_env, error)
       goods_sizes = lamb_goods_X_get_size(market_X%goods(i_sector), error)
       forall (i = 1 : lamb_goods_num)
          max_goods_sizes(i) = MAX(goods_sizes(i), max_goods_sizes(i))
       end forall
    enddo

    xchg_input(1:lamb_goods_num) = max_goods_sizes(1:lamb_goods_num)
    xchg_input(lamb_goods_num+1) = int(n_sectors, kind=int_big)
    if (debug) &
         write(*,*)routineN//" my sizes", xchg_input
    call lamb_mp_max_v (market_X%mp_env,&
         int(lamb_goods_num+1, kind=int_big), xchg_input, xchg_output, error)
    if (debug) &
         write(*,*)routineN//" mx sizes", xchg_output
    market_X%largest_goods(1:lamb_goods_num) = xchg_output(1:lamb_goods_num)
    market_X%most_goods = int(xchg_output(lamb_goods_num+1))

    if (.false.) then
       allocate(market_X%recv_buffers(1)%index(market_X%largest_goods(1)), stat=stat)
       call lamb_memory_check (stat, "market_X%recv_buffers(1)%index", market_X%largest_goods(1), error)
       allocate(market_X%recv_buffers(1)%data(market_X%largest_goods(2)), stat=stat)
       call lamb_memory_check (stat, "market_X%recv_buffers(1)%data", market_X%largest_goods(2), error)
    else
       call lamb_mp_mem_allocate (market_X%recv_buffers(1)%index, market_X%largest_goods(1), error)
       call lamb_mp_mem_allocate (market_X%recv_buffers(1)%data, market_X%largest_goods(2), error)
    endif

    ! Start listening to requests.
    call lamb_mp_nullify_req (market_X%listen_req)
    call lamb_mp_mem_allocate (market_X%listen_request,&
         int(request_size, kind=int_big), error)
    tag = 0_the_mpi_integer_kind
    call lamb_mp_irecv (market_X%mp_env, lamb_mp_any_source,&
         int(request_size, kind=int_big),&
         market_X%listen_request, tag, market_X%listen_req, error)

    do i = 1, 2
       call lamb_mp_nullify_req (market_X%sending_req(i))
       call lamb_mp_nullify_req (market_X%recv_req(i))
    enddo
    ! Currently OK to send a request.
    market_X%recv_n = 0

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_open

! *****************************************************************************
!> \brief Closes access to a market.
!>
!> \param[in,out] market_X  Closes this market.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_close (market_X, error)
    TYPE(lamb_market_X_type), INTENT(INOUT):: market_X
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_close', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat
    integer :: n_sectors, i_sector

    CALL lamb_error_set (error, error_handle, routineN)

    ! Cancel the listener.
    call lamb_mp_cancel(market_X%listen_req, error)

    n_sectors = size(market_X%goods)
    do i_sector = 1, n_sectors
       call lamb_goods_X_close(market_X%goods(i_sector), error)
    enddo

    if (.false.) then
       deallocate(market_X%recv_buffers(1)%index, stat=stat)
       call lamb_memory_check (stat, "market_X%recv_buffers(1)%index", error)
       deallocate(market_X%recv_buffers(1)%data, stat=stat)
       call lamb_memory_check (stat, "market_X%recv_buffers(1)%data", error)
    else
       call lamb_mp_mem_deallocate (market_X%recv_buffers(1)%index, error)
       call lamb_mp_mem_deallocate (market_X%recv_buffers(1)%data, error)
    endif
    call lamb_mp_mem_deallocate (market_X%listen_request, error)

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_close


! *****************************************************************************
!> \brief Services market customers.
!>
!> Checks for data request and dispatches the data.
!>
!> \param[in,out] market_X  Services this market.
!> \param[in,out] error  Error object.
!> \author UB
! *****************************************************************************
  SUBROUTINE lamb_market_X_service (market_X, n_serviced, error)
    TYPE(lamb_market_X_type), INTENT(INOUT):: market_X
    integer, intent(out) :: n_serviced
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_service', &
         routineP = moduleN//':'//routineN

    integer :: i
    INTEGER :: error_handle, stat
    integer :: n_sectors, i_sector

    integer(kind=the_mpi_integer_kind) :: ierr
    type(lamb_mp_op_status_type) :: status
    logical :: flag
    logical, parameter :: debug = .false.
    integer(kind=proc_k) :: src
    integer(kind=the_mpi_integer_kind) :: tag
    integer(kind=cube_k) :: sector_i
    integer(kind=int_big), dimension(lamb_goods_num) :: goods_sizes


    CALL lamb_error_set (error, error_handle, routineN)

    call lamb_mp_test (market_X%mp_env, market_X%listen_req, flag, status, error)
    if (flag) then
       n_serviced = 1
       if (debug) then
          write(*,*)routineN//' servicing request'
       endif
       call lamb_mp_interpret_status(status, src, tag, ierr, error)
       ! The ierr should be ignored here ; any error is already
       ! registered in the return call of the MPI_Test.--
       if (debug) then
          write(*,*)routineN//' received request from', src,' with tag',tag,&
               ';',market_X%listen_request
          write(*,*)routineN//' request is', market_X%listen_request
       endif

       ! Currently only one request can be serviced at a time. Wait
       ! for the current one to finish.
       do i = 1, 2
          if (lamb_mp_req_exists (market_X%sending_req(i))) then
             if (debug) then
                write(*,*)routineN//' Previous request is still being serviced.'
             endif
             call lamb_mp_wait(market_X%mp_env,&
                  market_X%sending_req(i), status, error)
          endif
       enddo

       ! Sends off the requested data.
       sector_i = lamb_plexus_get_sector_i(market_X%plexus, &
            int(market_X%listen_request(1:2), kind=subset_k),&
            error)
       if (careful_mod) then
          call lamb_assert(int(sector_i), "LE", size(market_X%goods),&
               lamb_warning_level, lamb_internal_error,&
               "Invalid sector reference.",&
               routineN, __LINE__, error)
       endif
       goods_sizes = lamb_goods_X_get_size(market_X%goods(sector_i), error)
       tag = market_X%listen_request(3) 
       call lamb_mp_isend(market_X%mp_env, src,&
            goods_sizes(1), market_X%goods(sector_i)%index,&
            tag, market_X%sending_req(1), error)
       call lamb_mp_isend(market_X%mp_env, src,&
            goods_sizes(2), market_X%goods(sector_i)%data,&
            tag+1, market_X%sending_req(2), error)

       ! Now listen for the next request.
       tag = 0_the_mpi_integer_kind
       call lamb_mp_irecv (market_X%mp_env, lamb_mp_any_source,&
            int(request_size, kind=int_big),&
            market_X%listen_request, tag, market_X%listen_req, error)
    else
       n_serviced = 0
       if (debug) then
          write(*,*)routineN//' nothing to service'
       endif
    endif

    CALL lamb_error_stop (error, error_handle)
  END SUBROUTINE lamb_market_X_service

  subroutine lamb_market_X_request(market, src, what, error)
    TYPE(lamb_market_X_type), INTENT(INOUT):: market
    integer(kind=proc_k), intent(in) :: src
    integer, dimension(:), intent(in) :: what
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_request', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, i

    integer(kind=the_mpi_integer_kind) :: ierr
    type(lamb_mp_op_status_type) :: status
    integer(kind=the_mpi_integer_kind) :: tag

    logical, parameter :: debug = .false.

    CALL lamb_error_set (error, error_handle, routineN)

    if (debug) then
       write(*,*)routineN//' requesting data from proc.', src,';',what
    endif
    if (careful_mod) then
       call lamb_assert (request_size, "EQ", size(what),&
            lamb_warning_level, lamb_internal_error,&
            "Mismatch is request descriptor size", routineN, __LINE__, error)
    endif

    if (.false.) then
       do i = 1, 2
          if (lamb_mp_req_exists (market%recv_req(i))) then
             call lamb_mp_wait(market%mp_env,&
                  market%recv_req(i), status, error)
             if (debug) then
                write(*,*)'BW/MB/s=',lamb_mp_req_get_bw(market%recv_req(i))/(10.0**6)
             endif
          endif
       enddo
    endif

    ! Setup data receives.
    tag = int(what(3), kind=the_mpi_integer_kind)
    call lamb_mp_irecv(market%mp_env, src,&
         int(market%largest_goods(1),kind=int_big), market%recv_buffers(1)%index,&
         tag, market%recv_req(1), error)
    tag = tag + int(1, kind=the_mpi_integer_kind)
    call lamb_mp_irecv(market%mp_env, src,&
         int(market%largest_goods(2),kind=int_big), market%recv_buffers(1)%data,&
         tag, market%recv_req(2), error)

    ! Send the request.
    tag = 0_the_mpi_integer_kind
    call lamb_mp_send(market%mp_env, src, int(request_size,kind=int_big), what, tag, error)
    if (debug) then
       write(*,*)routineN//' request sent.'
    endif

    CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_market_X_request

  ! Waits for request (i.e., pull request) to finish.
  subroutine lamb_market_X_accept(market, error)
    TYPE(lamb_market_X_type), INTENT(INOUT):: market
    TYPE(lamb_error_type), INTENT(INOUT) :: error

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_accept', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, i

    integer(kind=the_mpi_integer_kind) :: ierr
    type(lamb_mp_op_status_type) :: status
    integer(kind=the_mpi_integer_kind) :: tag

    logical, parameter :: debug = .false.

    CALL lamb_error_set (error, error_handle, routineN)

    do i = 1, 2
       call lamb_mp_wait(market%mp_env,&
            market%recv_req(i), status, error)
       if (debug) then
          write(*,*)'BW/MB/s=',lamb_mp_req_get_bw(market%recv_req(i))/(10.0**6)
       endif
    enddo

    CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_market_X_accept

  ! Checks if a request has been delivery.
  subroutine lamb_market_X_check_delivery(market, delivered, error)
    TYPE(lamb_market_X_type), INTENT(INOUT):: market
    TYPE(lamb_error_type), INTENT(INOUT) :: error
    logical, intent(out) :: delivered

    CHARACTER(LEN=*), PARAMETER :: routineN = 'lamb_market_X_check_delivery', &
         routineP = moduleN//':'//routineN

    INTEGER :: error_handle, stat, i

    integer(kind=the_mpi_integer_kind) :: ierr
    type(lamb_mp_op_status_type) :: status
    integer(kind=the_mpi_integer_kind) :: tag
    logical :: flag

    logical, parameter :: debug = .false.

    CALL lamb_error_set (error, error_handle, routineN)

    delivered = .true.
    do i = 1, 2
       call lamb_mp_test (market%mp_env, market%recv_req(i), flag, status, error)
       delivered = delivered .and. flag
    enddo

    CALL lamb_error_stop (error, error_handle)
  end subroutine lamb_market_X_check_delivery


END MODULE lamb_market_X_methods
