program main
    implicit none
    integer, dimension(:,:), allocatable :: rounds
    integer :: fn = 12, err = 0, i = 1, rlen, lines = 0
    integer*16 :: out
    logical :: test = .false.
    character*256 :: ctmp
    character*256, allocatable :: arr(:)

    if (test) then
        open(fn, file="test.txt")
        rlen = 3
    else
        open(fn, file="input.txt")
        rlen = 4
    end if

    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do

    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr
    close(fn)

    allocate(rounds(rlen,2))

    read(arr(1)(index(arr(1), ":")+1:), *, iostat=err) rounds(:,1)
    read(arr(2)(index(arr(2), ":")+1:), *, iostat=err) rounds(:,2)

    call part1(rounds, rlen, out)
    print '(I0)', out

    call part2(rounds, rlen, out)
    print '(I0)', out
end program

subroutine part1(rounds, rlen, out)
    implicit none
    integer, intent(in) :: rlen
    integer, dimension(rlen, 2), intent(in) :: rounds
    integer*16, intent(out) :: out
    integer :: i, j, err, outval

    out = 1
    ! For each round.
    do i=1,rlen
        outval = 0
        ! For each possible millisecond in that round.
        do j=0,rounds(i,1)
            if((rounds(i,1)-j)*j .gt. rounds(i,2)) then
                outval = outval + 1
            end if
        end do
        out = out * outval
    end do
end subroutine

subroutine part2(rounds, rlen, out)
    implicit none
    integer, intent(in) :: rlen
    integer, dimension(rlen, 2), intent(in) :: rounds
    integer*16, intent(out) :: out
    integer*16 :: i, j, err, time, dist
    character*256 :: ctmp, val

    ! There's only one round, concatenate the values.
    write(val, *) rounds(1,1)
    do i=2,rlen
        write(ctmp, '(i0)') rounds(i,1)
        write(val, '(A)') trim(val)//trim(ctmp)
    end do
    read(val, *) time
    write(val, *) rounds(1,2)
    do i=2,rlen
        write(ctmp, '(i0)') rounds(i,2)
        write(val, '(A)') trim(val)//trim(ctmp)
    end do
    read(val, *) dist

    out = 0
    ! For each possible millisecond in that round.
    do j=1,time
        print *, j
        if((time-j)*j .gt. dist) then
            out = out + 1
        end if
    end do
end subroutine
