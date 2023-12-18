program main
    implicit none
    character(len=256), dimension(:), allocatable :: arr
    character(len=256) :: ctmp
    integer :: lines = 0, err = 0, fn = 12, out, i

    open(fn, file="input.txt")

    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do

    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr

    call part1(arr, lines, out)
    print '(i0)', out

    call part2(arr, lines, out)
    print '(i0)', out
end program

subroutine part1(arr, lines, out)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(in) :: arr
    character(len=256), dimension(lines) :: tmparr
    integer, intent(out) :: out
    integer :: i, j, tmp, support_load

    ! Copy array for mutation.
    tmparr = arr
    call float_north(tmparr, lines)

    out = support_load(tmparr, lines)
end subroutine

subroutine part2(arr, lines, out)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(in) :: arr
    character(len=256), dimension(lines) :: tmparr
    integer, intent(out) :: out
    integer :: i, j, tmp, support_load

    ! Copy array for mutation.
    tmparr = arr

    ! Run a billion times.
    do i=1,1000000000
        print '(i0)', i
        call float_north(tmparr, lines)
        call float_west(tmparr, lines)
        call float_south(tmparr, lines)
        call float_east(tmparr, lines)
    end do

    out = support_load(arr, lines)
end subroutine

function support_load(arr, lines) result(out)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(in) :: arr
    integer :: out, i, j, tmp
    out = 0
    do i=1,lines
        tmp = 0
        do j=1,len_trim(arr(i))
            if (arr(i)(j:j) .eq. 'O') then
                tmp = tmp + 1
            end if
        end do
        out = out + tmp*(lines - i + 1)
    end do
end function support_load

subroutine float_north(arr, lines)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(inout) :: arr
    integer :: i, j, k
    character :: tmp
    ! Sort each column.
    do i=1, len_trim(arr(1))
        ! Go through each position.
        do j=1, lines
            if (arr(j)(i:i) .eq. 'O') then
                ! Bubble sort back.
                k = j-1
                do while (k .ge. 1 .and. arr(k)(i:i) .ne. '#')
                    tmp = arr(k+1)(i:i)
                    arr(k+1)(i:i) = arr(k)(i:i)
                    arr(k)(i:i) = tmp
                    k = k - 1
                end do
            end if
        end do
    end do
end subroutine

subroutine float_south(arr, lines)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(inout) :: arr
    integer :: i, j, k
    character :: tmp
    ! Sort each column.
    do i=1, len_trim(arr(1))
        ! Go through each position.
        do j=lines,1,-1
            if (arr(j)(i:i) .eq. 'O') then
                ! Bubble sort forwards.
                k = j
                do while (k .lt. lines .and. arr(k+1)(i:i) .ne. '#')
                    tmp = arr(k+1)(i:i)
                    arr(k+1)(i:i) = arr(k)(i:i)
                    arr(k)(i:i) = tmp
                    k = k + 1
                end do
            end if
        end do
    end do
end subroutine

subroutine float_east(arr, lines)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(inout) :: arr
    integer :: i, j, k, length
    character :: tmp
    length = len_trim(arr(1))
    do j=1, lines
        do i=length,1,-1
            if (arr(j)(i:i) .eq. 'O') then
                ! Bubble sort forwards.
                k = i
                do while (k .lt. length .and. arr(j)(k+1:k+1) .ne. '#')
                    tmp = arr(j)(k:k)
                    arr(j)(k:k) = arr(j)(k+1:k+1)
                    arr(j)(k+1:k+1) = tmp
                    k = k + 1
                end do
            end if
        end do
    end do
end subroutine

subroutine float_west(arr, lines)
    implicit none
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(inout) :: arr
    integer :: i, j, k
    character :: tmp
    ! Sort each column.
    do i=1, len_trim(arr(1))
        ! Go through each position.
        do j=1, lines
            if (arr(i)(j:j) .eq. 'O') then
                ! Bubble sort back.
                k = j-1
                do while (k .ge. 1 .and. arr(i)(k:k) .ne. '#')
                    tmp = arr(i)(k+1:k+1)
                    arr(i)(k+1:k+1) = arr(i)(k:k)
                    arr(i)(k:k) = tmp
                    k = k - 1
                end do
            end if
        end do
    end do
end subroutine
