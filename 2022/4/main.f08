program main
integer, allocatable, dimension(:,:) :: arr
character*256 :: ctmp, str
integer :: lines = 0, err = 0, fn = 12, out, i, ind

open(fn, file="input.txt")

do while(err .eq. 0)
    lines = lines + 1
    read(fn, '(A10)', iostat=err) ctmp
end do

lines = lines - 1
allocate(arr(lines, 4))
rewind(fn)

call readarr(fn, arr, lines)

call part1(arr, lines, out)
print '(i0)', out

call part2(arr, lines, out)
print '(i0)', out

close(fn)
end program

subroutine part1(arr, lines, out)
integer, intent(in) :: lines
integer, dimension(lines, 4), intent(in) :: arr
integer, intent(out) :: out

integer :: i, sum = 0, min1, min2, max1, max2

do i=1,lines
    min1 = min(arr(i, 1), arr(i, 2))
    min2 = min(arr(i, 3), arr(i, 4))

    max1 = max(arr(i, 1), arr(i, 2))
    max2 = max(arr(i, 3), arr(i, 4))

    if (min1 .le. min2 .and. max1 .ge. max2) then
        sum = sum + 1
    else if (min2 .le. min1 .and. max2 .ge. max1) then
        sum = sum + 1
    end if
end do
out = sum
end subroutine

subroutine part2(arr, lines, out)
integer, intent(in) :: lines
integer, dimension(lines, 4), intent(in) :: arr
integer, intent(out) :: out

integer :: i, sum = 0, min1, min2, max1, max2

do i=1,lines
    min1 = min(arr(i, 1), arr(i, 2))
    min2 = min(arr(i, 3), arr(i, 4))

    max1 = max(arr(i, 1), arr(i, 2))
    max2 = max(arr(i, 3), arr(i, 4))

    if (min1 .le. min2 .and. max1 .ge. min2) then
        sum = sum + 1
    else if (min2 .le. min1 .and. max2 .ge. min1) then
        sum = sum + 1
    end if
end do
out = sum
end subroutine

subroutine readarr(fn, arr, lines)
integer, intent(in) :: lines, fn
integer, dimension(lines, 4), intent(inout) :: arr
character*256 :: str
integer :: err = 0, i

do i=1,lines
    read(fn, '(A12)', iostat=err) str
    if (str(2:2) .eq. '-') then
        read(str(1:1), *, iostat=err) arr(i, 1)
        str = str(3:len_trim(str))
    else
        read(str(1:2), *, iostat=err) arr(i, 1)
        str = str(4:len_trim(str))
    end if

    if (str(2:2) .eq. ',') then
        read(str(1:1), *, iostat=err) arr(i, 2)
        str = str(3:len_trim(str))
    else
        read(str(1:2), *, iostat=err) arr(i, 2)
        str = str(4:len_trim(str))
    end if

    if (str(2:2) .eq. '-') then
        read(str(1:1), *, iostat=err) arr(i, 3)
        str = str(3:len_trim(str))
    else
        read(str(1:2), *, iostat=err) arr(i, 3)
        str = str(4:len_trim(str))
    end if
    read(str, *, iostat=err) arr(i, 4)
end do
end subroutine
