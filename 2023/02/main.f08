program main
implicit none
character*256, allocatable :: arr(:)
character*256 :: ctmp
integer :: fn = 12, lines = 0, err = 0, out = 0
open(fn, file="input.txt")
do while(err .eq. 0)
    lines = lines + 1
    read (fn, '(A)', iostat=err) ctmp
end do
lines = lines - 1
allocate(arr(lines))
rewind(fn)
read(fn, '(A)') arr
call part1(arr, lines, out)
print '(i0)', out
call part2(arr, lines, out)
print '(i0)', out
close(fn)
end program

subroutine part1(arr, lines, out)
implicit none
integer, intent(in) :: lines
character*256, dimension(lines), intent(in) :: arr
integer, intent(out) :: out
integer :: i, err = 0, red = 12, green = 13, blue = 14, output = 0
integer, dimension(3) :: cols = 0
character*256 :: str, hand
do i=1,lines
    cols = 0
    str = arr(i)((index(arr(i), ':') + 1):len_trim(arr(i)))
    do while (index(str, ';') .ne. 0)
        hand = str(1:index(str, ';'))
        call handleround(hand, cols)
        str = str((index(str, ';') + 1):len_trim(str))
    end do
    call handleround(str, cols)
    if (cols(1) .le. red .and. cols(2) .le. green .and. cols(3) .le. blue) then
        output = output + i
    end if
end do
out = output
end subroutine

subroutine part2(arr, lines, out)
implicit none
integer, intent(in) :: lines
character*256, dimension(lines), intent(in) :: arr
integer, intent(out) :: out
integer :: i, err = 0, output = 0
integer, dimension(3) :: cols = 0
character*256 :: str, hand
do i=1,lines
    cols = 0
    str = arr(i)((index(arr(i), ':') + 1):len_trim(arr(i)))
    do while (index(str, ';') .ne. 0)
        hand = str(1:index(str, ';'))
        call handleround(hand, cols)
        str = str((index(str, ';') + 1):len_trim(str))
    end do
    call handleround(str, cols)
    print *, cols
    output = output + (cols(1) * cols(2) * cols(3))
    print *, output
end do
out = output
end subroutine

subroutine handleround(str, colours)
implicit none
character*256, intent(inout) :: str
integer, dimension(3), intent(inout) :: colours
character*256 :: temp
integer :: ind
ind = index(str, ',')
do while (ind .ne. 0)
    temp = str(1:ind)
    call setcolour(temp, colours)
    str = str((ind + 1):len_trim(str))
    ind = index(str, ',')
end do
call setcolour(str, colours)
end subroutine

subroutine setcolour(str, colours)
implicit none
character*256, intent(in) :: str
integer, dimension(3), intent(inout) :: colours
integer :: err = 0, tint = 0
if (index(str, "red") .ne. 0) then
    read(str, *, iostat=err) tint
    if (tint .gt. colours(1)) then
        colours(1) = tint
    end if
end if
if (index(str, "green") .ne. 0) then
    read(str, *, iostat=err) tint
    if (tint .gt. colours(2)) then
        colours(2) = tint
    end if
end if
if (index(str, "blue") .ne. 0) then
    read(str, *, iostat=err) tint
    if (tint .gt. colours(3)) then
        colours(3) = tint
    end if
end if
end subroutine
