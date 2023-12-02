program main
implicit none

character*256, allocatable :: arr(:)
character*256 :: ctmp
integer :: lines = 0, err = 0, fn = 12, out

open(fn, file="input.txt")

do while(err .eq. 0)
    lines = lines + 1
    read(fn, *, iostat=err) ctmp
end do

lines = lines -1
allocate(arr(lines))
rewind(fn)

read(fn, *) arr

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

integer :: i, ind, sum = 0, halfway, priority
character :: sym

do i=1,lines
    halfway = len_trim(arr(i))/2
    ind = scan(arr(i)(1:halfway), arr(i)(halfway+1:len_trim(arr(i))))
    sym = arr(i)(ind:ind)
    sum = sum + priority(sym)
end do
out = sum
end subroutine

subroutine part2(arr, lines, out)
implicit none
integer, intent(in) :: lines
character*256, dimension(lines), intent(in) :: arr
integer, intent(out) :: out

integer :: i, j, ind, sum = 0, priority
character :: sym

do i=3,lines, 3
    sym = '0'
    do j=1,len_trim(arr(i))
        if (scan(arr(i)(j:j), arr(i-1)) .ne. 0 .and. scan(arr(i)(j:j), arr(i-2)) .ne. 0) then
            sym = arr(i)(j:j)
            exit
        end if
    end do

!    print *, sym, " ", priority(sym), " ", arr(i)

    sum = sum + priority(sym)
end do
out = sum
end subroutine

integer function priority(sym)
character, intent(in) :: sym

if (sym .ge. 'a' .and. sym .le. 'z') then
    priority = iachar(sym) - 96
else if (sym .ge. 'A' .and. sym .le. 'Z') then
    priority = iachar(sym) - 38
else
    priority = 0
end if
end function priority
    
    
