program ReadNumbers
implicit none
integer :: fno = 12, err = 0, lines = 0, out = 0
character*256 :: ctmp
character*256, allocatable :: arr(:)

open(fno, file="input.txt")

do while (err .eq. 0)
    lines = lines + 1
    read(fno, *, iostat=err) ctmp
end do

lines = lines - 1
allocate(arr(lines))
rewind(fno)
read(fno, *) arr

call part1(arr, lines, out)
print '(i0)', out

call part2(arr, lines, out)
print '(i0)', out

close(fno)
end program

subroutine part1(arr, lines, output)
implicit none
integer, intent(in) :: lines
character*256, dimension(lines), intent(in) :: arr
integer, intent(out) :: output

integer :: err = 0, i, j, temp, out = 0
logical :: first = .true.
character :: a, b
character (len = 2) :: str

do i=1,lines
    first = .true.
    do j=1,LEN(arr(i))
        ! Check that the character is a digit.
        if (arr(i)(j:j) .ge. '0' .and. arr(i)(j:j) .le. '9') then
            if (first) then
                a = arr(i)(j:j)
                b = arr(i)(j:j)
                first = .false.
            else
                b = arr(i)(j:j)
            end if
        end if
    end do
    str = a // b
    read(str, *, iostat=err) temp
    if (err == 0) then
        out = out + temp
    else
        print *, err
        exit
    end if
end do
output = out
end subroutine

subroutine part2(arr, lines, output)
implicit none
integer, intent(in) :: lines
character*256, dimension(lines), intent(in) :: arr
integer, intent(out) :: output

integer :: err = 0, i, j, temp, out = 0
logical :: first = .true.
character :: a, b, o
character (len = 2) :: str

do i=1,lines
    first = .true.
    do j=1,LEN(arr(i))
        ! Get the integer here if it exists.
        call repnum(arr(i), j, o)
        if (o .ne. 'e') then
            if (first) then
                a = o
                b = o
                first = .false.
            else
                b = o
            end if
        end if
    end do
    ! Concatenate digits.
    str = a // b

    ! Parse the concatenated digits.
    read(str, *, iostat=err) temp
    if (err == 0) then
        out = out + temp
    else
        print *, err
        exit
    end if
end do
output = out
end subroutine

! Replaces a digit string with a digit.
subroutine repnum(line, j, output)
implicit none
character*256, intent (in) :: line
integer, intent (in) :: j
character, intent (out) :: output

! Check that the position is valid in the array.
if (j .lt. LEN(line)) then
    ! If the value is a digit, return that.
    if (line(j:j) .ge. '0' .and. line(j:j) .le. '9') then
        output = line(j:j)
        return
    end if
    
    ! start with the longest digits to minimize checking.
    if (j+4 .lt. LEN(line)) then
        select case (line(j:j+4))
            case ("three")
                output = '3'
                return
            case ("seven")
                output = '7'
                return
            case ("eight")
                output = '8'
                return
        end select
    end if
    if (j+3 .lt. LEN(line)) then
        select case (line(j:j+3))
            case ("zero")
                output = '0'
                return
            case ("four")
                output = '4'
                return
            case ("five")
                output = '5'
                return
            case ("nine")
                output = '9'
                return
        end select
    end if
    if (j+2 .lt. LEN(line)) then
        select case (line(j:j+2))
            case ("one")
                output = '1'
                return
            case ("two")
                output = '2'
                return
            case ("six")
                output = '6'
                return
        end select
    end if

    output = 'e'
end if
end subroutine

