program main
implicit none

character*3, allocatable :: arr(:)
character*3 :: ctmp
integer :: fn = 12, lines = 0, err = 0, out

open(fn, file="input.txt")

do while (err .eq. 0)
    lines = lines + 1
    read(fn, '(a3)', iostat=err) ctmp
end do

lines = lines - 1
allocate(arr(lines))
rewind(fn)

read(fn, '(a3)') arr

call part1(arr, lines, out)
print '(i0)', out

call part2(arr, lines, out)
print '(i0)', out

close(fn)
end program

! Score criteria:
! Rock -> 1
! Paper -> 2
! Scissors -> 3

! Loss -> 0
! Tie -> 3
! Win -> 6
subroutine part1(arr, lines, out)
implicit none
integer, intent(in) :: lines
character*3, dimension(lines), intent(in) :: arr
integer, intent(out) :: out

integer :: i, sc = 0, round = 0, me, you, sympick, score

do i=1,lines
    you = sympick(arr(i)(1:1))
    me = sympick(arr(i)(3:3))

    if (me .lt. 0 .or. you .lt. 0) then
        print *, "An error occured in function sympick on file line ", i, ". Value: ", arr(i)
        exit
    end if

    round = score(me, you)

    if (round .lt. 0) then
        print *, "An error occured in function score on file line ", i, ". Value: ", arr(i)
        exit
    end if

    sc = sc + round + me
end do
out = sc
end subroutine

subroutine part2(arr, lines, out)
implicit none
integer, intent(in) :: lines
character*3, dimension(lines), intent(in) :: arr
integer, intent(out) :: out

integer :: i, sc = 0, round = 0, me, you, sympick, score, decide

do i=1,lines
    you = sympick(arr(i)(1:1))
    ! Decide which choice should be made based on the symbol selected.
    me = decide(you, sympick(arr(i)(3:3)))

    if (me .lt. 0 .or. you .lt. 0) then
        print *, "An error occured in function sympick on file line ", i, ". Value: ", arr(i)
        exit
    end if

    round = score(me, you)

    if (round .lt. 0) then
        print *, "An error occured in function score on file line ", i, ". Value: ", arr(i)
        exit
    end if

    sc = sc + round + me
end do
out = sc
end subroutine

integer function sympick(ch)
implicit none
CHARACTER, intent(in) :: ch

select case(ch)
    case('A')
        sympick = 1
        return
    case('B')
        sympick = 2
        return
    case('C')
        sympick = 3
        return
    case('X')
        sympick = 1
        return
    case('Y')
        sympick = 2
        return
    case('Z')
        sympick = 3
        return
end select
sympick = - 1
end function sympick

integer function decide(a, b)
implicit none
integer, intent(in) :: a, b

if (a .eq. 1) then
    select case(b)
        case(1)
            decide = 3
            return
        case(2)
            decide = 1
            return
        case(3)
            decide = 2
            return
    end select
else if (a .eq. 2) then
    select case(b)
        case(1)
            decide = 1
            return
        case(2)
            decide = 2
            return
        case(3)
            decide = 3
            return
    end select
else if (a .eq. 3) then
    select case(b)
        case(1)
            decide = 2
            return
        case(2)
            decide = 3
            return
        case(3)
            decide = 1
            return
    end select
end if
decide = -1
end function decide

integer function score(a, b)
implicit none
integer, intent(in) :: a, b

if (a .eq. 1) then
    select case(b)
        case(1)
            score = 3
            return
        case(2)
            score = 0
            return
        case(3)
            score = 6
            return
    end select
else if (a .eq. 2) then
    select case(b)
        case(1)
            score = 6
            return
        case(2)
            score = 3
            return
        case(3)
            score = 0
            return
    end select
else if (a .eq. 3) then
    select case(b)
        case(1)
            score = 0
            return
        case(2)
            score = 6
            return
        case(3)
            score = 3
            return
    end select
end if
score = -1
end function score
