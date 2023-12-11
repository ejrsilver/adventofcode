module aoc_polygon
    type :: Point
        integer :: x, y
    end type
    type(Point), allocatable :: p(:)
    integer :: p_len

    public :: init_poly, getstart, pointinpoly, p_len, is_intersection, in_poly
contains
    subroutine init_poly(arr, lines)
        integer, intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer :: x, y, tmp
        character :: direction, cur
        ! Allocate polygon array.
        call getstart(arr, lines, x, y, direction)
        tmp = 1
        do while (arr(x)(y:y) .ne. 'S')
            tmp = tmp + 1
            call stepthrough(arr, x, y, direction)
        end do
        allocate(p(tmp))
        p_len = tmp

        ! Add all points to polygon.
        call getstart(arr, lines, x, y, direction)
        ! Add first value (not S)
        p(1)%x = x
        p(1)%y = y
        tmp = 2
        do while (arr(x)(y:y) .ne. 'S')
            call stepthrough(arr, x, y, direction)
            p(tmp)%x = x
            p(tmp)%y = y
            tmp = tmp + 1
        end do
    end subroutine

    subroutine getstart(arr, lines, x, y, direction)
        implicit none
        integer, intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer, intent(out) :: x, y
        character, intent(out) :: direction
        integer :: i, tmp
        character N, S, W, E
        x = 0
        y = 0
        ! Set x and y to the location of S.
        do i=1,lines
            tmp = index(arr(i), 'S')
            if (tmp .ne. 0) then
                x = i
                y = tmp
                exit
            end if
        end do

        if (x .eq. 0 .or. y .eq. 0) then
            print *, "Could not find start symbol S"
            return
        end if
        ! Top is valid.
        N = arr(x-1)(y:y)
        if (N .eq. '|' .or. N .eq. 'F' .or. N .eq. '7') then
            direction = 'N'
        end if
        ! Bottom is valid.
        S = arr(x+1)(y:y)
        if (S .eq. '|' .or. S .eq. 'L' .or. S .eq. 'J') then
            direction = 'S'
        end if
        ! Right is valid.
        E = arr(x)(y+1:y+1)
        if (E .eq. '-' .or. E .eq. '7' .or. E .eq. 'J') then
            direction = 'E'
        end if
        ! Left is valid.
        W = arr(x)(y-1:y-1)
        if (W .eq. '-' .or. W .eq. 'L' .or. W .eq. 'F') then
            direction = 'W'
        end if
        ! Two of the above conditions will be met, but we only care about one.
        if (direction .eq. 'N') then
            x = x - 1
        elseif(direction .eq. 'S') then
            x = x + 1
        elseif(direction .eq. 'W') then
            y = y - 1
        elseif(direction .eq. 'E') then
            y = y + 1
        else
            print *, "No valid directions."
            return
        end if
    end subroutine

    subroutine stepthrough(arr, x, y, direction)
        character(len=512), intent(in) :: arr(*)
        integer, intent(inout) :: x, y
        character, intent(inout) :: direction
        if (direction .eq. 'N') then
            if (arr(x)(y:y) .eq. 'F') then
                y = y + 1
                direction = 'E'
            elseif (arr(x)(y:y) .eq. '7') then
                y = y - 1
                direction = 'W'
            else
                x = x - 1
            end if
        elseif(direction .eq. 'S') then
            if (arr(x)(y:y) .eq. 'L') then
                y = y + 1
                direction = 'E'
            elseif (arr(x)(y:y) .eq. 'J') then
                y = y - 1
                direction = 'W'
            else
                x = x + 1
            end if
        elseif(direction .eq. 'W') then
            if (arr(x)(y:y) .eq. 'F') then
                x = x + 1
                direction = 'S'
            elseif (arr(x)(y:y) .eq. 'L') then
                x = x - 1
                direction = 'N'
            else
                y = y - 1
            end if
        elseif(direction .eq. 'E') then
            if (arr(x)(y:y) .eq. '7') then
                x = x + 1
                direction = 'S'
            elseif (arr(x)(y:y) .eq. 'J') then
                x = x - 1
                direction = 'N'
            else
                y = y + 1
            end if
        else
            print *, "We should not be here."
        end if
    end subroutine

    logical function is_intersection(c)
        character, intent(in) :: c
        is_intersection = c .ne.'I'.and. c .ne.'O'.and. c .ne.'-'
    end function is_intersection

    logical function in_poly(x, y)
        integer, intent(in) :: x, y
        integer :: i
        in_poly = .false.
        do i=1,p_len
            if (p(i)%x .eq. x .and. p(i)%y .eq. y) then
                in_poly = .true.
                return
            end if
        end do
        in_poly = .false.
    end function in_poly
end module aoc_polygon

program main
    character(len=512), allocatable :: arr(:)
    character(len=512) :: ctmp
    integer :: fn = 12, lines = 0, err = 0, out
    ! Part 1: test.txt, test2.txt
    ! Part 2: test3.txt, test4.txt, test5.txt
    open(fn, file="input.txt")
    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do
    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr
    call part1(arr ,lines, out)
    print '(i0)', out
    call part2(arr ,lines, out)
    print '(i0)', out
end program

subroutine part1(arr, lines, out)
    use aoc_polygon
    integer, intent(in) :: lines
    character(len=512), dimension(lines), intent(in) :: arr
    integer, intent(out) :: out
    integer :: x ,y, tmp
    character :: direction = '0'
    call getstart(arr, lines, x, y, direction)
    tmp = 0
    do while (arr(x)(y:y) .ne. 'S')
        tmp = tmp + 1
        call stepthrough(arr, x, y, direction)
    end do
    ! Add one if number of steps is odd.
    if (mod(tmp, 2) .eq. 0) then
        out = tmp/2
    else
        out = (tmp/2) + 1
    end if
end subroutine

subroutine part2(arr, lines, out)
    use aoc_polygon
    implicit none
    integer, intent(in) :: lines
    character(len=512), dimension(lines), intent(inout) :: arr
    integer, intent(out) :: out
    integer :: i, j, overlaps
    character :: direction = '0', sym, cur
    call init_poly(arr, lines)
    out = 0
    ! Run through all points.
    do i=1,lines
        overlaps = 0
        j = 1
        do while (j .le. len_trim(arr(i)))
            if (arr(i)(j:j) .eq. '.') then
                ! If we've counted an odd number of overlaps from the left, it's contained.
                if (mod(overlaps, 2) .ne. 0) then
                    arr(i)(j:j) = 'I'
                    out = out + 1
                else
                    arr(i)(j:j) = 'O'
                end if
                j = j + 1
            else
                ! If this symbol is part of the polygon.
                if (in_poly(i, j)) then
                    ! Save the symbol to decide if it's an intersection.
                    sym = arr(i)(j:j)
                    ! Move to the next symbol (either a horizontal pipe or not)
                    j = j + 1
                    ! Skip all horizontal pipes THAT ARE PART OF THE POLYGON!!
                    do while (arr(i)(j:j) .eq. '-' .and. in_poly(i, j))
                        j = j + 1
                    end do
                    ! Check for zig-zag pattern, since those count.
                    if (sym .eq. 'F' .and. arr(i)(j:j) .eq. 'J') then
                        overlaps = overlaps + 1
                    elseif (sym .eq. 'L' .and. arr(i)(j:j) .eq. '7') then
                        overlaps = overlaps + 1
                    elseif (sym .eq. '|') then
                        overlaps = overlaps + 1
                    end if
                else
                    if (mod(overlaps, 2) .ne. 0) then
                        arr(i)(j:j) = 'I'
                        out = out + 1
                    end if
                    j = j + 1
                end if
            end if
        end do
    end do
end subroutine

