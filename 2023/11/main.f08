module aoc_day11
    implicit none
    private
    public :: init_galaxies, part1, part2
    type :: Galaxy
        integer(kind=16) :: x, y
    end type
    type(Galaxy), allocatable :: gals(:)
    integer(kind=16) :: instances, multiple = 1
contains
    subroutine init_galaxies(arr, lines)
        integer(kind=16), intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer(kind=16) :: i, j

        instances = 0
        do i=1,lines
            do j=1,len_trim(arr(i))
                if (arr(i)(j:j) .eq. '#') then
                    instances = instances + 1
                end if
            end do
        end do

        allocate(gals(instances))
        instances = 1
        do i=1,lines
            do j=1,len_trim(arr(i))
                if (arr(i)(j:j) .eq. '#') then
                    gals(instances)%x = i
                    gals(instances)%y = j
                    instances = instances + 1
                end if
            end do
        end do

        instances = instances - 1
    end subroutine

    subroutine part1(arr, lines, out)
        integer(kind=16), intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer(kind=16), intent(out) :: out
        integer(kind=16) :: i, j, dist
        out = 0
        do i=1,instances
            do j=i+1,instances
                dist = abs(gals(i)%x - gals(j)%x) + abs(gals(i)%y - gals(j)%y)
                dist = dist + empty_rows(arr, lines, gals(i)%x, gals(j)%x)
                dist = dist + empty_cols(arr, lines, gals(i)%y, gals(j)%y)
                out = out + dist
            end do
        end do
    end subroutine

    subroutine part2(arr, lines, out)
        integer(kind=16), intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer(kind=16), intent(out) :: out
        integer(kind=16) :: i, j, dist
        out = 0
        multiple = 999999
        do i=1,instances
            do j=i+1,instances
                dist = abs(gals(i)%x - gals(j)%x) + abs(gals(i)%y - gals(j)%y)
                dist = dist + empty_rows(arr, lines, gals(i)%x, gals(j)%x)
                dist = dist + empty_cols(arr, lines, gals(i)%y, gals(j)%y)
                out = out + dist
            end do
        end do
    end subroutine

    integer(kind=16) function empty_rows(arr, length, x1, x2)
        integer(kind=16), intent(in) :: x1, x2, length
        character(len=512), dimension(length), intent(in) :: arr
        character(len=512) :: ctmp
        integer(kind=16) :: res, inc, i
        res = 0
        if (x1 .lt. x2) then
            inc = 1
        else
            inc = -1
        end if
        do i=x1,x2,inc
            if (index(arr(i), '#') .eq. 0) then
                res = res + multiple
            end if
        end do
        empty_rows = res
    end function empty_rows

    integer(kind=16) function empty_cols(arr, length, y1, y2)
        integer(kind=16), intent(in) :: y1, y2, length
        character(len=512), dimension(length), intent(in) :: arr
        character(len=512) :: ctmp
        integer(kind=16) :: res, inc, i
        res = 0
        if (y1 .lt. y2) then
            inc = 1
        else
            inc = -1
        end if
        do i=y1,y2,inc
            ! Write vertical string to temporary.
            write(ctmp, *) arr(:)(i:i)
            if (index(ctmp, '#') .eq. 0) then
                res = res + multiple
            end if
        end do
        empty_cols = res
    end function empty_cols
end module aoc_day11

program main
    use aoc_day11
    implicit none
    character(len=512), allocatable :: arr(:)
    character(len=512) :: ctmp
    integer(kind=16) :: fn = 12, lines = 0, err = 0, out, calc
    logical :: wegood

    open(fn, file="input.txt")

    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do

    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr
    call init_galaxies(arr, lines)

    call part1(arr, lines, out)
    print '(i0)', out

    call part2(arr, lines, out)
    print '(i0)', out
end program
