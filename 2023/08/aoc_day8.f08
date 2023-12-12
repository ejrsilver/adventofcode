module aoc_day8
    use aoc_tree
    implicit none
    private
    public :: init, part1, part2
    integer :: fn = 12
contains
    subroutine init(test)
        character(len=512), allocatable :: arr(:)
        character(len=512) :: ctmp
        integer, intent(in) :: test
        integer :: err = 0, lines = 0
        if(test .eq. 1) then
            open(fn, file="test.txt")
        elseif(test .eq. 2) then
            open(fn, file="test2.txt")
        elseif(test .eq. 3) then
            open(fn, file="test3.txt")
        else
            open(fn, file="input.txt")
        end if

        do while (err .eq. 0)
            lines = lines + 1
            read(fn, '(A)', iostat=err) ctmp
        end do

        lines = lines - 1
        allocate(arr(lines))
        rewind(fn)
        read(fn, '(A)', iostat=err) arr
        call init_tree(arr, lines)
    end subroutine

    subroutine part1(out)
        integer(kind=8), intent(out) :: out
        out = follow_directions()
    end subroutine

    subroutine part2(out)
        integer(kind=8), intent(out) :: out
        out = follow_parallel_directions()
    end subroutine
end module aoc_day8
