program main
    use aoc_day7
    implicit none
    integer :: out
    call init(.false.)
    call part1(out)
    print '(i0)', out
    call part2(out)
    print '(i0)', out
    call deinit()
end program

