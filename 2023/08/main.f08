program main
    use aoc_day8
    integer(kind=8) :: out
    integer :: test = 0
    call init(test)
    if (test .le. 2) then
        call part1(out)
        print '(i0)', out
    end if

    call part2(out)
    print '(i0)', out
end program
