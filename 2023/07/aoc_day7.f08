module aoc_day7
    implicit none
    private
    public :: init, part1, part2, deinit
    character(len=256), allocatable :: arr(:)
    integer :: fn = 12, lines = 0, err = 0
contains
    subroutine init(test)
        logical, intent(in) :: test
        character(len=256) :: ctmp
        if (test) then
            open(fn, file="test.txt")
        else
            open(fn, file="input.txt")
        end if
        do while(err .eq. 0)
            lines = lines + 1
            read(fn, '(A)', iostat=err) ctmp
        end do
        lines = lines - 1
        allocate(arr(lines))
        rewind(fn)
        read(fn, '(A)', iostat=err) arr
    end subroutine init

    subroutine deinit()
        close(fn)
        deallocate(arr)
    end subroutine

    subroutine part1(out)
        use aoc_sets
        implicit none
        integer, intent(out) :: out
        type(Hand), dimension(lines) :: sets
        integer :: i
        do i=1,lines
            read(arr(i), '(A5)', iostat=err) sets(i)%cards
            read(arr(i)(index(arr(i), ' '):), *, iostat=err) sets(i)%bid
            sets(i)%rank = p1rank(sets(i))
        end do

        call setpart(.true.)
        call sort_sets(sets, 1, lines)
        out = 0
        do i=1,lines
            out = out + i*sets(i)%bid
        end do
    end subroutine

    subroutine part2(out)
        use aoc_sets
        implicit none
        integer, intent(out) :: out
        type(Hand), dimension(lines) :: sets
        integer :: i
        do i=1,lines
            read(arr(i), '(A5)', iostat=err) sets(i)%cards
            read(arr(i)(index(arr(i), ' '):), *, iostat=err) sets(i)%bid
            sets(i)%rank = p2rank(sets(i))
        end do
        call setpart(.false.)
        call sort_sets(sets, 1, lines)
        out = 0
        do i=1,lines
            out = out + i*sets(i)%bid
        end do
    end subroutine

    function p1rank(this) result(rank)
        use aoc_sets
        type(Hand), intent(inout) :: this
        integer, dimension(13) :: cnt
        integer :: rank
        call counts(this, cnt)
        rank = cardrank(cnt)
    end function p1rank

    function p2rank(this) result(rank)
        use aoc_sets
        type(Hand), intent(inout) :: this
        integer, dimension(13) :: cnt
        integer :: mx, i, rank, tmp
        call counts(this, cnt)

        mx = maxloc(cnt, 1)
        if (mx .ne. 4) then
            cnt(mx) = cnt(mx) + cnt(4)
            cnt(4) = 0
        else
            ! Save the jokers and zero them out.
            tmp = cnt(4)
            cnt(4) = 0
            mx = maxloc(cnt,1)
            cnt(mx) = cnt(mx) + tmp
        end if

        rank = cardrank(cnt)
    end function p2rank

    function cardrank(cnt) result(rank)
        integer, dimension(13), intent(in) :: cnt
        integer :: cnt2, j, rank

        if (maxval(cnt) .eq. 5) then
            rank = 7
        elseif(maxval(cnt) .eq. 4) then
            rank = 6
        elseif(maxval(cnt) .eq. 3) then
            cnt2 = 0
            do j=1,13
                if (cnt(j) .eq. 2) then
                    cnt2 = 1
                end if
            end do
            if (cnt2 .ne. 0) then
                rank = 5
            else
                rank = 4
            end if
        elseif(maxval(cnt) .eq. 2) then
            cnt2 = 0
            do j=1,13
                if (cnt(j) .eq. 2) then
                    cnt2 = cnt2 + 1
                end if
            end do
            if (cnt2 .ge. 2) then
                rank = 3
            else
                rank = 2
            endif
        elseif(maxval(cnt) .eq. 1) then
            rank = 1
        end if
    end function cardrank
end module aoc_day7
