module aoc_sets
    implicit none
    private
    public :: Hand, counts, cmp, cval, sort_sets, setpart
    type Hand
        integer :: bid
        character(len=5) :: cards
        ! 1=High, 2=OneP, 3=TwoP, 4=Three, 5=Full, 6=Four, 7=Five
        integer :: rank
    end type Hand
    logical :: part
contains
    subroutine setpart(val)
        logical, intent(in) :: val
        part = val
    end subroutine

    subroutine counts(this, cnt)
        type(Hand), intent(in) :: this
        integer, dimension(13), intent(out) :: cnt
        integer :: i
        cnt = 0
        do i=1,5
            select case(this%cards(i:i))
            case('A')
                cnt(1) = cnt(1) + 1
            case('K')
                cnt(2) = cnt(2) + 1
            case('Q')
                cnt(3) = cnt(3) + 1
            case('J')
                cnt(4) = cnt(4) + 1
            case('T')
                cnt(5) = cnt(5) + 1
            case('9')
                cnt(6) = cnt(6) + 1
            case('8')
                cnt(7) = cnt(7) + 1
            case('7')
                cnt(8) = cnt(8) + 1
            case('6')
                cnt(9) = cnt(9) + 1
            case('5')
                cnt(10) = cnt(10) + 1
            case('4')
                cnt(11) = cnt(11) + 1
            case('3')
                cnt(12) = cnt(12) + 1
            case('2')
                cnt(13) = cnt(13) + 1
            end select
        end do
    end subroutine

    function cmp(set1, set2) result(val)
        type(Hand), intent(in) :: set1, set2
        integer :: i, val
        if (set1%rank .gt. set2%rank) then
            val = 1
            return
        elseif(set1%rank .lt. set2%rank) then
            val = -1
            return
        else
            do i=1,5
                if (cval(set1%cards(i:i)) .gt. cval(set2%cards(i:i))) then
                    val = 1
                    return
                elseif (cval(set1%cards(i:i)) .lt. cval(set2%cards(i:i))) then
                    val = -1
                    return
                end if
            end do
        end if
        val = 0
    end function cmp

    function cval(a) result(val)
        character, intent(in) :: a
        integer :: val
        if (part) then
            select case(a)
            case('A')
                val = 13
            case('K')
                val = 12
            case('Q')
                val = 11
            case('J')
                val = 10
            case('T')
                val = 9
            case('9')
                val = 8
            case('8')
                val = 7
            case('7')
                val = 6
            case('6')
                val = 5
            case('5')
                val = 4
            case('4')
                val = 3
            case('3')
                val = 2
            case('2')
                val = 1
            end select
        else
            select case(a)
            case('A')
                val = 13
            case('K')
                val = 12
            case('Q')
                val = 11
            case('T')
                val = 10
            case('9')
                val = 9
            case('8')
                val = 8
            case('7')
                val = 7
            case('6')
                val = 6
            case('5')
                val = 5
            case('4')
                val = 4
            case('3')
                val = 3
            case('2')
                val = 2
            case('J')
                val = 1
            end select
        end if
    end function cval

    recursive subroutine sort_sets(sets, start, end)
        integer, intent(in) :: start, end
        type(Hand), intent(inout) :: sets(*)
        type(Hand) :: tmp, pivot
        integer :: i, j

        pivot = sets((start+end)/2)
        i = start
        j = end

        do
            do while (cmp(sets(i), pivot) .lt. 0)
                i = i + 1
            end do
            do while (cmp(pivot, sets(j)) .lt. 0)
                j = j - 1
            end do

            if (i .ge. j) then
                exit
            end if

            tmp = sets(i)
            sets(i) = sets(j)
            sets(j) = tmp
            i = i + 1
            j = j - 1
        end do

        if (start .lt. i-1) then
            call sort_sets(sets, start, i-1)
        end if
        if (j+1 .lt. end) then
            call sort_sets(sets, j+1, end)
        end if
    end subroutine
end module aoc_sets
