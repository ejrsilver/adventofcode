module aoc_day12
    implicit none
    private
    public :: part1
    type :: Row
        character(len=256) :: str
        integer, dimension(10) :: nums
        integer :: nlen
    end type

contains
    subroutine part1(arr, lines, out)
        integer, intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer(kind=16), intent(out) :: out
        integer, dimension(:), allocatable :: nums
        integer :: i, j, nlen, err = 0, res
        character(len=:), allocatable :: ctmp, str

        out = 0
        do i=1,lines
            str = arr(i)(1:index(arr(i), ' ')-1)
            ctmp = arr(i)(index(arr(i), ' ')+1:len_trim(arr(i)))
            nlen = 0
            do j=1,len_trim(ctmp)
                if (ctmp(j:j) .eq. ',') then
                    nlen = nlen + 1
                end if
            end do
            nlen = nlen + 1
            allocate(nums(nlen))
            read (ctmp, *, iostat=err) nums
            res = check_possible(str, nums, nlen)
            print *, i, res
            deallocate(nums)
            out = out + res
        end do
    end subroutine

    recursive function check_possible(str, nums, nlen) result(possible)
        integer, intent(in) :: nlen
        integer, dimension(nlen), intent(in) :: nums
        character(len=*), intent(in) :: str

        integer(kind=16) :: possible, i, cnt
        logical :: valid

        if (nlen .le. 0) then
            possible = 1
            return
        end if

        cnt = 0
        do i=1,len_trim(str) - nums(1) + 1
            valid = .true.
            if (scan(str(i:i+nums(1)-1), '.') .ne. 0) then
                valid = .false.
            end if
            if (i + nums(1) .lt. len_trim(str) .and. str(i+nums(1):i+nums(1)) .eq. '#') then
                valid = .false.
            end if
            if (i .gt. 1 .and. str(i-1:i-1) .eq. '#') then
                valid = .false.
            end if

            if (nlen .eq. 1) then
                if (scan(str(i+nums(1):len_trim(str)), '#') .ne. 0) then
                    valid = .false.
                end if
            end if

            if (valid) then
                cnt = cnt + check_possible(str(i+nums(1)+1:), nums(2:), nlen-1)
            end if

            if (str(i:i) .eq. '#') then
                exit
            end if
        end do
        possible = cnt
    end function check_possible
end module aoc_day12

! Plan: Sort numbers high-low, find each option for the max,
! then each configuration for the next smallest, etc.
! (This will have to be recursive)

program main
    use aoc_day12
    implicit none
    character(len=512), allocatable :: arr(:)
    character(len=512) :: ctmp
    integer :: fn = 12, lines = 0, err = 0, i
    integer(kind=16) :: out
    open(fn, file="input.txt")

    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do

    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr

    call part1(arr, lines, out)
    print '(i0)', out
end program

