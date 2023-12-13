module aoc_day12
    implicit none
    private
    public :: part1, part2, get_time
    integer(kind=16), dimension(512, 512) :: cache
contains
    subroutine part2(arr, lines, out)
        integer, intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer(kind=16), intent(out) :: out
        integer, dimension(512) :: nums
        integer :: i, j, nlen, err = 0
        integer(kind=16) :: res
        character(len=512) :: ctmp, ctmp2, str, str2

        out = 0
        do i=1,lines
            cache = -1
            str2 = arr(i)(1:index(arr(i), ' ')-1)
            str = str2
            do j=1,4
                str = str(:len_trim(str)) // '?' // str2(:len_trim(str2))
            end do
            ctmp2 = arr(i)(index(arr(i), ' ')+1:len_trim(arr(i)))
            ctmp = ctmp2
            do j=1,4
                ctmp = ctmp(:len_trim(ctmp)) // ',' // ctmp2(:len_trim(ctmp))
            end do

            nlen = 0
            do j=1,len_trim(ctmp)
                if (ctmp(j:j) .eq. ',') then
                    nlen = nlen + 1
                end if
            end do
            nlen = nlen + 1
            read (ctmp, *, iostat=err) nums
            res = check_possible_cached(str, 1, len_trim(str), nums, 1, nlen)
            out = out + res
        end do
    end subroutine

    subroutine part1(arr, lines, out)
        integer, intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        integer(kind=16), intent(out) :: out
        integer, dimension(512) :: nums
        integer :: i, j, nlen, err = 0
        integer(kind=16) :: res
        character(len=512) :: ctmp, str

        out = 0
        do i=1,lines
            cache = -1
            str = arr(i)(1:index(arr(i), ' ')-1)
            ctmp = arr(i)(index(arr(i), ' ')+1:len_trim(arr(i)))
            nlen = 0
            do j=1,len_trim(ctmp)
                if (ctmp(j:j) .eq. ',') then
                    nlen = nlen + 1
                end if
            end do
            nlen = nlen + 1
            read (ctmp, *, iostat=err) nums
            res = check_possible(str(:len_trim(str)), 1, len_trim(str), nums, 1, nlen)
            out = out + res
        end do
    end subroutine

    recursive function check_possible_cached(str, istr, slen, nums, inum, nlen) result(possible)
        integer, intent(in) :: istr, slen, inum, nlen
        integer, dimension(nlen), intent(in) :: nums
        character(len=slen), intent(in) :: str
        integer(kind=16) :: possible, cnt
        integer :: i
        logical :: valid
        if (inum .gt. nlen) then
            possible = 1
            return
        end if
        if (istr .lt. slen .and. cache(inum, istr) .gt. 0) then
            possible = cache(inum, istr)
            return
        end if
        cnt = 0
        do i=istr,slen - nums(inum) + 1
            valid = .true.
            if (scan(str(i:i+nums(inum)-1), '.') .ne. 0) then
                valid = .false.
            end if
            if (i + nums(inum) .lt. slen .and. str(i+nums(inum):i+nums(inum)) .eq. '#') then
                valid = .false.
            end if
            if (i .gt. istr .and. str(i-1:i-1) .eq. '#') then
                valid = .false.
            end if
            if (inum .eq. nlen) then
                if (scan(str(i+nums(inum):), '#') .ne. 0) then
                    valid = .false.
                end if
            end if
            if (valid) then
                cnt = cnt + check_possible_cached(str, i+nums(inum)+1, slen, nums, inum+1, nlen)
            end if
            if (str(i:i) .eq. '#') then
                exit
            end if
        end do
        if (istr .lt. slen) then
            cache(inum, istr) = cnt
        end if
        possible = cnt
    end function check_possible_cached

    recursive function check_possible(str, istr, slen, nums, inum, nlen) result(possible)
        integer, intent(in) :: istr, slen, inum, nlen
        integer, dimension(nlen), intent(in) :: nums
        character(len=slen), intent(in) :: str
        integer(kind=16) :: possible, cnt
        integer :: i
        logical :: valid
        if (inum .gt. nlen) then
            possible = 1
            return
        end if
        cnt = 0
        do i=istr,slen - nums(inum) + 1
            valid = .true.
            if (scan(str(i:i+nums(inum)-1), '.') .ne. 0) then
                valid = .false.
            end if
            if (i + nums(inum) .lt. slen .and. str(i+nums(inum):i+nums(inum)) .eq. '#') then
                valid = .false.
            end if
            if (i .gt. istr .and. str(i-1:i-1) .eq. '#') then
                valid = .false.
            end if
            if (inum .eq. nlen) then
                if (scan(str(i+nums(inum):), '#') .ne. 0) then
                    valid = .false.
                end if
            end if
            if (valid) then
                cnt = cnt + check_possible(str, i+nums(inum)+1, slen, nums, inum+1, nlen)
            end if
            if (str(i:i) .eq. '#') then
                exit
            end if
        end do
        possible = cnt
    end function check_possible

    integer function get_time() result(tms)
        integer, dimension(8) :: values
        call DATE_AND_TIME(values=values)
        tms = (values(5))*60
        tms = (tms + values(6))*60
        tms = (tms + values(7))*1000
        tms = tms + values(8)
    end function get_time
end module aoc_day12

! Plan: Sort numbers high-low, find each option for the max,
! then each configuration for the next smallest, etc.
! (This will have to be recursive)

program main
    use aoc_day12
    implicit none
    character(len=512), allocatable :: arr(:)
    character(len=512) :: ctmp
    integer :: fn = 12, lines = 0, err = 0, t
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

    t = get_time()
    call part1(arr, lines, out)
    t = get_time() - t
    print *, "Time elapsed (ms): ", t
    print '(i0)', out

    t = get_time()
    call part2(arr, lines, out)
    t = get_time() - t
    print *, "Time elapsed (ms): ", t
    print '(i0)', out
end program

