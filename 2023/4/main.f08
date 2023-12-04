program main
    implicit none
    character*256, allocatable :: arr(:)
    character*256 :: ctmp
    integer :: fn = 12, lines = 0, err = 0, out, iwin, icard, tmp
    logical :: test = .false.
    if (test) then
        open(fn, file="test.txt")
        iwin = 5
        icard = 8
    else
        open(fn, file="input.txt")
        iwin = 10
        icard = 25
    end if
    do while (err .eq. 0)
        read(fn, '(A)', iostat=err) ctmp
        lines = lines + 1
    end do
    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr
    call part1(arr, lines, iwin, icard, out)
    print '(i0)', out
    call part2(arr, lines, iwin, icard, out)
    print '(i0)', out
    close(fn)
end program

subroutine part1(arr, lines, iwin, icard, out)
    implicit none
    integer, intent(in) :: lines, iwin, icard
    character*256, dimension(lines), intent(in) :: arr
    integer, intent(out) :: out
    integer :: i, j, err, points, ipoint, find
    integer, dimension(iwin) :: wins
    integer, dimension(icard) :: cards
    logical :: flag = .false.
    character*256 :: str
    err = 0
    points = 0
    do i=1,lines
        flag = .false.
        ipoint = 0
        str = arr(i)(index(arr(i), ":")+1:)
        read(str(1:index(str, "|")-1), *, iostat=err) wins
        read(str(index(str, "|")+1:), *, iostat=err) cards
        do j=1,icard
            if (find(wins, iwin, cards(j)) .ne. 0) then
                if (.not. flag) then
                    flag = .true.
                    ipoint = 1
                else
                    ipoint = ipoint * 2
                end if
            end if
        end do
        points = points + ipoint
    end do
    out = points
end subroutine

subroutine part2(arr, lines, iwin, icard, out)
    implicit none
    integer, intent(in) :: lines, iwin, icard
    character*256, dimension(lines), intent(in) :: arr
    integer, intent(out) :: out
    integer :: i, j, k, err, points, ipoint, find
    integer, dimension(iwin) :: wins
    integer, dimension(icard) :: cards
    integer, dimension(lines) :: icount
    character*256 :: str, winstr, cardstr
    ! Start with one of each card.
    icount(:) = 1
    points = 0
    err = 0
    do i=1,lines
        do j=1,icount(i)
            ipoint = 0
            str = arr(i)(index(arr(i), ":")+1:)
            read(str(1:index(str, "|")-1), *, iostat=err) wins
            read(str(index(str, "|")+1:), *, iostat=err) cards
            do k=1,icard
                if (find(wins, iwin, cards(k)) .ne. 0) then
                    ipoint = ipoint + 1
                end if
            end do
            do k=1,ipoint
                icount(i+k) = icount(i+k) + 1
            end do
        end do
    end do
    do i=1,lines
        points = points + icount(i)
    end do
    out = points
end subroutine

integer function find(arr, len, val)
    implicit none
    integer, intent(in) :: len, val
    integer, dimension(len), intent(in) :: arr
    integer :: i, ret
    ret = 0
    do i=1,len
        if (arr(i) .eq. val) then
            ret = i
        end if
    end do
    find = ret
end function find
