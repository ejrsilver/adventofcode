program main
    implicit none
    character*256, allocatable :: arr(:)
    character*256 :: ctmp
    integer :: lines = 0, err = 0, fn = 12, i, out
    open(fn, file="input.txt")
    do while(err .eq. 0)
        lines = lines + 1
        read (fn, '(A)', iostat=err) ctmp
    end do
    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read (fn, '(A)', iostat=err) arr
    call part1(arr, lines, out)
    print '(i0)', out

    call part2(arr, lines, out)
    print '(i0)', out
    close(fn)
end program

subroutine part1(arr, lines, out)
    implicit none
    integer, intent(in) :: lines
    character*256, dimension(lines), intent(in) :: arr
    character*256 :: str
    integer, intent(out) :: out
    integer :: i, j, part = 0, err, output = 0, sum = 0, ind = 1, start = 1, temp
    logical :: chksur
    do i=1,lines ! For each line.
        ind = scan(arr(i), "0123456789")
        do while (ind .ne. 0)
            if (scan(arr(i)(ind+1:ind+1), "0123456789") .eq. 0) then
                read(arr(i)(ind:ind), *, iostat = err) temp
                start = ind + 1
                if (chksur(arr, lines, i, ind-1, ind+1)) then
                    sum = sum + temp
                end if
            elseif (scan(arr(i)(ind+2:ind+2), "0123456789") .eq. 0) then
                read(arr(i)(ind:ind+1), *, iostat = err) temp
                start = ind + 2
                if (chksur(arr, lines, i, ind-1, ind+2)) then
                    sum = sum + temp
                end if
            else
                read(arr(i)(ind:ind+2), *, iostat = err) temp
                start = ind + 3
                if (chksur(arr, lines, i, ind-1, ind+3)) then
                    sum = sum + temp
                end if
            end if
            ind = scan(arr(i)(start:len_trim(arr(i))), "0123456789")
            if (ind .ne. 0) then
                ind = ind + start - 1
            end if
        end do
    end do
    out = sum
end subroutine

subroutine part2(arr, lines, out)
    implicit none
    integer, intent(in) :: lines
    character*256, dimension(lines), intent(in) :: arr
    character*256 :: str
    integer, intent(out) :: out
    integer :: i, j, part = 0, err, output = 0, sum = 0, ind = 1, start = 1, temp
    logical :: chksur
    do i=1,lines ! For each line.
        ind = scan(arr(i), "*") ! Find the next gear.
        do while (ind .ne. 0)
            call getnums(arr, lines, i, ind, output)
            start = ind + 1
            sum = sum + output
            ind = scan(arr(i)(start:len_trim(arr(i))), "*")
            if (ind .ne. 0) then
                ind = ind + start -1
            end if
        end do
    end do
    out = sum
end subroutine

logical function chksur(arr, lines, i, lower, upper)
    implicit none
    integer, intent(in) :: lines, i, lower, upper
    character*256, dimension(lines), intent(in) :: arr
    if (i .ge. 1 .and. scan(arr(i-1)(lower:upper), "@#$%&*/+=-") .ne. 0) then
        chksur = .true.
    elseif (scan(arr(i)(lower:upper), "@#$%&*/+=-") .ne. 0) then
        chksur = .true.
    elseif (scan(arr(i+1)(lower:upper), "@#$%&*/+=-") .ne. 0) then
        chksur = .true.
    else
        chksur = .false.
    end if
end function chksur

! Shape of return:
! 123
! 4 5
! 678
subroutine chksur2(arr, lines, i, ind, flags)
    implicit none
    integer, intent(in) :: lines, i, ind
    character*256, dimension(lines), intent(in) :: arr
    logical, dimension(8), intent(out) :: flags
    flags(1) = scan(arr(i-1)(ind-1:ind-1), "0123456789") .ne. 0
    flags(2) = scan(arr(i-1)(ind:ind), "0123456789") .ne. 0
    flags(3) = scan(arr(i-1)(ind+1:ind+1), "0123456789") .ne. 0
    flags(4) = scan(arr(i)(ind-1:ind-1), "0123456789") .ne. 0
    flags(5) = scan(arr(i)(ind+1:ind+1), "0123456789") .ne. 0
    flags(6) = scan(arr(i+1)(ind-1:ind-1), "0123456789") .ne. 0
    flags(7) = scan(arr(i+1)(ind:ind), "0123456789") .ne. 0
    flags(8) = scan(arr(i+1)(ind+1:ind+1), "0123456789") .ne. 0
end subroutine

subroutine getnums(arr, lines, i, ind, out)
    implicit none
    integer, intent(in) :: lines, i, ind
    character*256, dimension(lines), intent(in) :: arr
    integer, intent(out) :: out
    logical, dimension(8) :: flags
    logical :: failure = .false.
    integer :: num1, num2, temp, top1, top2, mid1, mid2, numind = 1
    integer, dimension(6) :: nums

    call chksur2(arr, lines, i, ind, flags)
    nums = 0
    numind = 1

    if (flags(1) .and. flags(2) .and. flags(3)) then
        read (arr(i-1)(ind-1:ind+1), *) temp
        nums(numind) = temp
        numind = numind + 1
    elseif (flags(1) .and. flags(2)) then
        ! If two digits.
        if (scan(arr(i-1)(ind-2:ind-2), "0123456789") .eq. 0) then
            read (arr(i-1)(ind-1:ind), *) temp
        else
            read (arr(i-1)(ind-2:ind), *) temp
        end if
        nums(numind) = temp
        numind = numind + 1
    elseif (flags(2) .and. flags(3)) then
        ! If two digits.
        if (scan(arr(i-1)(ind+2:ind+2), "0123456789") .eq. 0) then
            read (arr(i-1)(ind:ind+1), *) temp
        else
            read (arr(i-1)(ind:ind+2), *) temp
        end if
        nums(numind) = temp
        numind = numind + 1
    else
        if (flags(1)) then
            if (scan(arr(i-1)(ind-2:ind-2), "0123456789") .eq. 0) then ! if one digit.
                read (arr(i-1)(ind-1:ind-1), *) temp
            elseif(scan(arr(i-1)(ind-3:ind-3), "0123456789") .eq. 0) then ! if two digits.
                read (arr(i-1)(ind-2:ind-1), *) temp
            else ! if three digits.
                read (arr(i-1)(ind-3:ind-1), *) temp
            end if
            nums(numind) = temp
            numind = numind + 1
        end if
        if (flags(3)) then
            if (scan(arr(i-1)(ind+2:ind+2), "0123456789") .eq. 0) then ! if one digit.
                read (arr(i-1)(ind+1:ind+1), *) temp
            elseif(scan(arr(i-1)(ind+3:ind+3), "0123456789") .eq. 0) then ! if two digits.
                read (arr(i-1)(ind+1:ind+2), *) temp
            else ! if three digits.
                read (arr(i-1)(ind+1:ind+3), *) temp
            end if
            nums(numind) = temp
            numind = numind + 1
        end if
    end if

    ! Handle middle row.
    if (flags(4)) then
        if (scan(arr(i)(ind-2:ind-2), "0123456789") .eq. 0) then ! if one digit.
            read (arr(i)(ind-1:ind-1), *) temp
        elseif(scan(arr(i)(ind-3:ind-3), "0123456789") .eq. 0) then ! if two digits.
            read (arr(i)(ind-2:ind-1), *) temp
        else ! if three digits.
            read (arr(i)(ind-3:ind-1), *) temp
        end if
        nums(numind) = temp
        numind = numind + 1
    end if

    if (flags(5)) then
        if (scan(arr(i)(ind+2:ind+2), "0123456789") .eq. 0) then ! if one digit.
            read (arr(i)(ind+1:ind+1), *) temp
        elseif(scan(arr(i)(ind+3:ind+3), "0123456789") .eq. 0) then ! if two digits.
            read (arr(i)(ind+1:ind+2), *) temp
        else ! if three digits.
            read (arr(i)(ind+1:ind+3), *) temp
        end if
        nums(numind) = temp
        numind = numind + 1
    end if

    if (flags(6) .and. flags(7) .and. flags(8)) then
        read (arr(i+1)(ind-1:ind+1), *) temp
        nums(numind) = temp
        numind = numind + 1
    elseif (flags(6) .and. flags(7)) then
        ! If two digits.
        if (scan(arr(i+1)(ind-2:ind-2), "0123456789") .eq. 0) then
            read (arr(i+1)(ind-1:ind), *) temp
        else
            read (arr(i+1)(ind-2:ind), *) temp
        end if
        nums(numind) = temp
        numind = numind + 1
    elseif (flags(7) .and. flags(8)) then
        ! If two digits.
        if (scan(arr(i+1)(ind+2:ind+2), "0123456789") .eq. 0) then
            read (arr(i+1)(ind:ind+1), *) temp
        else
            read (arr(i+1)(ind:ind+2), *) temp
        end if
        nums(numind) = temp
        numind = numind + 1
    else
        if (flags(6)) then
            if (scan(arr(i+1)(ind-2:ind-2), "0123456789") .eq. 0) then ! if one digit.
                read (arr(i+1)(ind-1:ind-1), *) temp
            elseif(scan(arr(i+1)(ind-3:ind-3), "0123456789") .eq. 0) then ! if two digits.
                read (arr(i+1)(ind-2:ind-1), *) temp
            else ! if three digits.
                read (arr(i+1)(ind-3:ind-1), *) temp
            end if
            nums(numind) = temp
            numind = numind + 1
        end if
        if (flags(8)) then
            if (scan(arr(i+1)(ind+2:ind+2), "0123456789") .eq. 0) then ! if one digit.
                read (arr(i+1)(ind+1:ind+1), *) temp
            elseif(scan(arr(i+1)(ind+3:ind+3), "0123456789") .eq. 0) then ! if two digits.
                read (arr(i+1)(ind+1:ind+2), *) temp
            else ! if three digits.
                read (arr(i+1)(ind+1:ind+3), *) temp
            end if
            nums(numind) = temp
            numind = numind + 1
        end if
    end if

    if (nums(3) .ne. 0) then
        out = 0
        return
    end if
    out = nums(1)*nums(2)
end subroutine
