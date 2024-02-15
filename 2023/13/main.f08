program main
    implicit none
    character(len=256), allocatable :: arr(:)
    character(len=256) :: ctmp, ctmp2
    integer :: i, j, fn = 12, err = 0, lines = 0, out, baseInd, tmp
    open(fn, file="input.txt")

    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do

    lines = lines - 1
    allocate(arr(lines))
    rewind(fn)
    read(fn, '(A)', iostat=err) arr

    baseInd = 1
    out = 0
    do i=1,lines
        if (len_trim(arr(i)) .eq. 0) then
            call parserefs(arr(baseInd:i-1), i-baseInd, tmp)
            out = out + tmp
            baseInd = i+1
        elseif(i .eq. lines) then
            call parserefs(arr(baseInd:i), i-baseInd+1, tmp)
            out = out + tmp
        end if
    end do
    print *, 'Part 1 ', out

    baseInd = 1
    out = 0
    do i=1,lines
        if (len_trim(arr(i)) .eq. 0) then
            call part2parse(arr(baseInd:i-1), i-baseInd, tmp)
            out = out + tmp
            baseInd = i+1
        elseif(i .eq. lines) then
            call part2parse(arr(baseInd:i), i-baseInd+1, tmp)
            out = out + tmp
        end if
    end do
    print *, 'Part 2 (Better) ', out

    baseInd = 1
    out = 0
    do i=1,lines
        if (len_trim(arr(i)) .eq. 0) then
            call parse_and_fix(arr(baseInd:i-1), i-baseInd, tmp)
            out = out + tmp
            baseInd = i+1
        elseif(i .eq. lines) then
            call parse_and_fix(arr(baseInd:i), i-baseInd+1, tmp)
            out = out + tmp
        end if
    end do

!    ! Print modified array out.
!    open(13, file="output.txt", status='new')
!    do i=1,lines
!        write(13, '(A)') arr(i)(:len_trim(arr(i)))
!    end do
    print *, 'Part 2 ', out
end program main

subroutine parserefs(arr, lines, out)
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(in) :: arr
    integer, intent(out) :: out

    character(len=256) :: ctmp, ctmp2
    integer :: i, j, k, fn = 12, err = 0, baseInd, tmp
    logical :: valid

    out = 0
    ! For each horizontal, check if mirrored. (Only one mirror per)
    do i=1,lines-1
        valid = .true.
        do k=1,min(i, lines-i)
            if (line_errs(arr(i-k+1), arr(i+k)) .ne. 0) then
                valid = .false.
            end if
        end do

        if (valid) then
            out = out + i*100
            return
        end if
    end do

    ! For each vertical position, check if vertically mirrored.
    do j=1,len_trim(arr(1))-1
        valid = .true.
        ! Handle edge case where the first two columns are identical.
        if (j .eq. 1) then
            write(ctmp, *) arr(:)(1:1)
            write(ctmp2, *) arr(:)(2:2)
            if (line_errs(ctmp, ctmp2) .ne. 0) then
                valid = .false.
            end if
        else
            do k=1,min(j, len_trim(arr(1))-j)
                write(ctmp, *) arr(:)(j-k+1:j-k+1)
                write(ctmp2, *) arr(:)(j+k:j+k)
                if (line_errs(ctmp, ctmp2) .ne. 0) then
                    valid = .false.
                end if
            end do
        end if

        if (valid) then
            out = out + j
            return
        end if
    end do
end subroutine

subroutine part2parse(arr, lines, out)
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(inout) :: arr
    integer, intent(out) :: out
    logical :: almost_valid
    out = 0
    do i=1,lines-1
        if (almost_valid(arr, lines, i, .false.)) then
            out = out + i*100
            return
        end if
    end do

    do j=1,len_trim(arr(1))-1
        if (almost_valid(arr, lines, i, .true.)) then
            out = out + i
            return
        end if
    end do
end subroutine

subroutine parse_and_fix(arr, lines, out)
    integer, intent(in) :: lines
    character(len=256), dimension(lines), intent(inout) :: arr
    integer, intent(out) :: out

    character(len=256) :: ctmp, ctmp2
    integer :: i, j, k, fn = 12, err = 0, baseInd, tmp
    logical :: valid, fixed

    out = 0
    ! For each row, check mirroring.
    do i=1,lines-1
        valid = .true.
        fixed = .false.
        ! Keep going while there are no unfixable errors.
        do while (valid .and. .not. fixed)
            do k=1,min(i, lines-i)
                if (line_errs(arr(i-k+1), arr(i+k)) .gt. 1) then
                    valid = .false.
                elseif (line_errs(arr(i-k+1), arr(i+k)) .eq. 1 .and. .not. fixed) then ! If there's a single error.
                    call fix_horizontal_err(arr, lines, i-k+1, i+k)
                    fixed = .true.
                else
                    fixed = .true. ! Set the exit flag if this section is correct.
                end if
            end do
        end do
        if (valid) then
            out = out + i*100
            return
        end if
    end do

    ! For each vertical position, check if vertically mirrored.
    do j=1,len_trim(arr(1))-1
        valid = .true.
        fixed = .false.
        ! Handle edge case where the first two columns are identical.
        if (j .eq. 1) then
            write(ctmp, *) arr(:)(1:1)
            write(ctmp2, *) arr(:)(2:2)
            if (line_errs(ctmp, ctmp2) .gt. 1) then
                valid = .false.
            elseif (line_errs(ctmp, ctmp2) .eq. 1 .and. .not. fixed) then ! If there's a single error.
                call fix_vertical_err(arr, lines, 1, 2)
                fixed = .true.
            end if
        else
            ! Keep going while there are no unfixable errors.
            do while (valid .and. .not. fixed)
                do k=1,min(j, len_trim(arr(1))-j)
                    write(ctmp, *) arr(:)(j-k+1:j-k+1)
                    write(ctmp2, *) arr(:)(j+k:j+k)
                    ! If there are multiple errors, that's it.
                    if (line_errs(ctmp, ctmp2) .gt. 1) then
                        valid = .false.
                    elseif (line_errs(ctmp, ctmp2) .eq. 1 .and. .not. fixed) then ! If there's a single error.
                        call fix_vertical_err(arr, lines, j-k+1, j+k)
                        fixed = .true.
                    else
                        fixed = .true. ! Set the exit flag if this section is correct.
                    end if
                end do
            end do
        end if
        if (valid) then
            out = out + j
            return
        end if
    end do
end subroutine

subroutine fix_horizontal_err(arr, lines, a, b)
    integer, intent(in) :: lines, a, b
    character(len=256), dimension(lines), intent(inout) :: arr
    do i=1,len_trim(arr(a))
        if (arr(a)(i:i) .ne. arr(b)(i:i)) then
            arr(a)(i:i) = arr(b)(i:i)
            return
        end if
    end do
end subroutine

subroutine fix_vertical_err(arr, lines, a, b)
    integer, intent(in) :: lines, a, b
    character(len=256), dimension(lines), intent(inout) :: arr
    do i=1,lines
        if (arr(i)(a:a) .ne. arr(i)(b:b)) then
            arr(i)(a:a) = arr(i)(b:b)
            return
        end if
    end do
end subroutine

function almost_valid(arr, lines, i, horz) result(val)
    integer, intent(in) :: lines, i
    character(len=256), dimension(lines), intent(in) :: arr
    character(len=256) :: ctmp, ctmp2
    logical, intent(in) :: horz
    logical :: val
    integer :: k, out
    out = 0
    if (horz) then
        if (i .eq. 1) then
            write(ctmp, *) arr(:)(1:1)
            write(ctmp2, *) arr(:)(2:2)
            out = out + line_errs(ctmp, ctmp2)
        else
            do k=1,min(i, len_trim(arr(1))-i)
                write(ctmp, *) arr(:)(i-k+1:i-k+1)
                write(ctmp2, *) arr(:)(i+k:i+k)
                out = out + line_errs(ctmp, ctmp2)
            end do
        end if
    else
        do k=1,min(i, lines-i)
            out = out + line_errs(arr(i-k+1), arr(i+k))
        end do
    end if
    ! If this position is almost a valid reflection.
    val = out .le. 1
end function almost_valid

function line_errs(a, b) result(out)
    character(len=256), intent(in) :: a, b
    integer :: out, i, j
    out = 0
    do i=1,len_trim(a)
        if (a(i:i) .ne. b(i:i)) then
            out = out + 1
        end if
    end do
end function line_errs
