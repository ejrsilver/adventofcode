program main
    implicit none
    character(len=256) :: ctmp
    integer :: fn = 12, lines = 0, err = 0, nlen, i
    logical :: test = .false.
    integer(kind=8), allocatable :: ntmp(:)
    integer(kind=8), allocatable :: nums(:,:)
    integer(kind=8) :: tmp, pt1, pt2, out
    if (test) then
        open(fn, file="test.txt")
        nlen = 6
    else
        open(fn, file="input.txt")
        nlen = 21
    end if
    allocate(ntmp(nlen))
    do while (err .eq. 0)
        lines = lines + 1
        read(fn, *, iostat=err) ntmp
    end do
    lines = lines - 1
    allocate(nums(lines, nlen))
    rewind(fn)
    pt1 = 0
    pt2 = 0
    do i=1, lines
        read(fn, *, iostat=err) nums(i, :)
        call predict(nums(i, :), nlen, tmp, .true.)
        pt1 = pt1 + tmp
        call predict(nums(i, :), nlen, tmp, .false.)
        pt2 = pt2 + tmp
    end do
    print '(i0)', pt1
    print '(i0)', pt2
end program

subroutine predict(row, nlen, out, forwards)
    implicit none
    integer, intent(in) :: nlen
    integer(kind=8), dimension(nlen), intent(in) :: row
    integer(kind=8), intent(out) :: out
    logical, intent(in) :: forwards
    integer(kind=8), dimension(nlen+1, nlen+1) :: pred
    integer :: i, j
    pred = 0
    pred(1,:) = row
    ! Fill in the rows of values.
    do i=1,nlen-1
        do j=1,nlen-1
            pred(i+1,j+1) = pred(i,j+1) - pred(i,j)
        end do
    end do
    ! Place in the new values. (Excluding final)
    do i=nlen,2,-1
        pred(i,i-1) = pred(i,i) - pred(i+1,i)
    end do
    ! Place in the new values.
    do i=nlen,1,-1
        pred(i,nlen+1) = pred(i,nlen) + pred(i+1,nlen+1)
    end do
    if (forwards) then
        out = pred(1, nlen+1)
    else
        out = pred(1,1) - pred(2,1)
    end if
end subroutine
