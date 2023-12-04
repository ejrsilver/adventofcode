program main
    implicit none
    character*256 ctmp
    character*256, allocatable :: arr(:)
    character, allocatable:: out(:)
    integer :: fn = 12, err = 0, lines = 0, i, step, j, ind = 1, dim, dim2
    logical :: test = .false.
    if (test) then
        open(fn, file="test.txt")
        dim = 3
        dim2 = 3
    else
        open(fn, file="input.txt")
        dim = 9
        dim2 = 8
    end if
    do while (err .eq. 0)
        lines = lines + 1
        read(fn, '(A)', iostat=err) ctmp
    end do
    lines = lines - 1
    allocate(arr(lines))
    allocate(out(dim))
    rewind(fn)

    read(fn, '(A)', iostat=err) arr
    call part1(arr, lines, dim, dim2, out)
    print *, out
end program

subroutine part1(arr, lines, dim, dim2, out)
    implicit none
    integer, intent(in) :: lines, dim, dim2
    character*256, dimension(lines), intent(in) :: arr
    character, dimension(dim), intent(out) :: out
    integer :: i, j, err, ind, move, from, to, lentrim, top1, top2
    character :: chararr(dim, dim*dim)
    character*256 :: str

    str = arr(dim2+1)
    ! Set initial values.
    chararr(:,:) = ' '

    ! Get chararr values.
    do i=1,dim
        ! Get the current number.
        ind = index(str, char(i+48))
        do j=1,dim2
            chararr(i, j) = arr(dim2+1-j)(ind:ind)
        end do
    end do

    ! Now process intructions.
    do i=dim+3,lines
        str = arr(i)(index(arr(i), "move")+4:index(arr(i), "from")-1)
        read(str, *, iostat=err) move
        str = arr(i)(index(arr(i), "from")+4:index(arr(i), "to")-1)
        read(str, *, iostat=err) from
        str = arr(i)(index(arr(i), "to")+3:)
        read(str, *, iostat=err) to

        do j=1,move
            top1 = lentrim(chararr(from, :), dim*dim)
            top2 = lentrim(chararr(to, :), dim*dim)
            chararr(to, top2+1) = chararr(from, top1)
            chararr(from, top1) = ' '
        end do
    end do

    print *, chararr(1, :)
    print *, chararr(2, :)
    print *, chararr(3, :)
    print *, chararr(4, :)
    print *, chararr(5, :)
    print *, chararr(6, :)
    print *, chararr(7, :)
    print *, chararr(8, :)
    print *, chararr(9, :)
    do i=1,dim
        out(i) = chararr(i, lentrim(chararr(i, :), dim*dim))
    end do
end subroutine 

integer function lentrim(s, length)
    implicit none
    integer, intent(in) :: length
    character, dimension(length), intent(in) :: s

    do lentrim = length, 1, -1
        if (s(lentrim) .ne. ' ') return
    end do
end function lentrim
