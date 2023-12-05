program main
    implicit none
    character*256, allocatable :: arr(:)
    character*256 :: ctmp
    integer*8, dimension(7, 128, 3) :: maps ! Max map dimensions.
    integer*8 :: seeds(20) ! Max seeds.
    integer*8 :: fn = 12, lines = 0, err = 0, mapind = 0, keyind = 1, strind = 1, itmp, seedsind = 0, out, i
    logical :: first = .true., test = .false.

    seeds = -1
    maps = -1

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

    do i=1,lines
        ! Get seeds.
        if (first) then
            read(arr(i)(index(arr(i),':')+1:len_trim(arr(i))), *, iostat=err) seeds
            first = .false.
        else
            if (index(arr(i), ':') .ne. 0) then
                mapind = mapind + 1
                keyind = 1
            else
                read(arr(i), *, iostat=err) maps(mapind, keyind, :)
                keyind = keyind + 1
            end if
        end if
    end do

    call part1(seeds, maps, out)
    print '(i0)', out

    call part2(seeds, maps, out)
    print '(i0)', out

    close(fn)
end program

subroutine part1(seeds, maps, out)
    implicit none
    integer*8, dimension(7,128,3), intent(in) :: maps
    integer*8, dimension(20), intent(in) :: seeds
    integer*8, intent(out) :: out
    integer*8 :: val, i, getmap, outval
    outval = 1000000000
    do i=1,20
        if (seeds(i) .ge. 0) then
            val = getmap(seeds(i), maps(1, :, :))
            val = getmap(val, maps(2, :, :))
            val = getmap(val, maps(3, :, :))
            val = getmap(val, maps(4, :, :))
            val = getmap(val, maps(5, :, :))
            val = getmap(val, maps(6, :, :))
            val = getmap(val, maps(7, :, :))
            if (val .lt. outval) then
                outval = val
            end if
        end if
    end do

    out = outval
end subroutine

subroutine part2(seeds, maps, out)
    implicit none
    integer*8, dimension(7,128,3), intent(in) :: maps
    integer*8, dimension(20), intent(in) :: seeds
    integer*8, intent(out) :: out
    integer*8 :: val, i, j, getmap, outval
    integer*8, dimension(7,128,3) :: minmaps
    outval = 1000000000
    ! Run through each seed init and range.

    ! Logic: Map Range 10000 - 20000
    ! Seed Range: 15000 - 25000

    call minimizemaps(maps, minmaps)

    do concurrent i=1,10,2
        if (seeds(i) .ge. 0 .and. seeds(i+1) .ge. 0) then
            j = seeds(i)
            do concurrent (j = seeds(i):seeds(i)+seeds(i+1))
                val = getmap(j, maps(1, :, :))
                val = getmap(val, maps(2, :, :))
                val = getmap(val, maps(3, :, :))
                val = getmap(val, maps(4, :, :))
                val = getmap(val, maps(5, :, :))
                val = getmap(val, maps(6, :, :))
                val = getmap(val, maps(7, :, :))
                if (val .lt. outval) then
                    outval = val
                end if
            end do
        end if
    end do

    out = outval
end subroutine

integer*8 function getmap(seed, map)
    implicit none
    integer*8, intent(in) :: seed
    integer*8, dimension(128, 3), intent(in) :: map
    integer*8 :: j, val
    getmap = seed
    do j=1,128
        if (map(j,2) .le. seed .and. seed .le. map(j,2) + map(j,3) - 1) then
            getmap = seed + map(j,1) - map(j,2)
            exit
        end if
    end do
end function getmap
