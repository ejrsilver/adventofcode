module aoc_tree
    implicit none
    type Node
        character(len=3) :: val
        character(len=3) :: left
        character(len=3) :: right
    end type
    type(Node), allocatable :: tree(:)
    integer :: length
    character(len=512) :: directions
contains
    subroutine init_tree(arr, lines)
        integer, intent(in) :: lines
        character(len=512), dimension(lines), intent(in) :: arr
        character(len=3) :: str
        integer :: i
        length = lines-2
        allocate(tree(length))
        directions = arr(1)
        do i=1,length
            tree(i)%val = arr(i+2)(1:3)
            tree(i)%left = arr(i+2)(8:10)
            tree(i)%right = arr(i+2)(13:15)
        end do
    end subroutine init_tree

    elemental function gcd(a, b) result(res)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: a1, b1, t, res
        a1 = a
        b1 = b
        do while (b1 .ne. 0)
            t = b1
            b1 = mod(a1, b1)
            a1 = t
        end do
        res = a1
    end function gcd

    elemental function lcm(a, b) result(res)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: res
        res = a*b/gcd(a,b)
    end function lcm

    function lcmarr(arr, alen) result(val)
        integer(kind=8) :: val
        integer, intent(in) :: alen
        integer(kind=8), dimension(alen), intent(in) :: arr
        integer :: i
        val = 1
        do i=1,alen
            val = lcm(val, arr(i))
        end do
    end function lcmarr

    function follow_parallel_directions() result(steps)
        integer(kind=8) :: steps
        integer(kind=8), allocatable :: cnts(:)
        type(Node), allocatable :: nds(:)
        integer :: i, j, nlen = 0

        ! Get number of start nodes.
        do i=1,length
            if (tree(i)%val(3:3) .eq. 'A') then
                nlen = nlen + 1
            end if
        end do

        allocate(nds(nlen))
        allocate(cnts(nlen))

        ! Set start nodes.
        j = 1
        do i=1,length
            if (tree(i)%val(3:3) .eq. 'A') then
                nds(j) = tree(i)
                j = j + 1
            end if
        end do
        cnts = 0
        i = 1

        do j=1,nlen
            do while (nds(j)%val(3:3) .ne. 'Z')
                if(directions(i:i) .eq. 'L') then
                    nds(j) = get_node(nds(j)%left)
                else
                    nds(j) = get_node(nds(j)%right)
                end if
                cnts(j) = cnts(j) + 1
                ! Allow directions to loop.
                i = 1 + mod(i, len_trim(directions))
            end do
        end do

        steps = lcmarr(cnts, nlen)
    end function follow_parallel_directions

    function follow_directions() result(steps)
        integer :: steps, i, mx
        type(Node) :: nd
        nd = get_node('AAA')
        steps = 0
        i = 1
        mx = len_trim(directions)
        do while (nd%val .ne. 'ZZZ')
            if(directions(i:i) .eq. 'L') then
                nd = get_node(nd%left)
            else
                nd = get_node(nd%right)
            end if
            steps = steps + 1
            i = 1 + mod(i, len_trim(directions))
        end do
    end function follow_directions

    function init_crawl() result(steps)
        integer :: steps
        steps = walk_tree(get_node('AAA'))
    end function init_crawl

    pure function get_node(str) result(nd)
        character(len=3), intent(in) :: str
        type(Node) :: nd
        integer :: i
        do i=1,length
            if (tree(i)%val .eq. str) then
                nd = tree(i)
            end if
        end do
    end function get_node

    recursive function walk_tree(nd) result(steps)
        integer :: steps, l, r
        type(Node), intent(in) :: nd
        ! If walking is done, we're done.
        if (nd%val .eq. 'ZZZ') then
            steps = 0
            return
        end if
        if(nd%val .ne. nd%right) then
            r = walk_tree(get_node(nd%right))
        else
            r = -1
        end if
        if(nd%val .ne. nd%left) then
            l = walk_tree(get_node(nd%left))
        else
            l = -1
        end if
        if (l .ne. -1 .and. r .ne. -1) then
            steps = 1 + min(l,r)
        elseif (l .ne. -1) then
            steps = 1 + l
        elseif(r .ne. -1) then
            steps = 1 + r
        else
            steps = -1
        end if
    end function walk_tree
end module aoc_tree
