program app

    implicit none

    integer :: sz
    character, dimension(:), allocatable :: play_1
    character, dimension(:), allocatable :: play_2
    integer :: i
    integer :: total_score = 0

    inquire(file="input", size=sz)
    !print *, sz / 4

    allocate(play_1(sz / 4 + 1))
    allocate(play_2(sz / 4 + 1))
    open(1, file="input", action="read")
    do i = 0, sz / 4 - 1
        read(1, *) play_1(i), play_2(i)
    end do

    ! do i = 0, sz / 4 - 1
    !     print *, "'", play_1(i), "' '", play_2(i), "'"
    ! end do

    ! star 1
    do i = 0, sz / 4 - 1
        if (play_2(i) == 'X') then
            total_score = total_score + 1
        else if (play_2(i) == 'Y') then
            total_score = total_score + 2
        else if (play_2(i) == 'Z') then
            total_score = total_score + 3
        end if

        if (play_1(i) == 'A') then
            if (play_2(i) == 'X') then
                total_score = total_score + 3
            else if (play_2(i) == 'Y') then
                total_score = total_score + 6
            end if
        else if (play_1(i) == 'B') then
            if (play_2(i) == 'Y') then
                total_score = total_score + 3
            else if (play_2(i) == 'Z') then
                total_score = total_score + 6
            end if
        else if (play_1(i) == 'C') then
            if (play_2(i) == 'Z') then
                total_score = total_score + 3
            else if (play_2(i) == 'X') then
                total_score = total_score + 6
            end if
        end if
    end do
    print *, total_score

    ! star 2
    total_score = 0
    do i = 0, sz / 4 - 1
        if (play_2(i) == 'X') then
            total_score = total_score + 0
            if (play_1(i) == 'A') then
                total_score = total_score + 3
            else if (play_1(i) == 'B') then
                total_score = total_score + 1
            else if (play_1(i) == 'C') then
                total_score = total_score + 2
            end if
        else if (play_2(i) == 'Y') then
            total_score = total_score + 3
            if (play_1(i) == 'A') then
                total_score = total_score + 1
            else if (play_1(i) == 'B') then
                total_score = total_score + 2
            else if (play_1(i) == 'C') then
                total_score = total_score + 3
            end if
        else if (play_2(i) == 'Z') then
            total_score = total_score + 6
            if (play_1(i) == 'A') then
                total_score = total_score + 2
            else if (play_1(i) == 'B') then
                total_score = total_score + 3
            else if (play_1(i) == 'C') then
                total_score = total_score + 1
            end if
        end if
    end do
    print *, total_score

end program app
