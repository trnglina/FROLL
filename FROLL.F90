module froll
    implicit none
    private

    type roll_result
        integer, allocatable :: rolls(:)
        !! Individual roll results.
        integer :: modifier
        !! Modifier for the fragment. Is added onto roll total.
        integer :: total
        !! Final total of the roll fragment. Includes all
        !! rolls and the modifier.
    end type roll_result

    type formula_fragment
        integer :: dice_count
        !! Number of dice being rolled.
        integer :: dice_sides
        !! Number of sides each dice has.
        integer :: modifier
        !! Modifier for the fragment
        logical :: negative
        !! If the fragment is negative
    end type formula_fragment

    public :: roll
    ! public :: split_formula, parse_formula_fragment
    public :: roll_result, formula_fragment
contains
    function roll(formula) result(r)
        character(len=*), intent(in) :: formula
        character(len=50) :: string_formula_fragment(size(split_formula(formula, "+", "-")))
        integer, allocatable :: rolls(:)
        integer :: i, j, gi, total_modifier, current_roll, rolls_size
        type(roll_result) :: r
        type(formula_fragment) :: working_fragment

        string_formula_fragment = split_formula(formula, "+", "-")
        total_modifier = 0
        rolls_size = 0
        gi = 0

        do i = 1, size(string_formula_fragment)
            working_fragment = parse_formula_fragment(string_formula_fragment(i))
            rolls_size = rolls_size + working_fragment%dice_count
        end do

        allocate(rolls(rolls_size))

        do i = 1, size(string_formula_fragment)
            working_fragment = parse_formula_fragment(string_formula_fragment(i))
            ! write(*,*) "dice count: ", working_fragment%dice_count
            if (working_fragment%dice_count > 0) then
                do j = 1, working_fragment%dice_count
                    gi = gi + 1
                    current_roll = random_between(1, working_fragment%dice_sides)
                    if (working_fragment%negative) then
                        rolls(gi) = current_roll * (-1)
                    else
                        rolls(gi) = current_roll
                    end if
                    ! write(*,*) "roll: ", current_roll
                    ! write(*,*) "rolls: ", rolls
                end do
            end if
            total_modifier = total_modifier + working_fragment%modifier
        end do

        r = roll_result(rolls, total_modifier, sum(rolls) + total_modifier)
    end function roll

    function random_between(low, high)
        real :: r
        integer :: low, high, random_between
        call init_random_seed()
        call random_number(r)
        random_between = floor((r * high) + low)
    end function random_between

    subroutine init_random_seed()
        integer, allocatable :: seed(:)
        integer :: i, n, pid, t(2), s
        integer(8) :: count

        call random_seed(size = n)
        allocate(seed(n))
        call system_clock(count)
        if (count /= 0) then
            t = transfer(count, t)
        end if
        s = ieor(t(1), t(2))
        pid = getpid() + 1099279
        s = ieor(s, pid)
        if (n >= 3) then
            seed(1) = t(1) + 36269
            seed(2) = t(2) + 72551
            seed(3) = pid
            if (n > 3) then
                seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
            end if
        else
            seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
        end if
        call random_seed(put=seed)
    end subroutine init_random_seed

    pure function parse_formula_fragment(string_fragment) result(roll_fragment)
        character(len=*), intent(in) :: string_fragment
        character(len=len_trim(string_fragment)) :: working_string
        type(formula_fragment) :: roll_fragment
        integer :: i, dice_count, dice_sides, modifier
        logical :: negative

        working_string = trim(string_fragment)
        negative = .false.

        if (scan(working_string, "-+") > 0) then
            if (working_string(1:1) == "-") then
                negative = .true.
            end if
            working_string = working_string(2:)
        end if

        i = scan(working_string, "d")

        if (i /= 0) then
            read(working_string(1:i - 1),*) dice_count
            read(working_string(i + 1:),*) dice_sides
            modifier = 0
        else
            dice_count = 0
            dice_sides = 0
            read(working_string(:),*) modifier
        end if

        roll_fragment = formula_fragment(dice_count, dice_sides, modifier, negative)
    end function parse_formula_fragment

    pure function split_formula(in_string, delimiter, delimiter2) result(out_string_array)
        character(len=*), intent(in) :: in_string
        character(len=1), intent(in) :: delimiter, delimiter2
        character(len=len_trim(in_string)) :: working_string
        integer :: separations, scan_index, i
        character(len=50) :: out_string_array(1:count_segments(in_string, delimiter) + count_segments(in_string, delimiter2) + 1)

        separations = count_segments(in_string, delimiter) + count_segments(in_string, delimiter2) + 1

        working_string = replace_text(in_string, " ", "")

        do i = 1, separations
            scan_index = scan(working_string, delimiter // delimiter2, .true.)
            if (scan_index > 0) then
                out_string_array(separations + 1 - i) = working_string(scan_index:)
                working_string = working_string(:scan_index - 1)
            else
                out_string_array(separations + 1 - i) = working_string(:)
            end if
        end do
    end function split_formula

    pure function count_segments(in_string, query) result(count)
        character(len=*), intent(in) :: in_string
        character(len=1), intent(in) :: query
        integer :: in_string_length, count, i

        in_string_length = len_trim(in_string)
        count = 0

        do i = 2, in_string_length
            if (in_string(i:i) == query) then
                count = count + 1
            end if
        end do
    end function count_segments

    pure function replace_text(in_string, query, replacement) result(out_string)
        character(len=*), intent(in) :: in_string, query, replacement
        character(len=:), allocatable :: out_string
        integer :: i, nt, nr

        out_string = in_string
        nt = len_trim(query)
        nr = len_trim(replacement)

        do
            i = index(out_string, query) ; if (i == 0) exit
            out_string = trim(out_string(:i - 1) // replacement // out_string(i + 1 + nt:))
        end do
    end function replace_text
end module froll

program froll_test
    use froll
    implicit none

    character(len=100) :: input
    integer :: i
    type(roll_result) :: exec_roll_result

    write(*,'("Enter dice notation: ")')
    read(*,'(A)') input

    exec_roll_result = roll(input)
    do i = 1, size(exec_roll_result%rolls)
        write(*,*) "roll: ", exec_roll_result%rolls(i)
    end do
    write(*,*) "mod : ", exec_roll_result%modifier
    write(*,*) "tot : ", exec_roll_result%total
end program froll_test
