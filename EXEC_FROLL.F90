program exec_froll
    use froll
    implicit none

    character(len=100) :: input, args
    integer :: i
    type(roll_result) :: exec_roll_result

    do while(.true.)

        call get_command_argument(1, args)

        write(*,'("Enter dice notation: ")')
        read(*,'(A)') input

        exec_roll_result = roll(input)
        do i = 1, size(exec_roll_result%rolls)
            write(*,*) "roll: ", exec_roll_result%rolls(i)
        end do
        write(*,*) "mod : ", exec_roll_result%modifier
        write(*,*) "tot : ", exec_roll_result%total
    end do
end program exec_froll
