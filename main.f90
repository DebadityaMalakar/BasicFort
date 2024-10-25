program main
    implicit none
    character(len=100) :: line, command, var_name, var_name2
    real :: var_value, value2
    logical :: found
    type :: variable
        character(len=100) :: name
        real :: value
    end type variable
    type(variable), allocatable :: variables(:)
    integer :: var_count = 0
    integer :: i

    ! Main loop
    do
        print *, "Enter a command (e.g., LET X = 10, PRINT X, or EXIT to quit):"
        read *, command
        command = trim(adjustl(command))  ! Remove leading spaces
        command = upper(command)           ! Convert to uppercase

        ! Remove all non-printable characters from the command
        do while (index(command, char(0)) /= 0)
            command = command(1:index(command, char(0)) - 1) // command(index(command, char(0)) + 1:)
        end do

        print *, "Command read: ", command

        if (command == "EXIT") exit

        ! Process HELP command
        if (command == "HELP") then
            print *, "Available Commands:"
            print *, "  LET <variable> = <value> : Assign a value to a variable."
            print *, "  PRINT <variable>         : Print the value of a variable."
            print *, "  ADD <variable> <value>   : Add a value to a variable."
            print *, "  SUBTRACT <variable> <value> : Subtract a value from a variable."
            print *, "  MULTIPLY <variable> <value> : Multiply a variable by a value."
            print *, "  DIVIDE <variable> <value>   : Divide a variable by a value."
            print *, "  POWER <variable> <value>    : Raise a variable to the power of a value."
            print *, "  HELP                       : Display this help information."
            print *, "  EXIT                       : Exit the program."
            cycle
        end if

        ! Process LET command
        if (index(command, "LET") == 1) then
            read(command(5:), '(A, F10.2)') var_name, var_value  ! Specify width for F format specifier

            ! Check if variable exists
            found = .false.
            do i = 1, var_count
                if (trim(variables(i)%name) == trim(var_name)) then
                    variables(i)%value = var_value
                    found = .true.
                    exit
                end if
            end do

            ! If variable does not exist, add it
            if (.not. found) then
                var_count = var_count + 1
                allocate(variables(var_count))
                variables(var_count)%name = trim(var_name)
                variables(var_count)%value = var_value
            end if

        ! Process PRINT command
        else if (index(command, "PRINT") == 1) then
            read(command(6:), '(A)') var_name
            found = .false.
            do i = 1, var_count
                if (trim(variables(i)%name) == trim(var_name)) then
                    print *, trim(var_name), "=", variables(i)%value
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                print *, "Variable not found"
            end if

        ! Process arithmetic operations
        else if (index(command, "ADD") == 1 .or. &
                 index(command, "SUBTRACT") == 1 .or. &
                 index(command, "MULTIPLY") == 1 .or. &
                 index(command, "DIVIDE") == 1 .or. &
                 index(command, "POWER") == 1) then

            read(command(7:), '(A, F10.2)') var_name2, value2  ! Read second variable or value
            found = .false.
            do i = 1, var_count
                if (trim(variables(i)%name) == trim(var_name)) then
                    select case (command(1:4))
                    case ("ADD")
                        variables(i)%value = variables(i)%value + value2
                        print *, "Added ", value2, " to ", trim(var_name), "=", variables(i)%value
                    case ("SUBTRACT")
                        variables(i)%value = variables(i)%value - value2
                        print *, "Subtracted ", value2, " from ", trim(var_name), "=", variables(i)%value
                    case ("MULTIPLY")
                        variables(i)%value = variables(i)%value * value2
                        print *, "Multiplied ", trim(var_name), " by ", value2, "=", variables(i)%value
                    case ("DIVIDE")
                        if (value2 /= 0.0) then
                            variables(i)%value = variables(i)%value / value2
                            print *, "Divided ", trim(var_name), " by ", value2, "=", variables(i)%value
                        else
                            print *, "Error: Division by zero!"
                        end if
                    case ("POWER")
                        variables(i)%value = variables(i)%value ** value2
                        print *, "Raised ", trim(var_name), " to the power of ", value2, "=", variables(i)%value
                    end select
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                print *, "Variable not found"
            end if

        else
            print *, "Unrecognized command: ", trim(command)
        end if
    end do

    print *, "Exiting program."
contains
    ! Function to convert string to uppercase
    function upper(str) result(upper_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: upper_str
        integer :: i

        do i = 1, len_trim(str)
            upper_str(i:i) = achar(iachar(str(i:i)) - iachar('a') + iachar('A'))
        end do
    end function upper
end program main
