program circle_area
    implicit none
    real :: radius, area
    real, parameter :: pi = 3.14159

    ! Prompt the user for the radius
    print *, "Enter the radius of the circle in cm:"
    read *, radius

    ! Calculate the area of the circle
    area = pi * radius**2

    ! Display the result
    print *, "The area of the circle is:", area, "cm^2"
end program circle_area

