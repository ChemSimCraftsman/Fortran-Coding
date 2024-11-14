program quantum_harmonic_oscillator
    implicit none
    integer :: n
    real(8) :: h, nu, energy

    ! Define Planck's constant in Joule-seconds (J s)
    h = 6.62607015e-34

    ! Prompt the user for the quantum number n and frequency nu
    print *, "Enter the quantum number n (0, 1, 2, ...):"
    read *, n
    print *, "Enter the frequency of the oscillator in Hz:"
    read *, nu

    ! Calculate the energy of the quantum harmonic oscillator
    energy = (n + 0.5) * h * nu

    ! Display the result
    print *, "The energy at quantum level", n, "is:", energy, "Joules"
end program quantum_harmonic_oscillator

