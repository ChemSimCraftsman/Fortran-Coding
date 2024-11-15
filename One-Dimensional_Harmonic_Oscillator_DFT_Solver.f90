program dft_harmonic
  implicit none
  integer, parameter :: n = 100
  real(8), dimension(n) :: rho, v_ext, energy
  real(8) :: dx, x_min, x_max, x
  integer :: i

  x_min = -5.0
  x_max = 5.0
  dx = (x_max - x_min) / (n - 1)

  ! Initialize density and external potential
  do i = 1, n
    x = x_min + (i - 1) * dx
    v_ext(i) = 0.5 * x**2  ! Harmonic potential
    rho(i) = exp(-x**2)    ! Trial density
  end do

  ! Calculate energy
  energy = rho * v_ext * dx
  print *, "Energy (Hartree):", sum(energy)
end program dft_harmonic

