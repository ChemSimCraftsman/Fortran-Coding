program lj_force
  implicit none
  integer, parameter :: n = 2
  real(8), dimension(n, 3) :: r  ! Positions of particles
  real(8), dimension(n, 3) :: f  ! Forces
  real(8) :: epsilon, sigma, r2, r6, r12, r_inv, fx, fy, fz, dx, dy, dz
  integer :: i, j, k

  epsilon = 0.238      ! Lennard-Jones parameters (kcal/mol)
  sigma = 3.4          ! Angstrom

  r(1, :) = (/0.0, 0.0, 0.0/)  ! Positions of two particles
  r(2, :) = (/3.5, 0.0, 0.0/)

  f = 0.0               ! Initialize forces

  do i = 1, n-1
    do j = i+1, n
      dx = r(i, 1) - r(j, 1)
      dy = r(i, 2) - r(j, 2)
      dz = r(i, 3) - r(j, 3)
      r2 = dx**2 + dy**2 + dz**2
      r_inv = 1.0 / r2
      r6 = (sigma**2 * r_inv)**3
      r12 = r6**2

      fx = 24.0 * epsilon * (2.0 * r12 - r6) * dx * r_inv
      fy = 24.0 * epsilon * (2.0 * r12 - r6) * dy * r_inv
      fz = 24.0 * epsilon * (2.0 * r12 - r6) * dz * r_inv

      f(i, 1) = f(i, 1) + fx
      f(i, 2) = f(i, 2) + fy
      f(i, 3) = f(i, 3) + fz

      f(j, 1) = f(j, 1) - fx
      f(j, 2) = f(j, 2) - fy
      f(j, 3) = f(j, 3) - fz
    end do
  end do

  print *, 'Forces:', f
end program lj_force

