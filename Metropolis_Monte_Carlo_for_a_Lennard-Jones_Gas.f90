rogram mc_metropolis
  implicit none
  integer, parameter :: n = 100, steps = 10000
  real(8), dimension(n, 3) :: positions
  real(8) :: box, beta, energy_old, energy_new, delta_e, dx, dy, dz
  integer :: i, step

  box = 10.0    ! Simulation box length
  beta = 1.0    ! 1/(kB*T)

  call random_number(positions)
  positions = positions * box

  energy_old = lj_energy(positions, n, box)

  do step = 1, steps
    i = int(n * rand()) + 1
    dx = (rand() - 0.5) * 0.1
    dy = (rand() - 0.5) * 0.1
    dz = (rand() - 0.5) * 0.1

    positions(i, :) = mod(positions(i, :) + (/dx, dy, dz/), box)

    energy_new = lj_energy(positions, n, box)
    delta_e = energy_new - energy_old

    if (rand() < exp(-beta * delta_e)) then
      energy_old = energy_new
    else
      positions(i, :) = positions(i, :) - (/dx, dy, dz/)
    end if
  end do

  print *, "Final Energy:", energy_old
end program mc_metropolis

~                                                                                                                                                                                                          
~                                               
