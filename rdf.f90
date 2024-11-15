program rdf
  implicit none
  integer, parameter :: n_particles = 1000, n_bins = 100
  real(8), dimension(n_particles, 3) :: positions
  real(8), dimension(n_bins) :: g, bin_size, r, rho, box
  integer :: i, j, k

  box = 10.0
  bin_size = box / real(n_bins)
  rho = n_particles / box**3

  ! Initialize positions
  call random_number(positions)
  positions = positions * box

  g = 0.0

  do i = 1, n_particles
    do j = i+1, n_particles
      r = sqrt(sum((positions(i, :) - positions(j, :))**2))
      k = int(r / bin_size) + 1
      if (k <= n_bins) g(k) = g(k) + 2
    end do
  end do

  g = g / (n_particles * rho * 4.0 * pi * bin_size**3)
  print *, "RDF:", g
end program rdf

