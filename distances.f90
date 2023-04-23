      subroutine  distances(nx,ny,lon,lat,delta_x, delta_y)

      implicit none

      integer, intent(in) :: nx,ny

      real,intent(in) :: lon(nx), lat(ny)

      real, intent(out) :: delta_x(nx,ny), delta_y(nx,ny)

      real :: dx, dy

      integer :: i, j

      !OMP PARALLEL DO
      do j = 1,ny
        do i = 2,nx-1
          call gsw_distance(lon(i+1),lon(i-1),lat(j),lat(j),dx)
          delta_x(i,j) = dx/2
        end do
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do j = 1,ny
        call gsw_distance(lon(2),lon(1),lat(j),lat(j),delta_x(1,j))
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do j = 1,ny
        call gsw_distance(lon(nx),lon(nx-1),lat(j),lat(j),delta_x(nx,j))
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do j = 2,ny-1
        do i = 1,nx
          call gsw_distance(lon(i),lon(i),lat(j+1),lat(j-1),dy)
          delta_y(i,j) = dy/2
        end do      
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do i = 1,nx
        call gsw_distance(lon(i),lon(i),lat(2),lat(1),delta_y(i,1))
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do i = 1,nx
        call gsw_distance(lon(i),lon(i),lat(nx),lat(nx-1),delta_y(i,ny))
      end do
      !OMP END PARALLEL DO

      end subroutine
