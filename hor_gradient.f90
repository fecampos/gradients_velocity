      subroutine  hor_gradient(nx,ny,missing_val,lon,lat,f,gx,gy)

      implicit none

      integer, intent(in) :: nx,ny

      real,intent(in) :: f(nx,ny), lon(nx), lat(ny), missing_val

      real, intent(out) :: gx(nx,ny), gy(nx,ny)

      real :: g(nx,ny)

      integer :: i, j

      g = missing_val

      !OMP PARALLEL DO
      do i = 2,nx-1
        do j = 1,ny
          if ((f(i+1,j).ne.missing_val).and.(f(i-1,j).ne.missing_val)) then
            gx(i,j) = (f(i+1,j)-f(i-1,j))/2                    
          end if
        end do
      end do
      !OMP END PARALLEL DO
 
      !OMP PARALLEL DO
      do j = 1,ny
        if ((f(2,j).ne.missing_val).and.(f(1,j).ne.missing_val)) then
          gx(1,j) = f(2,j)-f(1,j)
        end if
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do j = 1,ny
        if ((f(nx,j).ne.missing_val).and.(f(nx-1,j).ne.missing_val)) then
          gx(nx,j) = f(nx,j)-f(nx-1,j)
        end if
      end do
      !OMP END PARALLEL DO


      !OMP PARALLEL DO
      do i = 1,nx
        do j = 2,ny-1
          if ((f(i,j+1).ne.missing_val).and.(f(i,j-1).ne.missing_val)) then
            gy(i,j) = (f(i,j+1)-f(i,j-1))/2
          end if
        end do      
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do i = 1,nx
        if ((f(i,2).ne.missing_val).and.(f(i,1).ne.missing_val)) then
          gy(i,1) = f(i,2)-f(i,1)
        end if
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do i = 1,nx
        if ((f(i,ny).ne.missing_val).and.(f(i,ny-1).ne.missing_val)) then
          gy(i,ny) = f(i,ny)-f(i,ny-1)
        end if
      end do
      !OMP END PARALLEL DO

      end subroutine
