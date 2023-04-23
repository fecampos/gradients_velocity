      module param

      implicit none

      character(len=*),parameter :: file_in = "in.nc"

      character(len=*),parameter :: time_NAME = "time"
      character(len=*),parameter :: depth_NAME = "depth"      
      character(len=*),parameter :: lon_NAME = "longitude"
      character(len=*),parameter :: lat_NAME = "latitude"

      character(len=*),parameter :: u_NAME = "uo"
      character(len=*),parameter :: v_NAME = "vo"
      
      integer, parameter :: nx = 721, ny = 421, nz = 1, nt = 1

      integer :: i, j, k

      real, parameter :: missing_val = 9.96921e+36, pi= 4*atan(1.)

      real :: T(nt), X(nx), Y(ny), Z(nz), u(nx,ny,nz,nt), v(nx,ny,nz,nt)

      integer :: ncid, retval, timevarid, lonvarid, latvarid, depthvarid

      integer :: u_varid, v_varid

      end module
