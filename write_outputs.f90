      subroutine write_outputs(nx,ny,nz,nt,X,Y,Z,T,missing_value,term1,term2)
      
      use netcdf 

      implicit none      

      character(len=*), parameter :: file_out="output.nc"

      real :: missing_value

      integer :: nx, ny, nz,nt

      real :: X(nx), Y(ny), Z(nz), T(nt)
      real ::  term1(nx,ny,nz,nt), term2(nx,ny,nz,nt)

      integer, parameter :: NDIMS4 = 4

      character(len=*), parameter :: t_NAME="time"
      character(len=*), parameter :: z_NAME="depth"
      character(len=*), parameter :: x_NAME="longitude"
      character(len=*), parameter :: y_NAME="latitude"

      integer :: t_dimid, z_dimid, y_dimid, x_dimid
      integer :: t_varid, z_varid, y_varid, x_varid

      character(len=*), parameter :: term1_NAME="vort"
      character(len=*), parameter :: term2_NAME="divh"

      integer :: term1_varid, term2_varid, dimids4(NDIMS4)

      character(len=*), parameter :: UNITS = "units"

      character(len=*), parameter :: t_UNITS=&    
      & "hours since 1950-01-01"
      character(len=*), parameter :: z_UNITS="depth"
      character(len=*), parameter :: y_UNITS="degrees_north"
      character(len=*), parameter :: x_UNITS="degrees_east"

      character(len=*), parameter :: term1_UNITS="1/s"
      character(len=*), parameter :: term2_UNITS="1/s"

      character(len=*), parameter :: LNAME="long_name"

      character(len=*), parameter :: t_LNAME=&
      &"hours since 1950-01-01"
      character(len=*), parameter :: z_LNAME="Depth"
      character(len=*), parameter :: y_LNAME="Latitude"
      character(len=*), parameter :: x_LNAME="Longitude"
   
      character(len=*), parameter :: term1_LNAME=&
      &"relative vorticity"
      character(len=*), parameter :: term2_LNAME=&
      &"horizontal divergence"

      integer :: retval, ncid, rhvarid

      retval = nf90_create(file_out,ior(nf90_noclobber,nf90_64bit_offset),ncid)

      retval = nf90_def_dim(ncid, t_NAME, NF90_UNLIMITED, t_dimid)
      retval = nf90_def_dim(ncid, z_NAME, NZ, z_dimid)
      retval = nf90_def_dim(ncid, y_NAME, NY, y_dimid)
      retval = nf90_def_dim(ncid, x_NAME, NX, x_dimid)

      retval = nf90_def_var(ncid, t_NAME, NF90_REAL, t_dimid, t_varid)
      retval = nf90_def_var(ncid, z_NAME, NF90_REAL, z_dimid, z_varid)      
      retval = nf90_def_var(ncid, y_NAME, NF90_REAL, y_dimid, y_varid)
      retval = nf90_def_var(ncid, x_NAME, NF90_REAL, x_dimid, x_varid)

      retval = nf90_put_att(ncid, t_varid, UNITS, t_UNITS)
      retval = nf90_put_att(ncid, z_varid, UNITS, z_UNITS)
      retval = nf90_put_att(ncid, y_varid, UNITS, y_UNITS)
      retval = nf90_put_att(ncid, x_varid, UNITS, x_UNITS)

      retval = nf90_put_att(ncid, t_varid, LNAME, t_LNAME)
      retval = nf90_put_att(ncid, z_varid, LNAME, z_LNAME)      
      retval = nf90_put_att(ncid, y_varid, LNAME, y_LNAME)
      retval = nf90_put_att(ncid, x_varid, LNAME, x_LNAME)

      retval = nf90_put_att(ncid, rhvarid,"title",&
                &"written by fecg: fcampos@cicese.edu.mx")

      dimids4(1) = x_dimid
      dimids4(2) = y_dimid
      dimids4(3) = z_dimid
      dimids4(4) = t_dimid

      retval = nf90_def_var(ncid, term1_NAME, NF90_REAL, dimids4, term1_varid)
      retval = nf90_def_var(ncid, term2_NAME, NF90_REAL, dimids4, term2_varid)
            
      retval = nf90_put_att(ncid, term1_varid, UNITS, term1_UNITS)
      retval = nf90_put_att(ncid, term1_varid, LNAME, term1_LNAME)
      retval = nf90_put_att(ncid, term2_varid, UNITS, term2_UNITS)
      retval = nf90_put_att(ncid, term2_varid, LNAME, term2_LNAME)

      retval = nf90_put_att(ncid, term1_varid,'missing_value', missing_value)
      retval = nf90_put_att(ncid, term2_varid,'missing_value', missing_value)

      retval = nf90_enddef(ncid)

      retval = nf90_put_var(ncid, t_varid, T)
      retval = nf90_put_var(ncid, z_varid, Z)
      retval = nf90_put_var(ncid, y_varid, Y)
      retval = nf90_put_var(ncid, x_varid, X)

      retval = nf90_put_var(ncid, term1_varid, term1)
      retval = nf90_put_var(ncid, term2_varid, term2)
      retval = nf90_close(ncid)

      return

      end subroutine
