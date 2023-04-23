      program main_program

      use netcdf

      use param

      implicit none

      real, dimension(nx,ny) :: dx, dy, area, u_x, u_y, v_x, v_y, nothing 
      real, dimension(nx,ny,nz,nt) :: masku, maskv, vort, divh

      retval = nf90_open(file_in, NF90_NOWRITE, ncid)

      retval = nf90_inq_varid(ncid, lon_NAME, lonvarid)
      retval = nf90_get_var(ncid, lonvarid, X)
      retval = nf90_inq_varid(ncid, lat_NAME, latvarid)
      retval = nf90_get_var(ncid, latvarid, Y)
      retval = nf90_inq_varid(ncid, time_NAME, timevarid)
      retval = nf90_get_var(ncid, depthvarid, Z)
      retval = nf90_inq_varid(ncid, depth_NAME, depthvarid)
      retval = nf90_get_var(ncid, timevarid, T)
      retval = nf90_inq_varid(ncid, u_NAME, u_varid)      
      retval = nf90_get_var(ncid, u_varid, u)
      retval = nf90_inq_varid(ncid, v_NAME, v_varid)
      retval = nf90_get_var(ncid, v_varid, v)      
      retval = nf90_close(ncid)

!     =========================
!     creating umask and vmask:
!     =========================
      masku = 0
      where (u.ne.missing_val)
        masku = 1
      end where

      maskv = 0
      where (v.ne.missing_val)
        maskv = 1
      end where

!     ========================================
!     computing distances delta_x and delta_y:
!     ========================================

      call distances(nx,ny,X,Y,dx,dy)
      area = 1/(dx*dy)

!     ==================================
!     computing gradients from velocity:
!     ==================================
      !OMP PARALLEL DO
      do i = 1,nt
        do j = 1,nz
          call hor_gradient(nx,ny,missing_val,X,Y,u(:,:,j,i)*dy*masku(:,:,j,i),u_x,nothing)
          call hor_gradient(nx,ny,missing_val,X,Y,u(:,:,j,i)*dx*masku(:,:,j,i),nothing,u_y)
          call hor_gradient(nx,ny,missing_val,X,Y,v(:,:,j,i)*dy*maskv(:,:,j,i),v_x,nothing)
          call hor_gradient(nx,ny,missing_val,X,Y,v(:,:,j,i)*dx*maskv(:,:,j,i),nothing,v_y)

          vort(:,:,j,i) = (v_x-u_y)*area
          divh(:,:,j,i) = (u_x+v_y)*area
        end do  
      end do
      !OMP END PARALLEL DO     
         
!     =====================
!     considering landmask:
!     =====================

      where ((u.eq.missing_val).and.(v.eq.missing_val))
        vort = missing_val
        divh = missing_val
      end where


      call write_outputs(nx,ny,nz,nt,X,Y,Z,T,missing_val,vort,divh)

      end program
