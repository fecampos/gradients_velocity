      elemental subroutine gsw_distance(lon2,lon1,lat2,lat1,dist)

      implicit none

      real, intent(in) :: lon2, lon1, lat2, lat1

      real, intent(out) :: dist

      real, parameter :: pi = 3.1415927, earth_radius = 6.3781e+6

      real :: dlon, dlat, a

      dlon = (lon2-lon1)*pi/180
      dlat = (lat2-lat1)*pi/180

      a = (sin(0.5*dlat))**2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(0.5*dlon))**2

      dist = earth_radius*2*atan2(sqrt(a),sqrt(1-a))

      return

      end subroutine
