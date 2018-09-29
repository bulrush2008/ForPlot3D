module mod_plot3d


  implicit none

  type, public :: plot3d_type

    integer :: in
    integer :: jn
    integer :: kn

  contains

    procedure :: create

  end type plot3d_type

contains

  subroutine Create_2D()

  end subroutine Create_2D


end module mod_plot3d
