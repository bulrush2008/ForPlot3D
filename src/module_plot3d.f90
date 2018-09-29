module mod_plot3d

  use stringifor
  implicit none

  type, public :: plot3d_type

    integer :: in=0
    integer :: jn=0
    integer :: kn=0
    integer :: ln=0
    real(8), allocatable :: x(:, :, :)
    real(8), allocatable :: y(:, :, :)
    real(8), allocatable :: z(:, :, :)
    real(8), allocatable :: q(:, :, :, :)
    type(string) :: grid
    type(string) :: func

  contains

    generic :: Create=> create_2D, Create_3D, Create_null
    procedure :: ReadGrid=>ReadGrid_3D
    procedure :: ReadFunc=>ReadFunc_3D
    procedure :: WriteGrid
    procedure :: WriteFunc

    procedure, private :: Create_2D
    procedure, private :: Create_3D
    procedure, private :: Create_null


  end type plot3d_type

contains

  subroutine Create_null(this)

    implicit none
    class(plot3d_type), intent(inout) :: this

  end subroutine Create_null

  subroutine Create_2D(this, in, jn)

    implicit none
    class(plot3d_type), intent(inout) :: this
    integer, intent(in) :: in
    integer, intent(in) :: jn

    this%in=in; this%jn=jn; this%kn=1
    print*, 'in=', in
    print*, 'jn=', jn
    if(allocated(this%x)) then
      deallocate(this%x, this%y, this%z)
    endif
    allocate(this%x(in, jn, this%kn))
    allocate(this%y(in, jn, this%kn))
    allocate(this%z(in, jn, this%kn))

  end subroutine Create_2D

  subroutine Create_3D(this, in, jn, kn)

    implicit none
    class(plot3d_type), intent(inout) :: this
    integer, intent(in) :: in
    integer, intent(in) :: jn
    integer, intent(in) :: kn

    this%in=in; this%jn=jn; this%kn=kn
    print*, 'in=', in
    print*, 'jn=', jn
    print*, 'kn=', kn

    if(allocated(this%x)) then
      deallocate(this%x, this%y, this%z)
    endif
    allocate(this%x(in, jn, kn))
    allocate(this%y(in, jn, kn))
    allocate(this%z(in, jn, kn))

  end subroutine Create_3D

  subroutine ReadGrid_3D(this, fn)

    implicit none
    class(plot3d_type), intent(inout) :: this
    character(len=*), intent(in) :: fn
    integer :: in, jn, kn
    integer :: i, j, k

    this%grid=trim(fn)
    open(99, file=this%grid//'', form='unformatted')
    print*, 'Reading Grid file', this%grid//''
    read(99)
    read(99)in, jn, kn
    call this%Create(in, jn, kn)
    read(99)(((this%x(i, j, k), i=1, this%in), j=1, this%jn), k=1, this%kn), &
            (((this%y(i, j, k), i=1, this%in), j=1, this%jn), k=1, this%kn), &
            (((this%z(i, j, k), i=1, this%in), j=1, this%jn), k=1, this%kn)
    close(99)

  end subroutine ReadGrid_3D

  subroutine ReadFunc_3D(this, fn)

    implicit none
    class(plot3d_type), intent(inout) :: this
    character(len=*), intent(in) :: fn
    integer :: in, jn, kn, ln
    integer :: i, j, k, l

    this%func=trim(fn)
    open(99, file=this%func//'', form='unformatted')
    print*, 'Reading function file', this%func//''
    read(99)
    read(99)in, jn, kn, ln
    print*, 'in=', in
    print*, 'jn=', jn
    print*, 'kn=', kn
    print*, 'ln=', ln
    this%ln=ln
    if(allocated(this%q)) deallocate(this%q)
    allocate(this%q(this%in, this%jn, this%kn, this%ln))
    read(99)((((this%q(i, j, k, l), i=1, this%in), j=1, this%jn), k=1, this%kn), &
                l=1, this%ln)
    close(99)

  end subroutine ReadFunc_3D

  subroutine WriteGrid(this)

    class(plot3d_type), intent(inout) :: this

    open(99, file=this%grid//'', form='unformatted')
    print*, 'Writing grid file', this%Grid//''
    print*, 'in=', this%in
    print*, 'jn=', this%jn
    print*, 'kn=', this%kn
    write(99)1
    write(99)this%in, this%jn, this%kn
    write(99)this%x, this%y, this%z
    close(99)

  end subroutine WriteGrid

  subroutine WriteFunc(this)

    class(plot3d_type), intent(inout) :: this
    integer :: i, j, k, l

    open(99, file=this%func//'', form='unformatted')
    print*, 'Writing function file', this%func//''
    print*, 'in=', this%in
    print*, 'jn=', this%jn
    print*, 'kn=', this%kn
    print*, 'ln=', this%ln
    write(99)1
    write(99)this%in, this%jn, this%kn, this%ln
    write(99)((((this%q(i, j, k, l), i=1, this%in), j=1, this%jn), k=1, this%kn), &
                l=1, this%ln)
    close(99)

  end subroutine WriteFunc

end module mod_plot3d
