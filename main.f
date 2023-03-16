	program program_name
	implicit none
	external :: pgclos, pgimag ! PGPLOT imports
	


		
		integer, parameter :: N     = 200
		real :: matrix(N, N)
		real :: fmin, fmax
		integer:: k, r
		real :: i
		real    :: vm(6)
		integer :: rc
		

		do k = 1, N
			do r = 1, N
				call RANDOM_NUMBER(i)
				Print "(f6.3)", i
				matrix(r, k) = i*100

			end do
			
		end do
		
		call pgplot_init('/XWINDOW', N, N, vm, rc)
		if (rc < 0) stop 'Error: Failed to open output device'
		fmin = minval(matrix)
    	fmax = maxval(matrix)
		call pgimag(matrix, N, N, 1, N, 1, N, fmin, fmax, vm)

		call pgclos()

	contains
		subroutine pgplot_init(gd, n, m, vm, stat)
		!! print a real number

			!! Opens the given graphics device, sets the transformation matrix,
			!! initialises PGPLOT, and sets the colour palette.
			integer  :: pgopen
			external :: pgbox, pgctab, pgenv, pgmtxt, pgwnad

			character(len=*), intent(in)            :: gd
			real,             intent(out)           :: vm(6)
			integer,          intent(in)            :: n
			integer,          intent(in)            :: m
			integer,          intent(out), optional :: stat

			integer :: rc
			real    :: bright, contra
			real 	:: a(9), b(9), c(9), d(9)
			if (present(stat)) stat = -1

			! Open the given graphics device.
			rc = pgopen(gd)
			if (rc < 0) return

			! Set the coordinate transformation matrix. In this case, world coordinate
			! equals pixel number.
			vm = [ 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ]
			
			! Set up window and viewport.
			call pgwnad(0.0, 1.0 + n, 0.0, 1.0 + m)

			! Add border and output text.
			call pgbox('bcts', 0.0, 0, 'bcts', 0.0, 0)
			call pgmtxt('t', 1.0, 0.0, 0.0, 'pgplot')
			a = [ -0.5, 0.0, 0.17, 0.33, 0.50, 0.67, 0.83, 1.0, 1.7 ]
			b = [ 0.0, 0.0,  0.0,  0.0,  0.6,  1.0,  1.0, 1.0, 1.0 ]
			c = [ 0.0, 0.0,  0.0,  1.0,  1.0,  1.0,  0.6, 0.0, 1.0 ]
			d = [ 0.0, 0.3,  0.8,  1.0,  0.3,  0.0,  0.0, 0.0, 1.0 ]
			! Set up the rainbow colour palette.
			bright = 0.5; contra = 1.0
		    call pgctab(a,b,c,d, 9, contra, bright)

			if (present(stat)) stat = 0
		end subroutine pgplot_init
	end program program_name