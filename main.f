	program program_name
	use stdlib_random, only: random_seed
	use stdlib_stats_distribution_normal, only: norm => rvs_normal


	implicit none
	external :: pgclos, pgimag ! PGPLOT imports
	

		integer :: seed_put, seed_get
		integer, parameter :: N     = 200
		real    :: vm(6)
		integer :: rc
		
		
		
		call pgplot_init('/XWINDOW', N, N, vm, rc)
		if (rc < 0) stop 'Error: Failed to open output device'
		call pgpage
		call random_plot(N, N, vm, rc)
		call pgpage
		call normal_plot(N, N, vm, rc)
		call pgend
		call pgclos

	contains
		subroutine normal_plot(n, m, vm, stat)

			integer,          intent(in)            :: n
			integer,          intent(in)            :: m
			
			real,             intent(in)           :: vm(6)
			integer,          intent(out), optional :: stat
			integer 								:: k, r
			real 									:: i
			real        							:: matrix(n, m)
			real 									:: fmin, fmax

			do k = 1, n
				do r = 1, m
					!! norm(mean, standard_deviation)
					matrix(r, k) = norm(0.0, 1.0)
				end do
			end do

			fmin = minval(matrix)
			fmax = maxval(matrix)
			call pgimag(matrix, n, m, 1, n, 1, m, fmin, fmax, vm)

		end subroutine normal_plot
		subroutine random_plot(n, m, vm, stat)
		
			integer,          intent(in)            :: n
			integer,          intent(in)            :: m
			
			real,             intent(in)           :: vm(6)
			integer,          intent(out), optional :: stat
			integer 								:: k, r
			real 									:: i
			real        							:: matrix(n, m)
			real 									:: fmin, fmax

			do k = 1, n
				do r = 1, m
					call RANDOM_NUMBER(i)
					matrix(r, k) = i*100
				end do	
			end do

			fmin = minval(matrix)
			fmax = maxval(matrix)
			call pgimag(matrix, n, m, 1, n, 1, m, fmin, fmax, vm)
		end subroutine random_plot

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
			call pgask(.true.)
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