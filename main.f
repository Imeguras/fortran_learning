	program program_name
	use stdlib_random, only: random_seed
	use stdlib_stats_distribution_normal, only: norm => rvs_normal


	implicit none
	external :: pgclos, pgimag ! PGPLOT imports
	

		integer :: seed_put, seed_get
		integer, parameter :: N	 = 200
		real	:: vm(6)
		integer :: rc
		seed_put = 45067		
		call random_seed(seed_put, seed_get)
		
		call pgplot_init('/XWINDOW', N, N, vm, rc)
		if (rc < 0) stop 'Error: Failed to open output device'
		call pgpage
		call random_plot(N, N, vm, rc)
		call pgpage
		call normal_plot(N, N, vm, rc)
		call pgpage
		call perlin_noise_plot(N, N, vm, rc)
		call pgend
		call pgclos

	contains
		subroutine perlin_noise_plot(n, m, vm, stat)

			integer,		  intent(in)			:: n
			integer,		  intent(in)			:: m
			
			real,			 intent(in)		   :: vm(6)
			integer,		  intent(out), optional :: stat
			integer 								:: k, r
            real                                    :: wild
			real 									:: i
			real									:: matrix(n, m)
			real 									:: fmin, fmax
            do k = 1, n
                do r = 1, m
                    call RANDOM_NUMBER(wild)
                    matrix(r, k) = perlin_noise(wild+r, wild+k)
                end do
            end do
            fmin = minval(matrix)
			fmax = maxval(matrix)
			call pgimag(matrix, n, m, 1, n, 1, m, fmin, fmax, vm)

		end subroutine perlin_noise_plot

		subroutine normal_plot(n, m, vm, stat)

			integer,		  intent(in)			:: n
			integer,		  intent(in)			:: m
			
			real,			 intent(in)		   :: vm(6)
			integer,		  intent(out), optional :: stat
			integer 								:: k, r
			real 									:: i
			real									:: matrix(n, m)
			real 									:: fmin, fmax

			do k = 1, n
				matrix(:, k) = norm(0.0, 1.0, m)
			end do

			fmin = minval(matrix)
			fmax = maxval(matrix)
			call pgimag(matrix, n, m, 1, n, 1, m, fmin, fmax, vm)

		end subroutine normal_plot

		subroutine random_plot(n, m, vm, stat)
		
			integer,		  intent(in)			:: n
			integer,		  intent(in)			:: m
			
			real,			 intent(in)		   :: vm(6)
			integer,		  intent(out), optional :: stat
			integer 								:: k, r
			real 									:: i
			real									:: matrix(n, m)
			real 									:: fmin, fmax

			do k = 1, n
				do r = 1, m
					call RANDOM_NUMBER(i)
					matrix(r, k) = i
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

			character(len=*), intent(in)			:: gd
			real,			 intent(out)		   :: vm(6)
			integer,		  intent(in)			:: n
			integer,		  intent(in)			:: m
			integer,		  intent(out), optional :: stat

			integer :: rc
			real	:: bright, contra
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
        function dot_grid_gradient( ix, iy, x, y) result(ret)
			integer, intent(in) :: ix, iy
			real, intent(in)	:: x, y
			real 				:: dx, dy
			real 				:: gx, gy
		 	real                :: ret
            dx = x - ix;
	 		dy = y - iy;
			call RANDOM_NUMBER(gx)
			call RANDOM_NUMBER(gy)

			ret = dx*gx + dy*gy
		end function dot_grid_gradient
		function perlin_noise(posX, posY) result(value)
			real, intent(in)    :: posX, posY
			real                :: sx, sy
			integer             :: x0, y0, x1, y1
			real                :: n0, n1, ix0, ix1, value
			
			x0 = int(posX)
			y0 = int(posY)
			x1 = x0+1
			y1 = y0+1
		
			n0 = dot_grid_gradient(x0, y0, posX, posY);
			n1 = dot_grid_gradient(x1, y0, posX, posY);
			ix0 = lerp(n0, n1, sx);

			n0 = dot_grid_gradient(x0, y1, posX, posY);
			n1 = dot_grid_gradient(x1, y1, posX, posY);
			ix1 = lerp(n0, n1, sx);

			value = lerp(ix0, ix1, sy);
			

		end function perlin_noise
		function lerp (a0, b0, w) result(ret)
			real, intent(in)  :: a0, b0, w
			real              :: ret
            ret = (1.0-w)*a0 + w*b0
		end function lerp
		
		

	end program program_name