include 'kind_module.f90'
include 'toolbox_standalone.f90'
include 'toolbox_outer.f90'
include 'prob.f90'
include 'Global_Values.f90'
include 'solver.f90'
include 'SMM.f90'
include 'counterfactuals.f90'

program MAIN

!use kind_module
use toolbox
use toolbox_outer
use prob
use Global_Values
use solver
use SMM
use counterfactuals

implicit none

integer :: im
real*8 :: dummy_fval(1,1)
character(len=10) :: arg1,arg2,arg3 ! in order MC_yes,optimal_parms,MC_seed

smopec = .FALSE. ! fixed interest rate iteration (2% annualized to five year intervals)
ge_verbose = .FALSE. ! GE verbose
smm_verbose = .TRUE. ! verbose during SMM
!MC_yes = .FALSE.
!optimal_parms = .TRUE. ! has to be run after R program picking out the minimum value !
!counterfactuals_run = .TRUE.
demand_calibration = .FALSE.  ! hard coded after manual calibration

! read the logicals (everything else is permanent and hard coded)
! Read the first and second command line arguments
call get_command_argument(1, arg1)
call get_command_argument(2, arg2)
call get_command_argument(3, arg3)

! whether or not MC is happening
read(arg1, *) MC_yes
! whether or not parameters are optimal
if(MC_yes)then
	optimal_parms = .FALSE.
	
	! and read in the seed
	read(arg3, *) MC_seed
	MC_seed_original = MC_seed

else
	read(arg2, *) optimal_parms
	if(optimal_parms)then 
		counterfactuals_run = .TRUE.
	endif
endif

!if(MC_yes)then
	
	! set seed by read in
!	write(*,*) 'Enter a seed number, 7 digits:'
!	read(*,*) MC_seed
	
	
!endif
if(demand_calibration)then

	! read
	write(*,*) 'Enter g_z value:'
	read(*,*) g_Z
	write(*,*) 'Enter Z_0 value:'
	read(*,*) Z_0
	
	g_z_Z0_out(1) = g_Z
	g_z_Z0_out(2) = Z_0

else

	g_Z = 0.0057d0
	Z_0 = 16.315d0

endif

! allocate the price vector
if(smopec)then
	allocate(price_vector_competitive(4))
	allocate(price_vector_monop(3+JJ+1))
else
	allocate(price_vector_competitive(5))
	allocate(price_vector_monop(4+JJ+1))
endif


if(optimal_parms)then
		
	! read in optimal parms
	in_string(1:13) = "parms_min.txt"
	open(116177, file = in_string(1:13),status="old")
	read(116177,*) parms
	close(116177)
			
!	write(*,*) 'Optimal parameters from Monte Carlo simulations:'
!	write(*,*) parms
	
endif
	
! Monte carlo
if(MC_yes)then
		
	call assign_data_moments() 
	call update_parms(.TRUE.) ! call this to set the max min bounds on uniform draws

	do im = 1,MCsamples! repeat the monte carlo until finished
		
		call tic()
		! make sure the seed updates
		call test_moments()
			
		! assign parms and loss to output matrix
		parms_out(im,:) = parms
		dummy_fval = matmul(transpose(loss_func),loss_func)
		loss_func_out(im) = dummy_fval(1,1)
		write(*,*) 'im:',im
		call toc()
		
	enddo
	
else
	
	if(optimal_parms)then
		
		! read in optimal parms
		in_string(1:13) = "parms_min.txt"
		open(116177, file = in_string(1:13),status="old")
		read(116177,*) parms
		close(116177)
		
		write(*,*) 'Optimal parameters from Monte Carlo simulations:'
		write(*,*) parms
		
		! assign data moments
		call assign_data_moments() 
		call test_moments()
			
		! counterfactuals
		if(counterfactuals_run)then
			
			write(*,*) 'Running counterfactuals.'
			call run_counterfactuals()
			
		endif
		
	else
		
		! assign data moments
		call assign_data_moments() 
		call test_moments()
			
	endif
endif

! write out post MC calibration loss function
if(demand_calibration)then
    
    ! write g_z, Z_0 out
	out_string(1:10) = "gzZ0_D.txt"
	inquire(file = out_string(1:10), exist=exists)
    if(exists) then
       open(120011, file = out_string(1:10), status = "old", action = "write")
    else
 	   open(120011, file = out_string(1:10), status = "new", action = "write")
    end if
    write(120011,*) g_z_Z0_out
    close(120011)


endif

! write out the MC samples to disk, including the seed number
if(MC_yes)then

	! write loss function
	out_string(1:4) = "loss"
	out_string(5:11) = trim(str(MC_seed_original))
	out_string(12:15) = ".txt"
	inquire(file = out_string(1:15), exist=exists)
    if(exists) then
       open(120000, file = out_string(1:15), status = "old", action = "write")
    else
 	   open(120000, file = out_string(1:15), status = "new", action = "write")
    end if
    write(120000,*) loss_func_out
    close(120000)

	! write parameters
    out_string(1:4) = "parm"
	out_string(5:11) = trim(str(MC_seed_original))
	out_string(12:12) = "_"
    out_string(14:17) = ".txt"
    do im = 1,Nparms

      out_string(13:13) = trim(str(im))

      inquire(file = out_string(1:17), exist=exists)
      if(exists) then
        open(120000+im, file = out_string(1:17), status = "old", action = "write")
      else
 	    open(120000+im, file = out_string(1:17), status = "new", action = "write")
      end if
      write(120000+im,*) parms_out(:,im)
      close(120000+im)

    end do

endif

! write out all of the model time series required for analysis to disk
if(optimal_parms)then

	! write loss function
	out_string(1:10) = "loss_D.txt"
	inquire(file = out_string(1:10), exist=exists)
    if(exists) then
       open(120000, file = out_string(1:10), status = "old", action = "write")
    else
 	   open(120000, file = out_string(1:10), status = "new", action = "write")
    end if
    write(120000,*) loss_out_demand
    close(120000)

	! write the growth rates
	out_string(1:6) = "gy_sim"
	out_string(7:10) = ".txt"
	inquire(file = out_string(1:10), exist=exists)
	if(exists) then
		open(12002, file = out_string(1:10), status = "old", action = "write")
	else
		open(12002, file = out_string(1:10), status = "new", action = "write")
	end if
	write(12002,*) gy(:)
	close(12002)

    do im = 1,Nmoments
    
    	! write data and simulated moments
		if(im < 10)then
		
			! data
			out_string(1:12) = "data_moments"
			out_string(13:13) = trim(str(im))
			out_string(14:17) = ".txt"
			inquire(file = out_string(1:17), exist=exists)
			if(exists) then
				open(120001+im, file = out_string(1:17), status = "old", action = "write")
			else
				open(120001+im, file = out_string(1:17), status = "new", action = "write")
			end if
			write(120001+im,*) data_moments(:,im)
			close(120001+im)
			
			! siml
			out_string(1:12) = "siml_moments"
			out_string(13:13) = trim(str(im))
			out_string(14:17) = ".txt"
			inquire(file = out_string(1:17), exist=exists)
			if(exists) then
				open(120001+im, file = out_string(1:17), status = "old", action = "write")
			else
				open(120001+im, file = out_string(1:17), status = "new", action = "write")
			end if
			write(120001+im,*) simulated_moments(:,im)
			close(120001+im)
			
		else
			
			! data
			out_string(1:12) = "data_moments"
			out_string(13:14) = trim(str(im))
			out_string(15:18) = ".txt"
			inquire(file = out_string(1:18), exist=exists)
			if(exists) then
				open(120001+im, file = out_string(1:18), status = "old", action = "write")
			else
				open(120001+im, file = out_string(1:18), status = "new", action = "write")
			end if
			write(120001+im,*) data_moments(:,im)
			close(120001+im)
			
			! siml
			out_string(1:12) = "siml_moments"
			out_string(13:14) = trim(str(im))
			out_string(15:18) = ".txt"
			inquire(file = out_string(1:18), exist=exists)
			if(exists) then
				open(120001+im, file = out_string(1:18), status = "old", action = "write")
			else
				open(120001+im, file = out_string(1:18), status = "new", action = "write")
			end if
			write(120001+im,*) simulated_moments(:,im)
			close(120001+im)
			
			
		endif
			

    end do

endif

! write out counterfactual time series
if(counterfactuals_run)then

	call write_counterfactuals()

endif

deallocate(price_vector_competitive)
deallocate(price_vector_monop)

end program MAIN
