module SMM

use kind_module
use toolbox
use toolbox_outer
use prob
use Global_Values
use solver

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	update internal parameters for SMM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine update_parms(initial_smm) 

	implicit none
	integer :: it
	logical, intent(in) :: initial_smm
	
	if(initial_smm)then

		! A_h
		A_h0 = 0.106300628707999d0
		
		do it = 1,4
		
			g_h(it) = 0.01720613d0
			
		enddo 
		
		! xi
		xi = 44d0

		! assign initial parameters to the vector
		parms(1) = A_h0
		parms(2:5) = g_h
		
		! xi
		parms(6) = xi

		! parameter bounds
		! assign minima and maxima
		! mins
		parms_min(1) = 0.06d0
		parms_min(2:5) = -0.017d0
		parms_min(6) = 30d0
		
		! maxs	
		parms_max(1) = 0.15d0
		parms_max(2:5) = 0.12d0
		parms_max(6) = 60d0
		
	else
	
		! pass parameters back block
		A_h0 = parms(1)
		g_h = parms(2:5)
		xi = parms(6)
	!	g_h(2) = 0.01d0 ! test
	!	g_h(3) = 0.05d0 !test
	!	g_h(4) = 0.15d0 ! test

	end if	


end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	subroutine to assign data moments to the vector
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine assign_data_moments()

	! moments (summary statistics for which we have time series data)
	! 1) q-prices relative to base year, take sample average errors 
	! 2) life expectancy for an entrant into the economy at each 1960-2015
	! 3) health aggregate share of consumption
	! 4) capital share of all capital in health production
	! 5) labor share of all labor in health production
	
	! This subroutine also reads in immutable parameter values.
	! ... mu, pop0, mAcc, g_c, 
	
	implicit none
	integer :: it,im,ij
	real*8 :: p_prices(12), life_expectancies(12)
	real*8 :: health_share_spending(12)
	real*8 :: health_capital_share(12), health_labor_share(12)
	
	!!!!! READ IN DATA MOMENTS FOR SMM MONTE CARLO !!!!!!
	
	! all data run from 1960-2015 !
	! prices (we call them q in the code for some reason)
	in_string(1:16) = "p_price_data.txt"
	open(116174, file = in_string(1:16),status="old")
	read(116174,*) p_prices
	close(116174)

	! life expectancies of a 20 year old living in 1960-2015
	in_string(1:21) = "life_expectancies.txt"
	open(116177, file = in_string(1:21),status="old")
	read(116177,*) life_expectancies
	close(116177)
		
	! health share of aggregate consumption expenditure (1960-2015)
	in_string(1:16) = "health_share.txt"
	open(11619, file = in_string(1:16),status="old")
	read(11619,*) health_share_spending
	close(11619)
	
	! capital share of all capital in health production
	in_string(1:11) = "k_share.txt"
	open(116178, file = in_string(1:11),status="old")
	read(116178,*) health_capital_share
	close(116178)
	
	! labor share of all labor in health production (by full-time equivalent employees)
	in_string(1:11) = "l_share.txt"
	open(116179, file = in_string(1:11),status="old")
	read(116179,*) health_labor_share
	close(116179)
	
	! assign to data_moments vector (for SMM)
	data_moments(:,1) = p_prices
	data_moments(:,2) = life_expectancies
	data_moments(:,3) = health_share_spending
	data_moments(:,4) = health_capital_share
	data_moments(:,5) = health_labor_share
	! project data moments 3:5 on to real line
	do im = 3,5
		do it = 1,Tobs
			data_moments(it,im) = log(1d0 / data_moments(it,im) - 1d0)
		enddo
	enddo
	
	!!!!!!!!!!!!! read in immutable parameter values !!!!!!!!!!!!!!!
	! price mark-ups 
	! horenstein and santos 2019 as base level for 1975 (table 5), then we extrapolate back and forward using identified growth rates
	in_string(1:6) = "mu.txt"
	open(116170, file = in_string(1:6),status="old")
	read(116170,*) mu_DATA
	close(116170)

	! initial twenty year olds pop (1950-2015)
	in_string(1:8) = "pop0.txt"
	open(116175, file = in_string(1:8),status="old")
	read(116175,*) pop0_DATA
	close(116175)
	
	! accidental mortality rates, five year intervals, 1950-2015 using the mid-point age (22,27,32,37, etc.)
	do ij = 1,JJ

		if(ij < 10) then
	
			in_string(1:4) = "mAcc"
			in_string(5:5) = trim(str(ij))
			in_string(6:9) = ".txt"
			open(116175+ij, file = in_string(1:9),status="old")
			read(116175+ij,*) m_acc(ij,0:13)
			close(116175+ij)
			
		else
		
			in_string(1:4) = "mAcc"
			in_string(5:6) = trim(str(ij))
			in_string(7:10) = ".txt"
			open(116175+ij, file = in_string(1:10),status="old")
			read(116175+ij,*) m_acc(ij,0:13)
			close(116175+ij)
		
		endif
	

	enddo
	! assume m_acc after period 13 are all the same
	do it = 14,TT
		m_acc(:,it) = m_acc(:,13)
	enddo
	
	! g_c (TFP FOR NON-HEALTH SECTOR USING Feenstra et al. 2015)
	in_string(1:7) = "g_c.txt"
	open(116175, file = in_string(1:7),status="old")
	read(116175,*) g_c_DATA
	close(116175)
	
	! g_h (TFP FOR HEALTH SECTOR ESTIMATED FROM EXACT IDENTIFICATION EXERCISE)
!	in_string(1:7) = "g_h.txt"
!	open(116176, file = in_string(1:7),status="old")
!	read(116176,*) g_h_DATA
!	close(116176)
	
	! gn (gross population growth of 20-year olds from UN Population Data
	in_string(1:6) = "gn.txt"
	open(1161229, file = in_string(1:6),status="old")
	read(1161229,*) gn_DATA
	close(1161229)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	subroutine to compute model moments
!		happens after terminal steady state has been
!		reached at end of each simulation
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine compute_model_moments()

	implicit none
	integer :: it,ij,im
	real*8 :: LX(JJ), LLX(JJ), TX
	
	! relative prices, first period is period t = 2
	do it = 1,Tobs
		
		! prices
		! normalize to 1960 = 1
		simulated_moments(it,1) = p(it+1) / p(2)
		
	
		! life expectancies
		LX(1) = 1d0
		do ij = 2,JJ
			LX(ij) = LX(ij-1) * s_eqm(ij-1,it+1)
			LLX(ij-1) = (LX(ij-1) + LX(ij)) / 2d0
		enddo
		TX = 0d0
		do ij = JJ-1,1,-1
			TX = TX + LLX(ij)
		enddo
		simulated_moments(it,2) = 20d0 + TX * 5d0

		! health share of aggregate expenditure (nominal)
		if((CC(it+1) + p(it+1) * HH(it+1)) > 0d0)then
			simulated_moments(it,3) = HH(it+1) * p(it+1) / &
			& (CC(it+1) + p(it+1) * HH(it+1))
		else
			simulated_moments(it,3) = 0.9999d0
		endif
		
		
		! share of capital in the production of health care
		if(KK(it+1) > 0d0) then
			simulated_moments(it,4) = KK_H(it+1) / KK(it+1)
		else
			simulated_moments(it,4) = 0.00001d0
		endif
		
		! share of labor in the production of health care
		if(LL(it+1) > 0d0) then
			simulated_moments(it,5) = LL_H(it+1) / LL(it+1)
		else
			simulated_moments(it,5) = 0.00001d0
		endif
		
		
	end do
	
	! share, price, H, and X predictions
	do it = 1,Tcutoff
		
		! share
		if((CC(it+1) + p(it+1) * HH(it+1)) > 0d0)then
			price_share_predictions(it,1) = HH(it+1) * p(it+1) / &
			& (CC(it+1) + p(it+1) * HH(it+1))
		else
			price_share_predictions(it,1) = 0.9999d0
		endif
		
		! prices
		! normalize to 1960 = 1
		price_share_predictions(it,2) = p(it+1) / p(2)
		
		! prices not normalized
		price_share_predictions(it,3) = p(it+1)
		
		! health
		price_share_predictions(it,4) = HH(it+1)
		
		! total expenditure
		price_share_predictions(it,5) = (CC(it+1) + p(it+1) * HH(it+1))
	
	end do
	
	! growth rate
	do it = 1,Tcutoff
	
		gy(it) = (YY(it) / YY(it-1)) ** (0.2d0) -1 
	
	enddo
	
	! project model moments in shares to real line, leaving price and life-expectancy alone
	do im = 3,5
		do it = 1,12
			simulated_moments(it,im) = log(1d0 / min(max(simulated_moments(it,im),0.00001d0),0.9999d0) - 1d0)
		enddo
	enddo
		
	
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	test moments manually
!
!	This does not run the GMM minimizer, 
!	... just tests and prints the moments.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine test_moments()

	implicit none
	real*8 :: fret
	
!	call tic()
	if(MC_yes)then
		call sample_parameters()
		initial_smm = .FALSE.
	else	
		if(optimal_parms)then ! optimal parms have been read in after monte carlo analysis
			initial_smm = .FALSE.
			call update_parms(initial_smm)
		else ! one time call for arbitrary parameters
			initial_smm = .TRUE.
			call update_parms(initial_smm)
		endif
	endif
	fret = parameter_eval(parms) ! initialize the model solver
	!call parameter_eval(Nparms,parms,fret)
	
	! print moments
	if(smm_verbose)then
		call print_model_moments()
	endif
!	call toc()

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	samples parameters via monte carlo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine sample_parameters()

	implicit none
	integer :: ij
	real*8 :: u
	
	do ij = 1,Nparms
	
		u = r8_uniform_01 ( MC_seed )
		parms(ij) = u * (parms_max(ij)-parms_min(ij)) + parms_min(ij)
	
	end do
	
	! pass parameters back block
	A_h0 = parms(1)
	g_h = parms(2:5)
	xi = parms(6)

!	write(*,*) 'A_h0', A_h0 ! initial A_h assuming A_c0 = 1
!	write(*,*) 'Z_0', Z_0
!	write(*,*) 'g_Z', g_Z ! growth in the non-accidentaly mortality TFP
!	write(*,*) 'chi', chi ! Hall and Jones 2007 is 26d0


end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	print model moments
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine print_model_moments()

	implicit none
	integer :: it
	
	
	write(*,*) 'PRINTING MODEL MOMENTS AND DATA MOMENTS DIRECTLY TARGETED:'
	write(*,*) 'q MODEL:'
	write(*,*) simulated_moments(:,1)
	write(*,*) 'q DATA:'
	write(*,*) data_moments(:,1)
	write(*,*) 'life expectancy MODEL:'
	write(*,*) simulated_moments(:,2)
	write(*,*) 'life expectancy DATA:'
	write(*,*) data_moments(:,2)
	write(*,*) 'health share of spending MODEL:'
	write(*,*) simulated_moments(:,3)
	write(*,*) 'health share of spending DATA:'
	write(*,*) data_moments(:,3)
	write(*,*) 'health capital share MODEL:'
	write(*,*) simulated_moments(:,4)
	write(*,*) 'health capital share DATA:'
	write(*,*) data_moments(:,4)
	write(*,*) 'health labor share MODEL:'
	write(*,*) simulated_moments(:,5)
	write(*,*) 'health labor share DATA:'
	write(*,*) data_moments(:,5)
	

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	function performing the parameter evaluation
!		over the full time series of the model
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function parameter_eval(p_in)

	implicit none
	real*8, intent(in) :: p_in(:)
	real*8 :: parameter_eval,dummy_fval(1,1)
	integer :: im,it
	
	! initial endogenous model objects
	call initialize_values()
	
	! initialize parameters
	parms = p_in ! this will get written over if initial_smm = .TRUE.
	call update_parms(initial_smm)
	
	!call tic()
	do it = 0,TT
		call solve_general_eqm(it)
	end do
	!call toc()
	
	! compute the model moments, 1960-2015
	call compute_model_moments()
	
	! compute the loss function
	do im = 1,Nmoments
        
		loss_func(im,1) = sqrt(sum((data_moments(:,im) - simulated_moments(:,im))**2d0)) &
		& / real(Tobs,kind=8)
                    
    end do 
        
	dummy_fval = matmul(transpose(loss_func),loss_func)
	!f_out = dummy_fval(1,1)
	parameter_eval = dummy_fval(1,1)
	
	! verbose SMM?
	write(*,*) 'SMM loss function:', parameter_eval
	write(*,*) 'A_h0', A_h0 ! initial A_h assuming A_c0 = 1
	write(*,*) 'g_h(1)', g_h(1)
	write(*,*) 'g_h(2)', g_h(2)
	write(*,*) 'g_h(3)', g_h(3)
	write(*,*) 'g_h(4)', g_h(4)
	write(*,*) 'xi',xi
		
	if(optimal_parms)then
	
		loss_out_demand = parameter_eval
	
	endif
	
end function

end module
