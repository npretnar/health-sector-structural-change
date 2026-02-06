module Global_Values

use toolbox

implicit none

public

	!!!!!!!!!!!!!!!! GLOBAL OBJECT PARAMETERS (FIXED) !!!!!!!!!!!!!

	! make sure grid sizes are always even numbers

	! number of periods until economy stops growing and we suppose far-off steady state convergence begins
	integer, parameter :: Tgrowth = 30
	
	! transition path cut-off for analysis
	integer, parameter :: Tcutoff = 20
	
	! number of transition periods
    integer, parameter :: TT = Tgrowth + 15! ! transition from 1950 to 2100 then simulate for another 16 periods, 80 years, thereafter
    ! this goes to 15 over Tgrowth, because we start at time 0

    ! maximum age
    integer, parameter :: JJ = 16

    ! retirement age
    integer, parameter :: JR = 10
    
    ! health grid
    integer, parameter :: NH = 30 ! for assistance
    
    ! number of gridpoints for assets available this period (this is savings with which one enters the period)
    integer, parameter :: NA = 30  
    
    ! number of endogenous cash grid points (this will be very sparse)
    integer, parameter :: NC = 31 ! 30+1
    
	! set up parameters for grid (this will be used for both the calculations of h and a_plus)
 !   real*8, parameter :: h_l = 0.001d0
 !   real*8, parameter :: h_u = 5d0   !10      !  35   
 !   real*8, parameter :: h_grow = 0.01d0    ! 0.05  0.01d0
    
    ! set up parameters for asset grid
    real*8, parameter :: a_l = 0d0
    real*8, parameter :: a_u = 10d0           !  35 sonst!    
    real*8, parameter :: a_grow = 0.01d0      ! 0.05 0.01d0 
    
    ! set up parameters for cash grid for age J agents
    real*8, parameter :: ca_l = 0.001d0
    real*8, parameter :: ca_u = 16d0           !  35 sonst!    
    real*8, parameter :: ca_grow = 0.01d0      ! 0.05 0.01d0 
    
    ! initialize parameters for utility function
    real*8, parameter :: gam = 2.0d0 !2d0 Hall and Jones 2007
 !   real*8, parameter :: xi = 44d0!40d0 !120d0 ! multiplies utility to control expenditure share !40d0, 50d0, 65d0 is a good one. !gam = 2 and xi = 40 is pretty good
    real*8, parameter :: beta = 0.983d0**5d0     
    
    ! initialize parameters
    real*8 :: Z_0 != 16.4665276309072d0 ! scale of survival function
	real*8 :: g_Z != 0.00237319117015754d0 ! growth in the non-accidentaly mortality TFP
	real*8, parameter :: chi = 191.998654371765d0 ! Hall and Jones 2007 is 26d0   
  
	! initialize fixed parameters for survival function
	real*8, parameter :: phi(JJ) = (/6.7987,6.1929,6.1929,5.3966,5.3966,&
	& 4.3935,4.3935,3.236,3.236,&
	& 2.2828,2.2828,1.416,1.416,&
	& 0.6157,0.6157,0.6157/) ! phi age profile from Hall and Jones 2007
	real*8, parameter :: theta(JJ) = (/0.15703,0.19885,0.19885,0.23727,&
	& 0.23727,0.24746,0.24746,0.17051,0.17051,0.12512,0.12512,0.10218,&
	& 0.10218,0.041686,0.041686,0.041686/) ! assign health production parameters (Hall and Jones 2007)
	    
    ! initialize parameters for production functions
    !real*8, parameter :: alpha = 0.4d0 ! Horenstein and Santos 2019
    real*8 :: alpha(0:TT), alpha_DATA(0:13) ! variable alpha from Karabarbounis and Neiman 2014
    real*8, parameter :: alpha_h = 0.26d0 ! Donahoe 2000
    real*8, parameter :: delta = 1d0 - (1d0 - 0.04d0)**5 ! 4\% annual depreciation (Krueger and Ludwig 2007)

    ! population growth, 5-year cohorts of entrants, 1990-2019
    real*8, parameter :: pop_rescale = 1d0 ! for rescaling the initial population
    
    !!!!!!!!!!!!!! GLOBAL OBJECT VARIABLES !!!!!!!!!!!!!!!!
    
    ! inverse of gam
    real*8 :: gam_inv = 1d0 / gam
    
    ! initialize individual arrays 
    ! grids
	real*8 :: a(0:NA), cash(0:NC,JJ)  !, hgrid(0:NH)
	
	! labor force efficiency by age
	real*8 :: eff(JJ)	
	
	! choices optimized in Vconsumption
	real*8 :: a_plus(0:NC, JJ, 0:TT) ! grid now
	real*8 :: h(0:NC, JJ, 0:TT)
	real*8 :: c(0:NC, JJ, 0:TT)  ! choices first, h , a+, state a, type j, period t
	real*8 :: VV(0:NC, JJ, 0:TT)
		
	! survival rate and Hall and Jones health production parameters
	real*8 :: surv(0:NC, JJ, 0:TT) ! endogenous
	real*8 :: m_acc(JJ, 0:TT) ! import
	real*8 :: zeta(JJ,0:TT) ! import
	real*8 :: pop(JJ,0:TT) ! endogenous
	real*8 :: pop0(0:TT) ! import, initial population of initial cohort, to be imported from outside of the model
	! with this value there will need to be eventual convergence to an asymptotic S.S. where there is no pop growth
	real*8 :: pop_residual(JJ,0:TT)
	
	! prices (all endogenous)
	real*8 :: p(0:TT)
	real*8 :: w(0:TT)
	real*8 :: r(0:TT)
	real*8 :: div(JJ,0:TT)
	real*8 :: beq(0:TT)
	! mark-up (exogenous)
	real*8 :: mu(0:TT)
	
	! pensions
	real*8 :: pen(JJ,0:TT),pens(0:TT)
	real*8 :: taup(0:TT)
	real*8, allocatable :: price_vector_competitive(:),price_vector_monop(:) !price_vector(4+JJ+JJ-JR+1)
	
	! eqm path variables
	real*8 :: a_eqm(JJ,0:TT), h_eqm(JJ,0:TT), c_eqm(JJ,0:TT), s_eqm(JJ,0:TT)
!	real*8 :: V_eqm(JJ,0:TT)
	
	! macroeconomic variables
	real*8 :: KK(0:TT), LL(0:TT), HH(0:TT), KK1(0:TT), PENB(0:TT), PENR(0:TT)
	real*8 :: GG(0:TT), CC(0:TT), II(0:TT), BQ(0:TT), PI_H(0:TT) !, DIFF(0:TT)
	real*8 :: YY(0:TT)
	real*8 :: KK_H(0:TT), LL_H(0:TT)
	real*8 :: KK_C(0:TT), LL_C(0:TT)
	
	! communication variables (will be declared thread private)
    integer :: ij_com, it_com !,itb_com
    integer :: it_in_com ! use for g.e. solver
    real*8 :: available, VV_interp, c_future, h_in
    logical :: check_return, check_return2

	! steady state flag variable, first solve SS then solve transition path
	logical :: iss_on, ge_verbose
	! fix the interest rate and solve
	logical :: smopec 
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PARAMETERS & MOMENTS TO GMM OVER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer, parameter :: MCsamples = 10000
	integer, parameter :: Nparms = 6 ! number of internal parameters (A_h0,g_h1,g_h2,g_h3,g_h4
	integer, parameter :: Nmoments = 5 ! steady state in 1950, estimator starts in 1960
	integer, parameter :: Tobs = 12 ! observation periods of data
	real*8 :: parms(Nparms), parms_min(Nparms), parms_max(Nparms)
	logical :: initial_smm, smm_verbose, call_minimizer
	real*8 :: data_moments(Tobs,Nmoments), simulated_moments(Tobs,Nmoments), loss_func(Nmoments,1) 
	real*8 :: gy(1:Tcutoff)
	
	! GMM LOSS FUNCTION VARIABLES IN MONTE CARLO
	logical :: MC_yes, optimal_parms, demand_calibration
	real*8 :: loss_func_out(MCsamples), parms_out(MCsamples,Nparms),loss_out_demand,g_z_Z0_out(2)
	integer :: MC_seed, MC_seed_original

	! parameters determined directly by SMM parameters and data read into the calibrator
	real*8 :: g_c,A_c(0:TT)
	real*8 :: g_c_DATA(1) ! for reading in values
	real*8 :: mu_DATA(1:13),pop0_DATA(0:13)
	real*8 :: A_h(0:TT)
	real*8 :: Z(0:TT) 
	real*8 :: gn, gn_DATA(1) ! gross population growth 5-year intervals, 20 year olds

	! tuning parameters 
	real*8, parameter :: eps = 0.001d0 ! epsilon for internal h iteration
	
	! parameters in SMM
	real*8 :: xi ! xi utility scalar
	real*8 :: A_h0 ! initial A_h assuming A_c0 = 1
	real*8 :: g_h(4) ! growth of health sector TFP
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! COUNTERFACTUAL OUTCOMES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! in order (primarily negative counterfactuals first)
	integer, parameter :: Ncounters = 6
	logical :: counterfactuals_run
	
	! counterfactual series
	real*8 :: mu_counter(0:TT,Ncounters),A_c_counter(0:TT,Ncounters)
	real*8 :: A_h_counter(0:TT,Ncounters),Z_counter(0:TT,Ncounters)
	real*8 :: pop0_counter(0:TT,Ncounters),zeta_counters(JJ,0:TT,Ncounters)
	
	! counterfactual objects for outputting
	real*8 :: p_counter(0:TT,Ncounters)
	real*8 :: life_exp_counter(0:TT,Ncounters), health_share(0:TT,Ncounters)
	real*8 :: health_cap_counter(0:TT,Ncounters)
	real*8 :: health_lab_counter(0:TT,Ncounters)
	
	! 5-year GDP growth rates
	real*8 :: gy_counter(1:Tcutoff,Ncounters)

	! for reading and writing
	character(len=25) :: in_string, out_string ! for reading from and writing to disk
	logical :: exists ! for testing whether a file has already been made
	

contains
  
  

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!
	!	str
	!
	!	Integer to string function.
	!
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	character(len = 20) function str(k)

		!   "Convert an integer to string."
		integer, intent(in) :: k

		write (str, *) k

		str = adjustl(str)

	end function str


end module Global_Values
