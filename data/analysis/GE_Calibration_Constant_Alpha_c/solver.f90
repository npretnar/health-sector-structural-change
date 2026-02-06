module solver

use toolbox
use toolbox_outer
use Global_Values

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    initialize_prices
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize_prices(it_in)

	implicit none
	integer, intent(in) :: it_in
	integer :: ij, it
	
	it = it_in

	if(iss_on)then
	
		p(it) = 1d0 
		w(it) = 1d0 
		if(smopec)then
			r(it) = (1d0 + 0.02d0)**5d0 - 1 ! about 10.4% over five years
		else
			r(it) = 0.02d0 
		endif
		beq(it) = 1d0
		if(mu(it) > 1d0)then
			div(:,it) = 0.001d0
		else	
			div(:,it) = 0d0
		endif

		! pensions
		do ij = 1,JJ

			if(ij < JR)then
				pen(ij,it) = 0d0
			else
				pen(ij,it) = 1d0
			endif

		enddo
		pens(it) = 1d0
		
	else

		! solving backwards
		p(it) = p(it-1)
		w(it) = w(it-1)
		r(it) = r(it-1)
		beq(it) = beq(it-1)
		if(mu(it) > 1d0)then
			div(:,it) = div(:,it-1)
		else	
			div(:,it) = 0d0
		endif

		! pensions
		do ij = 1,JJ

			if(ij < JR)then
				pen(ij,it) = pen(ij,it-1)
			else
				pen(ij,it) = pen(ij,it-1)
			endif

		enddo
		pens(it) = pens(it-1)

	end if
	
	! assign prices to price_vector
	if(smopec)then
	
		! competitive prices
		price_vector_competitive(1) = p(it)
		price_vector_competitive(2) = w(it) 
		price_vector_competitive(3) = beq(it) 
		price_vector_competitive(4) = pens(it)

		! monop prices
		price_vector_monop(1) = p(it)
		price_vector_monop(2) = w(it) 
		price_vector_monop(3) = beq(it) 
		do ij = 1,JJ
    
			price_vector_monop(3+ij) = div(ij,it)
			
		end do
		price_vector_monop(3+JJ+1) = pens(it)
	
	else
	
		! competitive prices
		price_vector_competitive(1) = max(min(p(it),10d0),0.00000001d0)
		price_vector_competitive(2) = max(min(w(it),10d0),0.00000001d0)
		price_vector_competitive(3) = max(min(r(it),10d0),0.00000001d0)
		price_vector_competitive(4) = max(min(beq(it),100d0),0.00000001d0)
		price_vector_competitive(5) = max(min(pens(it),100d0),0.00000001d0)

		! monop prices
		price_vector_monop(1) = max(min(p(it),10d0),0.00000001d0)
		price_vector_monop(2) = max(min(w(it),10d0),0.00000001d0)
		price_vector_monop(3) = max(min(r(it),10d0),0.00000001d0)
		price_vector_monop(4) = max(min(beq(it),100d0),0.00000001d0)
		do ij = 1,JJ
    
			price_vector_monop(4+ij) = max(min(div(ij,it),100d0),0.00000001d0)
			
		end do
		price_vector_monop(4+JJ+1) = max(min(pens(it),100d0),0.00000001d0)
	
	endif
	
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    initialize_values
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize_values()

implicit none
integer :: it

! discretize asset grid (what is chosen next perriod)
call grid_cons_Grow(a, a_l, a_u, a_grow)

! discretize cash grid for age J agent
call grid_Cons_Grow(cash(:,JJ), ca_l, ca_u, ca_grow)

! initialize age earnings process ! Hansen 1993
eff(1) = (0.78 + 0.69) / 2.00 ! dividing by two to take the average over men and women
eff(2) = (1.14 + 0.89) / 2.00
eff(3) = (1.14 + 0.89) / 2.00
eff(4) = (1.37 + 0.90) / 2.00
eff(5) = (1.37 + 0.90) / 2.00
eff(6) = (1.39 + 0.87) / 2.00
eff(7) = (1.39 + 0.87) / 2.00
eff(8) = (1.33 + 0.84) / 2.00
eff(9) = (1.33 + 0.84) / 2.00
eff(JR:JJ) = 0d0

! assign Hall and Jones 2007 health production TFPs
! note that these TFPs go out until 2100. We will assume that TFPs stop growing in 2100 allowing for a true terminal steady state to be reached.
zeta(1,0:(Tgrowth-1)) = (/0.41618,0.45431,0.49594,0.54137,0.59097,0.64512,&
& 0.70423,0.76875,0.83918,0.91607,1.000,1.0916,1.1916,1.3008,1.42,1.5501,1.6921,&
& 1.8472,2.0164,2.2011,2.4028,2.623,2.8633,3.1256,3.412,3.7246,4.0658,&
& 4.4383,4.845,5.2889/)

zeta(2,0:(Tgrowth-1)) = (/0.45372,0.49103,0.53141,0.57511,0.62241,0.67359,&
& 0.72898,0.78893,0.8538,0.92401,1.000,1.0822,1.1712,1.2675,1.3718,1.4846,1.6067,&
& 1.7388,1.8818,2.0365,2.204,2.3852,2.5814,2.7937,3.0234,3.272,3.5411,&
& 3.8323,4.1474,4.4885/)

zeta(3,0:(Tgrowth-1)) = (/0.45372,0.49103,0.53141,0.57511,0.62241,0.67359,&
& 0.72898,0.78893,0.8538,0.92401,1.000,1.0822,1.1712,1.2675,1.3718,1.4846,1.6067,&
& 1.7388,1.8818,2.0365,2.204,2.3852,2.5814,2.7937,3.0234,3.272,3.5411,&
& 3.8323,4.1474,4.4885/)

zeta(4,0:(Tgrowth-1)) = (/ 0.40857,0.44683,0.48867,0.53443,0.58447,0.6392,&
& 0.69905,0.76451,0.83609,0.91438,1.000,1.0936,1.196,1.308,1.4305,1.5645,1.711,&
& 1.8712,2.0464,2.238,2.4475,2.6767,2.9274,3.2015,3.5012,3.8291,4.1876,&
& 4.5797,5.0085,5.4775/)

zeta(5,0:(Tgrowth-1)) = (/0.40857,0.44683,0.48867,0.53443,0.58447,0.6392,&
& 0.69905,0.76451,0.83609,0.91438,1.000,1.0936,1.196,1.308,1.4305,1.5645,1.711,&
& 1.8712,2.0464,2.238,2.4475,2.6767,2.9274,3.2015,3.5012,3.8291,4.1876,&
& 4.5797,5.0085,5.4775/)

zeta(6,0:(Tgrowth-1)) = (/0.37102,0.40969,0.45239,0.49955,0.55162,0.60911,&
& 0.6726,0.74271,0.82012,0.90561,1.000,1.1042,1.2193,1.3464,1.4868,1.6417,1.8129,&
& 2.0018,2.2105,2.4409,2.6953,2.9762,3.2864,3.629,4.0072,4.4249,4.8861,&
& 5.3954,5.9578,6.5788 /)

zeta(7,0:(Tgrowth-1)) = (/0.37102,0.40969,0.45239,0.49955,0.55162,0.60911,&
& 0.6726,0.74271,0.82012,0.90561,1.000,1.1042,1.2193,1.3464,1.4868,1.6417,1.8129,&
& 2.0018,2.2105,2.4409,2.6953,2.9762,3.2864,3.629,4.0072,4.4249,4.8861,&
& 5.3954,5.9578,6.5788 /)

zeta(8,0:(Tgrowth-1)) = (/0.29524,0.33355,0.37682,0.42572,0.48095,0.54336,&
& 0.61386,0.69351,0.78349,0.88515,1.000,1.1298,1.2763,1.4419,1.629,1.8404,2.0792,&
& 2.349,2.6538,2.9981,3.3871,3.8266,4.3231,4.884,5.5177,6.2336,7.0425,&
& 7.9562,8.9886,10.155/)

zeta(9,0:(Tgrowth-1)) = (/0.29524,0.33355,0.37682,0.42572,0.48095,0.54336,&
& 0.61386,0.69351,0.78349,0.88515,1.000,1.1298,1.2763,1.4419,1.629,1.8404,2.0792,&
& 2.349,2.6538,2.9981,3.3871,3.8266,4.3231,4.884,5.5177,6.2336,7.0425,&
& 7.9562,8.9886,10.155/)

zeta(10,0:(Tgrowth-1)) = (/0.24656,0.28362,0.32624,0.37527,0.43167,0.49655,&
& 0.57117,0.65702,0.75576,0.86935,1.000,1.1503,1.3232,1.522,1.7508,2.0139,2.3166,&
& 2.6647,3.0652,3.5259,4.0558,4.6654,5.3665,6.1731,7.1008,8.168,9.3956,&
& 10.808,12.432,14.3 /)

zeta(11,0:(Tgrowth-1)) = (/0.24656,0.28362,0.32624,0.37527,0.43167,0.49655,&
& 0.57117,0.65702,0.75576,0.86935,1.000,1.1503,1.3232,1.522,1.7508,2.0139,2.3166,&
& 2.6647,3.0652,3.5259,4.0558,4.6654,5.3665,6.1731,7.1008,8.168,9.3956,&
& 10.808,12.432,14.3/)

zeta(12,0:(Tgrowth-1)) = (/0.22285,0.25895,0.30089,0.34963,0.40626,0.47207,&
& 0.54853,0.63739,0.74063,0.8606,1.000,1.162,1.3502,1.5689,1.823,2.1183,2.4615,&
& 2.8602,3.3235,3.8618,4.4874,5.2142,6.0588,7.0402,8.1806,9.5057,11.045,&
& 12.835,14.914,17.329/)

zeta(13,0:(Tgrowth-1)) = (/ 0.22285,0.25895,0.30089,0.34963,0.40626,0.47207,&
& 0.54853,0.63739,0.74063,0.8606,1.000,1.162,1.3502,1.5689,1.823,2.1183,2.4615,&
& 2.8602,3.3235,3.8618,4.4874,5.2142,6.0588,7.0402,8.1806,9.5057,11.045,&
& 12.835,14.914,17.329 /)

zeta(14,0:(Tgrowth-1)) = (/0.22285,0.25895,0.30089,0.34963,0.40626,0.47207,&
& 0.54853,0.63739,0.74063,0.8606,1.000,1.162,1.3502,1.5689,1.823,2.1183,2.4615,&
& 2.8602,3.3235,3.8618,4.4874,5.2142,6.0588,7.0402,8.1806,9.5057,11.045,&
& 12.835,14.914,17.329 /)

zeta(15,0:(Tgrowth-1)) = (/0.22285,0.25895,0.30089,0.34963,0.40626,0.47207,&
& 0.54853,0.63739,0.74063,0.8606,1.000,1.162,1.3502,1.5689,1.823,2.1183,2.4615,&
& 2.8602,3.3235,3.8618,4.4874,5.2142,6.0588,7.0402,8.1806,9.5057,11.045,&
& 12.835,14.914,17.329/)

zeta(16,0:(Tgrowth-1)) = (/0.22285,0.25895,0.30089,0.34963,0.40626,0.47207,&
& 0.54853,0.63739,0.74063,0.8606,1.000,1.162,1.3502,1.5689,1.823,2.1183,2.4615,&
& 2.8602,3.3235,3.8618,4.4874,5.2142,6.0588,7.0402,8.1806,9.5057,11.045,&
& 12.835,14.914,17.329 /)

! make zetas after Tgrowth -1 all the same
do it = Tgrowth,TT

	zeta(:,it) = zeta(:,(Tgrowth-1))

end do

!!!!!!!!! INITIALIZE Z'S !!!!!!!!!!
! Z's for rescaling phi assuming that phi is some fixed value
!g_Z = 0.00237319117015754d0
Z = Z_0
! Assume Z follows the growth path until Tgrowth
do it = 1,(Tgrowth-1)
	
	Z(it) = Z(it-1) * (1d0 + g_Z)
	
enddo
Z(Tgrowth:TT) = Z(Tgrowth-1)

!!!!!!!! INITIALIZE MUS !!!!!!!!!!
mu(1:13) = mu_DATA
mu(0) = mu(1) ! assume 1950 is the same as 1955
mu(14:TT) = mu(13)

!!!!!!!! INITIALIZE POPULATION OF ENTRANTS !!!!!!!!
pop0(0:13) = pop0_DATA
gn = gn_DATA(1)
! inflate initial population level out to 2100, using average rate for growth from 1990-2019
! this is probably a generous growth rate, and we will want to consider alternatives later on
do it = 14,(Tgrowth-1)

	pop0(it) = gn * pop0(it-1)

end do
! after 2100, initial population is stable and constant
do it = Tgrowth,TT

	pop0(it) = pop0(Tgrowth-1)

end do
pop0 = pop0 * pop_rescale

!!!!!!!! INITIALIZE PRODUCTIVITIES !!!!!!!!
g_c = g_c_DATA(1) ! growth in multi-factor productivity by 5-year average

! After Tgrowth it is constant until TT
A_c = 1d0
do it = 1,(Tgrowth-1)
	
	A_c(it) = A_c(it-1) * (1d0 + g_c)
	
enddo
! After Tgrowth it is constant until TT
A_c(Tgrowth:TT) = A_c(Tgrowth-1)
	
! Assume A_h follows the growth path until Tgrowth
A_h = A_h0
!write(*,*) 'Enter 1955 to 2015 A_h in order:'
!do it = 1,(Tgrowth - 1)
!	write(*,*) it
!	read(*,*) A_h(it)
!end do
do it = 1,4 !(Tgrowth-1)
	
	A_h(it) = A_h(it-1) * (1d0 + g_h(1)) 
	
enddo
do it = 5,6 !(Tgrowth-1)
	
	A_h(it) = A_h(it-1) * (1d0 + g_h(2)) 
	
enddo
do it = 7,9 !(Tgrowth-1)
	
	A_h(it) = A_h(it-1) * (1d0 + g_h(3)) 
	
enddo
do it = 10,(Tgrowth-1)
	
	A_h(it) = A_h(it-1) * (1d0 + g_h(4)) 
	
enddo
! After Tgrowth it is constant until TT
A_h(Tgrowth:TT) = A_h(Tgrowth-1)

! initialize policy functions
a_plus = 0.02d0; 
c = 0.2d0 !; omega = 0.01625d0
VV = chi 
h = 1d0
surv = 0.99d0
surv(:, JJ, :) = 0d0 ! nobody survives from J to J+1
pop = 1d0

! initialize tax for social security, same for all years for now
taup = 0.086d0 ! S.S. tax rate, 1990-2000 average Ventura, Guner 

! initialize aggregate variables
CC = 0d0; LL = 0d0; HH = 0d0 ; LL_H = 0d0; KK_H = 0d0
KK = 0d0; II = 0d0 ; LL_C = 0d0; KK_C = 0d0
KK1 = 0d0; PENB = 0d0; PENR = 0d0; PI_H = 0d0; YY = 0d0
BQ = 0d0; GG =0d0

! initialize eqm series
a_eqm = 2d0; h_eqm = 1d0; c_eqm = 1d0; s_eqm = 0.8d0

! population residual (for tuning)
pop_residual = 0d0

end subroutine ! initial values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! 	prices 
!	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine prices(it)  
        
	integer, intent(in) :: it
	integer :: ij
	real*8 :: dist(JJ) ! internal use only to compute the asset distribution by cohort
			
	if(smopec)then
	
		! normalize aggregate consumption TFP
		w(it) = A_c(it) * (1d0 - alpha)*(max(KK_C(it),0.0000001d0) &
		& /max(LL_C(it),0.0000001d0))**alpha
	!	r(it) = A_c(it) * alpha *(KK_C(it)/LL_C(it))**(alpha - 1d0)
	
		! consumption + investment price is equal to 1, health price is marginal cost times markup
		p(it) = mu(it) * (1d0/A_h(it)) * (r(it)/alpha_h)**alpha_h &
		& * (w(it)/(1d0-alpha_h))**(1d0-alpha_h) ! price equals markup over marginal cost
        
		! bequests
		beq(it) = BQ(it) / sum(pop(:,it))
        
		! pensions
	!	write(*,*) 'pop retired:',sum(pop(JR:JJ,it))
		if(sum(pop(JR:JJ,it)) > 0d0)then
			pens(it) = PENB(it) / sum(pop(JR:JJ,it))
		endif
		do ij = JR,JJ
		
			if(sum(pop(JR:JJ,it)) > 0d0)then
				pen(ij,it) = pens(it)
			else
				pen(ij,it) = 0d0
			endif
		
		enddo
        
		! dividends
		if(mu(it) > 1d0)then
	
			do ij = 1,JJ
		
				dist(jj) =  (a_eqm(ij, it) + beq(it)) / KK(it) 
				div(ij,it) = PI_H(it) * dist(jj)
		
			end do
	        	
		else
    
			div(:,it) = 0d0
		
		endif
	
	else
		! normalize aggregate consumption TFP
		w(it) = A_c(it) * (1d0 - alpha)*(max(KK_C(it),0.000000001d0) &
		& /max(LL_C(it),0.000000001d0))**alpha
		r(it) = A_c(it) * alpha *(max(KK_C(it),0.000000001d0) &
		& /max(LL_C(it),0.000000001d0))**(alpha - 1d0)
	
		! consumption + investment price is equal to 1, health price is marginal cost times markup
		p(it) = mu(it) * (1d0/A_h(it)) * (r(it)/alpha_h)**alpha_h &
		& * (w(it)/(1d0-alpha_h))**(1d0-alpha_h) ! price equals markup over marginal cost
        
		! bequests
		beq(it) = BQ(it) / sum(pop(:,it))
        
		! pensions
		if(sum(pop(JR:JJ,it)) > 0.0000001d0)then
			pens(it) = PENB(it) / sum(pop(JR:JJ,it))
		else
			pens(it) = 0d0
		endif
		do ij = JR,JJ
		
			pen(ij,it) = pens(it)
		
		enddo
        
		! dividends
		if(mu(it) > 1d0)then
	
			do ij = 1,JJ
		
				dist(jj) =  (a_eqm(ij, it) + beq(it)) / max(KK(it),0.000000001d0)
				div(ij,it) = PI_H(it) * dist(jj)
		
			end do
	        	
		else
    
			div(:,it) = 0d0
		
		endif
		
	endif
	
	! impose boundary conditions on prices
	p(it) = min(p(it),10000d0)
	p(it) = max(p(it),0.00000000000001d0)
	w(it) = min(w(it),10000d0)
	w(it) = max(w(it),0.00000000000001d0)
	if(.NOT. smopec)then
		r(it) = min(r(it),10000d0)
		r(it) = max(r(it),0.00000000000001d0)
	endif
	beq(it) = min(beq(it),10000d0)
	beq(it) = max(beq(it),0.00000000000001d0)
	! give the pensions more room
	pens(it) = min(pens(it),10000d0)
	pens(it) = max(pens(it),0.00000000000001d0)

end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!		population_distribution
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine population_distribution(it_in)

	integer, intent(in) :: it_in
	integer :: ij,it

	it = it_in

	! COMPUTE THE STEADY STATE DISTRIBUTION, OTHERWISE ITERATE FORWARD
	if( (it_in .EQ. 0 .OR. it_in .EQ. TT) .AND. iss_on) then
	
		pop(1,it) = pop0(0)
	
		! cohort population as a percentage of population of first generation
		do ij = 2,JJ
	
			pop(ij,it) = pop(ij-1,it) * s_eqm(ij-1, it) + pop_residual(ij-1, it)
	
		enddo
		
	else ! ITERATE FORWARD
	
		pop(1,it) = pop0(it) ! eventually stops changing, exogenously imposed
	
		! cohort population this period given last period's population
		do ij = 2,JJ
		
			pop(ij,it) = pop(ij-1,it-1) * s_eqm(ij-1,it-1) + pop_residual(ij-1,it)
		
		enddo
	
	endif	
	
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	aggregation
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine aggregation(it_in)
	
	integer, intent(in) :: it_in
	integer :: it,ij
	real*8 :: marginal_cost 
	real*8 :: MC_h, MC_c 
	
	it = it_in
	
	CC(it) = 0d0; LL(it) = 0d0; HH(it) = 0d0; LL_H(it) = 0d0; KK_H(it) = 0d0
	KK(it) = 0d0; II(it) = 0d0; LL_C(it) = 0d0; KK_C(it) = 0d0
	KK1(it) = 0d0; PENB(it) = 0d0; BQ(it) = 0d0; PI_H(it) = 0d0
	PENR(it) = 0d0; YY(it) = 0d0; GG(it) = 0d0
	
	do ij = 1,JJ
	
		! labor supply when labor is inelastic (in efficiency units weighted by cohort population)
		LL(it) = LL(it) + pop(ij,it) * eff(ij)
		
		! capital today
		KK(it) = KK(it) + pop(ij,it) * (a_eqm(ij, it) + beq(it))
		
		if(it .EQ. 0 .AND. iss_on)then
			! capital tomorrow
			KK1(it) = KK1(it) + pop(ij,it) * a_eqm(ij+1,it)
			! per-capita bequests made before the next period starts
			BQ(it) = BQ(it) + (1d0-s_eqm(ij,it)) * a_eqm(ij+1,it) * pop(ij,it)
		else
			! capital tomorrow
			KK1(it) = KK1(it) + pop(ij,it) * a_eqm(ij+1,it+1)
			! per-capita bequests made before the next period starts
			BQ(it) = BQ(it) + (1d0-s_eqm(ij,it)) * a_eqm(ij+1,it+1) * pop(ij,it)
		end if	
		
		! health
		HH(it) = HH(it) + pop(ij,it) * h_eqm(ij,it)
		
		! consumption
		CC(it) = CC(it) + pop(ij,it) * c_eqm(ij,it)
		
		! pension payments made
		PENB(it) = PENB(it) + pop(ij,it) * w(it) * eff(ij) * taup(it)
		
		! pension receipts
		PENR(it) = PENR(it) + pop(ij,it) * pen(ij,it)
		
	enddo
	
	! aggregation and general equilibrium diffs
	if(it_in .EQ.0 .AND. iss_on)then
		II(it) = delta * KK(it) 
	else
		II(it) = KK1(it) - (1d0 - delta) * KK(it)
	endif
	
	! labor and capital by sector
	! marginal costs
	MC_c = (1d0/A_c(it)) * (r(it)/alpha)**alpha * (w(it)/(1d0-alpha))**(1d0-alpha)
	MC_h = (1d0/A_h(it)) * (r(it)/alpha_h)**alpha_h *(w(it)/(1d0-alpha_h))**(1d0-alpha_h)
   
	! capital split
	if((MC_c * alpha * (CC(it) + II(it)) + MC_h * alpha_h * HH(it)) > 0d0)then
		KK_C(it) = MC_c * alpha * (CC(it) + II(it)) &
		& / (MC_c * alpha * (CC(it) + II(it)) + MC_h * alpha_h * HH(it)) * KK(it)
	else
		KK_C(it) = 0d0
	endif
	KK_H(it) = KK(it) - KK_C(it)

    ! labor split
    if((MC_c * (1d0-alpha) * (CC(it) + II(it)) + MC_h * (1d0-alpha_h) * HH(it)) > 0d0)then
		LL_C(it) = MC_c * (1d0-alpha) * (CC(it) + II(it)) &
		& / (MC_c * (1d0-alpha) * (CC(it) + II(it)) + MC_h * (1d0-alpha_h) * HH(it)) * LL(it)
	else
		LL_C(it) = 0d0
	endif
	LL_H(it) = LL(it) - LL_C(it)
    	
    ! health sector profits (only update if profits occur)
    if(mu(it) > 1d0)then
		
		marginal_cost = (1d0/A_h(it)) * (r(it)/alpha_h)**alpha_h * (w(it)/(1d0-alpha_h))**(1d0-alpha_h)
		PI_H(it) = mu(it)* marginal_cost * HH(it) - marginal_cost * HH(it)

		
	endif
	
	! gov't
	GG(it) = PENB(it) - PENR(it)

	! output
!	YY(it) = CC(it) + II(it) + HH(it)
	YY(it) = A_c(it)*max(KK_C(it),0.0000001d0)**alpha*max(LL_C(it),0.0000001d0)**(1d0-alpha) &
	& + A_h(it)*max(KK_H(it),0.0000001d0)**alpha_h*max(LL_H(it),0.0000001d0)**(1d0-alpha_h)
!	YY(it) = damp * Yold + (1d0 - damp) * YY(it)
    
	
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   solve_household
!
!	solves the h-problem with logs
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine solve_household(it_in)

	!!!! solves for optimal policy functions !!!!
	integer, intent(in) :: it_in
    integer :: it, ij, ia, ic, itb
    real*8 :: h_internal
    
    it = it_in
	
	! last period of life
	do ic = 0, NC ! loop over assets you have
				
		c(ic,JJ,it) =  cash(ic,JJ) 
		if( c(ic,JJ,it) > 0d0)then
				if(gam .NE. 1d0)then
					VV(ic,JJ,it) = chi + xi * c(ic,JJ,it)**(1d0 - gam) / (1d0 - gam)
				else
					VV(ic,JJ,it) = chi + xi * log(c(ic,JJ,it)) 
				endif
		else
			VV(ic,JJ,it) = 0d0
		endif
		if(VV(ic,JJ,it) < 0d0)then
			VV(ic,JJ,it) = 0d0
		endif
		
		h(:,JJ,it) = 0d0
		a_plus(:,JJ,it) = 0d0
		surv(:, JJ, it) = 0d0
				
	enddo

	! cohort do loop over ages
	do ij = JJ-1, 1 ,-1
		
	!	it = year(it_in,ij)
	!	itb = year(it_in,ij+1)
		
		! get passed to the solver
		!if(iss_on)then
			!itb = it
		!else
			!itb = min(it+1,TT)
		!endif
		ij_com = ij; it_com = it !; itb_com = itb
		
		! bottom of the grid
		c(0,ij,it) = ca_l ! >0 
		h(0,ij,it) = 0d0
		a_plus(0,ij,it) = 0d0
		surv(0, ij, it) = 0d0
		
		do ia = 0,NA
			
			! next period available
			available = w(it)*eff(ij+1)*(1d0-taup(it)) &
				+ (1d0+r(it) - delta)  &
				* (a(ia) + beq(it)) &
				+ pen(ij+1,it) &
				+ div(ij+1,it)
			
	!		write(*,*) 'ia:',ia,'ij:',ij,'avail:',available
	!		write(*,*) 'cash(:,ij+1):',cash(:,ij+1)
	!		write(*,*) 'c(:,ij+1,it):',c(:,ij+1,it)
				
			! compute interpolated c_future
			if(available < ca_l) then
				c_future = ca_l
			else
				if(available > maxval(cash(:,ij+1)))then
					c_future = c(NC,ij+1,it)
				!	available = maxval(cash(:,ij+1))
				!	c_future = linint_Gen(available, cash(:,ij+1), &
				!	& c(:,ij+1,it))
				else
					!write(*,*) 'avail:',available
					!write(*,*) 'cash:',cash(:,ij+1)
					!write(*,*) 'c(:,ij+1,it):',c(:,ij+1,it)
					c_future = linint_Gen(available, cash(:,ij+1), &
					& c(:,ij+1,it),5)
				endif
			endif
				
			! compute interpolated future value
			VV_interp = linint_Gen(available, cash(:,ij+1), &
				& VV(:,ij+1,it),5)
			if(VV_interp < 0d0)then
				VV_interp = 0d0
			endif
			
    
			if(VV_interp > 0d0 .AND. c_future > 0d0)then
			
				! get implied h(a+) at every a+ choice
				! initialize h
				!h_in = Log(0.1d0 * available)
				
				!write(*,*) 'c_future:',c_future,'VV_interp:',VV_interp
				!write(*,*) 'h_in:', h_in
				!write(*,*) 'thing_inside:',phi(ij_com) * (zeta(ij_com,it_com) * Exp(h_in)) &
				!& **theta(ij_com) - &
				!& m_acc(ij_com,it_com) * phi(ij_com) * &
				!& (zeta(ij_com,it_com) * Exp(h_in))**theta(ij_com) - 1d0
		
				!call fzero(h_in, h_log_zero, check_return)
				
			!	write(*,*) 'ij:',ij,'ia:',ia,'return_status:',check_return
			!	write(*,*) 'c_future:',c_future,'VV_interp:',VV_interp
				!h(ia+1,ij,it) = Exp(h_in)
				
				! h(V+,c+) (initial approximation)
				h(ia+1,ij,it) = (theta(ij) * VV_interp * c_future ** gam/ &
				& (xi * Z(it)* phi(ij) * zeta(ij, it)** theta(ij) &
				& * (1d0+r(it_com)-delta)))**(1d0/(theta(ij) + 1d0))

				! survival rate
				if(h(ia+1,ij,it) > 0d0)then
					surv(ia+1, ij, it) = 1d0 - m_acc(ij,it) - 1d0 & 
					& / (Z(it) * phi(ij) * (zeta(ij, it)*h(ia+1,ij,it)) ** theta(ij))
				else
					surv(ia+1, ij, it) = 0d0
					h(ia+1,ij,it) = 0d0
				end if
			
				! check if survival is in (0,1)
				surv(ia+1, ij, it) = max(surv(ia+1, ij, it),0d0)
				surv(ia+1, ij, it) = min(surv(ia+1, ij, it),1d0)
				
				! compute h_internal for iteration (we do this so we don't have to call the solver, which is finnicky)
				h_internal = (theta(ij)*VV_interp*c_future**gam*surv(ia+1,ij,it)/ &
				& (p(it) * xi * Z(it)* phi(ij) * zeta(ij, it)** theta(ij) &
				& * (1d0+r(it_com)-delta)))**(1d0/(theta(ij) + 1d0))
				
				do while (abs(h_internal - h(ia+1,ij,it)) > eps)
				
					h(ia+1,ij,it) = h_internal
				
					! survival rate
					if(h_internal > 0d0)then
						surv(ia+1, ij, it) = 1d0 - m_acc(ij,it) - 1d0 & 
						& / (Z(it) * phi(ij) * (zeta(ij, it)*h_internal) ** theta(ij))
					else
						surv(ia+1, ij, it) = 0d0
						h_internal = 0d0
					end if
			
					! check if survival is in (0,1)
					surv(ia+1, ij, it) = max(surv(ia+1, ij, it),0d0)
					surv(ia+1, ij, it) = min(surv(ia+1, ij, it),1d0)
				
					! compute h_internal
					h_internal = (theta(ij)*VV_interp*c_future**gam*surv(ia+1,ij,it)/ &
					& (p(it) * xi * Z(it)* phi(ij) * zeta(ij, it)** theta(ij) &
					& * (1d0+r(it_com)-delta)))**(1d0/(theta(ij) + 1d0))
				
				end do
				! assign h
				h(ia+1,ij,it) = h_internal
		
				! c
				if(surv(ia+1,ij,it) > 0d0)then
					c(ia+1,ij,it) = c_future / &
					& ((beta * surv(ia+1, ij, it) * (1d0+r(it) - delta)) &
					& ** gam_inv)
				else
					c(ia+1,ij,it) = available
				endif
				if(c(ia+1,ij,it) < ca_l) then
					c(ia+1,ij,it) = ca_l
				endif
			
				! calculate endogenous grid
				cash(ia+1,ij) = a(ia) + c(ia+1,ij,it) + p(it) * h(ia+1,ij,it)
			
				! get aplus
				a_plus(ia+1,ij,it) = a(ia)
				
			else
			
				h(ia+1,ij,it) = 0d0
				surv(ia+1, ij, it) = 0d0
				c(ia+1,ij,it) = available
				a_plus(ia+1,ij,it) = 0d0
				
				! calculate endogenous grid
				cash(ia+1,ij) = c(ia+1,ij,it)
			
			endif
			
		enddo
		
		! check that grid is monotone
		if(ca_l .GE. cash(1,ij)) then
			cash(0,ij) = max(cash(1,ij) - 0.0000000001d0,0d0)
		else
			cash(0,ij) = ca_l
		endif
		
		! update value function
		do ic  = 0,NC
		
			! next period available
			available = w(it)*eff(ij+1)*(1d0-taup(it)) &
				+ (1d0+r(it) - delta)  &
				* (a_plus(ic,ij,it) + beq(it)) &
				+ pen(ij+1,it) &
				+ div(ij+1,it)
			
			! compute interpolated future value
			VV_interp = linint_Gen(available, cash(:,ij+1), &
				& VV(:,ij+1,it),5)
			if(VV_interp < 0d0)then
				VV_interp = 0d0
			endif
				
!			write(*,*) 'vv_interp:',VV_interp,'surv:',surv(ic, ij, it),'c(ic,ij,it):',c(ic,ij,it)
			
			! compute new value function
			if(c(ic,ij,it) >0d0)then
				if(gam .NE. 1d0)then
					VV(ic,ij,it) = chi + xi * c(ic,ij,it)**(1d0 - gam) / (1d0 - gam) &
					& + beta * surv(ic, ij, it) * VV_interp
				else
					VV(ic,ij,it) = chi + xi * log(c(ic,ij,it)) &
					& + beta * surv(ic, ij, it) * VV_interp
				endif
			else
				VV(ic,ij,it) = 0d0
			endif
			if(VV(ic,ij,it) < 0d0)then
				VV(ic,ij,it) = 0d0
			endif
				
		end do
		
	end do
	
end subroutine 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!		solve_general_eqm
!	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine solve_general_eqm(it_in)

	implicit none
	integer, intent(in) :: it_in
	integer :: it
	
	it_in_com = it_in
	it = it_in
	if(it .EQ. 0 .OR. it .EQ. TT) then
		iss_on = .TRUE. ! start with the steady state solver on
	else
		iss_on = .FALSE. ! don't solve steady state
	end if
			
	! initialize prices
	!call tic()
	call initialize_prices(it)
	
	if(mu(it) > 1d0)then
		call fzero_outer(price_vector_monop, ge_zero_monop, check_return2)
	else
		call fzero_outer(price_vector_competitive, ge_zero_competitive, check_return2)
	endif
	!call toc()
			
	if(ge_verbose)then
	
		if(it .GT. 0 .AND. it .LE. Tcutoff)then	
		
			write(*,*) 'Transition path period #:',it,'G.E. status:',check_return2
			write(*,*) 'Period:', it
			write(*,*) 'Prices:'
			write(*,*) 'r:',r(it)
			write(*,*) 'p:',p(it)
			write(*,*) 'w:',w(it)
			write(*,*) 'beq:',beq(it)
			write(*,*) 'pens:',pens(it)
			write(*,*) 'Aggregates:'
			write(*,*) 'CC:', CC(it)
			write(*,*) 'HH:', HH(it)
			write(*,*) 'KK:', KK(it)
			write(*,*) 'LL:', LL(it)
			write(*,*) 'KK/YY:', KK(it) / max(YY(it),0.000000001d0)
			
		endif
	endif
	
end subroutine 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!		solve_transition
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine solve_transition()

	implicit none
	integer :: it
	
	! first solve for initial and final steady states
	! initial
	call solve_general_eqm(0)
	! terminal
	call solve_general_eqm(TT)
	
	! solve forwards using MIT shock to prices
	do it = 1,TT
	
		call solve_general_eqm(it)
	
	end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!		function for finding a zero of the G.E.
!			using Broyden's Method
!
!		a la Krueger and Ludwig 2007
!
!		Find the G.E. eq'm when health markets are competitive
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function ge_zero_competitive(x_in)
    
	implicit none
    real*8, intent(in) :: x_in(:)
    real*8 :: ge_zero_competitive(size(x_in, 1)), x_update(size(x_in, 1))
    integer :: it, ij
    
    it = it_in_com
    
    if(smopec)then
    
		p(it) = sqrt(x_in(1)**2d0)
		w(it) = sqrt(x_in(2)**2d0)
		!r(it) = x_in(3)		
		beq(it) = sqrt(x_in(3)**2d0)
	!	do ij = 1,JJ
    
	!		if(mu(it) > 1d0)then
	!			div(ij,it) = x_in(3+ij)
	!		else	
	!			div(ij,it) = 0d0
	!		endif
	
	!	end do
		do ij = 1,(JJ-JR+1)
	
			pen(ij+JR-1,it) = sqrt(x_in(4)**2d0)
	
		end do
	
		! solve household
		call solve_household(it)
		
		! get eqm decision path
		call optimal_decision_path(it)
		
		! compute population distribution
		call population_distribution(it)
	
		! aggregate
		call aggregation(it)
		
		! price update
		call prices(it)
	
		! updated prices
		x_update(1) = p(it)
		x_update(2) = w(it)
	!	x_update(3) = log(r(it))
	!	if(beq(it) > 0d0)then
		x_update(3) = beq(it)
		!else
		!	x_update(3) = -10000000000d0
		!endif
		!x_update(1:4) = log(x_update(1:4))
	!	do ij = 1,JJ
    
	!		if(mu(it) > 1d0)then
	!			x_update(3+ij) = div(ij,it) !log(div(ij,it))
	!		else	
	!			x_update(3+ij) = 0d0 !-1000000000d0
	!		endif
	
	!	end do
		!do ij = 1,(JJ-JR+1)
	
		if(pens(it) < ca_u)then
			x_update(4) = pens(it) !log(pen(ij+JR-1,it))
		else
			x_update(4) = ca_u
		endif
	
	!	end do
	
		! function evaluation
		ge_zero_competitive = x_in - x_update
    
    else
		
		p(it) = sqrt(x_in(1)**2d0)
		w(it) = sqrt(x_in(2)**2d0)
		r(it) = sqrt(x_in(3)**2d0)
		beq(it) = sqrt(x_in(4)**2d0)
	!	do ij = 1,JJ
    
	!		if(mu(it) > 1d0)then
	!			div(ij,it) = x_in(4+ij)
	!		else	
	!			div(ij,it) = 0d0
	!		endif
	
	!	end do
		do ij = 1,(JJ-JR+1)
	
			pen(ij+JR-1,it) = sqrt(x_in(5)**2d0)
	
		end do
	
		! solve household
		call solve_household(it)
		
		! get eqm decision path
		call optimal_decision_path(it)
		
		! compute population distribution
		call population_distribution(it)
	
		! aggregate
		call aggregation(it)
		
		! price update
		call prices(it)
	
		! updated prices
		x_update(1) = p(it)
		x_update(2) = w(it)
		x_update(3) = r(it)
		!if(beq(it) > 0d0)then
		x_update(4) = beq(it)
	!	else
	!		x_update(4) = -10000000000d0
	!	endif		!x_update(1:4) = log(x_update(1:4))
	!	do ij = 1,JJ
    
	!		if(mu(it) > 1d0)then
	!			x_update(4+ij) = div(ij,it) !log(div(ij,it))
	!		else	
	!			x_update(4+ij) = 0d0 !-1000000000d0
	!		endif
	
	!	end do
		if(pens(it) < ca_u)then
			x_update(5) = pens(it) !log(pen(ij+JR-1,it))
		else
			x_update(5) = ca_u
		endif
	
		! function evaluation
		ge_zero_competitive = x_in - x_update
	
	endif
    
    
end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!		function for finding a zero of the G.E.
!			using Broyden's Method
!
!		a la Krueger and Ludwig 2007
!
!		Find the G.E. eq'm when health markets are monopolistic
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function ge_zero_monop(x_in)
    
	implicit none
    real*8, intent(in) :: x_in(:)
    real*8 :: ge_zero_monop(size(x_in, 1)), x_update(size(x_in, 1))
    integer :: it, ij
    
    it = it_in_com
    
    if(smopec)then
    
		p(it) = sqrt(x_in(1)**2d0)
		w(it) = sqrt(x_in(2)**2d0)
		!r(it) = x_in(3)		
		beq(it) = sqrt(x_in(3)**2d0)
		do ij = 1,JJ
    
			div(ij,it) = sqrt(x_in(3+ij)**2d0)
			
		end do
		do ij = 1,(JJ-JR+1)
	
			pen(ij+JR-1,it) = sqrt(x_in(3+JJ+1)**2d0)
	
		end do
	
		! solve household
		call solve_household(it)
		
		! get eqm decision path
		call optimal_decision_path(it)
		
		! compute population distribution
		call population_distribution(it)
	
		! aggregate
		call aggregation(it)
		
		! price update
		call prices(it)
	
		! updated prices
		x_update(1) = p(it)
		x_update(2) = w(it)
	!	x_update(3) = log(r(it))
	!	if(beq(it) > 0d0)then
		x_update(3) = beq(it)
		do ij = 1,JJ
    
			x_update(3+ij) = div(ij,it) != sqrt(x_in(3+ij)**2d0)
			
		end do
	
		if(pens(it) < ca_u)then
			x_update(3+JJ+1) = pens(it) !log(pen(ij+JR-1,it))
		else
			x_update(3+JJ+1) = ca_u
		endif
	
	!	end do
	
		! function evaluation
		ge_zero_monop = x_in - x_update
    
    else
		
		p(it) = sqrt(x_in(1)**2d0)
		w(it) = sqrt(x_in(2)**2d0)
		r(it) = sqrt(x_in(3)**2d0)
		beq(it) = sqrt(x_in(4)**2d0)
		do ij = 1,JJ
    
			div(ij,it) = sqrt(x_in(4+ij)**2d0)
			
		end do
		do ij = 1,(JJ-JR+1)
	
			pen(ij+JR-1,it) = sqrt(x_in(4+JJ+1)**2d0)
	
		end do
	
		! solve household
		call solve_household(it)
		
		! get eqm decision path
		call optimal_decision_path(it)
		
		! compute population distribution
		call population_distribution(it)
	
		! aggregate
		call aggregation(it)
		
		! price update
		call prices(it)
	
		! updated prices
		x_update(1) = max(min(p(it),10d0),0.00000001d0)
		x_update(2) = max(min(w(it),10d0),0.00000001d0)
		x_update(3) = max(min(r(it),10d0),0.00000001d0)
		!if(beq(it) > 0d0)then
		x_update(4) = max(min(beq(it),100d0),0.00000001d0)
		do ij = 1,JJ
    
			x_update(4+ij) = max(min(div(ij,it),100d0),0.00000001d0) != sqrt(x_in(3+ij)**2d0)
			
		end do
		if(pens(it) < ca_u)then
			x_update(4+JJ+1) = max(min(pens(it),100d0),0.00000001d0) !log(pen(ij+JR-1,it))
		else
			x_update(4+JJ+1) = ca_u
		endif
	
		! function evaluation
		ge_zero_monop = x_in - x_update
	
	endif
    
    
end function
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!		optimal_decision_path
!	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine optimal_decision_path(it_in)

    implicit none
    integer, intent(in) :: it_in 
    integer :: ij, it !, itp
	
	! first period of life assets
	a_eqm(1,it_in) = 0d0 ! everyone starts with nothing except what they inherit
	
	it = it_in
	
	if(iss_on)then
		
		do ij = 1,JJ-1
			
			! available cash
		available = w(it) * eff(ij) * (1d0-taup(it)) + &
			& (1d0 + r(it) - delta) &
			& * (a_eqm(ij,it) + beq(it)) &
			& + pen(ij, it) &
			& + div(ij,it)
		! consumption and assets
		c_eqm(ij,it) = linint_Gen(available, cash(:,ij), &
				& c(:,ij,it),5)
		h_eqm(ij,it) = linint_Gen(available, cash(:,ij), &
				& h(:,ij,it),5)
		s_eqm(ij,it) = linint_Gen(available, cash(:,ij), &
				& surv(:,ij,it),5)
		
		! steady state
		!if(it .EQ. 0 .AND. iss_on)then
		a_eqm(ij+1, it) = linint_Gen(available, cash(:,ij), &
				& a_plus(:,ij,it),5)
		!else
		!	a_eqm(ij+1, itp) = linint_Gen(available, cash(:,ij), &
		!		& a_plus(:,ij,it))
		!end if
		
		enddo
	
		! age JJ choices
		available = (1d0 + r(it) - delta) &
			& * (a_eqm(JJ,it) + beq(it)) &
			& + pen(JJ, it) &
			& + div(JJ,it)
		c_eqm(JJ,it) = available 
		h_eqm(JJ,it) = 0d0
		s_eqm(JJ,it) = 0d0
		
	else
	
		do ij = 1,JJ-1
			
			! available cash
			available = w(it) * eff(ij) * (1d0-taup(it)) + &
			& (1d0 + r(it) - delta) &
			& * (a_eqm(ij,it-1) + beq(it)) &
			& + pen(ij, it) &
			& + div(ij,it)
			! consumption and assets
			c_eqm(ij,it) = linint_Gen(available, cash(:,ij), &
				& c(:,ij,it),5)
			h_eqm(ij,it) = linint_Gen(available, cash(:,ij), &
				& h(:,ij,it),5)
			s_eqm(ij,it) = linint_Gen(available, cash(:,ij), &
				& surv(:,ij,it),5)
		
			! steady state
			!if(it .EQ. 0 .AND. iss_on)then
			a_eqm(ij+1, it) = linint_Gen(available, cash(:,ij), &
				& a_plus(:,ij,it),5)
			!else
			!	a_eqm(ij+1, itp) = linint_Gen(available, cash(:,ij), &
			!		& a_plus(:,ij,it))
			!end if
		
		enddo
	
		! age JJ choices
		available = (1d0 + r(it) - delta) &
			& * (a_eqm(JJ,it-1) + beq(it)) &
			& + pen(JJ, it) &
			& + div(JJ,it)
		c_eqm(JJ,it) = available 
		h_eqm(JJ,it) = 0d0
		s_eqm(JJ,it) = 0d0
		
	endif
		
	
end subroutine 


    
! check policy functions
subroutine check_policy_functions(it_in)

	implicit none
	integer,intent(in) :: it_in
	integer :: it,ij
	
	it = it_in

	write(*,*) 'Check policy and Value functions:'
	do ij = 1,JJ	
	
		write(*,*) 'Age:', ij
		write(*,*) 'a+:'
		write(*,*) a_plus(:,ij,it)
		write(*,*) 'c:'
		write(*,*) c(:,ij,it)
		write(*,*) 'h:'
		write(*,*) h(:,ij,it)
		write(*,*) 'surv:'
		write(*,*) surv(:,ij,it)
		write(*,*) 'VV (full value function at age j accounting for continuation):'
		write(*,*) VV(:,ij,it)
	
	enddo
	
end subroutine

! check for the maximum cash grid point used at each age
subroutine check_grid_ca(camax, it)
    
    implicit none
    integer, intent(in) :: it
    integer :: camax(JJ), ij, ill, ilr
    real*8 :: available, varphi_ca

    write(*,*) 'check eqm. cash grid'
    
    do ij = 1,JJ
    
		! available cash
		if(iss_on)then
			available = a_eqm(ij+1,it) + p(it) * h_eqm(ij,it)
		else
			available = a_eqm(ij+1,it+1) + p(it) * h_eqm(ij,it)
		endif
			 
		! interpolate between cash gridpoints which affect the future value
        call linint_Grow(available, ca_l, ca_u, ca_grow, NC, ill, ilr, varphi_ca)
                    
        ! restrictions for interpolation
        ill = min(ill, NC)
        ilr = min(ilr, NC)
        varphi_ca = max(min(varphi_ca, 1d0),0d0)
        
        if(varphi_ca .GE. 0.5d0)then
        
			camax(ij) = ill
			
		else
		
			camax(ij) = ilr
			
		endif
			
			
        write(*,*) ij, camax(ij)
            
	enddo

end subroutine
       

end module solver
