module counterfactuals

use toolbox
use toolbox_outer
use prob
use Global_Values
use solver

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	run counterfactuals
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine run_counterfactuals()

	implicit none
	integer :: it, im, ij
	real*8 :: LX(JJ), LLX(JJ), TX

	! loop over the counterfactuals
	do im = 1,Ncounters
	
		! initial endogenous model objects
		call initialize_values() 
	
		! perturb the parameters accordingly
		if(im < 4) then
			write(*,*) 'Running counterfactuals 1:3, negative counterfactuals'
		else
			write(*,*) 'Running counterfactuals 4:6, positive counterfactuals'
		endif

		call set_counterfactual_series(im)
	
		! solve the G.E.
		do it = 0,TT
			call solve_general_eqm(it)
		end do
		
		! assign simulated objects to counterfactual arrays
		! inputs
		mu_counter(:,im) = mu
		A_c_counter(:,im) = A_c
		A_h_counter(:,im) = A_h
		Z_counter(:,im) = Z
		pop0_counter(:,im) = pop0
		zeta_counters(:,:,im) = zeta
	
		! outputs
		! p
		p_counter(:,im) = p
		
		! others ...
		do it = 0,TT
	
			! life expectancies
			LX(1) = 1d0
			do ij = 2,JJ
				LX(ij) = LX(ij-1) * s_eqm(ij-1,it)
				LLX(ij-1) = (LX(ij-1) + LX(ij)) / 2d0
			enddo
			TX = 0d0
			do ij = JJ-1,1,-1
				TX = TX + LLX(ij)
			enddo
			life_exp_counter(it,im) = 20d0 + TX * 5d0
			
			! health share of aggregate expenditure (nominal)
			if((CC(it) + p(it) * HH(it)) > 0d0)then
				health_share(it,im) = HH(it) * p(it) / &
				& (CC(it) + p(it) * HH(it))
			else
				health_share(it,im) = 0.9999d0
			endif
			
			! share of capital in the production of health care
			if(KK(it+1) > 0d0) then
				health_cap_counter(it,im) = KK_H(it+1) / KK(it+1)
			else
				health_cap_counter(it,im) = 0.00001d0
			endif
			
			! share of labor in the production of health care
			if(LL(it+1) > 0d0) then
				health_lab_counter(it,im) = LL_H(it+1) / LL(it+1)
			else
				health_lab_counter(it,im) = 0.00001d0
			endif	
			
		enddo
		
		! growth rate
		do it = 1,Tcutoff
	
			gy_counter(it,im) = (YY(it) / YY(it-1)) ** (0.2d0) -1 
	
		enddo
		
	enddo
	

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	set counterfactual series
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_counterfactual_series(im_in)

	implicit none
	integer, intent(in) :: im_in ! which counterfactual we running?
	integer :: it
	real*8 :: g_h_counter
	
	! first 
	if(im_in .EQ. 1)then
	
		write(*,*) 'Counter #1: Fixed zetas by age, g_z, and gn'
		write(*,*) 'NO GE EFFECTS'
	
		pop0 = pop0_DATA(0)
		do it = 0,TT
	
			zeta(:,it) = zeta(:,0)
		
		end do	
		! pass parameters back block
		A_h0 = parms(1)
		g_h = parms(2:5)
		g_Z = 0d0
	!	chi = parms(4)

		Z = Z_0

	endif

	! second
	if(im_in .EQ. 2)then
	
		write(*,*) 'Counter #1: Fixed mu'
		write(*,*) 'NO MARKUPS'
	
		mu = mu(0) ! reset all the mu's, no monop competition
		
		! pass parameters back block
		A_h0 = parms(1)
		g_h = parms(2:5)
	
	endif
	
	! third 
	if(im_in .EQ. 3)then
	
		write(*,*) 'Counter #3: Fixed g_c and g_h'
		write(*,*) 'NO COST DISEASE'
	
		! g_c
		g_c = 0d0
		! After Tgrowth it is constant until TT
		A_c = 1d0
		do it = 1,(Tgrowth-1)
	
			A_c(it) = A_c(it-1) * (1d0 + g_c)
	
		enddo
		! After Tgrowth it is constant until TT
		A_c(Tgrowth:TT) = A_c(Tgrowth-1)
		
		! g_h
		g_h_counter = 0d0
	
		! pass parameters back block
		A_h0 = parms(1)
		g_h = parms(2:5)
		
		! reset A_h
		A_h = A_h0
		do it = 1,(Tgrowth-1)
	
			A_h(it) = A_h(it-1) * (1d0 + g_h_counter)
	
		enddo
		! After Tgrowth it is constant until TT
		A_h(Tgrowth:TT) = A_h(Tgrowth-1)
	
		! pass parameters back block
		!A_h0 = parms(1)
		!g_h = parms(2:5)
	
	endif
	

	
	! fourth 
	if(im_in .EQ. 4)then
	
		write(*,*) 'Counter #4: Fixed g_c, g_h, and mu'
		write(*,*) 'ONLY GE EFFECTS'
		
		mu = mu(0) ! reset all the mu's, no monop competition
	
		g_c = 0d0
		g_h_counter = 0d0
	
		! pass parameters back block
		A_h0 = parms(1)
	!	Z_0 = parms(2)
	!	g_Z = parms(3)
	!	chi = parms(4)
		
		! After Tgrowth it is constant until TT
		A_c = 1d0
		do it = 1,(Tgrowth-1)
	
			A_c(it) = A_c(it-1) * (1d0 + g_c)
	
		enddo
		! After Tgrowth it is constant until TT
		A_c(Tgrowth:TT) = A_c(Tgrowth-1)
		
		! reset A_h
		A_h = A_h0
		do it = 1,(Tgrowth-1)
	
			A_h(it) = A_h(it-1) * (1d0 + g_h_counter)
	
		enddo
		! After Tgrowth it is constant until TT
		A_h(Tgrowth:TT) = A_h(Tgrowth-1)
	
	endif
	
	! fifth 
	if(im_in .EQ. 5)then
	
		write(*,*) 'Counter #5: Fixed g_c, g_h, g_z, g_zeta, g_n'
		write(*,*) 'ONLY MU'
		
		! g_c
		g_c = 0d0
		! After Tgrowth it is constant until TT
		A_c = 1d0
		do it = 1,(Tgrowth-1)
	
			A_c(it) = A_c(it-1) * (1d0 + g_c)
	
		enddo
		! After Tgrowth it is constant until TT
		A_c(Tgrowth:TT) = A_c(Tgrowth-1)
		
		! g_h
		g_h_counter = 0d0
	
		! pass parameters back from block
		A_h0 = parms(1)
		g_h = parms(2:5)
		
		! reset A_h
		A_h = A_h0
		do it = 1,(Tgrowth-1)
	
			A_h(it) = A_h(it-1) * (1d0 + g_h_counter)
	
		enddo
		! After Tgrowth it is constant until TT
		A_h(Tgrowth:TT) = A_h(Tgrowth-1)
		
		! GE fixed
		pop0 = pop0_DATA(0)
		do it = 0,TT
	
			zeta(:,it) = zeta(:,0)
		
		end do	
		! pass parameters back block
	!	A_h0 = parms(1)
	!	g_h = parms(2:5)
		g_Z = 0d0
	!	chi = parms(4)

		Z = Z_0

		
	endif
	
	! sixth 
	if(im_in .EQ. 6)then
	
		write(*,*) 'Counter #6: Fixed mu, g_z, g_zeta, g_n'
		write(*,*) 'ONLY COST DISEASE'

		! mu fixed
		mu = mu(0) ! reset all the mu's, no monop competition
		
		! GE fixed
		pop0 = pop0_DATA(0)
		do it = 0,TT
	
			zeta(:,it) = zeta(:,0)
		
		end do	
		! pass parameters back block
		A_h0 = parms(1)
		g_h = parms(2:5)
		g_Z = 0d0
	!	chi = parms(4)

		Z = Z_0
		
	endif
	
	
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	write counterfactuals
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine write_counterfactuals()

	implicit none
	integer :: im
	
	do im = 1,Ncounters
    
    	! write counter series and re-simulated moments
		if(im < 10)then
		
		
			! gy_counters
			out_string(1:10) = "gy_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120001+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120001+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120001+im,*) gy_counter(:,im)
			close(120001+im)
		
			! mu
			out_string(1:10) = "mu_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120002+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120002+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120002+im,*) mu_counter(:,im)
			close(120002+im)
			
			! A_c
			out_string(1:10) = "Ac_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120003+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120003+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120003+im,*) A_c_counter(:,im)
			close(120003+im)
			
			! A_h
			out_string(1:10) = "Ah_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120004+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120004+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120004+im,*) A_h_counter(:,im)
			close(120004+im)
			
			! Z
			out_string(1:9) = "Z_counter"
			out_string(10:10) = trim(str(im))
			out_string(11:14) = ".txt"
			inquire(file = out_string(1:14), exist=exists)
			if(exists) then
				open(120005+im, file = out_string(1:14), status = "old", action = "write")
			else
				open(120005+im, file = out_string(1:14), status = "new", action = "write")
			end if
			write(120005+im,*) Z_counter(:,im)
			close(120005+im)
						
			! pop0
			out_string(1:10) = "p0_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120005+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120005+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120005+im,*) pop0_counter(:,im)
			close(120005+im)
			
			! zeta
			out_string(1:10) = "zt_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120006+im,*) zeta_counters(:,0,im)
			close(120006+im)
			
			! p
			out_string(1:9) = "p_counter"
			out_string(10:10) = trim(str(im))
			out_string(11:14) = ".txt"
			inquire(file = out_string(1:14), exist=exists)
			if(exists) then
				open(120005+im, file = out_string(1:14), status = "old", action = "write")
			else
				open(120005+im, file = out_string(1:14), status = "new", action = "write")
			end if
			write(120005+im,*) p_counter(:,im)
			close(120005+im)
			
			! life_exp_counter
			out_string(1:10) = "le_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120006+im,*) life_exp_counter(:,im)
			close(120006+im)
			
			! health share counter
			out_string(1:10) = "he_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120006+im,*) health_share(:,im)
			close(120006+im)
			
			! health cap counter
			out_string(1:10) = "hk_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120006+im,*) health_cap_counter(:,im)
			close(120006+im)
			
			! health labor counter
			out_string(1:10) = "hl_counter"
			out_string(11:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120006+im,*) health_lab_counter(:,im)
			close(120006+im)
						
		else
		
			! gy_counters
			out_string(1:10) = "gy_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120001+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120001+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120001+im,*) gy_counter(:,im)
			close(120001+im)
			
			! mu
			out_string(1:10) = "mu_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120002+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120002+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120002+im,*) mu_counter(:,im)
			close(120002+im)
			
			! A_c
			out_string(1:10) = "Ac_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120003+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120003+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120003+im,*) A_c_counter(:,im)
			close(120003+im)
			
			! A_h
			out_string(1:10) = "Ah_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120004+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120004+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120004+im,*) A_h_counter(:,im)
			close(120004+im)
			
			! Z
			out_string(1:9) = "Z_counter"
			out_string(10:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120005+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120005+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120005+im,*) Z_counter(:,im)
			close(120005+im)
						
			! pop0
			out_string(1:10) = "p0_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120005+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120005+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120005+im,*) pop0_counter(:,im)
			close(120005+im)
			
			! zeta
			out_string(1:10) = "zt_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120006+im,*) zeta_counters(:,0,im)
			close(120006+im)
			
			! p
			out_string(1:9) = "p_counter"
			out_string(10:11) = trim(str(im))
			out_string(12:15) = ".txt"
			inquire(file = out_string(1:15), exist=exists)
			if(exists) then
				open(120005+im, file = out_string(1:15), status = "old", action = "write")
			else
				open(120005+im, file = out_string(1:15), status = "new", action = "write")
			end if
			write(120005+im,*) p_counter(:,im)
			close(120005+im)
			
			! life_exp_counter
			out_string(1:10) = "le_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120006+im,*) life_exp_counter(:,im)
			close(120006+im)
			
			! health share counter
			out_string(1:10) = "he_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120006+im,*) health_share(:,im)
			close(120006+im)
			
			! health cap counter
			out_string(1:10) = "hk_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120006+im,*) health_cap_counter(:,im)
			close(120006+im)
			
			! health labor counter
			out_string(1:10) = "hl_counter"
			out_string(11:12) = trim(str(im))
			out_string(13:16) = ".txt"
			inquire(file = out_string(1:16), exist=exists)
			if(exists) then
				open(120006+im, file = out_string(1:16), status = "old", action = "write")
			else
				open(120006+im, file = out_string(1:16), status = "new", action = "write")
			end if
			write(120006+im,*) health_lab_counter(:,im)
			close(120006+im)
						
			
		endif
			

    end do
   
end subroutine

end module
