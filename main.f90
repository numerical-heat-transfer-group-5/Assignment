program main
implicit none
real::L=1
real::lou=1
real::gamma=0.1
real u(0:2)
integer i
u(0:1)=(/0.1,2.5/)
Do i=1,2,1
	call mid_differ(L,lou,gamma,u(i-1))
	call cal_upwind(L,lou,gamma,u(i-1))
	call mixed(L,lou,gamma,u(i-1))
    call cal_explicit(L,lou,gamma,u(i-1))
end Do
end program main
	