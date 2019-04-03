subroutine mixed(length,density,coefficient,velocity)
	implicit none
	
	integer::time
	real D
	real F
	real length
	real density
	real coefficient
	real velocity
	real::error
	integer::OutDev=10
	integer i
	integer n
	real fai0(0:11)
	real fai1(0:11)
	real err(0:11)
	
	F=density*velocity	
	D=coefficient*10/length	
	fai1(0)=1
	fai1(1:10)=0
	fai1(11)=0
    error=1
    time=1
	
	Do while (error>0.01)
		fai0(0:11)=fai1(0:11)
		Do i=1,10,1
			if(i==1) then
				fai1(i)=(2*max(F,D+F/2,0.0)*fai0(i-1)+max(-F,D-F/2,0.0)*fai0(i+1))/(2*max(F,D+F/2,0.0)+max(-F,D-F/2,0.0))
			end if
			if(i==10) then
				fai1(i)=(max(F,D+F/2,0.0)*fai0(i-1)+2*max(-F,D-F/2,0.0)*fai0(i+1))/(max(F,D+F/2,0.0)+2*max(-F,D-F/2,0.0))
			end if
			if(i/=1.and.i/=10) then
				fai1(i)=(max(F,D+F/2,0.0)*fai0(i-1)+max(-F,D-F/2,0.0)*fai0(i+1))/(max(F,D+F/2,0.0)+max(-F,D-F/2,0.0))
			end if
		end do
		time=time+1
		err(0:11)=fai1(0:11)-fai0(0:11)
		error=max(abs(err(1)),abs(err(2)),abs(err(3)),abs(err(4)),abs(err(5)),abs(err(6)),abs(err(7)),abs(err(8)),abs(err(9)),abs(err(10)))
	end do
	open(OutDev,file='mixed.txt')
	
	write(OutDev,'("Number of Iteration:",i6)') time
	write(OutDev,'("Distribution of fai:")')
	Do n=1,10,1
		write(OutDev,'(e10.5)') fai1(n)
	end Do
	
	end subroutine mixed