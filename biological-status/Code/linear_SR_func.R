
# Function which computes linear-regression estimates of parameters and MSY 
# parameters and plots. The function returns... TODO
# Nyrs: the number of years for each CU
# S and R:  year x CU matrices of fish counts for spawners and recruiters 
# LNRS: log(R/S)     BSC: could be created inside the function with R and S to limit the number of parameters to pass in
# StNames: the same of the CUs
# 
LinReg <- function(S, R){
	
  StNames <- colnames(S)
	Nstocks <- ncol(S)
	a <- vector(length = Nstocks)  # tau ?
	b <- a
	
	# ngrows <- 4
	# ngcol <- trunc(Nstocks/ngrows) + 1
	
	if(Nstocks < 5){
	  ngrows <- Nstocks
	  ngcol <- 1
	}else if(5 <= Nstocks & Nstocks < 9){
	  ngrows <- 4
	  ngcol <- 2
	}else if(9 <= Nstocks & Nstocks < 13){
	  ngrows <- 4
	  ngcol <- 3
	}else if(13 <= Nstocks & Nstocks < 17){
	  ngrows <- 4
	  ngcol <- 4
	}else if(17 <= Nstocks & Nstocks < 20){
	  ngrows <- 4
	  ngcol <- 5
	}
	
	par(mfcol = c(ngrows,ngcol),
	    mai = c(.3,.3,.3,.2),     # size of margin size in inches
	    omi = c(0.1,0.1,0.1,0.1)) # size outer margins in inches
	
	for (i in 1:Nstocks) {
	  
	  Rcu <- R[,i]            # BSC: remove the NAs that are now present (to address SP's comment in HBSRM.R)
	  Rcu <- Rcu[!is.na(Rcu)]   
	  Scu <- S[,i]            
	  Scu <- Scu[!is.na(Scu)]
	  
	  LNRS <- log(Rcu/Scu)
	  
		reg <- lm(LNRS ~ Scu)
		a[i] <- as.double(reg$coefficients[1])           #;tau[i]=as.double(1/sd(reg$residuals)^2)
		b[i] <- as.double(abs(reg$coefficients[2]))	

		# Compute production parameters
		b1 <- a[i]/b[i]
		Prod <- round(exp(a[i]),digits = 1)
		Smsy <- round(b1 * (0.5 - 0.07 * a[i]), digits = 0)
		Smax <- round(b1 / a[i], digits = 0)
		Uopt <- round(0.5 * a[i] - 0.07 * a[i]^2, digits = 2)
		
		print(c(a[i],b[i]))
		print(c("Prod=",Prod))
		print(c("Smsy=",Smsy))
		print(c("Smax=",Smax))
		print(c("Uopt=",Uopt))

		# Plot basic SR curves and data
		# plot(S[,i], LNRS[1:Nyrs[i],i],bty='l',xlab="Spawners",xlim=c(0,max(S)),ylim=c(0,max(LNRS)),ylab="Ln(R/S)",main=StNames[i])
		# abline(reg,lty=1,lwd=2)

		Sx <- seq(0,max(Scu),by = 100)
		pR <- Sx * exp(a[i] - b[i] * Sx)
		plot(x = Scu,y = Rcu,
		     bty = 'l',xlab = "",ylab = "",
		     xlim = c(0,max(Scu)),ylim = c(0,max(Rcu)),
		     main = StNames[i])
		lines(pR ~ Sx,lty = 1,lwd = 2)
		abline(a = 0,b = 1,lty = 2)
	}
	return(c(a,b))
}
