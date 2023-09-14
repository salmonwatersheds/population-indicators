
# Function that calculates the lower stock-recruitment benchmark, Sgen (i.e. the 
# spawner abundance that leads to Smsy in one generation with no harvest and 
# assuming constant environmental conditions). 
#' This function calculates Sgen1, or the spawner abundances that would result 
#' in recovery to Smsy within one generation. 
#' 
#' @param Sgen.hat A proposed numeric value for Sgen
#' @param theta A numeric vector containing the estimated parameters a, b, and
#' sigma from the Ricker function fit to data
#' @param Smsy The calculated value of Smsy based on theta, from the calcSmsy
#' function.
#' @return Returns the numeric value of Sgen1.
#' 
#' @examples 
#' 
calcSgen <- function(Sgen.hat, theta, Smsy){
  
  # the function returns:
  # - minimum: the value of Sgen.hat that minimises Sgen.optim(Sgen.hat, theta, Smsy)
  # in the interval c(0, Smsy)
  # - objective: the corresponding minimal value of Sgen.optim(Sgen.hat, theta, Smsy)
  fit <- optimize(f = Sgen.optim, interval = c(0, Smsy), theta = theta, Smsy = Smsy)
  
  # Give warning if Sgen1 is at upper or lower bound
  if(round(fit$minimum) == 0){
    warning("Sgen1 at lower bound of zero")
  }
  
  if(round(fit$minimum) == round(Smsy)){
    warning("Lower benchmark greater than upper benchmark (Sgen1 > Smsy). Set to NA.")
    return(Sgen1 = NA)
  } else {
    return(Sgen1 = as.numeric(fit$minimum))
  }
}

# Function that calculates the upper stock-recruitment benchmark = the maximum 
# Sustainability Yield (Smsy)
#' This function calculates Smsy, or the spawner abundance projected to 
#' maintain long-term maximum sustainable yield from a population with 
#' Ricker dynamics. It applied the explicit solution for Smsy given by
#' Scheuerell (2016), PeerJ, DOI 10.7717/peerj.1623
#' This function uses the lambertW0 function from the `lamW` library.
#'
#' @param a A numeric value giving Ricker parameter a (or log alpha), estimated
#' from the observed spawner-recruitment data.
#' @param b A numeric value giving Ricker parameter b (strength of density 
#' dependence; units spawners^(-1)), estimated from the observed spawner-
#' recruitment data.
#' @return Returns the value of Smsy.
#'
#' @examples
#' #

calcSmsy <- function(a, b) {
  
  require(lamW) # for the Lambert-W Function
  
  Smsy = (1 - lamW::lambertW0(exp(1 - a))) / b
  
  return(as.numeric(Smsy))
}

# Function that takes a vector of string characters and returns the same vector but
# with the first character of each string in lower and upper case inside [].
character_lowerHigerCase_fun <- function(characterVec){
  
  characterVec_lhc <- sapply(X = characterVec,FUN = function(x){
    # x <- characterVec[1]
    x_st <- substring(text = x,first = 1,last = 1)
    x_rest <- substring(text = x,first = 2,last = nchar(x))
    x_st_lowerC <- tolower(x_st)
    x_st_upperC <- toupper(x_st)
    x_stComb <- paste0("[",x_st_upperC,"|",x_st_lowerC,"]")
    output <- paste0(x_stComb,x_rest)
    return(output)
  })
  return(characterVec_lhc)
}

# BM_data <- benchmarks_df
# methods = NA
# size_box_cm = 6
# nameRegion_show = nameSpecies_show = T
figure_compare_benchamrks_fun <- function(BM_data,
                                          nameRegion_show = T,
                                          nameSpecies_show = T,
                                          print_fig = F,
                                          methods = NA,
                                          size_box_cm = 6, # the of the plots in cm when exported
                                          wd_figures, 
                                          addTonameFile = "",
                                          coeff_width_adj = 0.1){
  
  benchCols <- c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F")
  
  if(is.na(methods)){
    methods <- c("medQuan","HPD","HS_percentiles")
  }
  # methods <- factor(methods)
  
  # 
  regions <- unique(BM_data$region)
  
  for(i_r in 1:length(regions)){
    
    # i_r <- 1
    
    species <- unique(subset(x = BM_data, subset = region == regions[i_r])$species)
    
    for(i_s in 1:length(species)){
      
      # i_s <- 1
      
      BM_data_sub <- BM_data[BM_data$region == regions[i_r] & BM_data$species == species[i_s],]
      
      CUs <- unique(BM_data_sub$CU)
      nCUs <- length(CUs)
      
      side1_large <- size_box_cm / 6   # 1.0
      side1_small <- size_box_cm / 10  # 0.6
      side2_large <- size_box_cm / 3   # 2.0
      side2_small <- size_box_cm / 20  # 0.3
      side3 <- size_box_cm / 10
      side4 <- size_box_cm / 20
      
      if(nCUs <= 2){
        nrow <- 1
        ncol <- nCUs
      }else if(3 <= nCUs & nCUs <= 4){
        nrow <- 2
        ncol <- 2
      }else if(5 <= nCUs & nCUs <= 6){
        nrow <- 2
        ncol <- 3
      }else if(7 <= nCUs & nCUs <= 9){
        nrow <-3
        ncol <- 3
      }else if(10 <= nCUs & nCUs <= 12){
        nrow <- 3
        ncol <- 4
      }else if(13 <= nCUs & nCUs <= 16){
        nrow <- 4
        ncol <- 4
      }else if(17 <= nCUs & nCUs <= 20){
        nrow <- 4
        ncol <- 5
      }else if(21 <= nCUs & nCUs <= 25){
        nrow <- 5
        ncol <- 5
      }else if(26 <= nCUs & nCUs <= 30){
        nrow <- 5
        ncol <- 6
      }else if(31 <= nCUs & nCUs <= 36){
        nrow <- 6
        ncol <- 6
      }
      
      if(nrow == 1 & ncol %in% c(1,2)){
        side2_large <- size_box_cm / 2  # for better display
        side1_large <- size_box_cm / 4
      }
      
      # margins and labels
      side1 <- c(rep(rep(side1_small,ncol),nrow-1),rep(side1_large,ncol))
      side2 <- rep(c(side2_large,rep(side2_small,ncol-1)),nrow)
      side3 <- rep(side3,ncol * nrow)
      side4 <- rep(side4,ncol * nrow)
      
      xlab_show <- side1 == side1_large
      ylab_show <- side2 == side2_large
      
      # 
      layout_m <- matrix(data = 1:(nrow*ncol),nrow = nrow, byrow = T)
      
      header <- 0
      
      if(nameRegion_show | nameSpecies_show){ # add a row at the top
        layout_m <- layout_m + 1
        layout_m <- rbind(rep(1,ncol(layout_m)),layout_m)
        header <- 1
      }
      
      # size_box_cm <- 6 # the size of the squared plots in cm when exported
      
      width_fig_cm <- size_box_cm * ncol + sum(side2[1:ncol]) + sum(side4[1:ncol])
      height_fig_cm <- size_box_cm * nrow + 
        header * size_box_cm/5 + 
        sum(matrix(side1,nrow = nrow, byrow = T)[,1]) +
        sum(matrix(side3,nrow = nrow, byrow = T)[,1])
      height_fig_cm_noHeader <- height_fig_cm -  header * size_box_cm/5
      
      if(print_fig){
        pathFile <- paste0(wd_figures,"/",regions[i_r],"_",species[i_s],
                           "_benchmarks_comparisons",addTonameFile,".jpeg")
        
        jpeg(file = pathFile, 
             width = width_fig_cm, 
             height = height_fig_cm, 
             units = "cm", res = 300)
      }
      
      width_onePlot_cm <- width_fig_cm / ncol 
      height_onePlot_cm <- height_fig_cm_noHeader / nrow
      
      widths <- 1
      if(ncol > 1){
        widths <- c((width_onePlot_cm - side2_small - side4[1] - coeff_width_adj)/(width_onePlot_cm - side2_large - side4[1]),
                    rep(1,ncol - 1))
      }
      heights <- 1
      if(nrow > 1){
        heights <- c(rep(1,nrow - 1),
                     (height_onePlot_cm - side1_small - side3[1])/(height_onePlot_cm - side1_large - side3[1]))
      }
      
      if(nameRegion_show | nameSpecies_show){
        heights <- c(1/5,heights)
      }
      
      layout(mat = layout_m, widths = widths, heights = heights)
      
      # plot region and or species at the top of the figure
      if(nameRegion_show | nameSpecies_show){ # add a row at the top
        sep <- ""
        if(nameRegion_show | nameSpecies_show){
          sep <- " - "
        }
        header_text <- c(gsub("_"," ",region[i_r]),species[i_s])
        header_text <- paste0(header_text[c(nameRegion_show,nameSpecies_show)],collapse = sep)
        
        par(mar = rep(0,4))
        plot(1, type = "n", xlab = "",ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        legend("center",header_text,bty = "n", cex = ncol + 1)
      }
      
      for(i_cu in 1:length(CUs)){
        
        # i_cu <- 1
        
        BM_data_sub_cu <- subset(BM_data_sub,CU == CUs[i_cu])
        
        # coordinates benchmarks
        y_ticks <- 1:length(methods)
        offset <- 0.15
        y <- rev(sort(c(y_ticks - offset, y_ticks + offset)))
        
        x <- c()
        x_LCI <- c()
        x_UCI <- c()
        for(m in methods){
          # m <- "medQuan" # m <- "HPD" # m <- "HS_percentiles"
          if(m == "HS_percentiles"){
            CItype <- c("lower","upper")
          }else{
            CItype <- c("Sgen","Smsy")
          }
          
          for(b in CItype){
            # b <- "Sgen"  b <- "lower"
            x_new <- BM_data_sub_cu[BM_data_sub_cu$method == m & BM_data_sub_cu$benchmark == b,]$m
            x_LCI_new <- BM_data_sub_cu[BM_data_sub_cu$method == m & BM_data_sub_cu$benchmark == b,]$CI025
            x_UCI_new <- BM_data_sub_cu[BM_data_sub_cu$method == m & BM_data_sub_cu$benchmark == b,]$CI975
            if(length(x_new) == 0){
              x_new <- NA
            }
            if(length(x_LCI_new) == 0){
              x_LCI_new <- NA
            }
            if(length(x_UCI_new) == 0){
              x_UCI_new <- NA
            }
            x <- c(x,x_new)
            x_LCI <- c(x_LCI,x_LCI_new)
            x_UCI <- c(x_UCI,x_UCI_new)
          }
        }
        
        # coordinates CI
        xmin <- min(c(x,x_LCI,x_UCI),na.rm = T)
        xmax <- max(c(x,x_LCI,x_UCI),na.rm = T)
        if(is.infinite(xmin)){
          xmin <- 0
          xmax <- 1
        }
        
        # 
        # par(mar = c(side1[i_cu],side2[i_cu],3,0.5))
        cm_inch <- .393701
        par(mai = c(side1[i_cu], side2[i_cu], side3[i_cu], side4[i_cu]) * cm_inch)
        #
        plot(x = x, y = y, yaxt = "n", ylab = "", xlab = "",
             xlim = c(xmin,xmax), ylim = c(min(y_ticks) - .5, max(y_ticks) + .5),  
             col = rep(c(benchCols["r"],benchCols["g"]),length(methods)),
             pch = 16, cex = 2)
        if(ylab_show[i_cu]){
          ylab <- rev(methods)
        }else{
          ylab <- rep("",length(methods))
        }
        axis(side = 2, at = y_ticks, labels = ylab,las = 1)
        segments(x0 = x, x1 = x_LCI, y0 = y, y1 = y,
                 col = c(benchCols["r"],benchCols["g"]), lwd = 2)
        segments(x0 = x, x1 = x_UCI, y0 = y, y1 = y, 
                 col = c(benchCols["r"],benchCols["g"]), lwd = 2)
        mtext(CUs[i_cu],side = 3, line = .4)
        if(xlab_show[i_cu]){
          mtext("Number of spawners",side = 1, line = 2.5, cex = .9)
        }
        
      }
    }
    if(print_fig){
      dev.off()
    }
  }
}

# Function to calculate highest posterior density (HPD) and HPD interval
HPD <- function(x, xmax = NA, na.rm = TRUE,n = 5000){
  if(is.na(xmax)){
    xmax <- max(x, na.rm = na.rm)
  }
  dens <- density(x, from = 0, to = xmax, na.rm = na.rm, n = n)
  m <- dens$x[which(dens$y == max(dens$y))][1]
  
  mCI <- HPDinterval(mcmc(c(x)), prob = 0.95)[1,]
  
  output <- c(m, mCI)
  names(output) <- c("m","mCI")
  return(output)
}

# Function which computes linear-regression estimates of parameters and MSY 
# parameters (i.e., the simple linearized Ricker model) and plots. The function 
# returns a list of the coefficients a, b and sigma.
# Nyrs: the number of years for each CU
# S and R:  year x CU matrices of fish counts for spawners and recruiters 
# LNRS: log(R/S)     BSC: could be created inside the function with R and S to limit the number of parameters to pass in
# StNames: the same of the CUs
linRegRicker_fun <- function(S, R, plot_figures = T){
  
  CUs <- colnames(S)
  nCUs <- ncol(S)
  a <- vector(length = nCUs)  # Vector to store productivity parameter
  b <- rep(NA, nCUs)           # Vector to store density-dependence parameter
  sigma <- rep(NA, nCUs)         # Vector to store estimates of sigma
  
  # in case we want to plot:
  if(nCUs < 5){
    ngrows <- nCUs
    ngcol <- 1
  }else if(5 <= nCUs & nCUs < 9){
    ngrows <- 4
    ngcol <- 2
  }else if(9 <= nCUs & nCUs < 13){
    ngrows <- 4
    ngcol <- 3
  }else if(13 <= nCUs & nCUs < 17){
    ngrows <- 4
    ngcol <- 4
  }else if(17 <= nCUs & nCUs < 20){
    ngrows <- 4
    ngcol <- 5
  }else if(21 <= nCUs & nCUs < 26){
    ngrows <- 5
    ngcol <- 5
  }else if(26 <= nCUs & nCUs < 31){
    ngrows <- 5
    ngcol <- 6
  }else if(31 <= nCUs & nCUs < 36){
    ngrows <- 6
    ngcol <- 6
  }
  
  par(mfcol = c(ngrows,ngcol),
      mai = c(.4,.3,.3,.2),     # size of margin size in inches
      omi = c(0.1,0.1,0.1,0.1)) # size outer margins in inches
  
  for(i in 1:nCUs){
    
    Rcu <- R[,i, drop = F]            # remove the NAs that are now present (to address SP's comment in HBSRM.R)
    Rcu <- Rcu[!is.na(Rcu),,drop = F]   
    Scu <- S[,i, drop = F]            
    Scu <- Scu[!is.na(Scu), drop = F]
    
    LnRS <- log(Rcu/Scu)   # simple linearized Ricker model
    
    reg <- lm(LnRS ~ Scu)
    
    # Extract fitted parameters
    a[i] <- reg$coefficients[1]           #;tau[i]=as.double(1/sd(reg$residuals)^2)
    b[i] <- abs(reg$coefficients[2])
    sigma[i] <- summary(reg)$sigma
    
    output <- list(a = a, b = b, sigma = sigma)
    
    # Compute production parameters
    b1 <- a[i]/b[i]
    Prod <- round(exp(a[i]),digits = 1)
    Smsy <- round(b1 * (0.5 - 0.07 * a[i]), digits = 0)
    Smax <- round(b1 / a[i], digits = 0)
    Uopt <- round(0.5 * a[i] - 0.07 * a[i]^2, digits = 2)
    
    print(paste0("*** ",CUs[i]," ***"))
    print(do.call(what = cbind,args = list(a = a[i], b = b[i], sigma = sigma[i])))
    print(c("Prod=",Prod))
    print(c("Smsy=",Smsy))
    print(c("Smax=",Smax))
    print(c("Uopt=",Uopt))
    print("")
    
    # Plot basic SR curves and data
    # plot(S[,i], LNRS[1:Nyrs[i],i],bty='l',xlab="Spawners",xlim=c(0,max(S)),ylim=c(0,max(LNRS)),ylab="Ln(R/S)",main=CUs[i])
    # abline(reg,lty=1,lwd=2)
    
    if(plot_figures){
      
      Sx <- seq(0,max(Scu),by = 100)
      pR <- Sx * exp(a[i] - b[i] * Sx)
      plot(x = Scu,y = Rcu,
           bty = 'l',xlab = "",ylab = "",
           xlim = c(0,max(Scu)),ylim = c(0,max(Rcu)),
           main = CUs[i])
      lines(pR ~ Sx,lty = 1,lwd = 2)
      abline(a = 0,b = 1,lty = 2)
    }
    
  }
  output <- list(a = a, b = b, sigma = sigma)
  return(output)
}

# Function to calculate posterior median and quantiles
# * THis is the current method used in the PSE*
medQuan <- function(x, na.rm = TRUE){
  m <- median(x, na.rm = na.rm)
  mCI <- quantile(x, probs = c(0.025, 0.975), na.rm = na.rm)
  output <- c(m, mCI)
  names(output) <- c("m","mCI")
  return(output)
}

#' Function to boostrap confidence intervals based on modelled timeseries of
#' residuals. The functions returns a list containing the upper and lower benchmarks'
#' (1) median, (2) 95% CI, (3) the simulated data and (4) the % used. 
#' 
#' **Description needed.**
#' 
#' 
#' @param series Time series of spawner abundance data. Missing values should be
#' entered as NA
#' @param numLags The number of years for each block when implementing block
#' bootstrapping  BSC: is that relevant for this function? The function does not perform block bootstrapping
#' @param nBoot The number of permutations of the timeseries to be used for 
#' calculating the bootstrap confidence intervals on benchmarks
#' @param benchmarks The quantiles of the historical spawners series to be used
#' as the upper and lower benchmarks. Defaults to the 25th and 50th percentiles (
#' code{benchmarks = c(0.25, 0.5)}).
#' 
#' @return Returns a list; the first element is a matrix with the 95% CI 
#' (rows) on the lower and upper benchmarks (columns). The second element
#' is a matrix of dimension number of timesteps (\code{length(series)}) by 
#' \code{nBoot}containing the simulated timeseries (columns).
#'
#' @examples
#'
#' @export


modelBoot <- function(
    series, 
    numLags = 1, # numLags is the lag for the autocorrelation; default is just 1 year
    nBoot = 10000, 
    benchmarks = c(0.25, 0.5)
){
  
  if(sum(!is.na(series)) > 1){ # if there is at least two data points (to avoid crashing)
    
    # check if every odd or even year have consistently NAs like for Pink salmons.
    # in that case, the ar() function returns an error. So it that case, remove 
    # the odd or even years data points.
    # OTHE TSOLUTION THAT DOES NOT WORK: set numLags to 2
    series_odd <- series[1:length(series) %% 2 == 1]
    series_even <- series[1:length(series) %% 2 == 0]
    keep_odd <- sum(!is.na(series_odd)) > 0
    keep_even <- sum(!is.na(series_even)) > 0
    # if this is the case here:
    if(keep_odd & !keep_even){   # only keep odd row data points
      series <- series[1:length(series) %% 2 == 1]
    }else if(!keep_odd & keep_even){ # only keep even row data points
      series <- series[1:length(series) %% 2 == 0]
    }
    
    n <- length(series)
    
    ar.fit <- ar(
      log(series), # spawner time series
      demean = TRUE, # Estimate mean spawners
      intercept = FALSE, # Intercept = 0 for autocorrelation
      order.max = numLags, # lag for autocorrelation
      aic = FALSE, # estimate autocorrelation for all numLags 
      method = "yule-walker", # only method that allows for NAs
      na.action = na.pass #
    ) # standard OLS
    
    # Matrices to store bootstrapped residuals and spawners (obs)
    res.star <- matrix(nrow = n, ncol = nBoot)
    res.star[, ] <- sample(na.omit(ar.fit$resid), n * nBoot, replace = TRUE)
    
    obs.star.log <- matrix(nrow = n + numLags, ncol = nBoot) 
    
    # Matrix to store bootstrapped values of CI
    HS_benchBoot <- matrix(
      nrow = nBoot, 
      ncol = 2, 
      dimnames = list(c(1:nBoot), c("lower", "upper")))
    
    # Model bootstrap:
    for(i in 1:nBoot){
      
      # Initialize the simulated time series using the true data for the first
      # 1:numLags points, starting from a random place in the timeseries
      j.init <- sample(1 : (n - numLags + 1), 1) # starting point for initialization
      obs.star.log[1:numLags, i] <- log(series[j.init:(j.init + numLags - 1)])
      
      for(j in 1:n){ # For each timepoint in the simulated series
        obs.star.log[(numLags + j), i] <- ar.fit$x.mean + ar.fit$ar %*% (obs.star.log[j:(j + numLags - 1), i] - ar.fit$x.mean) + res.star[j, i]
      } #end j
      
      HS_benchBoot[i, ] <- quantile(exp(obs.star.log[(numLags + 1):(numLags + n), i]), benchmarks, na.rm = TRUE)
    } # end bootstrap loop
    
    #
    # plot(x = 1:(n+1), y = obs.star.log[,1], type = 'l', col = alpha('grey10',alpha = 0.5),
    #      lwd = 1.5, ylim = c(0,25))
    # for(i in 2:1000){
    #   lines(x = 1:(n+1), y = obs.star.log[,i], col = alpha('grey40',alpha = 0.5) ,lwd = 1.5)
    # }
    
    # get the median
    HS_benchmedian <- apply(HS_benchBoot, 2, median, na.rm = TRUE)
    
    # get the 95% CI
    HS_benchCI <- apply(HS_benchBoot, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
    
    # hist(HS_benchBoot)
    # polygon(x = c(HS_benchCI[,1],rev(HS_benchCI[,1])),
    #         y = c(0,0,3900,3900), border = "red", lty = 2,col = alpha('red',alpha = 0.5))
    # polygon(x = c(HS_benchCI[,2],rev(HS_benchCI[,2])),
    #         y = c(0,0,3900,3900), border = "chartreuse4", lty = 2,col = alpha('chartreuse4',alpha = 0.5))
    # segments(x0 = HS_benchmedian, x1 = HS_benchmedian, y0 = 0, y1 = 3900, lwd = 2)
    
    obs.star <- exp(tail(obs.star.log, n))
    
    ouptup <- list(m = HS_benchmedian, 
                   CI = HS_benchCI, 
                   simulatedSeries = obs.star, 
                   benchmarks = benchmarks)
  }else{
    
    ouptup <- list(m = c(NA,NA), 
                   CI = matrix(NA,nrow = 2, ncol = 2), 
                   simulatedSeries = NA, 
                   benchmarks = benchmarks)
  }
  
  return(ouptup)
}


# Function that returns the name of the regions. This is to ensure that no spelling
# mistakes are make.
regions_fun <- function(){
  
  regions <- data.frame(
    Central_coast = 'Central_coast',
    Columbia = "Columbia", 
    Fraser = 'Fraser',
    Haida_Gwaii = 'Haida_Gwaii',
    Nass = 'Nass',
    Skeena = 'Skeena',
    Yukon = 'Yukon')
  
  return(regions)
}

#' Optimization routine for calculation of Sgen
#' This function calculates the likelihood of residuals between the projected
#' recruits from an estimated Sgen.hat and a known value of Smsy, to be used in
#' the optimization of Sgen in the calcSgen function. This function is based on
#' the Sgen.optim function in the samSim package (Freshwater et al. 2018).
#' 
#' @param Sgen.hat A proposed numeric value for Sgen
#' @param theta A numeric vector containing the estimated parameters a, b, and
#' sigma from the Ricker function fit to data
#' @param Smsy The calculated value of Smsy based on theta, from the calcSmsy
#' function.
#' @return Returns the negative log likelihood of the residuals.

Sgen.optim <- function (Sgen.hat, theta, Smsy) {
  # # Add warning and adjustment for non-zero spawner abundances
  # if(any(Sgen.hat < 0.00001)){
  # 	Sgen.hat[Sgen.hat < 0.00001] <- 0.0001
  # 	print(c("Sgen.hat abundance must be > 0. Negative values replaced w/ small positive"))
  # }
  # 
  # if(any(Smsy < 0.00001)){
  # 	Smsy[Smsy < 0.00001] <- 0.0001
  # 	print(c("Smsy must be > 0. Negative values replaced w/ small positive"))
  # }
  
  a <- theta[1]
  b <- theta[2]
  sig <- exp(theta[3])  # sigma_bi
  
  # Compute projected recruits based on Sgen.hat
  Smsy.hat <- Sgen.hat * exp(a - b * Sgen.hat) 
  
  # Calculate residuals and negative log likelihood
  epsilon <- log(Smsy) - log(Smsy.hat)
  nloglike <- - sum(dnorm(x = epsilon, mean = 0, sd = sig, log = TRUE))	
  
  return(nloglike)
}

# Function that returns a data frame of the fish species names (as column names)
# and corresponding acronym.
# BSC: figure out what these are exactly --> to double check
species_acronym_fun <- function(){
  
  # species_acronym <- data.frame(
  #   name = c("Chinook","Chum","Coho","Pink","Sockeye","Steelhead","Cutthroat"),
  #   acronym = c("CK","CM","CO","PK","SX","SH","CT"))
  
  species_acronym <- data.frame(
    Chinook = "CK",
    Chum = "CM",         # to check
    Coho = "CO",         # to check
    Pink = "PK",
    Sockeye = 'SX',
    Steelhead = "SH",   # to check
    Cutthroat = "CT")   # to check
  
  return(species_acronym)
}

# Function that returns a list of two data frame from the _SRdata.txt for a given 
# species in a given region. The path of the 
# path_file <- fndata[i]  # the path of the _SRdata.text file
# MinSRpts: the minimum number of year (= data points) required 
# wd_Data: the biological-status/Data folder 
SRdata_fun <- function(path_file, wd_Data, MinSRpts = 3){
  
  # BSC: I replaced "stock" with "CU" in the code?
  CUs_nb <- scan(file = path_file, nlines = 1, skip = 1)
  
  # Import the prSmax and prCV for each CU
  d_prior <- read.table(file = path_file, header = T, skip = 2, nrows = CUs_nb)
  # **SP: These priors differ among stocks. Where do they come from?
  
  # Import the fish counts for R and S per year for each CU
  d0 <- read.table(file = path_file, header = T, skip = 3 + CUs_nb, 
                   fill = TRUE, stringsAsFactors = FALSE)
  d0$BY <- as.numeric(d0$BY) # brood year
  
  # Only retain rows with values
  d <- subset(x = d0, subset =  !is.na(Rec) & !is.na(Esc) & BY >= FBYr & Esc > 0) # BSC: why  Esc > 0  and not >= 0?
  
  # get the names of the CUs
  StNames <- as.character(unique(d$CU)) # name of CUs; as.character() is used because certain CUs have a number for name
  StNames <- unique(d$CU)
  
  # in case the CUID is given, replace it by the corresponding name of the CU(s)
  areThereCUID <- suppressWarnings(!is.na(as.numeric(StNames)))
  if(sum(areThereCUID) > 0){
    CUIDsHere <- as.numeric(StNames[areThereCUID])
    # Import Appendix 1: table from the tech report:
    CUIDs <- read.csv(paste0(wd_Data,"/appendix1.csv"),header = T, 
                      stringsAsFactors = F)
    # filter/subset
    CUIDs_cut <- CUIDs[CUIDs$CUID %in% CUIDsHere,]
    
    # preserve the order because %in% does not
    CUIDsHere_names <- CUIDs_cut[order(CUIDsHere),]$Conservation.Unit   
    StNames[areThereCUID] <- CUIDsHere_names
    
    # update d and d0_prior
    for(j in 1:length(CUIDsHere)){
      d$CU[d$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
      d_prior$CU[d_prior$CU == CUIDsHere[j]] <- CUIDsHere_names[j]
    }
  }
  
  # nb of year per CU
  Nyrs <- tapply(X = d, INDEX = d$CU, FUN = nrow) 
  
  # to check the range of years for each CU:
  # tapply(X = d$BY, INDEX = d$CU, FUN = function(x){range(x)})
  
  # retain CUs with enough data points (i.e., Nyrs >= MinSRpts)
  StNames <- StNames[which(Nyrs >= MinSRpts)]
  
  # update objects
  d <- subset(d, CU %in% StNames)
  d_prior <- subset(d_prior, CU %in% StNames)
  
  # 
  output <- list(d,d_prior)
  names(output) <- c("counts","priors")
  
  return(output)
}

# Function that returns the input _SRdata.txt file names from there region-specific 
# repository, which is defined by wd.
# wd <- wd_Data_input
SRdata_path_species_fun <- function(wd, species = NA, species_all = T){

  # Import the most recent individual fish counts for the region and species selected.
  # Note that the region path should be contained in wd already.
  files_list <- list.files(wd)
  
  # In the case we did not specify the species, find those that have data:
  if(species_all){                           
    files_s <- files_list[grepl(pattern = "_SRdata",files_list)]
    # get the species present:
    species <- unique(sub("_SRdata.*", "", files_s))
  }
  
  # Return the corresponding files, selecting the most recent ones eventually:
  SRdata <- sapply(X = species, FUN = function(s){
    # s <- species[3]
    files_s <- files_list[grepl(pattern = paste0(s,"_SRdata"),files_list)]
    
    # if multiple files, select the one with the most recent date modified:
    if(length(files_s) > 1){
      files_dates <- file.info(paste(wd,files_s,sep="/"))$mtime
      files_s <- files_s[files_dates == max(files_dates)]
    }
    # if no file is present --> NA
    if(length(files_s) == 0){
      files_s <- NA
      print(paste0("species ",s," does not have a file."))
    }
    return(files_s)
  })
  
  SRdata <- rbind(SRdata)           # coerce into an array
  SRdata <- SRdata[!is.na(SRdata)]  # remove species with that do not have file.
  
  # Get the full path:
  SRdata <- paste(wd,SRdata,sep = "/")
  
  output <- list(species,SRdata)
  names(output) <- c("species","SRdata")
    
  return(output)
}

# The dropbox path to the /population-indicators/biological-status folder:
wd_biological_status <- "1_Active/Population Methods and Analysis/population-indicators/biological-status"

# Function that returns a data frame with the path leading to the repository where 
# input data (i.e., run reconstructions) is located for each region.
# wd_Data_input_root is the root directory common to all the regions.
# BSC: we probably will need to organize these datasets better
wd_data_regions_fun <- function(wd_root = ""){
  
  wd_data_regions <- data.frame(
    Central_coast = paste0(wd_root,"/1_Active/Central Coast PSE/analysis/central-coast-status/HBM and status"),
    Columbia = paste0(wd_root,"/1_Active/Columbia/data & analysis/analysis/columbia-status"), # BSC: ? no HBM and status folder... to check at some point
    Fraser = paste0(wd_root,"/1_Active/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    # Fraser = paste0(wd_root,"/Fraser_VIMI/analysis/fraser-status/HBM and status"),
    Haida_Gwaii = paste0(wd_root,"/1_Active/Haida Gwaii PSE/Data & Assessments/haida-gwaii-status/HBM and status"),
    Nass = paste0(wd_root,"/1_Active/Nass/assessments/2_population/nass-status/HBM and status"),
    # Nass = paste0(wd_root,"/Nass/assessments/2_population/nass-status/HBM and status"),
    Skeena = paste0(wd_root,"/1_Active/Skeena Updates/skeena-status/HBM and status"),
    # Skeena = paste0(wd_root,"/Skeena Updates/skeena-status/HBM and status"),
    Yukon = paste0(wd_root,"/1_Active/Yukon/Data & Assessments/yukon-status/HBM-and-status"))
  
  return(wd_data_regions)
}




