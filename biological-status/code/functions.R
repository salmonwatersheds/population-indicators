
#'******************************************************************************
#' The goal of the script is to produce functions that are used in the 
#' biological-status project.
#' 
#' Data set imported:
#' - data/appendix1.csv : appendix 1 of tech-report
#'******************************************************************************

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

calcSgen <- function(Sgen.hat, theta, Smsy){
  
  # the function returns:
  # - minimum: the value of Sgen.hat that minimises Sgen.optim(Sgen.hat, theta, Smsy)
  # in the interval c(0, Smsy)
  # - objective: the corresponding minimal value of Sgen.optim(Sgen.hat, theta, Smsy)
  # fit <- optimize(f = Sgen.optim, interval = c(0, Smsy), theta = theta, Smsy = Smsy)
  
  fit <- tryCatch({
    optimize(f = Sgen.optim, interval = c(0, Smsy), theta = theta, Smsy = Smsy)
  },
  error = function(e){return(NULL)
    })
  
  if(is.null(fit)){
    warning("Sgen1 is NA because function Sgen.optim() produced an error.")
    return(Sgen1 = NA)
  }
  
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

calcSmsy <- function(a, b){
  
  require(lamW) # for the Lambert-W Function
  
  Smsy <- rep(NA,length(a))
  Smsy[a < 0] <- 0
  
  condition <- a >= 0
  Smsy[condition] <- (1 - lamW::lambertW0(exp(1 - a[condition]))) / b[condition]
  
  # previous edit suggested from Steph but that does not work with vectors
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1704908044745989?thread_ts=1704843807.322429&cid=CJ5RVHVCG
  # if(a < 0){
  #   # there is no such thing as “sustainable yield” if your population has negative growth.
  #   Smsy <- 0
  # }else{
  #   require(lamW) # for the Lambert-W Function
  #   Smsy <- (1 - lamW::lambertW0(exp(1 - a))) / b
  # }
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
# coeff_width_adj <- .1
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
    
    region_i <- gsub("_"," ",regions[i_r])
    if(region_i == "Central coast"){
      region_i <- "Central Coast"
    }
    
    if(regions[i_r] == "Vancouver Island & Mainland Inlets"){
      regionName <- "VIMI"
    }else{
      regionName <- regionName <- gsub(" ","_",region[i_rg])
    }
    
    species <- unique(subset(x = BM_data, subset = region == regions[i_r])$species)
    
    for(i_s in 1:length(species)){
      
      # i_s <- 1
      
      BM_data_sub <- BM_data[BM_data$region == regions[i_r] & BM_data$species == species[i_s],]
      
      CUs <- unique(BM_data_sub$CU)
      cuids <- sapply(X = CUs, FUN = function(cu){
        BM_data_sub$cuid[BM_data_sub$CU == cu][1]
      })
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
        pathFile <- paste0(wd_figures,"/",regionName,"_",species[i_s],
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
        header_text <- c(gsub("_"," ",region_i),species[i_s])
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
            CItype <- c("benchmark_0.25","benchmark_0.75")
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
        legend("topright",paste0("CUID: ",cuids[i_cu]),bty = 'n')
        
        # add the current spawner abundance
        csa <- BM_data_sub_cu$current_spawner_abundance[1]
        if(!is.na(csa)){
          segments(x0 = csa, x1 = csa, y0 = 0, y1 = 4, lty = 2)
        }
        
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

#' Function which computes linear-regression estimates of parameters and MSY 
#' parameters (i.e., the simple linearized Ricker model) and plots. The function 
#' returns a list of the coefficients a, b and sigma.
#'- Nyrs: the number of years for each CU
#'- S and R:  year x CU matrices of fish counts for spawners and recruiters 
#'- LNRS: log(R/S)     BSC: could be created inside the function with R and S to limit the number of parameters to pass in
#'- StNames: the same of the CUs
linRegRicker_fun <- function(S, R, plot_figures = F, verbose = F){
  
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
  }else if(17 <= nCUs & nCUs < 21){
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
    # i <- 1
    Rcu <- R[,i, drop = F]            # remove the NAs that are now present (to address SP's comment in HBSRM.R)
    Rcu <- Rcu[!is.na(Rcu),,drop = F]   
    Scu <- S[,i, drop = F]            
    Scu <- Scu[!is.na(Scu),,drop = F]
    
    # remove rows in both Rcu and Scu that NAs
    Rcu_df <- data.frame(year = as.numeric(rownames(Rcu)),
                         R = Rcu[,1])
    Scu_df <- data.frame(year = as.numeric(rownames(Scu)),
                         S = Scu[,1])
    RScu_df <- merge(x = Rcu_df, y = Scu_df, by = "year", all = T)
    RScu_df <- RScu_df[!is.na(RScu_df$R),]
    RScu_df <- RScu_df[!is.na(RScu_df$S),]
    Rcu <- RScu_df$R
    Scu <- RScu_df$S
    
    # replace 0s by 1 to avoid the lm(log(R/S)~ S) to crash
    Rcu[Rcu == 0] <- 1
    Scu[Scu == 0] <- 1
    
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
    
    if(verbose){
      print(paste0("*** ",CUs[i]," ***"))
      print(do.call(what = cbind,args = list(a = a[i], b = b[i], sigma = sigma[i])))
      print(c("Prod=",Prod))
      print(c("Smsy=",Smsy))
      print(c("Smax=",Smax))
      print(c("Uopt=",Uopt))
      print("")
    }

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
# * This is the current method used in the PSE*
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
# series <- spawnerAbundance
# nBoot <- 5000
# benchmarks <- c(0.25,0.75, 0.5)
modelBoot <- function(
    series, 
    numLags = 1, # numLags is the lag for the autocorrelation; default is just 1 year
    nBoot = 10000, 
    benchmarks = c(0.25, 0.75) # c(0.25, 0.5)
){
  
  if(sum(!is.na(series)) > 1){ # if there is at least two data points (to avoid crashing)
    
    # check if every odd or even year have consistently NAs like for Pink salmons,
    # which generate a error in the ar(). So it that case, remove 
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
    
    #
    benchmarksNames <- paste0("benchmark_",benchmarks)
    
    # Matrix to store bootstrapped values of CI
    HS_benchBoot <- matrix(
      nrow = nBoot, 
      ncol = length(benchmarksNames), 
      dimnames = list(c(1:nBoot), benchmarksNames))
    
    series_noNA <- series[!is.na(series)]
    n_noNA <- length(series_noNA)
    
    # Model bootstrap:
    for(i in 1:nBoot){
      # i <- 1
      # Initialize the simulated time series using the true data for the first
      # 1:numLags points, starting from a random place in the timeseries
      
      j.init <- sample(1:(n_noNA - numLags + 1), 1) # starting point for initialization
      obs.star.log[1:numLags, i] <- log(series_noNA[j.init:(j.init + numLags - 1)])
      
      #older code:
      # j.init <- sample(1:(n - numLags + 1), 1) # starting point for initialization
      # obs.star.log[1:numLags, i] <- log(series[j.init:(j.init + numLags - 1)])
      
      for(j in 1:n){ # For each timepoint in the simulated series
        obs.star.log[(numLags + j), i] <- ar.fit$x.mean + ar.fit$ar %*% (obs.star.log[j:(numLags + j - 1), i] - ar.fit$x.mean) + res.star[j, i]
      } #end j
      
      # obs.star.log[,i]
      
      HS_benchBoot[i, ] <- quantile(exp(obs.star.log[(numLags + 1):(numLags + n), i]), benchmarks, na.rm = TRUE)
    } # end bootstrap loop
    
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
                   benchmarks = benchmarks,
                   benchmarkBoot = HS_benchBoot)
  }else{
    
    ouptup <- list(m = rep(NA,length(benchmarks)), 
                   CI = matrix(NA,nrow = 2, ncol = length(benchmarks)), 
                   simulatedSeries = NA, 
                   benchmarks = benchmarks,
                   benchmarkBoot = NA)
  }
  
  return(ouptup)
}

# Function that returns the name of the regions. This is to ensure that no spelling
# mistakes are make.
regions_fun <- function(){
  
  regions <- data.frame(
    Central_Coast = 'Central Coast',
    Columbia = "Columbia", 
    Fraser = 'Fraser',
    Haida_Gwaii = 'Haida Gwaii',
    Nass = 'Nass',
    Skeena = 'Skeena',
    Northern_Transboundary = "Northern Transboundary",
    VIMI = "Vancouver Island & Mainland Inlets",
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
#' sigma (of the loglikelihood function of the HBSRM) from the Ricker function fit
#' to data.
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
  sig <- exp(theta[3])  # sd of likelihood of HBSRM
  # BSC: here theta[3] is used to account for how noisy this iteration for this
  # CU was in the HBSRM. Putting the exp() is arbitrary but aims at forgiving in
  # the likelihood estimation (?).
  # 
  
  # Compute projected recruits based on Sgen.hat
  Smsy.hat <- Sgen.hat * exp(a - b * Sgen.hat) 
  
  # Calculate residuals and negative log likelihood
  epsilon <- log(Smsy) - log(Smsy.hat)
  nloglike <- - sum(dnorm(x = epsilon, mean = 0, sd = sig, log = TRUE))	
  
  return(nloglike)
}

#' Function that returns a list of two dataframes from the _SRdata.txt for a given 
#' species in a given region.
#' path_file <- fndata[i]  # the path of the _SRdata.text file
#' MinSRpts: the minimum number of year (= data points) required 
#' wd_data: the biological-status/Data folder 
# path_file <- fndata[i_sp]
# wd_data <- NA
# MinSRpts <- 1
SRdata_fun <- function(path_file, wd_data, MinSRpts = 3){
  
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
    CUIDs <- read.csv(paste0(wd_data,"/appendix1.csv"),header = T, 
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
# wd <- wd_data_input
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
    # s <- species[1]
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


#' Function that retrieves and rbind the CSV files generated in the 
#' biological-status/output folder, for the regions and species provided as 
#' vector arguments. The three type of fils being gatehred are (for instance):
#' - Yukon_CK_biological_status.csv
#' - Yukon_CK_benchmarks_summary.CSV
#' - Yukon_CK_benchmarks_HS_percentiles_summary.csv
#' Arguments:
#' - wd_output: where the files are.
#' - pattern:  "biological_status", "benchmarks_summary", "benchmarks_HS_percentiles_summary"
#' - region: vector of regions
#' - species: vector of species acronyms (in species_acronym_fun())
#' - species_all: T or F, if T, takes precedence on whatever is specified for species
rbind_biologicalStatusCSV_fun <- function(pattern,wd_output,region,species = NA,
                                          species_all = F, 
                                          term_exclude = NA, term_include = NA){
  
  region <- gsub(" ","_",region)
  
  biological_status_df <- NULL
  for(rg in region){
    # rg <- region[3]
    
    if(rg == "Vancouver_Island_&_Mainland_Inlets"){
      rg <- "VIMI"
    }
    
    # returns all the files with pattern rg and "biological_status"
    list_files <- list.files(path = paste0(wd_output))
    cond <- grepl(rg,list_files) & grepl(pattern,list_files)
    if(!is.na(term_exclude)){
      cond <- cond & !grepl(term_exclude,list_files)
    }
    if(!is.na(term_include)){
      cond <- cond & grepl(term_include,list_files)
    }
    list_files <- list_files[cond]
    
    if(pattern == "biological_status"){
      list_filesToRemove <- list_files[grepl("SH_percentiles",list_files)]
      list_files <- list_files[! list_files %in% list_filesToRemove]
    }
    
    # select the species required
    if(!species_all & !is.na(species[1])){
      list_files<- sapply(X = species, FUN = function(sp){list_files[grepl(pattern = sp,x = list_files)]})
      list_files <- unlist(list_files_bis) # this gets rides of the empty elements
    }
    
    if(length(list_files) == 0){
      print(paste0("*** There is no data in for salmon in ",rg," ***"))
    }else{
      # import these files and rbind them
      for(i_f in 1:length(list_files)){
        # i_f <- 1
        fileHere <- read.csv(file = paste(wd_output,list_files[i_f],sep="/"),header = T)
        
        if(is.null(biological_status_df)){
          biological_status_df <- fileHere
        }else{
          biological_status_df <- rbind(biological_status_df,fileHere)
        }
      }
    }
  }
  return(biological_status_df)
}

#' Function that takes two matrices of same dimensions and with mathing 
#' column and ronames ; typically the S and R matrices. It is possible that
#' for data are missing in one matrix and not in another one for a same CU.
#' The function remove the corresponding rows in the column of the matrix 
#' that do not have missing data. The function was designed for the HBSR 
#' modelling.
cuSR_removeNA_fun <- function(R,S){
  
  colname_S <- colnames(S)
  colname_R <- colnames(R)
  if(identical(colname_R,colname_S) & identical(dim(R),dim(S))){
    for(i_c in 1:ncol(S)){
      # i_c <- 1
      colnameHere <- colname_S[i_c]
      S_here <- S[,colnameHere]
      R_here <- R[,colnameHere]
      R_here[is.na(S_here)] <- NA
      S_here[is.na(R_here)] <- NA
      S[,colnameHere] <- S_here
      R[,colnameHere] <- R_here
    }
    
  }else{
    if(!identical(colname_R,colname_S)){
      print("ERROR: Column names differ between S and R.")
    }
    if(identical(dim(R),dim(S))){
      print("ERROR:dimesions  differ between S and R.")
    }
  }
  
  # col <- 3
  # cbind(S[,col],R[,col])
  output <- list(R,S)
  names(output) <- c("R","S")
  return(output)
}

#' Function to produce the figure associated with the output of the 
#' sensitivity_nBoot_HSPercentBM_fun() or ??? function.
# benchSummary_nBoot_l <- benchSummary_nBoot_ll[[2]]
sensitivity_nBoot_HSPercentBM_figure_fun <- function(benchSummary_nBoot_l, 
                                                     lwd_seg = 2, cex_pt = 1.8,
                                                     xaxt = "n"){
  
  
  
  nBoot_v <- as.numeric(names(benchSummary_nBoot_l))
  
  y_up_m <- lapply(X = benchSummary_nBoot_l,FUN = function(bs){bs[2,c("m","CI025","CI975")]})
  y_up_m <- as.matrix(do.call(rbind,y_up_m)) # sapply() does not report a matrix that can be used by min, max, etc.
  y_lo_m <- lapply(X = benchSummary_nBoot_l,FUN = function(bs){bs[1,c("m","CI025","CI975")]})
  y_lo_m <- as.matrix(do.call(rbind,y_lo_m))
  
  x <- 1:length(nBoot_v)
  
  ymin <- min(y_lo_m)
  ymax <- max(y_up_m)
  
  digit_nb <- floor(log10(ymin)) + 1
  magnitude <- as.numeric(paste0(1,paste0(rep(0,digit_nb - 1),collapse = "")))
  
  floorMin <- floor(ymin/magnitude)*magnitude
  ceilingMax <- ceiling(ymax/magnitude)*magnitude
  
  interval <- round((ceilingMax - floorMin)/5)
  
  intevals <- c(floorMin)
  for(i in 1:5){
    intevals <- c(intevals,tail(intevals,1) + interval)
  }
  
  # par(mar = c(4.5,4.5,.5,.5))
  
  if(xaxt == "n"){
    xlab <- ""
  }else{
    xlab <- "Number of iterations"
  }
  
  plot(x = x, y = NULL, ylim = c(ymin,ymax),xlim = c(min(x) - 1/length(x),max(x)+1/length(x)),
       ylab = "Benchmarks (nb. of fish)", xlab = xlab, xaxt = "n", col = "white")
  if(xaxt != "n"){
    axis(side = 1,at = x,labels = nBoot_v)
  }
  segments(x0 = rep(0,length(intevals)), x1 = rep(tail(x,1) + 1,length(intevals)),
           y0 = intevals, y1 = intevals, lwd = lwd_seg, col = "grey80")
  
  points(x = x + 0.1/length(x), y = y_up_m[,"m"], pch = 16, cex = cex_pt)
  points(x = x - 0.1/length(x), y = y_lo_m[,"m"], pch = 16, cex = cex_pt)
  segments(x0 = x - 0.1/length(x), x1 = x - 0.1/length(x), 
           y0 = y_lo_m[,"CI025"],y1 = y_lo_m[,"CI975"], 
           lwd = lwd_seg)
  segments(x0 = x + 0.1/length(x), x1 = x + 0.1/length(x),
           y0 = y_up_m[,"CI025"],y1 = y_up_m[,"CI975"],
           lwd = lwd_seg)
}

#' Function that takes a list of lists of data frames; the latter (i.e., the lists
#' of dataframes) are generated by sensitivity_nBoot_HSPercentBM_fun() for each 
#' number of iterations in nBoot_v for the bootstrapping methods in modelBoot().
#' The head list names correspond to 1:nloops (i.e., the number of time the process
#' was repeated to obtained CV fof the benchmark medians, and upper and lower CI).
sensitivity_nBoot_HSPercentBM_CV_fun <- function(benchSummary_nBoot_ll){
  
  nBoot_v <- names(benchSummary_nBoot_ll[[1]])
  nloops <- max(as.numeric(names(benchSummary_nBoot_ll)))
  
  benchSummary_nBoot_CV <- NULL
  for(nb in nBoot_v){
    # nb <- nBoot_v[1]
    benchSummary_nBoot_nb <- NULL
    for(nl in 1:nloops){
      # nl <- nloops[1]
      bs_df <- benchSummary_nBoot_ll[[nl]][[nb]]
      benchSummary_nBoot_nb <- rbind(benchSummary_nBoot_nb,bs_df)
    }
    benchSummary_nBoot_CVHere <- benchSummary_nBoot_nb[1:2,c("region","species","CU","benchmark","m","CI025","CI975","nBoot")]
    for(bench in benchSummary_nBoot_CVHere$benchmark){
      # bench <- benchSummary_nBoot_CVHere$benchmark[1]
      for(var in c("m","CI025","CI975")){
        # var <- "m"
        valsHere <- benchSummary_nBoot_nb[,var][benchSummary_nBoot_nb$benchmark == bench]
        CVHere <- round(sd(valsHere)/mean(valsHere),3)
        benchSummary_nBoot_CVHere[,var][benchSummary_nBoot_CVHere$benchmark == bench] <- CVHere
      }
    }
    benchSummary_nBoot_CV <- rbind(benchSummary_nBoot_CV,benchSummary_nBoot_CVHere)
  }
  benchSummary_nBoot_CV$nloops <- nloops
  benchSummary_nBoot_CV$dataPointNb <- benchSummary_nBoot_ll[[1]][[1]]$dataPointNb
  return(benchSummary_nBoot_CV)
}

#' Function to conduct the process of calculating the percentile benchmarks based on
#' historical spawner abundance with different number of loops (nBoot_v).
#' The function allows to run the code in parallel.
#' TODO: test if it works for Mac and Linux OS
sensitivity_nBoot_HSPercentBM_fun <- function(region,species,CU,
                                              cuspawnerabundance,
                                              nBoot_v = c(1000,10000),
                                              benchmarks = c(0.25, 0.75),
                                              cores_nb = 1){
  
  require(parallel)
  
  # Documentation
  # https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
  
  if(Sys.info()["sysname"] == "Windows"){
    
    # start a cluster
    cl <- makeCluster(cores_nb)
    
    # export the different object to the cluster
    clusterExport(cl, "region")
    clusterExport(cl, "species")
    clusterExport(cl, "CU")
    clusterExport(cl, "cuspawnerabundance")
    clusterExport(cl, "nBoot_v")
    clusterExport(cl, "benchmarks")
    clusterExport(cl, "benchSummary_HSPercent_fun")
    clusterExport(cl, "modelBoot")
    
    # clusterEvalQ(cl, region)
    
    # Evaluate packages
    # clusterEvalQ(cl, {
    #   library(ggplot2)
    #   library(stringr)
    # })
    
    benchSummary_nBoot_l <- parLapply(cl = cl, X = nBoot_v, fun = function(nboot){
      benchSummary_HSPercent_fun(region = region,species = species,CU = CU,
                                 cuspawnerabundance = cuspawnerabundance,
                                 nBoot = nboot)
    })
  }else{ 
    #' TODO: need to be tested with Mac and Linux OS
    
    benchSummary_nBoot_l <- mclapply(X = nBoot_v,FUN = function(nboot){
      benchSummary_HSPercent_fun(region = region,species = species,CU = CU,
                                 cuspawnerabundance = cuspawnerabundance,
                                 nBoot = nboot)
    },mc.cores = cores_nb)
  }
  
  names(benchSummary_nBoot_l) <- nBoot_v
  
  return(benchSummary_nBoot_l)
}

#' Function that uses the modelBoot() function to determine the benchmarks for
#' spawner abundance data with the percentile approach. It returns a dataframe 
#' with the the median and CI of the lower and upper benchmarks.
benchSummary_HSPercent_fun <- function(region,species,CU,
                                       cuspawnerabundance,
                                       nBoot = 10000,
                                       numLags = 1,
                                       benchmarks = c(0.25, 0.75)){
  
  spawnerAbundance <- cuspawnerabundance$estimated_count[cuspawnerabundance$region == region & 
                                                           cuspawnerabundance$species_name == species &
                                                           cuspawnerabundance$cu_name_pse == CU]
  
  modelCI <- modelBoot(series = spawnerAbundance, 
                       numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                       nBoot = nBoot,
                       benchmarks = benchmarks)
  
  benchSummary <- data.frame(region = region,
                             species = species,
                             CU = rep(CU,2),
                             benchmark = c('lower','upper'),
                             method = rep('HS_percentiles',2))
  benchSummary$m <- modelCI$m
  benchSummary$CI025 <- modelCI$CI[,1]
  benchSummary$CI975 <- modelCI$CI[,2]
  benchSummary$benchmarks <- rep(paste(benchmarks,collapse = "-"),2)
  benchSummary$dataPointNb <- sum(!is.na(spawnerAbundance))
  benchSummary$nBoot <- nBoot
  
  return(benchSummary)
}


#' Function that takes the object produced by sensitivity_nBoot_HSPercentBM_CV_fun(),
#' (i.e., a data frame of CV values for the median and CI)
# benchSummary_CV <- benchSummary_nBoot_CV
# benchSummary_CV <- benchSummary_nbYear_CV
sensitivity_HSPercentBM_CV_figure_fun <- function(benchSummary_CV,
                                                  wd_figures,
                                                  printFig = F,
                                                  xvar = c("nBoot","dataPointNb")){
  xvar <- xvar[1]
  
  region <- benchSummary_CV$region[1]
  species <- benchSummary_CV$species[1]
  CU <- benchSummary_CV$CU[1]
  dataPointNb <- benchSummary_CV$dataPointNb[1]
  
  main <- paste(region,species,CU,sep = " - ")
  if(xvar == "nBoot"){
    main <- paste0(main," (",dataPointNb," data points)")
  }

  
  if(printFig){
    nameFig <- paste(region,species,CU,sep = " - ")
    nameFig <- gsub(" - ","_",nameFig)
    nameFig <- gsub(" ","_",nameFig)
    nameFig <- gsub("/","-",nameFig)
    jpeg(paste0(wd_figures,"/Sensitivity_",xvar,"_",nameFig,".jpg"),
         width = 25, height = 20, units = "cm", res = 300)
  }
  
  layout(matrix(1:2,nrow = 2), heights = c(1.2,1))
  for(bm in c("upper","lower")){
    # bm <- "lower"
    DS <- benchSummary_CV[benchSummary_CV$benchmark == bm,]
    
    x <- 1:nrow(DS)
    ymin <- 0
    ymax <- max(as.matrix(DS[,c("m","CI025","CI975")]))
    
    offset <- 0.7/length(x)
    
    cex_pt <- 1.5
    lwd_pt <- 1.5
    
    if(bm == "upper"){
      side1 <- 0.5
      side3 <- 8
      xaxt <- "n"
    }else{
      side1 <- 4.5
      side3 <- 0.5
      xaxt <- "s"
    }
    
    par(mar = c(side1,4.5,side3,0.5))
    plot(x = x, y = NULL, 
         ylim = c(ymin,ymax), xlim = c(min(x) - 1/length(x),max(x)+1/length(x)), 
         col = "white", las = 1, xaxt = "n",
         ylab = "CV", xlab = "")
    if(bm == "upper"){
      x_top <- lapply(X = x, FUN = function(x){c(x - offset,x,x + offset)})
      x_top <- unlist(x_top)
      axis(side = 3, at = x_top, labels = rep(c("CI025","m","CI975"),nrow(DS)),las = 2)
      mtext(main,side = 3,line = 5,cex = 1.5)
    }else{
      axis(side = 1, at = x, labels = DS[,xvar])
      if(xvar == "nBoot"){
        mtext("Number of iteration the bootstrap method",side = 1, line = 2.5)
      }else if(xvar == "dataPointNb"){
        mtext("Number data points used in time series",side = 1, line = 2.5)
      }
    }
    lines(x = x, y = DS$m, pch = 16, lwd = lwd_pt)
    lines(x = x - offset, y = DS$CI025, cex = cex_pt, lwd = lwd_pt)
    lines(x = x + offset, y = DS$CI975, cex = cex_pt, lwd = lwd_pt)
    points(x = x, y = DS$m, pch = 16, cex = cex_pt, lwd = lwd_pt)
    points(x = x - offset, y = DS$CI025, pch = 24, cex = cex_pt, lwd = lwd_pt, bg = "black")
    points(x = x + offset, y = DS$CI975, pch = 25, cex = cex_pt, lwd = lwd_pt, bg = "black")
    legend("topright",paste(bm,"benchmark"),bty="n")
    if(bm == "lower"){
      legend("bottomleft",paste(benchSummary_CV$nloops[1],"iterations"),bty="n")
    }
  }
  if(printFig){
    dev.off()
  }
}

#' Function that takes a dataframe produced by sensitivity_nbYear_HSPercentBM_CV_fun()
#' and calculate the CV for the median and CI of the lower and upper benchmarks
#' for the given CU. It is similar to sensitivity_nBoot_HSPercentBM_CV_fun().
sensitivity_nbYear_HSPercentBM_CV_fun <- function(benchSummary_nbYear_df){
  
  dataPtNb <- sort(unique(benchSummary_nbYear_df$dataPointNb))
  
  benchSummary_nbYear_CV <- NULL
  for(dpn in dataPtNb){
    # dpn <- dataPtNb[1]
    benchSummary_nbYear_df_dpn <- benchSummary_nbYear_df[benchSummary_nbYear_df$dataPointNb == dpn,]
    benchSummary_nbYear_CV_here <- benchSummary_nbYear_df_dpn[1:2,c("region","species","CU","benchmark","m","CI025","CI975","dataPointNb","nBoot")]
    for(bench in benchSummary_nbYear_CV_here$benchmark){
      # bench <- benchSummary_nbYear_CV_here$benchmark[1]
      for(var in c("m","CI025","CI975")){
        # var <- "m"
        valsHere <- benchSummary_nbYear_df_dpn[,var][benchSummary_nbYear_df_dpn$benchmark == bench]
        CVHere <- round(sd(valsHere)/mean(valsHere),3)
        benchSummary_nbYear_CV_here[,var][benchSummary_nbYear_CV_here$benchmark == bench] <- CVHere
      }
    }
    benchSummary_nbYear_CV <- rbind(benchSummary_nbYear_CV,benchSummary_nbYear_CV_here)
  }
  benchSummary_nbYear_CV$nloops <- max(benchSummary_nbYear_df$iteration)
  
  return(benchSummary_nbYear_CV)
}

#' Function to conduct the process of calculating the percentile benchmarks based on
#' historical spawner abundance with different number data points in the time series.
#' It produced the original benchSummary dataframe and also the one where data 
#' points are sampled in the original series at a number = yr_nb. Note that the 
#' data points sampled by chunk to preserve the sequence.
#' The function allows to run the code in parallel.
#' TODO: test if it works for Mac and Linux OS
sensitivity_nbYear_HSPercentBM_fun <- function(region,species,CU,
                                               nloop = 100,
                                               cuspawnerabundance,
                                               yr_nb = c(3,5,10,15,20),
                                               nBoot = 10000,
                                               benchmarks = c(0.25, 0.75),
                                               numLags = 1,
                                               cores_nb){
  if(length(CU) > 1){
    print("Only one CU at a time can be provided and the first value is retained here.")
  }
  region <- region[1]
  species <- species[1]
  CU <- CU[1]
  
  spawnerAbundance <- cuspawnerabundance$estimated_count[cuspawnerabundance$region == region & 
                                                           cuspawnerabundance$species_name == species &
                                                           cuspawnerabundance$cu_name_pse == CU]
  
  modelCI <- modelBoot(series = spawnerAbundance, 
                       numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                       nBoot = nBoot,
                       benchmarks = benchmarks)
  
  benchSummary_ref <- data.frame(region = region,
                                 species = species,
                                 CU = rep(CU,2),
                                 benchmark = c('lower','upper'),
                                 method = rep('HS_percentiles',2))
  benchSummary_ref$m <- modelCI$m
  benchSummary_ref$CI025 <- modelCI$CI[,1]
  benchSummary_ref$CI975 <- modelCI$CI[,2]
  benchSummary_ref$benchmarks <- rep(paste(benchmarks,collapse = "-"),2)
  benchSummary_ref$dataPointNb <- sum(!is.na(spawnerAbundance))
  benchSummary_ref$nBoot <- nBoot 
  benchSummary_ref$iteration <- 0
  
  # add the original number of data points
  yr_nb <- c(yr_nb,sum(!is.na(spawnerAbundance)))
  yr_nb <- sort(yr_nb)
  
  require(parallel)
  
  # Documentation
  # https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
  
  if(Sys.info()["sysname"] == "Windows"){
    
    cl <- makeCluster(cores_nb)
    
    # export the different object to the cluster
    clusterExport(cl, "region")
    clusterExport(cl, "species")
    clusterExport(cl, "CU")
    clusterExport(cl, "spawnerAbundance")
    clusterExport(cl, "numLags")
    clusterExport(cl, "nBoot")
    clusterExport(cl, "nloop")
    clusterExport(cl, "benchmarks")
    clusterExport(cl, "modelBoot")
    clusterExport(cl, "yr_nb")
    
    modelCI_l_i <- parLapply(cl = cl, X = 1:nloop, fun = function(i){
      # i <- 1
      
      modelCI_l <- lapply(X = yr_nb, FUN = function(yrnb){
        # yrnb <- yrnb[1]
        spawnerAbundance_noNA <- spawnerAbundance[!is.na(spawnerAbundance)]
        
        initialPt <- sample(x = 1:(length(spawnerAbundance_noNA) - yrnb + 1),size = 1)
        spawnerAbundanceHere <- spawnerAbundance_noNA[initialPt:(initialPt + yrnb - 1)]
        
        modelCI <- modelBoot(series = spawnerAbundanceHere, 
                             numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                             nBoot = nBoot,
                             benchmarks = benchmarks)
        
        benchSummary <- data.frame(region = region,
                                   species = species,
                                   CU = rep(CU,2),
                                   benchmark = c('lower','upper'),
                                   method = rep('HS_percentiles',2))
        
        benchSummary$m <- modelCI$m
        benchSummary$CI025 <- modelCI$CI[,1]
        benchSummary$CI975 <- modelCI$CI[,2]
        benchSummary$benchmarks <- rep(paste(benchmarks,collapse = "-"),2)
        benchSummary$dataPointNb <- yrnb
        benchSummary$nBoot <- nBoot
        benchSummary$iteration <- i
        return(benchSummary)
      })
      modelCI_l <- do.call(rbind,modelCI_l)
      return(modelCI_l)
    })
  }else{
    
    modelCI_l <- mclapply(X = 1:nloop,FUN = function(i){
      spawnerAbundance_noNA <- spawnerAbundance[!is.na(spawnerAbundance)]
      spawnerAbundanceHere <- sample(x = spawnerAbundance_noNA, size = yrnb, replace = F)
      
      modelCI <- modelBoot(series = spawnerAbundanceHere, 
                           numLags = numLags, # numLags is the lag for the autocorrelation; default is just 1 year
                           nBoot = nBoot,
                           benchmarks = benchmarks)
      
      benchSummary <- data.frame(region = region,
                                 species = species,
                                 CU = rep(CU,2),
                                 benchmark = c('lower','upper'),
                                 method = rep('HS_percentiles',2))
      
      benchSummary$m <- modelCI$m
      benchSummary$CI025 <- modelCI$CI[,1]
      benchSummary$CI975 <- modelCI$CI[,2]
      benchSummary$benchmarks <- rep(paste(benchmarks,collapse = "-"),2)
      benchSummary$dataPointNb <- yrnb
      benchSummary$nBoot <- nBoot
      benchSummary$iteration <- i
      return(benchSummary)
    })
    modelCI_l <- do.call(rbind,modelCI_l)
  }
  modelCI_l_i <- do.call(rbind,modelCI_l_i)
  modelCI_l_i <- rbind(benchSummary_ref,modelCI_l_i)
  return(modelCI_l_i)
}

#' Function that ...
#' - biological_status_df: dataframe returned by rbind_biologicalStatusCSV_fun() 
#' and with argument pattern = "biological_status" or "biological_status_SH_percentiles".
#' -  
# wd <- wd_figures
# biological_status_df <- biological_status_merged[condition_HBSRM,]
# group_var <- c("species","region")[2]
biological_status_compare_fun <- function(biological_status_df,wd,printFig = F,
                                          group_var = c("region","species")){
  
  
  # head(biological_status_df)
  
  # identify the dataset:
  colBench <- colnames(biological_status_df)[grepl("status_",colnames(biological_status_df))]
  
  if(grepl("Smsy",colBench)[1]){
    status1 <- colBench[grepl("Smsy_",colBench)]
    status2 <- colBench[grepl("Smsy80_",colBench)]
    figName <- "_Smsy_Smsy80_"
  }else if(grepl("[P|p]ercent",colBench)[1]){
    status1 <- colBench[grepl("[P|p]ercent_075",colBench)]
    status2 <- colBench[grepl("[P|p]ercent_05",colBench)]
    figName <- "Percentiles_75_50_"
  }
  
  # remove row with NAs
  biological_status_df <- biological_status_df[!is.na(biological_status_df[,colBench[1]]),] 
  
  colToRemove_biostatus <- c("CU_pse","CU_dfo","genLength_available","comment")
  biological_status_df <- biological_status_df[,!colnames(biological_status_df) %in% colToRemove_biostatus]
  
  # CUs that have contrasting biological status between Smsy80 and Smsy:
  colnamesSelect <- c("region","species","CU",colBench)
  
  biological_status_df$status1 <- sapply(X = 1:nrow(biological_status_df), 
                                         FUN = function(r){
                                           # r <- 1
                                           slice <- biological_status_df[r,status1]
                                           # out <- c("red","amber","green")[slice == max(slice)]
                                           out <- c("poor","fair","good")[slice == max(slice)]
                                           return(out)
                                         })
  
  biological_status_df$status2 <- sapply(X = 1:nrow(biological_status_df), 
                                         FUN = function(r){
                                           # r <- 1
                                           slice <- biological_status_df[r,status2]
                                           #out <- c("red","amber","green")[slice == max(slice)]
                                           out <- c("poor","fair","good")[slice == max(slice)]
                                           return(out)
                                         })
  
  # nrow(biological_status_df[biological_status_df$status1 != biological_status_df$status2,])
  
  # Figure
  
  if(! group_var %in% c("region","species")){
    print(paste("The argument group_var differ from possible values:",paste(c("region","species"), collapse = ", ")))
    print("region is selected.")
    group_var <- "region"
  }
  
  table_var <- NULL
  
  group_var_val <- unique(biological_status_df[,group_var])
  for(var in group_var_val){
    # var <- group_var_val[1]
    biological_status_df_cut <- biological_status_df[biological_status_df[,group_var] == var,]
    
    tableHere <- data.frame(variable = var,
                            n_same = sum(biological_status_df_cut$status1 == biological_status_df_cut$status2),
                            n_diff = sum(biological_status_df_cut$status1 != biological_status_df_cut$status2))
    
    colnames(tableHere)[1] <- group_var
    
    
    if(is.null(table_var)){
      table_var <- tableHere
    }else{
      table_var <- rbind(table_var,tableHere)
    }
  }
  
  table_var_m <- as.matrix(table_var[,c(2:3)])
  rownames(table_var_m) <- table_var[,group_var]
  colours <- rainbow(n = nrow(table_var_m))
  if(printFig){
    jpeg(paste0(wd_figures,"/comparison_bioStatus",figName,group_var,".jpg"), 
         width = 20,height = 15, units = "cm", res = 300)
  }
  
  if(group_var == "species"){
    species_acronym_df <- species_acronym_fun()
    rownamesSp <- sapply(X = table_var[,group_var], FUN = function(sp){
      return(species_acronym_df$species_name[species_acronym_df$species_acro == sp][1])
    })
    rownames(table_var_m) <- rownamesSp
  }
  
  par(mar=c(5,4.5,3,0.5))
  barplot(height = table_var_m,
          main = paste0("Comparison by ",group_var),
          ylab = "Number of CUs", xlab = "Biological status difference",
          col = colours, las = 1,
          names.arg = c("Same","Different"))
  
  if(group_var == "species"){
    var_legend <- paste(rev(rownames(table_var_m)),"                 ")
  }else{
    var_legend <- rev(rownames(table_var_m))
  }
  legend("topright",var_legend,fill = rev(colours), bty = "n")
  
  if(printFig){
    dev.off()
  }
}

#' Function to return a dataframe of the CUs that have high exploitation rate or
#' low production rates, as well as a final call on keeping or removing the CUs 
#' depending of their biostatus: the one with already a red/poor status are kept
#' (i.e. Clare's 8th rule). The function requires the biological_status_SH_percentiles
#' dataset because the 8th rules is applied to CUs evaluated with the percentile
#' method (and because all the CUs evaluated with the HBSRM method are in this 
#' dataset as well).
#' - biological_status_HSPercent_df: data frame of the biostatus probabilities 
#' obtained with percentile method. It is returned by the function 
#' biological_status_HSPercent_df() and the argument pattern = "biological_status_SH_percentiles".
cu_highExploit_lowProd_fun <- function(biological_status_percentile = NA,
                                       conservationunits_decoder = NA,
                                       wd_pop_indic_data_input_dropbox = NA,
                                       region = NA, wd_output = NA, species_all = T,
                                       export_csv = F){
  
  if(is.na(biological_status_percentile)[1]){
    pattern <- "biological_status_percentiles"
    biological_status_percentile <- rbind_biologicalStatusCSV_fun(pattern = pattern,
                                                                    wd_output = wd_output,
                                                                    region = region,
                                                                    species_all = species_all)
  }
  
  if(is.na(conservationunits_decoder)[1]){
    conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                       fromDatabase = F,
                                                       update_file_csv = F,
                                                       wd = wd_pop_indic_data_input_dropbox)
  }
  
  # List CUs with high exploitation/low productivity & cyclic dominance
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1700673066049189?thread_ts=1700604709.505309&cid=CJ5RVHVCG
  
  # To find the corresponding regions from conservationunits_decoder
  # sapply(X = highExploit_lowProd$CU_name, FUN = function(cu){
  #   out <- conservationunits_decoder$region[conservationunits_decoder$cu_name_pse == cu]
  # })
  
  # To find the corresponding species from conservationunits_decoder
  # sapply(X = highExploit_lowProd$CU_name, FUN = function(cu){
  #   out <- conservationunits_decoder$species_name[conservationunits_decoder$cu_name_pse == cu]
  # })
  
  # To find the corresponding cuid from conservationunits_decoder
  # sapply(X = highExploit_lowProd$CU_name, FUN = function(cu){
  #   out <- conservationunits_decoder$cuid[conservationunits_decoder$cu_name_pse == cu]
  # })
  
  # List of CU with high exploitation or low productivity
  cuid_here <- c(705,749,707,709,708,303,315,322,321)
  
  # Find relevant information in conservationunits_decoder
  cond <- conservationunits_decoder$cuid %in% cuid_here
  highExploit_lowProd <- conservationunits_decoder[cond,c("region","species_name","cuid","cu_name_pse")]
  
  #' Modification 1:
  #' All Chinook in Fraser and VIMI are low productivity/high exploitation
  #' As of Population meeting of 23/04/2014
  #' https://docs.google.com/document/d/1lw4PC7nDYKYCxb_yQouDjLcoWrblItoOb9zReL6GmDs/edit?usp=sharing
  cond <- conservationunits_decoder$species_name == "Chinook" &
    conservationunits_decoder$region %in% c("Vancouver Island & Mainland Inlets",
                                            "Fraser")
  highExploit_lowProd <- rbind(highExploit_lowProd,
                               conservationunits_decoder[cond,c("region","species_name","cuid","cu_name_pse")])
  
  highExploit_lowProd <- unique(highExploit_lowProd)
  
  # Export list if not already present in wd_pop_indic_data_input_dropbox
  if(export_csv){
    highExploit_lowProd_2 <- import_mostRecent_file_fun(wd = wd_pop_indic_data_input_dropbox, 
                                                        pattern = "CUs_highExploitation_lowProductivity")
    
    if(is.na(highExploit_lowProd_2)[1]){
      write.csv(highExploit_lowProd,paste0(wd_pop_indic_data_input_dropbox,
                                           "/CUs_highExploitation_lowProductivity.csv"),
                row.names = F)
      
    }else{ # check that the file present contains the same cuid
      cond_1 <- all(highExploit_lowProd$cuid %in% highExploit_lowProd_2$cuid)
      cond_2 <- all(highExploit_lowProd_2$cuid %in% highExploit_lowProd$cuid)
      
      if(!cond_1 | !cond_2){
        print(paste0("CUs_highExploitation_lowProductivity.csv in ",wd_pop_indic_data_input_dropbox," differs from list in the code."))
        print("File not exported to avoid risking losing information. Issue must be resolved manually.")
        
      }
    }
  }

  highExploit_lowProd$biostatus_percentile <- NA
  highExploit_lowProd$toRemove <- T
  for(i in 1:nrow(highExploit_lowProd)){
    # i <- 1
    sp <- highExploit_lowProd$species[i]
    cu <- highExploit_lowProd$cu_name_pse[i]
    biological_status_percentileHere <- biological_status_percentile[biological_status_percentile$cu_name_pse == cu,]
    if(nrow(biological_status_percentileHere) > 1){
      print(biological_status_percentileHere)
      biological_status_percentileHere <- biological_status_percentileHere[biological_status_percentileHere$species == sp,]
    }
    status <- biological_status_percentileHere$status_percent075
    highExploit_lowProd$biostatus_percentile[i] <- status
    if(!is.na(status)){
      if(status %in% c("red","poor")){ #' New rule from Claire:
        highExploit_lowProd$toRemove[i] <- F
      }
    }
    # print(biological_status_percentileHere$status_percent075)
  }
  return(highExploit_lowProd)
}

#' Function to return the a dataframe of the extinct CUs.
# See Population Analysis running notes Google Doc from Dec 12 2023 for list 
# given by Eric.
# https://docs.google.com/document/d/1gNmJxA4Us90W8DBf-QS6KfkjnOE71FMZUhFSPo7posg/edit?usp=sharing --> WHAT IS THAT?
# https://docs.google.com/document/d/1gNmJxA4Us90W8DBf-QS6KfkjnOE71FMZUhFSPo7posg/edit#heading=h.7jm714ax41xs
#' TODO: this list should be provided by pulling from another dataset (380?)
#' cf. PSE Data Update 2024-09-05 meeting
#' --> have a csv file 
cu_extinct_fun <- function(write_file = F, wd = NA, 
                           conservationunits_decoder = NA, 
                           wd_pop_indic_data_input_dropbox = NA){
  
  # To find the corresponding regions from conservationunits_decoder
  # sapply(X = cuid, FUN = function(cu){
  #   out <- conservationunits_decoder$region[conservationunits_decoder$cuid == cu]
  # })
  
  # To find the corresponding species from conservationunits_decoder
  # sapply(X = cuid, FUN = function(cu){
  #   out <- conservationunits_decoder$species_name[conservationunits_decoder$cuid == cu]
  # })
  
  # To find the corresponding cuid from conservationunits_decoder
  # sapply(X = cuid, FUN = function(cu){
  #   out <- conservationunits_decoder$cu_name_pse[conservationunits_decoder$cuid == cu]
  # })
  
  # 
  if(is.na(conservationunits_decoder)[1]){
    conservationunits_decoder <- datasets_database_fun(nameDataSet = datasetsNames_database$name_CSV[1],
                                                       fromDatabase = F,
                                                       update_file_csv = F,
                                                       wd = wd_pop_indic_data_input_dropbox)
  }
  
  #
  cuid_list <- c(760,756,757,753,758,761,763,759,936)
  cond <- conservationunits_decoder$cuid %in% cuid_list
  cu_extinct <- conservationunits_decoder[cond,c("region","species_name","cu_name_pse","cuid")]
  cu_extinct$source <- cu_extinct$comment <- NA
  cu_extinct$keep <- T
  
  # Updates: 2024-03-19
  # cuids 753, 756 and 757 are no longer extinct.
  # https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1710887067544039
  cuid_toRemove <- c(753,756,757)
  # cu_extinct <- cu_extinct[!cu_extinct$cuid %in% cuid_toRemove,]
  cond <- cu_extinct$cuid %in% cuid_toRemove
  cu_extinct$comment[cond] <- "no longer extinct due to updated COSEWIC status"
  cu_extinct$keep[cond] <- F
  cu_extinct$source[cond] <- "https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1710887067544039"
  
  # Update 2024-09-05
  # add cuid 936 to list (Village Bay (extirpated)
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1600364418002800
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1725559923069129
  # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1725564232280239?thread_ts=1725559923.069129&cid=CJ5RVHVCG
  # --> already in the list. BUT not in the decoder, normal? --> it was binned.
  # cond <- conservationunits_decoder$cuid == 936
  # conservationunits_decoder[cond,c("region","species_name","species_abbr","cuid","cu_name_pse")]
  cuid_toRemove <- c(936)
  # cu_extinct <- cu_extinct[!cu_extinct$cuid %in% cuid_toRemove,]
  cond <- cu_extinct$cuid %in% cuid_toRemove
  cu_extinct$comment[cond] <- "binned as feedback from the PSAC (Carrie Holt)"
  cu_extinct$keep[cond] <- T     # we keep it because it is still extinct, it will be removed in the workflow
  # because it is not in conservation_units_decoder.csv
  cu_extinct$source[cond] <- "https://salmonwatersheds.slack.com/archives/CKNVB4MCG/p1710887067544039"
  
  # Update 2024-09-12
  # cuid 761 in "Endangered" in last COSEWIC 
  # Dataset380_Sep52024.xlsx
  # https://www.dropbox.com/scl/fi/08xe29reciifybwpng1v9/Dataset380_Sep52024.xlsx?rlkey=r54b2mrz7rcjsh9qsnndgtngz&dl=0
  # cf. Population Team meeting note from that date
  cuid_toRemove <- c(761)
  cond <- cu_extinct$cuid %in% cuid_toRemove
  cu_extinct$comment[cond] <- "Listed as 'endangered' in COSEWIC"
  cu_extinct$keep[cond] <- F   
  cu_extinct$source[cond] <- "https://www.dropbox.com/scl/fi/08xe29reciifybwpng1v9/Dataset380_Sep52024.xlsx?rlkey=r54b2mrz7rcjsh9qsnndgtngz&dl=0"
  
  
  if(write_file){
    if(is.na(wd)){
      print("File not written, please provide working directory (wd)")
    }else{
      date <- Sys.Date()
      write.csv(cu_extinct,paste0(wd,"/CUs_extinct_list_",date,".csv"),row.names = F)
    }
  }
  return(cu_extinct)
}


#' Function to return the current spawner abundance of CUs and several associated 
#' information about it (e.g. start and end yer of available data, number of data
#' points available, etc.). When yearCurrentAbundance = NA, the current spawner 
#' abundance is calculated considering the end year with data as the upper range,
#' otherwise, this upper range is set yearCurrentAbundance and years with NA
#' are added if necessary.
current_spawner_abundance_fun <- function(cuids,
                                          cuspawnerabundance,
                                          yearCurrentAbundance = NA,
                                          CU_genLength){
  
  out_final <- NULL
  
  for(i in 1:length(cuids)){
    # cuid <- cuids[1]
    cuid <- cuids[i]
    
    # subset cuspawnerabundance_rg_sp
    cusa <- cuspawnerabundance[cuspawnerabundance$cuid == cuid,]
    
    # get the count
    spawnerAbundance <- cusa$estimated_count
    spawnerAbundance[spawnerAbundance == 0] <- 1
    spawnerAbundance[spawnerAbundance < 0] <- NA
    names(spawnerAbundance) <- cusa$year
    
    # Are there any spawner abundance data?
    if(all(is.na(spawnerAbundance))){
      
      currentSpawnerData_available <- F
      yrInitial <- NA
      yrFinal <- NA
      #' Eric: if estimated is not available, we can't apply spawner-recruit 
      #' benchmarks.
    }else{
      currentSpawnerData_available <- T
      yrFinal <- tail(as.numeric(names(spawnerAbundance[!is.na(spawnerAbundance)])),1)
    }
    
    #' Determine current spawner abundance. 
    if(is.na(yearCurrentAbundance)){ # current spawner abundance is calculated from the last year with available data
      # if there is data, remove the most recent year without data iteratively,
      # otherwise do nothing
      
      if(any(!is.na(spawnerAbundance))){
        while(is.na(tail(spawnerAbundance,1))){
          spawnerAbundance <- spawnerAbundance[-length(spawnerAbundance)]
        }
      }
      
    }else{ # current spawner abundance is calculated from year in yearCurrentAbundance
      #' add number of years to spawnerAbundance until yearCurrentAbundance if needed
      yearsHere <- as.numeric(names(spawnerAbundance))
      yearLast <- max(yearsHere)
      while(yearLast < yearCurrentAbundance){
        yearLast <- yearLast + 1
        datapointHere <- NA
        names(datapointHere) <- yearLast
        spawnerAbundance <- c(spawnerAbundance,datapointHere)
      }
    }
    
    # calculate current spawner abundance = the geometric mean over the last generation
    spawnerAbundance_lastGen <- tail(spawnerAbundance,CU_genLength[i])
    spawnerAbundance_lastGen_m <- mean_geom_fun(x = spawnerAbundance_lastGen)
    spawnerAbundance_lastGen_dataPointNb <- sum(!is.na(spawnerAbundance_lastGen))
    
    #' Is there is only NAs in spawnerAbundance_lastGen (should not happen
    #' if yearCurrentAbundance is NA).
    if(all(is.na(spawnerAbundance_lastGen))){
      currentSpawnerData_availableRecentEnough <- F
      # the 1st year with data for the calculation of the current spawner abundance
      yrInitial <- NA
    }else{
      currentSpawnerData_availableRecentEnough <- T
      # the 1st year with data for the calculation of the current spawner abundance
      yrInitial <- as.numeric(names(spawnerAbundance_lastGen[!is.na(spawnerAbundance_lastGen)]))[1]
    }
    
    out <- data.frame(region = unique(cusa$region),
                      species_name = unique(cusa$species_name),
                      cu_name_pse = unique(cusa$cu_name_pse),
                      cuid = unique(cusa$cuid),
                      curr_spw_abun = spawnerAbundance_lastGen_m,
                      dataPointNb = spawnerAbundance_lastGen_dataPointNb,
                      yr_withData_start = yrInitial,
                      yr_withData_end = yrFinal,
                      yr_end_imposed = yearCurrentAbundance,
                      curr_spw_available = currentSpawnerData_available,
                      curr_spw_availableRecentEnough = currentSpawnerData_availableRecentEnough
    )
    
    if(is.null(out_final)){
      out_final <- out
    }else{
      out_final <- rbind(out_final,out)
    }
  }
  return(out_final)
}

#' Function to plot the spawner abundance time series of a given CU with the 
#' benchmarks shown; 
#' Cf. an example at:
#' https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1717434872482819
# cuid <- 171 # for SR benchmarks
# cuid <- 175 # for percentile benchmarks
# cuid <- 599 # 
# cuid <- 811 # issue with Sgen > Ssmy ; 1004 : issue with biostatus percentile not match
# dataset101_biological_status <- biological_status_cu
# dataset102_benchmarks <- benchmarks_cu
# cuspawnerabundance <- spawnerabundance_cu
# dataset103_output <- cuspawnerabund_smooth
# figure_print <- F
plot_spawnerAbundance_benchmarks_fun <- function(cuid, 
                                                 cuspawnerabundance, # spawner abundance  
                                                 dataset101_biological_status,  # biostatus
                                                 dataset102_benchmarks,  # benchmark values
                                                 #dataset103_output,  # smooth spawner abundance NOT UPDATED so calculated in the function
                                                 conservationunits_decoder,  # for the generation length
                                                 log10_scale = F,
                                                 figure_print = F,
                                                 wd_figures = NA,
                                                 file_name_nchar = 25){
  
  require(zoo) # to use rollapply()
  
  # Estimated spawner abundance:
  cond <- cuspawnerabundance$cuid == cuid
  spawnerAbund <- cuspawnerabundance[cond,]
  
  y_range_min <- 0
  if(log10_scale){
    y_range_min <- 1
  }
  
  x_range <- range(spawnerAbund$year[!is.na(spawnerAbund$estimated_count)])
  x_range[1] <- x_range[1] - ((x_range[2] - x_range[1]) * .1) |> floor()
  x_range[2] <- x_range[2] + ((x_range[2] - x_range[1]) * .05) |> ceiling()
  y_range <- c(y_range_min,max(spawnerAbund$estimated_count, na.rm = T))
  y_range[2] <- y_range[2] + ((y_range[2] - y_range[1]) * .1)
  
  # Smoothed spawner abundance
  # cond <- dataset103_output$cuid == cuid
  # spawnerAbund_smooth <- dataset103_output[cond,]
  
  # generation length and more
  cond <- conservationunits_decoder$cuid == cuid
  genLength <- conservationunits_decoder$gen_length[cond]
  region <- conservationunits_decoder$region[cond]
  if(region == "Vancouver Island & Mainland Inlets"){
    region <- "VIMI"
  }
  cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]
  cu_name_pse <- gsub(" (even)","",cu_name_pse)
  cu_name_pse <- gsub(" (odd)","",cu_name_pse)
  if(nchar(cu_name_pse) > file_name_nchar){   # the figure can't print if the number of characters is too large
    cu_name_pse <- substr(x = cu_name_pse, start = 1, stop = file_name_nchar) 
  }
  
  if(any("species_quaified" == colnames(conservationunits_decoder))){
    species_abbr <- conservationunits_decoder$species_qualified[cond]
  }else{
    species_abbr <- conservationunits_decoder$species_abbr[cond]
  }
  
  cu_file_name <- paste0(c(region,species_abbr,cu_name_pse),collapse = " - ")
  cu_file_name <- gsub("/",".",cu_file_name)
  
  # Biostatus
  cond <- dataset101_biological_status$cuid == cuid
  biostatus <- dataset101_biological_status[cond,]
  
  # Benchmarks
  cond <- dataset102_benchmarks$cuid == cuid
  benchmarks <- dataset102_benchmarks[cond,]
  
  # Case with cyclic CUs NOT USED
  is_cyclic <- F
  if(any(grepl("cycle_line",colnames(benchmarks)))){
    is_cyclic <- T
    # retain row corresponding to the cycle-line that corresponds to the last year of data
    cond_cl_current <- benchmarks$cycle_line_current
    benchmarks_cl_no <- benchmarks[!cond_cl_current,]
    benchmarks <- benchmarks[cond_cl_current,]
  }
  
  # Find the benchmark values
  polygons_show <- T
  if(biostatus$psf_status_type == "sr"){

    if(any(grepl("smsy80",colnames(benchmarks)))){
      benchmark_up <- benchmarks$smsy80
      benchmark_up_025 <- benchmarks$smsy80_lower
      benchmark_up_975 <- benchmarks$smsy80_upper
      
      benchmark_low <- benchmarks$sgen
      benchmark_low_025 <- benchmarks$sgen_lower
      benchmark_low_975 <- benchmarks$sgen_upper
    }else if(any(grepl("smsy_",colnames(benchmarks)))){   # should still be smsy80
      benchmark_up <- benchmarks$smsy
      benchmark_up_025 <- benchmarks$smsy_lower
      benchmark_up_975 <- benchmarks$smsy_upper
      
      benchmark_low <- benchmarks$sgen
      benchmark_low_025 <- benchmarks$sgen_lower
      benchmark_low_975 <- benchmarks$sgen_upper
    }else{
      benchmark_low <- benchmarks$sr_lower
      benchmark_low_025 <- benchmarks$sr_lower_025
      benchmark_low_975 <- benchmarks$sr_lower_975
      benchmark_up <- benchmarks$sr_upper
      benchmark_up_025 <- benchmarks$sr_upper_025
      benchmark_up_975 <- benchmarks$sr_upper_975
    }
    method <- "HBSR"
    status <- biostatus$sr_status
    
  }else if(biostatus$psf_status_type == "percentile"){
    
    if(any(grepl("X25._spw",colnames(biostatus)))){
      benchmark_low <- benchmarks$X25._spw             # `25%_spw`
      benchmark_low_025 <- benchmarks$X25._spw_lower   # `25%_spw_lower`
      benchmark_low_975 <- benchmarks$X25._spw_upper   # `25%_spw_upper`
      benchmark_up <- benchmarks$X75._spw              # `75%_spw`
      benchmark_up_025 <- benchmarks$X75._spw_lower    # `75%_spw_lower`
      benchmark_up_975 <- benchmarks$X75._spw_upper    # `75%_spw_upper`
    }else if(any(grepl("25%_spw",colnames(biostatus)))){
      benchmark_low <- benchmarks$`25%_spw`
      benchmark_low_025 <- benchmarks$`25%_spw_lower`
      benchmark_low_975 <- benchmarks$`25%_spw_upper`
      benchmark_up <- benchmarks$`75%_spw`
      benchmark_up_025 <- benchmarks$`75%_spw_lower`
      benchmark_up_975 <- benchmarks$`75%_spw_upper`
    }else{
      benchmark_low <- benchmarks$percentile_lower
      benchmark_low_025 <- benchmarks$percentile_lower_025
      benchmark_low_975 <- benchmarks$percentile_lower_975
      benchmark_up <- benchmarks$percentile_upper
      benchmark_up_025 <- benchmarks$percentile_upper_025
      benchmark_up_975 <- benchmarks$percentile_upper_975
    }
    
    method <- "Percentiles"
    status <- biostatus$percentile_status
    
    # COMMENT:
    # This is indeed the 50% percentile and not the 75, despite the name being "75%_spw"
    # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707332952867199
    
  }else{ # there is no benchmark values
    print("There are no benchmark values for this CU.")
    polygons_show <- F
  }
  
  # adjust y_range eventually if values are below thresholds
  if(any(max(y_range) < c(benchmark_low_025,benchmark_up_025))){
    y_range[2] <- benchmark_up_975 + benchmark_up_975 * .2
  }
  
  # Get current spawner abundance
  csa_df <- current_spawner_abundance_fun(cuids = cuid, 
                                          cuspawnerabundance = spawnerAbund, 
                                          yearCurrentAbundance = NA,  # so it is calculated from the most recent year of available data 
                                          CU_genLength = genLength)
  # OR
  
  if(figure_print){
    cu_file_name2 <- gsub(" - ","_",cu_file_name)
    cu_file_name2 <- gsub(" ","_",cu_file_name2)
    pathFile <- paste0(wd_figures,"/",cu_file_name2,"_",cuid,"_biostatus_benchmarks.jpg")
    jpeg(file = pathFile, width = 25, height = 14, units = "cm", res = 300)
  }
  
  layout(matrix(1:2, nrow = 1), widths = c(1,.15))
  
  par(mar = c(5,5,3,0))
  
  ylab <- "Estimated spawwner abundance"
  las <- 0
  if(log10_scale){
    y_range <- log10(y_range)
    benchmark_low <- log10(benchmark_low)
    benchmark_low_025 <- log10(benchmark_low_025)
    benchmark_low_975 <- log10(benchmark_low_975)
    benchmark_up <- log10(benchmark_up)
    benchmark_up_025 <- log10(benchmark_up_025)
    benchmark_up_975 <- log10(benchmark_up_975)
    
    csa_df$curr_spw_abun <- log10(csa_df$curr_spw_abun)
    
    ylab <- "Estimated spawwner abundance (log10)"
    las <- 1
  }
  
  plot(NA, xlim = x_range, ylim = y_range, xaxs = 'i', yaxs = 'i', bty = 'l', las = las,
       ylab = ylab, xlab = "Years", main = paste0(cu_file_name," - ",cuid))
  
  # polygons
  alpha <- 0.4
  if(polygons_show){
    polygon(x = c(x_range,rev(x_range)), 
            y = c(benchmark_up,benchmark_up,y_range[2],y_range[2]), 
            col = colour_transparency_fun(status_cols["green"],alpha = alpha),
            border = F)
    polygon(x = c(x_range,rev(x_range)), 
            y = c(benchmark_low,benchmark_low,benchmark_up,benchmark_up), 
            col = colour_transparency_fun(status_cols["amber"],alpha = alpha),border = F)
    polygon(x = c(x_range,rev(x_range)), 
            y = c(min(y_range),min(y_range),benchmark_low,benchmark_low), 
            col = colour_transparency_fun(status_cols["red"],alpha = alpha),border = F)
  }
  
  # grid - y
  digits_nb <- nchar(ceiling(y_range[2]))
  grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 1)
  factor <- paste(c(1,rep(0,(digits_nb - 1))), collapse = "") |> as.numeric()
  grid_vals <- 0:as.numeric(grid_max) * factor
  if(length(grid_vals) < 5){ # add a segment to each 1/2 intervals as well
    grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 2)
    grid_vals <- 0:as.numeric(grid_max) 
    grid_vals <- grid_vals[grid_vals %% 5 == 0]
    grid_vals <- grid_vals * factor / 10
  }
  if(length(grid_vals) < 5){ # add a segment to each 1/5 intervals as well
    grid_vals <- 0:as.numeric(grid_max) 
    grid_vals <- grid_vals[grid_vals %% 2 == 0]
    grid_vals <- grid_vals * factor / 10
  }
  segments(x0 = rep(x_range[1],length(grid_vals)),
           x1 = rep(x_range[2],length(grid_vals)),
           y0 = grid_vals, y1 = grid_vals,
           lwd = 2, col = colour_transparency_fun("white",alpha = .5))
  
  # grid - x
  yrs <- min(spawnerAbund$year):max(spawnerAbund$year)
  grid_vals <- yrs[yrs %% 10 == 0]
  if(length(grid_vals) < 3){ # add a segment to each 5 years
    grid_vals <- yrs[yrs %% 5 == 0]
  }
  segments(y0 = rep(y_range[1],length(grid_vals)),
           y1 = rep(y_range[2],length(grid_vals)),
           x0 = grid_vals, x1 = grid_vals,
           lwd = 2, col = colour_transparency_fun("white",alpha = .5))
  
  # Plot current spawner abundance
  cond <- status == c("good","fair","poor","hfjfk","djahdjk")
  col_csa <- status_cols[cond]
  
  segments(x0 = csa_df$yr_withData_start, x1 = x_range[2], 
           y0 = csa_df$curr_spw_abun, y1 = csa_df$curr_spw_abun, 
           lwd = 3, col = col_csa)
  
  # show benchmarks
  segments(x0 = c(x_range[1],x_range[1]), x1 = c(x_range[2],x_range[2]),
           y0 = c(benchmark_low,benchmark_up), y1 = c(benchmark_low,benchmark_up), 
           col = c(status_cols["red"], status_cols["green"]), lwd = 2)
  
  # show benchmarks for other cyclic-lines (for cyclic CUs only)
  if(is_cyclic){
    for(r in 1:nrow(benchmarks_cl_no)){
      benchmark_low_cl <- benchmarks_cl_no$`25%_spw`[r]
      benchmark_up_cl <- benchmarks_cl_no$`75%_spw`[r]
      
      if(log10_scale){
        benchmark_low_cl <- log10(benchmark_low_cl)
        benchmark_up_cl <- log10(benchmark_up_cl)
      }
      
      segments(x0 = c(x_range[1],x_range[1]), x1 = c(x_range[2],x_range[2]),
               y0 = c(benchmark_low_cl,benchmark_up_cl), 
               y1 = c(benchmark_low_cl,benchmark_up_cl), 
               col = c(status_cols["red"], status_cols["green"]), lwd = 2, lty = 2)
    }
  }
  
  # plot estimated spawner abundance
  # remove odd and even years for PKO and PKE, respectively so the dots are 
  # connected.
  if(species_abbr == "PKO"){
    cond_to_keep <- spawnerAbund$year %% 2 == 1
  }else if(species_abbr == "PKE"){
    cond_to_keep <- spawnerAbund$year %% 2 == 0
  }else{
    cond_to_keep <- rep(T,length(spawnerAbund$year))
  }
  x <- spawnerAbund$year[cond_to_keep]
  y <- spawnerAbund$estimated_count[cond_to_keep]
  if(log10_scale){
    y <- log10(y)
  }
  points(x = x, y = y, pch = 16, type = 'o', lwd = 2, 
         col = colour_transparency_fun("black",alpha = alpha))
  
  # Add smoothed spawner abundance:
  # Smooth spawner abundance using running mean
  # log transform (and deal with 0s)
  
  # - remove the NAs at the tail of spawnerAbund$estimated_count
  x <- spawnerAbund$year
  y <- spawnerAbund$estimated_count
  while(is.na(tail(y,1))){
    y <- y[-length(y)]
    x <- x[-length(x)]
  }
  # - remove NAs at the head of spawnerAbund$estimated_count
  while(is.na(head(y,1))){
    y <- y[-1]
    x <- x[-1]
  }
  
  y[y == 0 & !is.na(y)] <- 1 # 0.01 # replace 0s by 0.01
  smooth.y <- rollapply(
    data = log(y), 
    FUN = mean, 
    width = genLength,
    # na.pad = TRUE, # deprecated. Use fill = NA instead of na.pad = TRUE
    na.rm = T, 
    fill = NA,
    align = "right") #
  
  # y <- exp(spawnerAbund_smooth$avg_escape_log)
  # points(x = spawnerAbund_smooth$year, y = y, type = 'l', lwd = 2.5, col = "red")
  y <- exp(smooth.y)
  if(log10_scale){
    y <- log10(y)
  }
  points(x = x, y = y, type = 'o', lwd = 2.5, pch = 16, cex = .7)
  
  # display issue
  if(benchmark_low > benchmark_up){
    legend("top","BENCHMARK ISSUE",bty = 'n', text.col = "red")
  }
  
  # Extend benchmarks and plot 95% CI and current spawner abundance
  par(mar = c(5,0,3,0.5))
  plot(NA, xlim = c(0,1), ylim = y_range, ylab = "", xlab = "", 
       main = method, cex.main = .9, font.main = 1, # plain font
       xaxt = 'n', yaxt = 'n', bty = 'n', xaxs = 'i', yaxs = 'i')
  
  # plot the current spawner abundance
  segments(x0 = 0, x1 = .8, 
           y0 = csa_df$curr_spw_abun, y1 = csa_df$curr_spw_abun, 
           lwd = 3, col = col_csa)
  
  # benchmarks
  segments(x0 = c(0,0), y0 = c(benchmark_low,benchmark_up),
           x1 = c(.4,.6), y1 = c(benchmark_low,benchmark_up), 
           col = c(status_cols["red"],status_cols["green"]), lwd = 2)
  points(x = c(.4,.6), y = c(benchmark_low,benchmark_up), pch = 16, 
         col = c(status_cols["red"],status_cols["green"]))
  segments(x0 = c(.4,.6), y0 = c(benchmark_low_025,
                                 benchmark_up_025),
           x1 = c(.4,.6), y1 = c(benchmark_low_975,
                                 benchmark_up_975), 
           col = c(status_cols["red"],status_cols["green"]), lwd = 2)
  
  #
  # show benchmarks for other cyclic-lines (for cyclic CUs only)
  coeff_x_cl <- .15
  if(is_cyclic){
    for(r in 1:nrow(benchmarks_cl_no)){
      # r <- 1
      benchmark_low_cl <- benchmarks_cl_no$`25%_spw`[r]
      benchmark_up_cl <- benchmarks_cl_no$`75%_spw`[r]
      benchmark_low_025_cl <-  benchmarks_cl_no$`25%_spw_lower`[r]
      benchmark_low_975_cl <-  benchmarks_cl_no$`25%_spw_upper`[r]
      benchmark_up_025_cl <-  benchmarks_cl_no$`75%_spw_lower`[r]
      benchmark_up_975_cl <-  benchmarks_cl_no$`75%_spw_upper`[r]
      
      if(log10_scale){
        benchmark_low_cl <- log10(benchmark_low_cl)
        benchmark_up_cl <- log10(benchmark_up_cl)
        benchmark_low_025_cl <- log10(benchmark_low_025_cl)
        benchmark_low_975_cl <- log10(benchmark_low_975_cl)
        benchmark_up_025_cl <- log10(benchmark_up_025_cl)
        benchmark_up_975_cl <- log10(benchmark_up_975_cl)
      }
      
      segments(x0 = c(0,0), x1 = c(.4 + coeff_x_cl,.6 + coeff_x_cl),
               y0 = c(benchmark_low_cl,benchmark_up_cl), 
               y1 = c(benchmark_low_cl,benchmark_up_cl), 
               col = c(status_cols["red"], status_cols["green"]), lwd = 2, lty = 2)
      points(x = c(.4 + coeff_x_cl,.6 + coeff_x_cl), 
             y = c(benchmark_low_cl,benchmark_up_cl), pch = 16, 
             col = c(status_cols["red"],status_cols["green"]))
      segments(x0 = c(.4 + coeff_x_cl,.6 + coeff_x_cl), y0 = c(benchmark_low_025_cl,
                                     benchmark_up_025_cl),
               x1 = c(.4 + coeff_x_cl,.6 + coeff_x_cl), y1 = c(benchmark_low_975_cl,
                                     benchmark_up_975_cl), 
               col = c(status_cols["red"],status_cols["green"]), lwd = 2)
      coeff_x_cl <- coeff_x_cl + .1
    }
  }

  if(figure_print){
    dev.off()
  }
}


#' Same as above but for cyclic CUs and percentile method
# cuid <- 728 # issue with Sgen > Ssmy ; 1004 : issue with biostatus percentile not match
# dataset101_biological_status <- biostatus_101
# dataset102_benchmarks <- benchmarks_102_cyclic
# cuspawnerabundance <- spawnerabundance
# dataset103_output <- biostatus_101
# figure_print <- F
# file_name_nchar <- 60
plot_spawnerAbundance_benchmarks_cyclic_percentile_fun <- function(cuid, 
                                                 cuspawnerabundance, # spawner abundance  
                                                 dataset101_biological_status,  # biostatus
                                                 dataset102_benchmarks,  # benchmark values
                                                 #dataset103_output,  # smooth spawner abundance NOT UPDATED so calculated in the function
                                                 conservationunits_decoder,  # for the generation length
                                                 log10_scale = F,
                                                 figure_print = F,
                                                 wd_figures = NA,
                                                 file_name_nchar = 25){
  
  require(zoo) # to use rollapply()
  
  # Estimated spawner abundance:
  cond <- cuspawnerabundance$cuid == cuid
  spawnerAbund <- cuspawnerabundance[cond,]
  
  y_range_min <- 0
  if(log10_scale){
    y_range_min <- 1
  }
  
  x_range <- range(spawnerAbund$year[!is.na(spawnerAbund$estimated_count)])
  x_range[1] <- x_range[1] - ((x_range[2] - x_range[1]) * .1) |> floor()
  x_range[2] <- x_range[2] + ((x_range[2] - x_range[1]) * .05) |> ceiling()
  y_range <- c(y_range_min,max(spawnerAbund$estimated_count, na.rm = T))
  y_range[2] <- y_range[2] + ((y_range[2] - y_range[1]) * .1)
  
  if(log10_scale){
    y_range <- log10(y_range)
  }
  
  # Smoothed spawner abundance
  # cond <- dataset103_output$cuid == cuid
  # spawnerAbund_smooth <- dataset103_output[cond,]
  
  # generation length and more
  cond <- conservationunits_decoder$cuid == cuid
  genLength <- conservationunits_decoder$gen_length[cond]
  region <- conservationunits_decoder$region[cond]
  if(region == "Vancouver Island & Mainland Inlets"){
    region <- "VIMI"
  }
  cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]
  cu_name_pse <- gsub(" (even)","",cu_name_pse)
  cu_name_pse <- gsub(" (odd)","",cu_name_pse)
  if(nchar(cu_name_pse) > file_name_nchar){   # the figure can't print if the number of characters is too large
    cu_name_pse <- substr(x = cu_name_pse, start = 1, stop = file_name_nchar) 
  }
  species_abbr <- conservationunits_decoder$species_abbr[cond]
  
  cu_file_name <- paste0(c(region,species_abbr,cu_name_pse),collapse = " - ")
  cu_file_name <- gsub("/",".",cu_file_name)
  
  # Biostatus
  cond <- dataset101_biological_status$cuid == cuid
  biostatus <- dataset101_biological_status[cond,]
  
  # Benchmarks
  cond <- dataset102_benchmarks$cuid == cuid
  benchmarks <- dataset102_benchmarks[cond,]
  
  # Check that the CU is cyclic
  cond_cyclic <- sapply(benchmarks$cu_name_pse |> unique(),function(cu){grepl("cyclic",cu)})
  if(!any(cond_cyclic)){
    
    print("Error")
    print("dataset102_benchmarks does not have the field 'cycle_line', which should be present if the CUs have cyclic dynamics.")
    print("Use plot_spawnerAbundance_benchmarks_cyclic_fun() for non-cyclic CU.")
    break
    
  }else{
    
    cycle_lines <- unique(benchmarks$cycle_line)
    
    # separate spawnerAbund in each of the cycle-lines
    spawnerAbund_cl_l <- list()
    for(cl in 1:4){
      i <- 1:nrow(spawnerAbund)
      i <- i[(i + cl - 2) %% 4 == 0]
      spawnerAbund_cl <- spawnerAbund[i,]
      spawnerAbund_cl_l[[cl]] <- spawnerAbund_cl
    }
    names(spawnerAbund_cl_l) <- 1:4

    if(figure_print){
      cu_file_name2 <- gsub(" - ","_",cu_file_name)
      cu_file_name2 <- gsub(" ","_",cu_file_name2)
      pathFile <- paste0(wd_figures,"/",cu_file_name2,"_",cuid,"_percentile_biostatus_benchmarks.jpg")
      jpeg(file = pathFile, width = 25, height = 25, units = "cm", res = 300)
    }
    
    layout(matrix(1:(length(cycle_lines)*2), nrow = length(cycle_lines), byrow = T), 
           widths = c(1,.15), heights = c(1.18,1,1,1.26))
    
    for(cl_yr in cycle_lines){
      # cl_yr <- 1
      
      cond_cl_yr <- benchmarks$cycle_line == cl_yr
      
      side1 <- side3 <- .5
      xaxt <- "n"
      main <- xlab <- ""
      if(cl_yr == 1){
        side3 <- 3.5
        main <-  paste0(cu_file_name," - ",cuid)
        
      }else if(cl_yr == 4){
        side1 <- 4.5
        xaxt <- 's'
        xlab <- "Year"
      }
      
      # Find the benchmark values
      polygons_show <- T
      if(biostatus$psf_status_type[1] == "sr"){
        # benchmark_low <- benchmarks$sgen[cond_cl_yr]
        # benchmark_low_025 <- benchmarks$sgen_lower[cond_cl_yr]
        # benchmark_low_975 <- benchmarks$sgen_upper[cond_cl_yr]
        benchmark_low <- benchmarks$sr_low[cond_cl_yr]
        benchmark_low_025 <- benchmarks$sr_low_025[cond_cl_yr]
        benchmark_low_975 <- benchmarks$sr_low_975[cond_cl_yr]
        
        # if(any(grepl("smsy80",colnames(benchmarks)))){
        #   benchmark_up <- benchmarks$smsy80[cond_cl_yr]
        #   benchmark_up_025 <- benchmarks$smsy80_lower[cond_cl_yr]
        #   benchmark_up_975 <- benchmarks$smsy80_upper[cond_cl_yr]
        # }else{
        #   benchmark_up <- benchmarks$smsy[cond_cl_yr]
        #   benchmark_up_025 <- benchmarks$smsy_lower[cond_cl_yr]
        #   benchmark_up_975 <- benchmarks$smsy_upper[cond_cl_yr]
        # }
        
        benchmark_up <- benchmarks$sr_upper[cond_cl_yr]
        benchmark_up_025 <- benchmarks$sr_upper_025[cond_cl_yr]
        benchmark_up_975 <- benchmarks$sr_upper_975[cond_cl_yr]
        
        method <- "HBSR"
        status <- biostatus$sr_status
        
      }else if(biostatus$psf_status_type[1] == "percentile"){
        
        # if(any(grepl("X25._spw",colnames(biostatus)))){
        #   benchmark_low <- benchmarks$X25._spw[cond_cl_yr]             # `25%_spw`
        #   benchmark_low_025 <- benchmarks$X25._spw_lower[cond_cl_yr]   # `25%_spw_lower`
        #   benchmark_low_975 <- benchmarks$X25._spw_upper[cond_cl_yr]   # `25%_spw_upper`
        #   benchmark_up <- benchmarks$X75._spw[cond_cl_yr]              # `75%_spw`
        #   benchmark_up_025 <- benchmarks$X75._spw_lower[cond_cl_yr]    # `75%_spw_lower`
        #   benchmark_up_975 <- benchmarks$X75._spw_upper[cond_cl_yr]    # `75%_spw_upper`
        # }else{
        #   benchmark_low <- benchmarks$`25%_spw`[cond_cl_yr]
        #   benchmark_low_025 <- benchmarks$`25%_spw_lower`[cond_cl_yr]
        #   benchmark_low_975 <- benchmarks$`25%_spw_upper`[cond_cl_yr]
        #   benchmark_up <- benchmarks$`75%_spw`[cond_cl_yr]
        #   benchmark_up_025 <- benchmarks$`75%_spw_lower`[cond_cl_yr]
        #   benchmark_up_975 <- benchmarks$`75%_spw_upper`[cond_cl_yr]
        # }
        benchmark_low <- benchmarks$percentile_lower[cond_cl_yr]
        benchmark_low_025 <- benchmarks$percentile_lower_025[cond_cl_yr]
        benchmark_low_975 <- benchmarks$percentile_lower_975[cond_cl_yr]
        benchmark_up <- benchmarks$percentile_upper[cond_cl_yr]
        benchmark_up_025 <- benchmarks$percentile_upper_025[cond_cl_yr]
        benchmark_up_975 <- benchmarks$percentile_upper_975[cond_cl_yr]
        
        method <- "Percentiles"
        status <- biostatus$percentile_status
        
        # COMMENT:
        # This is indeed the 50% percentile and not the 75, despite the name being "75%_spw"
        # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707332952867199
        
      }else{ # there is no benchmark values
        print("There are no benchmark values for this CU.")
        polygons_show <- F
      }
      
      # adjust y_range eventually if values are below thresholds
      # if(any(max(y_range) < c(benchmark_low_025,benchmark_up_025))){
      #   y_range[2] <- benchmark_up_975 + benchmark_up_975 * .2
      # }
      
      # Get current spawner abundance
      csa <- benchmarks$curr_spw[cond_cl_yr]
      
      par(mar = c(side1,5,side3,0))
      
      ylab <- "Estimated spawwner abundance"
      las <- 0
      if(log10_scale){
        
        benchmark_low <- log10(benchmark_low)
        benchmark_low_025 <- log10(benchmark_low_025)
        benchmark_low_975 <- log10(benchmark_low_975)
        benchmark_up <- log10(benchmark_up)
        benchmark_up_025 <- log10(benchmark_up_025)
        benchmark_up_975 <- log10(benchmark_up_975)
        
        csa <- log10(csa)
        
        ylab <- "Estimated spawwner abundance (log10)"
        las <- 1
      }
      
      plot(NA, xlim = x_range, ylim = y_range, xaxs = 'i', yaxs = 'i', bty = 'l', 
           las = las, ylab = ylab, xlab = xlab, main = main, xaxt = xaxt)
      
      # polygons
      alpha <- 0.4
      if(polygons_show){
        polygon(x = c(x_range,rev(x_range)), 
                y = c(benchmark_up,benchmark_up,y_range[2],y_range[2]), 
                col = colour_transparency_fun(status_cols["green"],alpha = alpha),
                border = F)
        polygon(x = c(x_range,rev(x_range)), 
                y = c(benchmark_low,benchmark_low,benchmark_up,benchmark_up), 
                col = colour_transparency_fun(status_cols["amber"],alpha = alpha),border = F)
        polygon(x = c(x_range,rev(x_range)), 
                y = c(min(y_range),min(y_range),benchmark_low,benchmark_low), 
                col = colour_transparency_fun(status_cols["red"],alpha = alpha),border = F)
      }
      
      # grid - y
      digits_nb <- nchar(ceiling(y_range[2]))
      grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 1)
      factor <- paste(c(1,rep(0,(digits_nb - 1))), collapse = "") |> as.numeric()
      grid_vals <- 0:as.numeric(grid_max) * factor
      if(length(grid_vals) < 5){ # add a segment to each 1/2 intervals as well
        grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 2)
        grid_vals <- 0:as.numeric(grid_max) 
        grid_vals <- grid_vals[grid_vals %% 5 == 0]
        grid_vals <- grid_vals * factor / 10
      }
      if(length(grid_vals) < 5){ # add a segment to each 1/5 intervals as well
        grid_vals <- 0:as.numeric(grid_max) 
        grid_vals <- grid_vals[grid_vals %% 2 == 0]
        grid_vals <- grid_vals * factor / 10
      }
      segments(x0 = rep(x_range[1],length(grid_vals)),
               x1 = rep(x_range[2],length(grid_vals)),
               y0 = grid_vals, y1 = grid_vals,
               lwd = 2, col = colour_transparency_fun("white",alpha = .5))
      
      # grid - x
      yrs <- min(spawnerAbund$year):max(spawnerAbund$year)
      grid_vals <- yrs[yrs %% 10 == 0]
      if(length(grid_vals) < 3){ # add a segment to each 5 years
        grid_vals <- yrs[yrs %% 5 == 0]
      }
      segments(y0 = rep(y_range[1],length(grid_vals)),
               y1 = rep(y_range[2],length(grid_vals)),
               x0 = grid_vals, x1 = grid_vals,
               lwd = 2, col = colour_transparency_fun("white",alpha = .5))
      
      # Plot current spawner abundance
      cond <- status[cl_yr] == c("good","fair","poor","hfjfk","djahdjk")
      col_csa <- status_cols[cond]
      
      segments(x0 = benchmarks$curr_spw_start_year[cond_cl_yr], x1 = x_range[2], 
               y0 = csa, y1 = csa, lwd = 3, col = col_csa)
      
      # show benchmarks
      segments(x0 = c(x_range[1],x_range[1]), x1 = c(x_range[2],x_range[2]),
               y0 = c(benchmark_low,benchmark_up), y1 = c(benchmark_low,benchmark_up), 
               col = c(status_cols["red"], status_cols["green"]), lwd = 2)
      
      # plot estimated spawner abundance
      # remove odd and even years for PKO and PKE, respectively so the dots are 
      # connected.
      if(species_abbr == "PKO"){
        cond_to_keep <- spawnerAbund$year %% 2 == 1
      }else if(species_abbr == "PKE"){
        cond_to_keep <- spawnerAbund$year %% 2 == 0
      }else{
        cond_to_keep <- rep(T,length(spawnerAbund$year))
      }
      x <- spawnerAbund$year[cond_to_keep]
      y <- spawnerAbund$estimated_count[cond_to_keep]
      if(log10_scale){
        y <- log10(y)
      }
      points(x = x, y = y, pch = 16, type = 'o', lwd = 2, 
             col = colour_transparency_fun("black",alpha = alpha))
      
      # Add the cycle-line
      y <-  spawnerAbund_cl_l[[cl_yr]]$estimated_count
      if(log10_scale){
        y <- log10(y)
      }
      x <- spawnerAbund_cl_l[[cl_yr]]$year
      points(x = x, y = y, type = 'o', lwd = 2.5, pch = 1, cex = 1)
      
      # display issue
      if(benchmark_low > benchmark_up){
        legend("top","BENCHMARK ISSUE",bty = 'n', text.col = "red")
      }
      
      # Extend benchmarks and plot 95% CI and current spawner abundance
      main <- ""
      if(cl_yr == 1){
        main <- method
      }
      par(mar = c(side1,0,side3,0.5))
      plot(NA, xlim = c(0,1), ylim = y_range, ylab = "", xlab = "", 
           main = main, cex.main = .9, font.main = 1, # plain font
           xaxt = 'n', yaxt = 'n', bty = 'n', xaxs = 'i', yaxs = 'i')
      
      # plot the current spawner abundance
      segments(x0 = 0, x1 = .8, y0 = csa, y1 = csa, lwd = 3, col = col_csa)
      
      # benchmarks
      segments(x0 = c(0,0), y0 = c(benchmark_low,benchmark_up),
               x1 = c(.4,.6), y1 = c(benchmark_low,benchmark_up), 
               col = c(status_cols["red"],status_cols["green"]), lwd = 2)
      points(x = c(.4,.6), y = c(benchmark_low,benchmark_up), pch = 16, 
             col = c(status_cols["red"],status_cols["green"]))
      segments(x0 = c(.4,.6), y0 = c(benchmark_low_025,
                                     benchmark_up_025),
               x1 = c(.4,.6), y1 = c(benchmark_low_975,
                                     benchmark_up_975), 
               col = c(status_cols["red"],status_cols["green"]), lwd = 2)
      
      legend("right",legend = benchmarks$curr_spw_end_year[cond_cl_yr],bty = 'n')
      
      # 
    } # for each cycle-line
    
    if(figure_print){
      dev.off()
    }
  } # if the CU is cyclic
}

#' Same as above but for cyclic CUs and Larkin method
# cuid <- 728 # issue with Sgen > Ssmy ; 1004 : issue with biostatus percentile not match
# dataset101_biological_status <- biostatus_101
# dataset102_benchmarks <- benchmarks_102_cyclic
# cuspawnerabundance <- spawnerabundance
# dataset103_output <- biostatus_101
# figure_print <- F
# file_name_nchar <- 60
plot_spawnerAbundance_benchmarks_cyclic_Larkin_fun <- function(cuid, 
                                                                   cuspawnerabundance, # spawner abundance  
                                                                   dataset101_biological_status,  # biostatus
                                                                   dataset102_benchmarks,  # benchmark values
                                                                   #dataset103_output,  # smooth spawner abundance NOT UPDATED so calculated in the function
                                                                   conservationunits_decoder,  # for the generation length
                                                                   log10_scale = F,
                                                                   figure_print = F,
                                                                   wd_figures = NA,
                                                                   file_name_nchar = 25){
  
  require(zoo) # to use rollapply()
  
  # Estimated spawner abundance:
  cond <- cuspawnerabundance$cuid == cuid
  spawnerAbund <- cuspawnerabundance[cond,]
  
  y_range_min <- 0
  if(log10_scale){
    y_range_min <- 1
  }
  
  x_range <- range(spawnerAbund$year[!is.na(spawnerAbund$estimated_count)])
  x_range[1] <- x_range[1] - ((x_range[2] - x_range[1]) * .1) |> floor()
  x_range[2] <- x_range[2] + ((x_range[2] - x_range[1]) * .05) |> ceiling()
  y_range <- c(y_range_min,max(spawnerAbund$estimated_count, na.rm = T))
  y_range[2] <- y_range[2] + ((y_range[2] - y_range[1]) * .1)
  
  if(log10_scale){
    y_range <- log10(y_range)
  }
  
  # Smoothed spawner abundance
  # cond <- dataset103_output$cuid == cuid
  # spawnerAbund_smooth <- dataset103_output[cond,]
  
  # generation length and more
  cond <- conservationunits_decoder$cuid == cuid
  genLength <- conservationunits_decoder$gen_length[cond]
  region <- conservationunits_decoder$region[cond]
  if(region == "Vancouver Island & Mainland Inlets"){
    region <- "VIMI"
  }
  cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]
  cu_name_pse <- gsub(" (even)","",cu_name_pse)
  cu_name_pse <- gsub(" (odd)","",cu_name_pse)
  if(nchar(cu_name_pse) > file_name_nchar){   # the figure can't print if the number of characters is too large
    cu_name_pse <- substr(x = cu_name_pse, start = 1, stop = file_name_nchar) 
  }
  species_abbr <- conservationunits_decoder$species_abbr[cond]
  
  cu_file_name <- paste0(c(region,species_abbr,cu_name_pse),collapse = " - ")
  cu_file_name <- gsub("/",".",cu_file_name)
  
  # Biostatus
  cond <- dataset101_biological_status$cuid == cuid
  biostatus <- dataset101_biological_status[cond,]
  
  # Benchmarks
  cond <- dataset102_benchmarks$cuid == cuid
  benchmarks <- dataset102_benchmarks[cond,]
  
  # Check that the CU is cyclic
  cond_cyclic <- sapply(benchmarks$cu_name_pse |> unique(),function(cu){grepl("cyclic",cu)})
  if(!any(cond_cyclic)){
    
    print("Error")
    print("dataset102_benchmarks does not have the field 'cycle_line', which should be present if the CUs have cyclic dynamics.")
    print("Use plot_spawnerAbundance_benchmarks_cyclic_fun() for non-cyclic CU.")
    break
    
  }else{
    
    years <- rev(sort(unique(biostatus$year)))

    if(figure_print){
      cu_file_name2 <- gsub(" - ","_",cu_file_name)
      cu_file_name2 <- gsub(" ","_",cu_file_name2)
      pathFile <- paste0(wd_figures,"/",cu_file_name2,"_",cuid,"_Larkin_biostatus_benchmarks.jpg")
      jpeg(file = pathFile, width = 25, height = 25, units = "cm", res = 300)
    }
    
    layout(matrix(1:(length(years)*2), nrow = length(years), byrow = T), 
           widths = c(1,.15), heights = c(1.18,1,1,1.26))

    for(yr in years){
      # yr <- years[4]
      
      cond_yr <- benchmarks$year == yr
      
      side1 <- side3 <- .5
      xaxt <- "n"
      main <- xlab <- ""
      if(yr == years[1]){
        side3 <- 3.5
        main <-  paste0(cu_file_name," - ",cuid)
        
      }else if(yr == years[4]){
        side1 <- 4.5
        xaxt <- 's'
        xlab <- "Year"
      }
      
      # Find the benchmark values
      polygons_show <- T
      if(biostatus$psf_status_type[1] == "sr"){
        benchmark_low <- benchmarks$sgen[cond_yr]
        benchmark_low_025 <- benchmarks$sgen_lower[cond_yr]
        benchmark_low_975 <- benchmarks$sgen_upper[cond_yr]
        
        if(any(grepl("smsy80",colnames(benchmarks)))){
          benchmark_up <- benchmarks$smsy80[cond_yr]
          benchmark_up_025 <- benchmarks$smsy80_lower[cond_yr]
          benchmark_up_975 <- benchmarks$smsy80_upper[cond_yr]
        }else{
          benchmark_up <- benchmarks$smsy[cond_yr]           # in this case it should still be smsy80 ; ask to update field name because it is super confusing
          benchmark_up_025 <- benchmarks$smsy_lower[cond_yr]
          benchmark_up_975 <- benchmarks$smsy_upper[cond_yr]
        }
        
        method <- "HBSR"
        status <- biostatus$sr_status
        
      }else if(biostatus$psf_status_type[1] == "percentile"){  # to remove eventually as this function is not for percentile 
        
        if(any(grepl("X25._spw",colnames(biostatus)))){
          benchmark_low <- benchmarks$X25._spw[cond_cl_yr]             # `25%_spw`
          benchmark_low_025 <- benchmarks$X25._spw_lower[cond_cl_yr]   # `25%_spw_lower`
          benchmark_low_975 <- benchmarks$X25._spw_upper[cond_cl_yr]   # `25%_spw_upper`
          benchmark_up <- benchmarks$X75._spw[cond_cl_yr]              # `75%_spw`
          benchmark_up_025 <- benchmarks$X75._spw_lower[cond_cl_yr]    # `75%_spw_lower`
          benchmark_up_975 <- benchmarks$X75._spw_upper[cond_cl_yr]    # `75%_spw_upper`
        }else{
          benchmark_low <- benchmarks$`25%_spw`[cond_cl_yr]
          benchmark_low_025 <- benchmarks$`25%_spw_lower`[cond_cl_yr]
          benchmark_low_975 <- benchmarks$`25%_spw_upper`[cond_cl_yr]
          benchmark_up <- benchmarks$`75%_spw`[cond_cl_yr]
          benchmark_up_025 <- benchmarks$`75%_spw_lower`[cond_cl_yr]
          benchmark_up_975 <- benchmarks$`75%_spw_upper`[cond_cl_yr]
        }
        
        method <- "Percentiles"
        status <- biostatus$percentile_status
        
        # COMMENT:
        # This is indeed the 50% percentile and not the 75, despite the name being "75%_spw"
        # https://salmonwatersheds.slack.com/archives/CJ5RVHVCG/p1707332952867199
        
      }else{ # there is no benchmark values
        print("There are no benchmark values for this CU.")
        polygons_show <- F
      }
      
      # adjust y_range eventually if values are below thresholds
      # if(any(max(y_range) < c(benchmark_low_025,benchmark_up_025))){
      #   y_range[2] <- benchmark_up_975 + benchmark_up_975 * .2
      # }
      
      # Get current spawner abundance
      csa <- benchmarks$curr_spw[cond_yr]
      
      par(mar = c(side1,5,side3,0))
      
      ylab <- "Estimated spawwner abundance"
      las <- 0
      if(log10_scale){
        
        benchmark_low <- log10(benchmark_low)
        benchmark_low_025 <- log10(benchmark_low_025)
        benchmark_low_975 <- log10(benchmark_low_975)
        benchmark_up <- log10(benchmark_up)
        benchmark_up_025 <- log10(benchmark_up_025)
        benchmark_up_975 <- log10(benchmark_up_975)
        
        if(is.infinite(benchmark_up)){
          benchmark_up <- 0
        }
        if(is.infinite(benchmark_up_025)){
          benchmark_up_025 <- 0
        }
        
        csa <- log10(csa)
        
        ylab <- "Estimated spawwner abundance (log10)"
        las <- 1
      }
      
      plot(NA, xlim = x_range, ylim = y_range, xaxs = 'i', yaxs = 'i', bty = 'l', 
           las = las, ylab = ylab, xlab = xlab, main = main, xaxt = xaxt)
      
      # polygons
      alpha <- 0.4
      if(polygons_show){
        polygon(x = c(x_range,rev(x_range)), 
                y = c(benchmark_up,benchmark_up,y_range[2],y_range[2]), 
                col = colour_transparency_fun(status_cols["green"],alpha = alpha),
                border = F)
        polygon(x = c(x_range,rev(x_range)), 
                y = c(benchmark_low,benchmark_low,benchmark_up,benchmark_up), 
                col = colour_transparency_fun(status_cols["amber"],alpha = alpha),border = F)
        polygon(x = c(x_range,rev(x_range)), 
                y = c(min(y_range),min(y_range),benchmark_low,benchmark_low), 
                col = colour_transparency_fun(status_cols["red"],alpha = alpha),border = F)
      }
      
      # grid - y
      digits_nb <- nchar(ceiling(y_range[2]))
      grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 1)
      factor <- paste(c(1,rep(0,(digits_nb - 1))), collapse = "") |> as.numeric()
      grid_vals <- 0:as.numeric(grid_max) * factor
      if(length(grid_vals) < 5){ # add a segment to each 1/2 intervals as well
        grid_max <- substr(x = ceiling(y_range[2]), start = 1, stop = 2)
        grid_vals <- 0:as.numeric(grid_max) 
        grid_vals <- grid_vals[grid_vals %% 5 == 0]
        grid_vals <- grid_vals * factor / 10
      }
      if(length(grid_vals) < 5){ # add a segment to each 1/5 intervals as well
        grid_vals <- 0:as.numeric(grid_max) 
        grid_vals <- grid_vals[grid_vals %% 2 == 0]
        grid_vals <- grid_vals * factor / 10
      }
      segments(x0 = rep(x_range[1],length(grid_vals)),
               x1 = rep(x_range[2],length(grid_vals)),
               y0 = grid_vals, y1 = grid_vals,
               lwd = 2, col = colour_transparency_fun("white",alpha = .5))
      
      # grid - x
      yrs <- min(spawnerAbund$year):max(spawnerAbund$year)
      grid_vals <- yrs[yrs %% 10 == 0]
      if(length(grid_vals) < 3){ # add a segment to each 5 years
        grid_vals <- yrs[yrs %% 5 == 0]
      }
      segments(y0 = rep(y_range[1],length(grid_vals)),
               y1 = rep(y_range[2],length(grid_vals)),
               x0 = grid_vals, x1 = grid_vals,
               lwd = 2, col = colour_transparency_fun("white",alpha = .5))
      
      # Plot current spawner abundance
      cond <- status[which(yr == years)] == c("good","fair","poor","hfjfk","djahdjk")
      col_csa <- status_cols[cond]
      if(is.na(col_csa[1])){
        col_csa <- colour_transparency_fun("black",alpha = alpha)
      }
      segments(x0 = benchmarks$curr_spw_start_year[cond_yr], x1 = x_range[2], 
               y0 = csa, y1 = csa, lwd = 3, col = col_csa)
      
      # show benchmarks
      segments(x0 = c(x_range[1],x_range[1]), x1 = c(x_range[2],x_range[2]),
               y0 = c(benchmark_low,benchmark_up), y1 = c(benchmark_low,benchmark_up), 
               col = c(status_cols["red"], status_cols["green"]), lwd = 2)
      
      # plot estimated spawner abundance
      # remove odd and even years for PKO and PKE, respectively so the dots are 
      # connected.
      if(species_abbr == "PKO"){
        cond_to_keep <- spawnerAbund$year %% 2 == 1
      }else if(species_abbr == "PKE"){
        cond_to_keep <- spawnerAbund$year %% 2 == 0
      }else{
        cond_to_keep <- rep(T,length(spawnerAbund$year))
      }
      x <- spawnerAbund$year[cond_to_keep]
      y <- spawnerAbund$estimated_count[cond_to_keep]
      if(log10_scale){
        y <- log10(y)
      }
      points(x = x, y = y, pch = 16, type = 'o', lwd = 2, 
             col = colour_transparency_fun("black",alpha = alpha))
      
      # display issue
      cond_issue <- (benchmark_low > benchmark_up) | is.na(benchmark_low) | is.na(benchmark_up)
      if(cond_issue){
        legend("top","BENCHMARK ISSUE",bty = 'n', text.col = "red")
      }
      
      # Extend benchmarks and plot 95% CI and current spawner abundance
      main <- ""
      if(yr == years[1]){
        main <- method
      }
      par(mar = c(side1,0,side3,0.5))
      plot(NA, xlim = c(0,1), ylim = y_range, ylab = "", xlab = "", 
           main = main, cex.main = .9, font.main = 1, # plain font
           xaxt = 'n', yaxt = 'n', bty = 'n', xaxs = 'i', yaxs = 'i')
      
      # plot the current spawner abundance
      segments(x0 = 0, x1 = .8, y0 = csa, y1 = csa, lwd = 3, col = col_csa)
      
      # benchmarks
      segments(x0 = c(0,0), y0 = c(benchmark_low,benchmark_up),
               x1 = c(.4,.6), y1 = c(benchmark_low,benchmark_up), 
               col = c(status_cols["red"],status_cols["green"]), lwd = 2)
      points(x = c(.4,.6), y = c(benchmark_low,benchmark_up), pch = 16, 
             col = c(status_cols["red"],status_cols["green"]))
      segments(x0 = c(.4,.6), y0 = c(benchmark_low_025,
                                     benchmark_up_025),
               x1 = c(.4,.6), y1 = c(benchmark_low_975,
                                     benchmark_up_975), 
               col = c(status_cols["red"],status_cols["green"]), lwd = 2)
      
      legend("right",legend = benchmarks$curr_spw_end_year[cond_yr],bty = 'n')
      
      # 
    } # for each cyr
    
    if(figure_print){
      dev.off()
    }
  } # if the CU is cyclic
}

#' Function to run the HBSRM models with JAGS. Possibility to run the Ricker model 
#' or the Larking model (for CUs with cyclic dynamics) with all the possible 
#' combination of predicting variables (as in Grant et al. 2020. The 2017 Fraser
#' sockeye...)
HBSRM_JAGS_fun <- function(model_name = c("Ricker",
                                          "Larkin_123",
                                          "Larkin_1","Larkin_2","Larkin_3",
                                          "Larkin_12","Larkin_13","Larkin_23"), 
                           modelFilename = NA,
                           jags.data,
                           jags.parms,
                           n.iter = 10000,
                           n.thin = 10,
                           n.burnin = 3000,
                           n.chains = 6){
  
  require(R2jags)
  
  if(is.na(modelFilename)){
    modelFilename <- paste0("HBSRM_",model_name,".txt")
    
  }else if(!grepl(".txt",modelFilename)){
    modelFilename <- paste0(modelFilename,".txt")
  }
  
  if(model_name == "Ricker"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	# For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 1:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_123"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b1[i] ~ dunif(0,100)
        		b2[i] ~ dunif(0,100)
        		b3[i] ~ dunif(0,100)
        		
         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 4:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b1[i] * S[j - 1, i] - b2[i] * S[j - 2, i] - b3[i] * S[j - 3, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_1"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b1[i] ~ dunif(0,100)

         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 2:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b1[i] * S[j - 1, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_2"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b2[i] ~ dunif(0,100)

         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 3:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b2[i] * S[j - 2, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_3"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b3[i] ~ dunif(0,100)

         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 4:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b3[i] * S[j - 3, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_12"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b1[i] ~ dunif(0,100)
        		b2[i] ~ dunif(0,100)
        		
         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 3:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b1[i] * S[j - 1, i] - b2[i] * S[j - 2, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_13"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b1[i] ~ dunif(0,100)
        		b3[i] ~ dunif(0,100)
        		
         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 4:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b1[i] * S[j - 1, i] - b3[i] * S[j - 3, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
    
  }else if(model_name == "Larkin_23"){
    cat("
        model{
        
        	# Hyper priors
        	
        	log_mu_a ~ dnorm(0.5, 1.0E-6)
        	mu_a <- exp(log_mu_a)
        	tau_a ~ dgamma(0.5, 0.5) 
        	sd_a <- pow(tau_a, -0.5)
        	
        	for(i in 1:nCUs) {	                  # For each CU, draw estimates from hyperdistribution
        	
        		a[i] ~ dnorm(mu_a, tau_a)
        		
        		b[i] ~ dlnorm(prmub[i], prtaub[i])	# prior on CU-dependent b
        		
        		b2[i] ~ dunif(0,100)
        		b3[i] ~ dunif(0,100)
        		
         		# sigma for the model likelihood estimation
        		sd[i] ~ dunif(0.05, 10)
        		tau[i] <- pow(sd[i], -2)	
        	}
        	
        	for(i in 1:nCUs){
        		for(j in 4:nYrs){
        		
        		 	# Model prediction for log R/S based on estimated parameters
        		 	pred_lnRS[j, i] <- a[i] - b[i] * S[j, i] - b2[i] * S[j - 2, i] - b3[i] * S[j - 3, i]
        		 	
        		 	# Likelihood
        		 	obs_lnRS[j, i] ~ dnorm(pred_lnRS[j, i], tau[i])
        		}
        	}
        }", fill = TRUE, file = modelFilename)
  }
  
  # Run Model
  print("Running Parallel")
  # **SP: Why have this message when it's not actually running in parallel?
  # **BSC: TODO: get the parallele to work on windows, MAC and Linuxf
  # https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
  
  ptm = proc.time()
  
  jagsfit.p <- jags(data = jags.data,  
                    parameters.to.save = jags.parms,
                    n.thin = n.thin,                     # thinning rate
                    n.iter = n.iter,  # n.iter 100000
                    model.file = modelFilename, 
                    n.burnin = n.burnin, # 5000
                    n.chains = n.chains) # 6
  
  endtime <- proc.time()-ptm
  endtime[3]/60
  
  post <- as.mcmc(jagsfit.p)
  
  out <- list(jagsfit.p,post)
  names(out) <- c("jagsfit.p","post")
  return(out)
}

#' Function that return prior values for the distribution of the beta 
#' parameter in the Ricker model: prCV and prSmax
#' with beta ~ logNormal(log(1/prSmax),prCV)
#' The default values are:
#' - prSmax = geometric mean (S)
#' - prCV = 10, which is highly non informative (note that a more informative 
#' value of 1 is used if there are convergence issues --> not in the scope of
#'this function).
#' Several SEL Cus have prSmax estimated using photosynthetic-based modelling
#' approach, in which case the values come from DFO report. In those cases, 
#' prCV = 0.3. This is the case for several SEL in Skeena, and prSmax values
#' come from (Korman & English 2013).
#' Note that Grant et al. 2020 also used photosynthetic-based values for prSmax
#' for SEL CUs in the Fraser (cf. Table 10) but it was decided in during a PSAC 
#' meeting in April 2019 to not use them:
#' https://www.dropbox.com/scl/fi/lht2ln9c7srvi3l3k4guw/PSAC-meeting-2-notes-final.docx?rlkey=nypzeiokod8bey2u2419ieeos&dl=0
#' 
# cuid <- CUs_cuid
# Sm <- SR_l$S
prior_beta_Ricker_fun <- function(cuid,conservationunits_decoder = NA,wd,Sm,prior_extern = NA){
  
  if(all(is.na(conservationunits_decoder))){
    
    conservationunits_decoder <- import_mostRecent_file_fun(wd = wd,
                                                            pattern = "conservationunits_decoder")
  }
  
  cond <- conservationunits_decoder$cuid %in% cuid
  coln <- c("region","species_name","cu_name_pse","cuid")
  cu_prior_df <- conservationunits_decoder[cond,coln]
  cu_prior_df$prSmax <- NA
  cu_prior_df$prCV <- 10
  
  if(nrow(cu_prior_df) != length(cuid)){
    print("PROBLEM: In prior_beta_Ricker_fun(): cuid(s) not in conservationunits_decoder")
  }
  
  #'* CU in the additional external dataset provide *
  if(any(cuid %in% prior_extra$cuid)){
    cuid_concerned <- cuid[cuid %in% prior_extra$cuid]
    
    for(cu in cuid_concerned){
      # cu <- cuid_concerned[1]
      cond <- prior_extra$cuid == cu
      prSmax_here <- prior_extra$prSmax[cond]
      
      cond <- cu_prior_df$cuid == cu
      cu_prior_df$prSmax[cond] <- prSmax_here
      cu_prior_df$prCV[cond] <- 0.3 # more informative than 10
    }
  }
  
  #'* Fraser SEL for which prSmax is estimated with a photosynthetic-based *
  # approach (from Grant et al. 2020; cf. Table 10)
  # cuid_Fraser_SEL <- c(711,715,721,735)
  cuid_Fraser_SEL <- c()
  
  use_habitat_prior_Fraser <- F # it was decided to not use 
  
  # The other CUs in Table 10 for which photosynthetic-based prSmax is obtained 
  # but not used --> we will ask if we should use them or not at our next PSCA meeting
  # TO KEEP COMMENTED OUT UNTIL THEN
  # cuid_Fraser_SEL <- c(cuid_Fraser_SEL,712,714,716,719,725,727,729,740) 
  
  if(nrow(cu_prior_df) != length(cuid)){
    print("PROBLEM: In prior_beta_Ricker_fun(): cuid(s) not in conservationunits_decoder")
  }
  
  if(any(cuid %in% cuid_Fraser_SEL) & use_habitat_prior_Fraser){
    
    cuid_concerned <- cuid[cuid %in% cuid_Fraser_SEL]
    
    cond <- cu_prior_df$cuid %in% cuid_Fraser_SEL
    cu_prior_df$prCV[cond] <- 0.3
    
    # create a data frame with the CU-specific values for prSmx
    cond <- conservationunits_decoder$cuid %in% cuid_concerned
    coln <- c("region","species_name","cu_name_pse","cuid")
    cuid_Fraser_SEL_df <- conservationunits_decoder[cond,coln]
    cuid_Fraser_SEL_df$prSmax <- NA
    cuid_Fraser_SEL_df$prCV <- 0.3
    
    cond <- cuid_Fraser_SEL_df$cu_name_pse == "Chilliwack-Early Summer (cyclic)"
    cuid_Fraser_SEL_df$prSmax[cond] <- 41000
    cond <- cuid_Fraser_SEL_df$cu_name_pse == "Pitt-Early Summer"
    cuid_Fraser_SEL_df$prSmax[cond] <- 115000
    cond <- cuid_Fraser_SEL_df$cu_name_pse == "Chilko-Summer"
    cuid_Fraser_SEL_df$prSmax[cond] <- 483000
    cond <- cuid_Fraser_SEL_df$cu_name_pse == "Bowron-Early Summer"
    cuid_Fraser_SEL_df$prSmax[cond] <- 41000
    
    # TO KEEP COMMENTED OUT until next PSAC meeting
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Cultus-Late"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 4125
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Harrison-Upstream Migrating-Late"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 85000
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Lillooet-Harrison-Late"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 164000
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Anderson-Seton-Early Summer"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 286000
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Francois-Fraser-Summer"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 600000
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Nadina-Francois-Early Summer"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 1350000
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Seton-Late (de novo)"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 188000
    # cond <- cuid_Fraser_SEL_df$cu_name_pse == "Kamloops-Early Summer"
    # cuid_Fraser_SEL_df$prSmax[cond] <- 445000
    
    # replace values in cu_prior_df
    for(cu in cuid_concerned){
      # cu <- cuid_concerned[1]
      cond <- cuid_Fraser_SEL_df$cuid == cu
      prSmax_here <- cuid_Fraser_SEL_df$prSmax[cond]
      
      cond <- cu_prior_df$cuid == cu
      cu_prior_df$prSmax[cond] <- prSmax_here
    }
  } # End Fraser SEL for which prSmax is estimated with a photosynthetic-based (Korman & English 2013)
  
  # For the rest of the CUs: prSmax = geo_mean(S):
  cond <- ! cuid %in% cuid_Skeena_SEL & ! cuid %in% cuid_Fraser_SEL
  cuid_concerned <- cuid[cond]
  
  cond <- is.na(cu_prior_df$prSmax)
  cuid_concerned <- cu_prior_df$cuid[cond]
  for(cu in cuid_concerned){
    # cu <- cuid_concerned[2]
    cond <- conservationunits_decoder$cuid == cu
    cu_name_pse <- conservationunits_decoder$cu_name_pse[cond]
    prSmax_here <- mean_geom_fun(x = Sm[,cu_name_pse])
    
    cond <- cu_prior_df$cuid == cu
    cu_prior_df$prSmax[cond] <- prSmax_here
  }
  
  return(cu_prior_df)
}

