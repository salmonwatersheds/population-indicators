

#' Function that returns a dataframe of the standard names of the NuSEDS datasets 
#' used in the present code.
NuSEDS_datasets_names_fun <- function(){
  output <- data.frame(all_areas_nuseds = "all_areas_nuseds",
                       conservation_unit_system_sites = "conservation_unit_system_sites")
  return(output)
}

#' Function to return one of the datasets (as a data frame) from the PSF database.
#' The function replace the values "-989898" by NA.
#'- name_dataSet: the name of the NuSEDS datasets given in NuSEDS_datasets_names_fun()
#'- from_NuSEDS_website: if TRUE, the dataset is downloaded from the NuSEDS website and saved in wd.
#'- wd: where the CSV files are located.
# wd <- wd_data_dropbox
#' TODO: find a way to check the date of last modification from API and compare it
#' to the local version of the file, then decide what to do. Packages and functions
#' to use:
#' - file.info()$mtime : file (last) modification date
#' - httr::HEAD(API_NuSEDS)$date: that does not work with the API_NuSEDS...
datasets_NuSEDS_fun <- function(name_dataSet, from_NuSEDS_website = F, wd){
  
  name_dataSet_avail <- NuSEDS_datasets_names_fun()
  
  if(!name_dataSet %in% name_dataSet_avail){
    print("The name of the dataset entered is not in NuSEDS_datasets_names_fun().")
    output <- NA
  }else{
    
    if(from_NuSEDS_website){
      
      print(paste0("Downloading the ",name_dataSet," data from the NuSEDS website to ",wd))
      
      if(name_dataSet == name_dataSet_avail$all_areas_nuseds){
        options(timeout = 190)
        url <- "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/c48669a3-045b-400d-b730-48aafe8c5ee6/attachments/All%20Areas%20NuSEDS.csv"

      }else if(name_dataSet == name_dataSet_avail$conservation_unit_system_sites){
        url <-"https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/c48669a3-045b-400d-b730-48aafe8c5ee6/attachments/conservation_unit_system_sites.csv"

      }
      nusedsFileName <- paste0(wd,"/",name_dataSet,"_", strftime(Sys.Date(), format = "%Y%m%d"), ".csv")
      download.file(url = url, destfile = nusedsFileName)
      
    }else{
      
      pattern <- paste0(name_dataSet,"_")
      listFiles <- list.files(path = wd)
      listFiles <- listFiles[grep(pattern = pattern, listFiles)]
      files_dates <- unlist(lapply(strsplit(listFiles, split = ".csv"), strsplit, split = pattern))
      files_dates <- as.Date(files_dates[which(files_dates != "")], format = "%Y%m%d")
      
      nusedsFileName <- listFiles[which(order(files_dates, decreasing = TRUE) == 1)]
      nusedsFileName <- paste(wd,nusedsFileName,sep="/")
    }
    
    output <- read.csv(nusedsFileName, header = T)
  }
  
  return(output)
}

# QUESTION: why doing this?
#' Function to convert the areas of the 'map of Pacific Fishery Management Areas"
#' (https://www.pac.dfo-mpo.gc.ca/fm-gp/maps-cartes/areas-secteurs/index-eng.html)
#' into???
Convert2StatArea <- function(area){
  StatArea <- as.character(area)
  StatArea[area %in% c("3A", "3B")] <- 3
  StatArea[area %in% c("4A", "4B", "4C", "4D")] <- 4
  valHere <- c("1", "2W", "3", "4", "5", "6", "7", "8", "9")
  StatArea[area %in% valHere] <- paste0("0", StatArea[area %in% valHere])
  return(StatArea)
}

#' Function to Compares the column names of two dataframes and print the 
#' differences if any.
compareColNamesDF_fun <- function(DF1,DF2){
  colnames1 <- colnames(DF1)
  colnames2 <- colnames(DF2)
  if(!identical(colnames1, colnames2)){
    print("Column names only in first dataframe:")
    toPrint <- colnames1[!colnames1 %in% colnames2]
    print(toPrint)
    cat("\n")
    print("Column names only in second dataframe:")
    toPrint <- colnames2[!colnames2 %in% colnames1]
    print(toPrint)
    cat("\n")
    print("Column names in both dataframes:")
    toPrint <- colnames2[colnames2 %in% colnames1]
    print(toPrint)
    cat("\n")
  }else{
    print("Column names are identical.")
  }
}

#' Function to return a list of list of the description of the fields in 
#' all_areas_nuseds and conservation_unit_system_sites.
# wd_references <- wd_references_dropbox
nuseds_fields_definitions_fun <- function(wd_references){
  
  #' Sources of information:
  #' - Data Dictionary NuSEDS: 
  #'    - /references/nuseds_report_definitions.csv
  #'    - https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/60c2827f-c439-3b37-ab84-42515eb1b521
  #'    - 
  #' Conservation_Unit_Report_Definitions:
  #' - /references/conservation_unit_report_definitions.csv
  #' - https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/894ba9df-8931-3cf1-bf4c-ba8876bcf515
  
  nuseds_report_def <- read.csv(paste(wd_references,"nuseds_report_definitions.csv",
                                      sep = "/"))
  conservation_unit_report_def <- read.csv(paste(wd_references,"conservation_unit_report_definitions.csv",
                                                 sep = "/"))
  
  nuseds_report_def_l <- list()
  for(r in 1:nrow(nuseds_report_def)){
    nuseds_report_def_l[[r]] <- nuseds_report_def$Field.Definition[r]
  }
  names(nuseds_report_def_l) <- nuseds_report_def$Field.Name
  
  conservation_unit_report_def_l <- list()
  for(r in 1:nrow(conservation_unit_report_def)){
    conservation_unit_report_def_l[[r]] <- conservation_unit_report_def$Field.Description[r]
  }
  names(conservation_unit_report_def_l) <- conservation_unit_report_def$Field.Name
  
  fields_def <- list(nuseds_report_def_l,conservation_unit_report_def_l)
  names(fields_def) <- c("all_areas_nuseds","cu_system_sites")
  return(fields_def)
}

#' Function to plot "MAX_ESTIMATE" (or "Returns") time series from the 
#' modified all_areas_nuseds data.
#' Options:
#' - show all GFE_IDs corresponding to each IndexId in IndexIds (vector).
#' - show all IndexIds corresponding to each GFE_ID in GFE_IDs (vector)
#'    - possibility in that later case to filter for certain species given as 
#'      acronyms (i.e. CM, CO, CN, SX, PK, PKE, PKO)
# IndexIds <- c("PKO_52704","SX_3302")
# GFE_IDs <- NA
# species_acro <- NA
# IndexIds <- iid
# GFE_IDs <- c(11485,3444)
# species_acro <- c("PK","CM")
plot_IndexId_GFE_ID_fun <- function(IndexIds = NA, GFE_IDs = NA, species_acro = NA,
                                    all_areas_nuseds, 
                                    xaxt = 's', yaxt = 's', xlab = NA, ylab = NA,
                                    Xlim = NA, Ylim = NA, main = "",
                                    y_var_name = c("MAX_ESTIMATE","Returns")){
  
  y_var_name <- y_var_name[1]
  
  # in case both IndedIds and GFE_IDs are provided and have the same length
  if(!all(is.na(IndexIds)) & !all(is.na(GFE_IDs)) & length(IndexIds) == length(GFE_IDs)){
    
    nusedsHere <- lapply(X = 1:length(IndexIds), FUN = function(i){
      cond <- all_areas_nuseds$IndexId == IndexIds[i] &
        all_areas_nuseds$GFE_ID == as.numeric(GFE_IDs[i])
      return(all_areas_nuseds[cond,])
    })
    nusedsHere <- do.call(rbind,nusedsHere)

    # if only certain species are displayed:
    if(all(!is.na(species_acro))){
      nusedsHereSub_l <- lapply(X = species_acro, FUN = function(s){
        out <- nusedsHere[grepl(s,nusedsHere$IndexId),]
        return(out)
      })
      nusedsHere <- do.call(rbind,nusedsHereSub_l)
    }
    
    if(is.na(xlab)){
      xlab = "Years"
    }
    if(is.na(ylab)){
      ylab = y_var_name
    }
    
    if(nrow(nusedsHere) == 0){ # in case there is no time series anymore 
      plot(NA, xlim = c(0,1), ylim = c(0,1), ylab = ylab, xlab = xlab, main = main, 
           xaxt = xaxt, yaxt = yaxt)
      
    }else{
      
      yr_min <- min(nusedsHere$Year)
      yr_max <- max(nusedsHere$Year)
      yrs <- yr_min:yr_max
      pop_max <- max(nusedsHere[,y_var_name], na.rm = T)
      if(is.infinite(pop_max)){
        pop_max <- 1
      }
      
      colfunc <- colorRampPalette(c("firebrick","chartreuse3","black","deepskyblue3"))
      cols <- colfunc(length(IndexIds))
      ltys <- 1:length(IndexIds)
      pchs <- 1:length(IndexIds)
      
      if(all(is.na(Xlim))){
        xlim <- c(yr_min-(yr_max - yr_min)/5,yr_max)
      }else{
        xlim <- Xlim
      }
      if(all(is.na(Ylim))){
        ylim <- c(0,pop_max + pop_max / 5)
      }else{
        ylim <- Ylim
      }
      
      plot(NA, xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab, main = main, 
           xaxt = xaxt, yaxt = yaxt)
      
      for(s in 1:length(IndexIds)){
        # s <- 1
        cond <- nusedsHere$IndexId == IndexIds[s] &
          nusedsHere$GFE_ID == as.numeric(GFE_IDs[s])
        dataHere <- nusedsHere[cond,]
        dataHere <- dataHere[order(dataHere$Year),c(y_var_name,"Year")]
        # print(dataHere)
        lines(y = dataHere[,y_var_name], x = dataHere$Year, lwd = 2, col = cols[s], 
              lty = ltys[s])
        points(y = dataHere[,y_var_name], x = dataHere$Year, 
               pch = pchs[s], col = cols[s], lwd = 2)
      }
      
      series_name <- sapply(X = 1:length(IndexIds),FUN = function(i){
        out <- paste(IndexIds[i],as.numeric(GFE_IDs[i]), sep = " - ")
        return(out)
      })

      legend("topleft",series_name, col = cols, lwd = 3, bty = "n", 
             lty = ltys, pch = pchs)
    }
    
  }else{
    
    # plot all the GFE_IDs found for each IndexId in IndexIds
    if(!all(is.na(IndexIds)) & all(is.na(GFE_IDs))){
      var_out <- "IndexId"
      var_in <- "GFE_ID"
      var_out_vals <- IndexIds
      
      # plot all the IndexIds for each GFE_ID in GFE_IDs 
    }else if(all(is.na(IndexIds)) & !all(is.na(GFE_IDs))){
      var_out <- "GFE_ID"
      var_in <- "IndexId"
      var_out_vals <- GFE_IDs
    }
    
    # plot each IndexIds & GFE_IDs series (no hierarchy in the variables) 
    for(var_out_val in var_out_vals){
      # var_out_val <- var_out_vals[1]
      nusedsHere <- all_areas_nuseds[all_areas_nuseds[,var_out] == var_out_val,]
      
      # if only certain species are displayed:
      if(all(!is.na(species_acro))){
        nusedsHereSub_l <- lapply(X = species_acro, FUN = function(s){
          out <- nusedsHere[grepl(s,nusedsHere$IndexId),]
          return(out)
        })
        nusedsHere <- do.call(rbind,nusedsHereSub_l)
      }
      
      if(is.na(xlab)){
        xlab = "Years"
      }
      if(is.na(ylab)){
        ylab = y_var_name
      }
      
      if(nrow(nusedsHere) == 0){ # in case there is no time series anymore 
        plot(NA, xlim = c(0,1), ylim = c(0,1), ylab = ylab, xlab = xlab, main = main, 
             xaxt = xaxt, yaxt = yaxt)
        
      }else{
        
        var_in_vals <- unique(nusedsHere[,var_in])
        yr_min <- min(nusedsHere$Year)
        yr_max <- max(nusedsHere$Year)
        yrs <- yr_min:yr_max
        pop_max <- max(nusedsHere[,y_var_name], na.rm = T)
        if(is.infinite(pop_max)){
          pop_max <- 1
        }
        
        colfunc <- colorRampPalette(c("firebrick","chartreuse3","black","deepskyblue3"))
        cols <- colfunc(length(var_in_vals))
        ltys <- 1:length(var_in_vals)
        pchs <- 1:length(var_in_vals)
        
        if(all(is.na(Xlim))){
          xlim <- c(yr_min-(yr_max - yr_min)/5,yr_max)
        }else{
          xlim <- Xlim
        }
        if(all(is.na(Ylim))){
          ylim <- c(0,pop_max + pop_max / 5)
        }else{
          ylim <- Ylim
        }
        
        plot(NA, xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab, main = main, 
             xaxt = xaxt, yaxt = yaxt)
        
        for(var_in_val in var_in_vals){
          # var_in_val <- var_in_vals[1]
          i <- which(var_in_val == var_in_vals)
          dataHere <- nusedsHere[nusedsHere[,var_in] == var_in_val,]
          dataHere <- dataHere[order(dataHere$Year),c(y_var_name,"Year")]
          # print(dataHere)
          lines(y = dataHere[,y_var_name], x = dataHere$Year, lwd = 2, col = cols[i], 
                lty = ltys[i])
          points(y = dataHere[,y_var_name], x = dataHere$Year, 
                 pch = pchs[i], col = cols[i], lwd = 2)
        }
      }
      
      legend("topleft",c(paste(var_out,"=",var_out_val),paste(var_in,"=",var_in_vals)), 
             col = c(NA,cols), lwd = 3, bty = "n", lty = c(NA,ltys), pch = c(NA,pchs))
      
    }
  }
}

#' 
plot_IndexId_GFE_ID_COMBO_fun <- function(iid_i,
                                          IndexId_GFE_ID_dupli,
                                          all_areas_nuseds){
  
  data <- IndexId_GFE_ID_dupli[IndexId_GFE_ID_dupli$iid_i == iid_i,]
  print(data)
  
  IndexId <- unique(data$IndexId)
  GFE_IDs <- data$GFE_ID
  
  condition <- all_areas_nuseds$IndexId == IndexId
  yrs_range_1 <- range(all_areas_nuseds$Year[condition],na.rm = T)
  var_range_1 <- range(all_areas_nuseds$MAX_ESTIMATE[condition],na.rm = T)
  
  sp_acro <- strsplit(data$IndexId, split = "_")[[1]][1]
  condition <- all_areas_nuseds$GFE_ID %in% data$GFE_ID & 
    grepl(sp_acro,all_areas_nuseds$IndexId)
  yrs_range_2 <- range(all_areas_nuseds$Year[condition],na.rm = T)
  var_range_2 <- range(all_areas_nuseds$MAX_ESTIMATE[condition],na.rm = T)
  
  Xlim <- c(min(c(yrs_range_1,yrs_range_2)),max(c(yrs_range_1,yrs_range_2)))
  Ylim <- c(min(c(var_range_1,var_range_2)),max(c(var_range_1,var_range_2)))
  
  nplots <- nrow(data) + 1
  layout(matrix(1:nplots,ncol = 1), heights = c(rep(1,nrow(data)),1.3))
  
  par(mar = c(.5,4.5,.5,.5))
  plot_IndexId_GFE_ID_fun(GFE_IDs = data$GFE_ID[data$iid_i == iid_i],
                          species_acro = sp_acro,
                          all_areas_nuseds = all_areas_nuseds, 
                          xaxt = 'n', xlab = "", Xlim = Xlim, Ylim = Ylim)
  
  par(mar = c(4.5,4.5,.5,.5))
  plot_IndexId_GFE_ID_fun(IndexIds = data$IndexId[data$iid_i == iid_i][1],
                          all_areas_nuseds = all_areas_nuseds, Xlim = Xlim, Ylim = Ylim)
  
  legend("topright",paste("iid_i =",iid_i),bty = "n")
}

#' Function to return the number and % of MAX_ESTIMATE data points overlapping 
#' between two time series defined by IndexIds and GFE_IDs.
# IndexIds <- unique(IndexId_GFE_ID_dupli$IndexId[IndexId_GFE_ID_dupli$iid_i == 6])
# GFE_IDs <- unique(IndexId_GFE_ID_dupli$GFE_ID[IndexId_GFE_ID_dupli$iid_i == 6])
overlap_two_timeSeries_fun <- function(IndexIds, GFE_IDs,
                                       all_areas_nuseds = all_areas_nuseds){
  
  data <- data.frame(IndexId = IndexIds, 
                     GFE_ID = GFE_IDs)
  
  
  all_areas_nuseds$IndexId
  nusedsHere <- all_areas_nuseds[,c("IndexId","GFE_ID","MAX_ESTIMATE","Year")]
  
  nuseds_sub <- NULL
  for(r in 1:nrow(data)){
    condition <- all_areas_nuseds$IndexId == data$IndexId[r] &
      all_areas_nuseds$GFE_ID == data$GFE_ID[r] 
    out <- all_areas_nuseds[condition,c("IndexId","GFE_ID","MAX_ESTIMATE","Year")]
    out <- out[!is.na(out$MAX_ESTIMATE),]
    
    # check for duplicated years
    if(sum(duplicated(out$Year))){
      yrs <- out$Year[duplicated(out$Year)]
      print("There are duplicated years in this series:")
      print(out[out$Year %in% yrs,])
    }
    
    if(is.null(nuseds_sub)){
      nuseds_sub <- out
    }else{
      nuseds_sub <- rbind(nuseds_sub,out)
    }
  }
  
  data$nb_dataPoints <- sapply(X = 1:nrow(data), FUN = function(r){
    condition <- nuseds_sub$IndexId == data$IndexId[r] &
      nuseds_sub$GFE_ID == data$GFE_ID[r]
    out <- sum(condition)
    return(out)
  })
  
  data$nb_dataPoints_overlap <- sum(duplicated(nuseds_sub[,c("MAX_ESTIMATE","Year")]))
  data$nb_dataPoints_overlap_percent <- round(data$nb_dataPoints_overlap / data$nb_dataPoints * 100,2)
  
  return(data)
}

#' Function to return a subset of all_areas_nuseds with duplicated MAX_ESTIMATE.
# condition <- IndexId_GFE_ID_dupli$iid_i == 7 & is.na(IndexId_GFE_ID_dupli$SYSTEM_SITE)
# IndexId <- IndexId_GFE_ID_dupli$IndexId[condition]
# GFE_ID <- IndexId_GFE_ID_dupli$GFE_ID[condition]
is_MAX_ESTIMATE_duplicate_fun <- function(IndexId,GFE_ID,all_areas_nuseds,shortCol = T){
  
  # remove the series to check for duplicates for
  condition <- all_areas_nuseds$IndexId == IndexId & 
    all_areas_nuseds$GFE_ID == GFE_ID
  nuseds_cut <- all_areas_nuseds[!condition,]
  nuseds_cut <- nuseds_cut[!is.na(nuseds_cut$MAX_ESTIMATE),]
  
  # isolate the focal time series
  nuseds_focal <- all_areas_nuseds[condition,]
  nuseds_focal <- nuseds_focal[!is.na(nuseds_focal$MAX_ESTIMATE)]
  
  if(shortCol){
    colNuSEDS <- c("SPECIES","IndexId","GFE_ID","WATERBODY","Year","MAX_ESTIMATE")
  }else{
    colNuSEDS <- colnames(all_areas_nuseds)
  }
  
  if(nrow(nuseds_focal) == 0){
    print("There is no data for this combination of IndexId and GFE_ID")
  }else{
    
    # for each data point in nuseds_focal, look if there is a duplicate in nuseds_cut
    for(r in 1:nrow(nuseds_focal)){
      # r <- 1
      condition <- nuseds_cut$Year == nuseds_focal$Year[r] &
        nuseds_cut$MAX_ESTIMATE == nuseds_focal$MAX_ESTIMATE[r]
      
      if(sum(condition) == 0){
        print("No duplicate for:")
        print(nuseds_focal[r,c("SPECIES","IndexId","GFE_ID","WATERBODY","Year","MAX_ESTIMATE")])
        print("***")
      }else{
        print("There are duplicates for:")
        print(nuseds_focal[r,c("SPECIES","IndexId","GFE_ID","WATERBODY","Year","MAX_ESTIMATE")])
        print("They are:")
        print(nuseds_cut[condition,colNuSEDS])
        print("***")
      }
    }
  }
}


#' Function that takes one IndexId ('iid') from conservation_unit_system_sites and
#' - 1) check if there are multiple GFE_IDs associated
#' - 2) else look if there is a time series with the iid & its GFE_ID ('gfeid') in 
#'      all_areas_nuseds
#' - 3) if there is not, that could be due to either (i) a typo in the IndexId or
#'      (ii) a typo in the GFE_ID. The the rest of the code looks for potential
#'      alternative series with either a different IndexId (but with the same species)
#'      or a different GFE_ID. Alternative series identified NOT present in 
#'      conservation_unit_system_sites are kept, the ones present are removed.
#' The function returns a simple dataframe with the IndexId and GFE_ID concerned
#' and associated comment and eventual potential alternative series that have to 
#' be checked manually after.
#' The function is used in cuss_nuseds_match_parallel_fun() to spead up the process.
#' Alternatively, it can also be used within a for loop like below (but that takes
#' much more time).
# for(iid in unique(conservation_unit_system_sites$IndexId)){
#   # iid <- unique(conservation_unit_system_sites$IndexId)[174]
#   i <- which(unique(conservation_unit_system_sites$IndexId) == iid)
#   trackRecord <- cuss_nuseds_match_single_fun(IndexId = iid, i = i,
#                                               conservation_unit_system_sites = conservation_unit_system_sites,
#                                               all_areas_nuseds = all_areas_nuseds)
#   
# }
cuss_nuseds_match_single_fun <- function(IndexId, i, prog_steps = 10,
                                         conservation_unit_system_sites, 
                                         all_areas_nuseds){
  
  iid <- IndexId[1]
  
  trackRecord <- NULL
  
  # iid <- unique(conservation_unit_system_sites$IndexId)[174] # case were the iid & gfeid is not in nuseds
  # iid <- unique(conservation_unit_system_sites$IndexId)[3489]
  condi_cuss_iid <- conservation_unit_system_sites$IndexId == iid
  gfeid <- conservation_unit_system_sites$GFE_ID[condi_cuss_iid]
  
  prog <- i/nrow(conservation_unit_system_sites) * 100
  if(prog > prog_steps){
    prog_steps <- prog_steps + 10
    print(paste("Progress:",round(prog,1),"%"))
  }
  
  trackRecord_here <- data.frame(i = i,
                                 IndexId = iid, 
                                 GFE_ID = gfeid,
                                 in_cuss = "yes",
                                 in_nused = "",
                                 comment = "")
  
  condi_nuseds_iid <- all_areas_nuseds$IndexId == iid
  
  if(length(gfeid) > 1){ # there one case with SX_45525
    
    trackRecord_here$comment <- paste("In CUSS: there are multiple GFE_IDs for",iid,":",
                                      paste(gfeid,collapse = ", "))
    
    trackRecord_here$in_nused <- sapply(X = gfeid,function(x){
      condi_nuseds_gfeid <- all_areas_nuseds$GFE_ID == x
      nuseds_iid_gfeid <- all_areas_nuseds[condi_nuseds_iid & condi_nuseds_gfeid,]
      
      if(nrow(nuseds_iid_gfeid) > 0){
        out <- "yes"
      }else{
        out <- "no"
      }
      return(out)
    })
    
  }else{
    
    # Look if there is a time series in all_areas_nuseds:
    condi_nuseds_gfeid <- all_areas_nuseds$GFE_ID == gfeid
    nuseds_iid_gfeid <- all_areas_nuseds[condi_nuseds_iid & condi_nuseds_gfeid,]
    
    if(nrow(nuseds_iid_gfeid) > 0){  # 1. above --> there is a time series
      
      # trackRecord_here$comment <- "There is a time series"
      trackRecord_here$in_nused <- "yes"
      
      # if(is.null(escapement)){
      #   escapement <- nuseds_iid_gfeid
      # }else{
      #   escapement <- rbind(escapement,nuseds_iid_gfeid)
      # }
      
    }else{ # 2. above --> there is no time series (# e.g. i = 174)
      
      trackRecord_here$in_nused <- "no"
      
      #' Look in nuseds for potential alternative series", with the potential 
      #' scenarios:
      #' - iid is correct and GFE_ID is wrong --> other series for iid but with 
      #'   different GFE_IDs 
      #' - iid is wrong and GFE_ID is correct --> other series for gfeid of the 
      #'   same species.
      #' For each potential altenative series, check if their Index_Id & GFE_ID 
      #' assocation is in CUSS. 
      #' - If YES, we assumes these alternative series have no mistake and do not 
      #'   consider them as alternatives.
      #'- If NO: the series can potenial canditates. 
      #'  - If there is only one series, replace either the IndexId or the GFE_ID
      #'    in nuseds.
      #'  - If there are multiple, look at the series and decide manually.
      
      comment_iid <- NULL
      comment_gfheid <- NULL
      
      #' 1) Scenario where iid is correct and GFE_ID is wrong:
      nuseds_iid <- all_areas_nuseds[condi_nuseds_iid,]
      d <- unique(nuseds_iid[,c("IndexId","GFE_ID")])
      iid_GFE_IDs_here <- data.frame(IndexId = d$IndexId,
                                     GFE_ID = d$GFE_ID,
                                     in_cuss = rep(NA,nrow(d)))
      
      if(nrow(iid_GFE_IDs_here) == 0){ # iid is not present in all_areas_nuseds
        comment_iid <- paste(iid,"is not in all_areas_nuseds") # NOT USED
        
      }else{
        for(r in 1:nrow(iid_GFE_IDs_here)){
          cond <- conservation_unit_system_sites$IndexId == iid_GFE_IDs_here$IndexId[r] &
            conservation_unit_system_sites$GFE_ID == iid_GFE_IDs_here$GFE_ID[r]
          if(nrow(conservation_unit_system_sites[cond,]) == 0){
            iid_GFE_IDs_here$in_cuss[r] <- "no"
          }else{
            iid_GFE_IDs_here$in_cuss[r] <- "yes"
          }
        }
        iid_GFE_IDs_here <- iid_GFE_IDs_here[iid_GFE_IDs_here$in_cuss == "no",]
      }
      
      #' 2) Scenario where iid is wrong and GFE_ID is correct:
      species_acr <- strsplit(iid,split = "_")[[1]][1]
      condi_nuseds_sp <- grepl(species_acr,all_areas_nuseds$IndexId)
      nuseds_sp_gfeid <- all_areas_nuseds[condi_nuseds_sp & condi_nuseds_gfeid,]
      
      IndexIds_gfeid_here <- unique(nuseds_sp_gfeid[,c("IndexId","GFE_ID")])
      
      d <- unique(nuseds_sp_gfeid[,c("IndexId","GFE_ID")])
      IndexIds_gfeid_here <- data.frame(IndexId = d$IndexId,
                                        GFE_ID = d$GFE_ID,
                                        in_cuss = rep(NA,nrow(d)))
      if(nrow(IndexIds_gfeid_here) == 0){ # species + gfeid is not present in all_areas_nuseds
        comment_gfeid <- paste(species_acr,"&",gfeid,"is not in all_areas_nuseds") # NOT USED
        
      }else{
        for(r in 1:nrow(IndexIds_gfeid_here)){
          cond <- conservation_unit_system_sites$IndexId == IndexIds_gfeid_here$IndexId[r] &
            conservation_unit_system_sites$GFE_ID == IndexIds_gfeid_here$GFE_ID[r]
          if(nrow(conservation_unit_system_sites[cond,]) == 0){
            IndexIds_gfeid_here$in_cuss[r] <- "no"
          }else{
            IndexIds_gfeid_here$in_cuss[r] <- "yes"
          }
        }
        IndexIds_gfeid_here <- IndexIds_gfeid_here[IndexIds_gfeid_here$in_cuss == "no",]
      }
      
      # combine the two
      alternatives <- rbind(iid_GFE_IDs_here,IndexIds_gfeid_here)
      
      if(nrow(alternatives) == 0){
        trackRecord_here$comment <- "There is no alternative series in all_areas_nuseds"
        
      }else{
        comment <- apply(alternatives[,c("IndexId","GFE_ID")], 1, paste, collapse = " & " )
        comment <- paste(comment, collapse = ", ")
        comment <- paste("Alternative series:",comment)
        trackRecord_here$comment <- comment
        print(alternatives)
      }
    }
  }
  
  if(is.null(trackRecord)){
    trackRecord <- trackRecord_here
  }else{
    trackRecord <- rbind(trackRecord,trackRecord_here)
  }
  return(trackRecord)
}


#' Function that calls cuss_nuseds_match_single_fun() for faster execution (cf. the
#' latter's description for further details).
# detectCores()
# detectCores(logical = FALSE)
# cores_nb <- 10
cuss_nuseds_match_parallel_fun <- function(conservation_unit_system_sites,
                                           all_areas_nuseds,
                                           cores_nb = 1){
  
  require(parallel)
  
  # IndexIds <- unique(conservation_unit_system_sites$IndexId)
  
  # Documentation
  # https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
  
  if(Sys.info()["sysname"] == "Windows"){
    
    cl <- makeCluster(cores_nb)
    
    # export the different object to the cluster
    clusterExport(cl, "conservation_unit_system_sites")
    clusterExport(cl, "all_areas_nuseds")
    # lusterExport(cl, "prog_steps")
    # clusterExport(cl, "IndexIds")
    clusterExport(cl, "cuss_nuseds_match_single_fun")
    
    trackRecord_l <- parLapply(cl = cl, X = unique(conservation_unit_system_sites$IndexId), 
                               fun = function(iid){
      # iid <- unique(conservation_unit_system_sites$IndexId)[174]
      # i <- which(IndexIds == iid)
      i <- which(unique(conservation_unit_system_sites$IndexId) == iid)
      trackRecord <- cuss_nuseds_match_single_fun(IndexId = iid,
                                                  i = i, 
                                                  #prog_steps = prog_steps,
                                                  conservation_unit_system_sites = conservation_unit_system_sites,
                                                  all_areas_nuseds = all_areas_nuseds)
      return(trackRecord)
    })
    
  }else{
    
    #' NOT TESTED
    trackRecord_l <- mclapply(cl = cl, X = IndexIds, fun = function(iid){
      # iid <- unique(conservation_unit_system_sites$IndexId)[174]
      i <- which(IndexIds == iid)
      trackRecord <- cuss_nuseds_match_single_fun(IndexId = iid,
                                                  i = i,
                                                  #prog_steps = prog_steps,
                                                  conservation_unit_system_sites = conservation_unit_system_sites,
                                                  all_areas_nuseds = all_areas_nuseds)
      return(trackRecord)
    })
  }
  stopCluster(cl)
  trackRecord <- do.call(rbind,trackRecord_l)
  return(trackRecord)
}


#' Function taking the dataframe all_areas_nuseds and removes the 
#' IndexId & GFE_ID time series that only have NAs for MAX_ESTIMATE. 
#' The function uses parallel computing.
remove_series_nodata_nuseds_parallel_fun <- function(all_areas_nuseds,
                                                     zeros_too = T,
                                                     cores_nb = 1){
  require(parallel)
  
  # Documentation
  # https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
  
  indexId_GFE_ID <- unique(all_areas_nuseds[,c("IndexId","GFE_ID")])
  
  if(Sys.info()["sysname"] == "Windows"){
    
    cl <- makeCluster(cores_nb)
    
    clusterExport(cl,"all_areas_nuseds")
    #clusterExport(cl,"zeros_too")
    
    #clusterExport(cl,"indexId_GFE_ID")
    #clusterExport(cl,"toremove")
    
    aan <- parLapply(cl = cl, X = 1:nrow(indexId_GFE_ID), fun = function(r){
      iid <- indexId_GFE_ID$IndexId[r]
      gfeid <- indexId_GFE_ID$GFE_ID[r]
      cond <- all_areas_nuseds$IndexId == iid & 
        all_areas_nuseds$GFE_ID == gfeid
      maxEstim <- all_areas_nuseds$MAX_ESTIMATE[cond]
      
      if(zeros_too){
        toremove <- all(is.na(maxEstim) | maxEstim == 0)
      }else{
        toremove <- all(is.na(maxEstim))
      }

      # return an empty slice of all_areas_nuseds
      if(toremove){
        aan <- all_areas_nuseds[F,]
      }else{
        aan <- all_areas_nuseds[cond,]
      }
      return(aan)
    })
    
  }else{
    
    #' TODO: need to be tested with Mac and Linux OS
    aan <- mclapply(X = 1:nrow(indexId_GFE_ID),FUN = function(r){
      iid <- indexId_GFE_ID$IndexId[r]
      gfeid <- indexId_GFE_ID$GFE_ID[r]
      cond <- all_areas_nuseds$IndexId == iid & 
        all_areas_nuseds$GFE_ID == gfeid
      maxEstim <- all_areas_nuseds$MAX_ESTIMATE[cond]
      
      if(zeros_too){
        toremove <- all(is.na(maxEstim) | maxEstim == 0)
      }else{
        toremove <- all(is.na(maxEstim))
      }
      
      # return an empty slice of all_areas_nuseds
      if(toremove){
        aan <- all_areas_nuseds[F,]
      }else{
        aan <- all_areas_nuseds[cond,]
      }
      return(aan)
    },mc.cores = cores_nb)
  }
  
  stopCluster(cl)
  aan <- do.call(rbind,aan)
  return(aan)
}

#' Function calculating the number of data points OR % of them of a time series
#' that are (1) complementary (i.e. not duplicated, nor conflictual), (2) duplicated
#' (i.e. same value for same year) and (3) conflictual (i.e. different value for 
#' same year). The series supplied are vectors of values (e.g.population size) 
#' with the time unit (e.g. year) are names.
compare_series_fun <- function(series_focal,series_compare,percentage = F){
  
  # make sure years are in the right order
  series_focal <- series_focal[order(as.numeric(names(series_focal)))]
  series_compare <- series_compare[order(as.numeric(names(series_compare)))]
  
  s_focal <- data.frame(year = as.numeric(names(series_focal)),
                        s_focal = series_focal)
  s_comp <- data.frame(year = as.numeric(names(series_compare)),
                       s_comp = series_compare)
  
  s <- merge(x = s_focal,y = s_comp, by = "year", all = T)
  
  dataPoint_nb <- sum(!is.na(series_focal))
  complementary <- dataPoint_nb - nrow(s[!is.na(s$s_focal) & !is.na(s$s_comp),])
  duplicate <- nrow(s[!is.na(s$s_focal) & !is.na(s$s_comp) & s$s_focal == s$s_comp,])
  conflict <- nrow(s[!is.na(s$s_focal) & !is.na(s$s_comp) & s$s_focal != s$s_comp,])
  
  if(percentage){
    complementary <- round(complementary/dataPoint_nb,1) * 100
    duplicate <- round(duplicate/dataPoint_nb,1) * 100
    conflict <- round(conflict/dataPoint_nb,1) * 100
  }
  
  out <- data.frame(nb_dataPt = dataPoint_nb,
                    complementary = complementary,
                    duplicate = duplicate,
                    conflict = conflict)
  return(out)
}


#' Function to return a list for the fields in all_areas_nuseds and 
#' conservation_unit_system_sites that are associated to unique IndexId and GFE_ID,
#' to both and to none. The assumption is based on if single or multiple values
#' of a given field is returned for each IndexId or GDE_ID.
fields_IndexId_GFE_ID_fun <- function(all_areas_nuseds,
                                      conservation_unit_system_sites,
                                      runProcess = F,                # takes a while
                                      newFieldsIncluded = T){
  
  #' the commented out code was used to obtained the vectors of fields. The procedure
  #' take a bit of time so instead those vectors were copy pasted.
  
  #' *** conservation_unit_system_sites ***
  CUSS_l <- list()
  
  # IndexId
  if(runProcess){
    fields_CUSS_asso <- association_twoFields_fun(fields_1 = c("IndexId","GFE_ID"),
                                                  fields_2 = colnames(conservation_unit_system_sites),
                                                  dataset = conservation_unit_system_sites,
                                                  silence = T)
    
    cond_iid <- fields_CUSS_asso$fields_1 == "IndexId" &
      fields_CUSS_asso$association == "single"
    out <- as.character(fields_CUSS_asso$fields_2[cond_iid])
    
  }else{
    out <- c('SPECIES_QUALIFIED','FAZ_ACRO','MAZ_ACRO','JAZ_ACRO',
             'CU_NAME','CU_ACRO','CU_LAT','CU_LONGT','CU_TYPE','CU_INDEX','FULL_CU_IN',
             'SBJ_ID','POP_ID','SPECIES',"IS_INDICATOR","CMNTS","EFFECTIVE_DT")
    
    if(newFieldsIncluded){
      out <- c(out,'species_acronym_ncc','IndexId')
      out <- unique(out)
    }
  }
  CUSS_l[[1]] <- out


  # GFE_ID
  if(runProcess){
    cond_gfeid <- fields_CUSS_asso$fields_1 == "GFE_ID" &
      fields_CUSS_asso$association == "single"
    fields_CUSS_asso[cond_gfeid,]
    out <- as.character(fields_CUSS_asso$fields_2[cond_gfeid])
    
    # remove fields that are known to be CU related only
    out <- out[!out %in% c('FAZ_ACRO','MAZ_ACRO','JAZ_ACRO')]
    
  }else{
    out <- c('GFE_ID','SYSTEM_SITE','GFE_TYPE','Y_LAT','X_LONGT','WATERSHED_CDE',
             'FWA_WATERSHED_CDE')
    
  }
  CUSS_l[[2]] <- out
  
  # both
  if(runProcess){
    out <- c()
    for(f in  unique(fields_CUSS_asso$fields_2)){
      cond1 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "IndexId" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "GFE_ID" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      if(all(c(cond1,cond2))){
        out <- c(out,f)
      }
    }
    out <- out
    
    # remove fields that are known to be CU related only
    out <- out[!out %in% c('FAZ_ACRO','MAZ_ACRO','JAZ_ACRO')]
    
  }else{
    out <- c()
  }
  CUSS_l[[3]] <- out
  
  # none
  if(runProcess){
    out <- c()
    for(f in  unique(fields_CUSS_asso$fields_2)){
      cond1 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "IndexId" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "GFE_ID" &
                                  fields_CUSS_asso$fields_2 == f,]$association == "single"
      if(all(c(!cond1,!cond2))){
        out <- c(out,f)
      }
    }

  }else{
    out <- c('MAP_LABEL')
  }
  CUSS_l[[4]] <- out
  
  # CU_NAME & species_acronym_ncc
  # if(runProcess & newFieldsIncluded){ # species_acronym_ncc is a new field
  #   
  #   newVar <- apply(conservation_unit_system_sites[,c("species_acronym_ncc","CU_NAME")],
  #                   1,paste, collapse = " ")
  #   
  #   CUSS_new <- conservation_unit_system_sites
  #   CUSS_new$species_CU_NAME <- newVar
  #   
  #   fields_CUSS_asso <- association_twoFields_fun(fields_1 = c("species_CU_NAME"),
  #                                                 fields_2 = colnames(CUSS_new),
  #                                                 dataset = CUSS_new,
  #                                                 silence = T)
  #   out <- c()
  #   for(f in  unique(fields_CUSS_asso$fields_2)){
  #     cond1 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "species_CU_NAME" &
  #                                 fields_CUSS_asso$fields_2 == f,]$association == "single"
  #     cond2 <- fields_CUSS_asso[fields_CUSS_asso$fields_1 == "species_CU_NAME" &
  #                                 fields_CUSS_asso$fields_2 == f,]$association == "single"
  #     if(all(c(cond1,cond2))){
  #       out <- c(out,f)
  #     }
  #   }
  #   
  # }else{
  #   out <- c('SPECIES_QUALIFIED','CU_NAME','CU_ACRO','CU_LAT','CU_LONGT','CU_TYPE',
  #            'CU_INDEX','FULL_CU_IN','SBJ_ID','SPECIES')
  #   
  #   if(newFieldsIncluded){
  #     out <- c(out,"species_acronym_ncc")
  #   }
  # }
  # 
  # CUSS_l[[5]] <- out
  
  #
  names(CUSS_l) <- c('IndexId','GFE_ID','both','none')
  
  
  #' *** all_areas_nuseds ***
  NUSEDS_l <- list()
  if(runProcess){
    fields_NUSEDS_asso <- association_twoFields_fun(fields_1 = c("IndexId","GFE_ID"),
                                                    fields_2 = colnames(all_areas_nuseds),
                                                    dataset = all_areas_nuseds,
                                                    silence = T)
    
    # IndexId
    cond_iid <- fields_NUSEDS_asso$fields_1 == "IndexId" &
      fields_NUSEDS_asso$association == "single"
    out <- as.character(fields_NUSEDS_asso$fields_2[cond_iid])
    
  }else{
    out <- c("SPECIES","POPULATION","RUN_TYPE","POP_ID")
    
    if(newFieldsIncluded){
      out <- c(out,'species_acronym_ncc','IndexId')
      out <- unique(out)
    }
  }
  NUSEDS_l[[1]] <- out
  
  # GFE_ID
  if(runProcess){
    cond_gfeid <- fields_NUSEDS_asso$fields_1 == "GFE_ID" &
      fields_NUSEDS_asso$association == "single"
    fields_NUSEDS_asso[cond_gfeid,]
    out <- as.character(fields_NUSEDS_asso$fields_2[cond_gfeid])
    
  }else{
    out <- c('AREA','WATERBODY','GAZETTED_NAME','LOCAL_NAME_1','LOCAL_NAME_2',
             'WATERSHED_CDE','WATERBODY_ID','GFE_ID')
    
    if(newFieldsIncluded){
      out <- c(out,'StatArea')
      out <- unique(out)
    }
  }
  NUSEDS_l[[2]] <- out
  
  # both
  if(runProcess){
    out <- c()
    for(f in  unique(fields_NUSEDS_asso$fields_2)){
      cond1 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "IndexId" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "GFE_ID" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      if(all(c(cond1,cond2))){
        out <- c(out,f)
      }
    }
    
  }else{
    out <- c()
    
  }
  NUSEDS_l[[3]] <- out
  
  # none
  if(runProcess){
    out <- c()
    for(f in  unique(fields_NUSEDS_asso$fields_2)){
      cond1 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "IndexId" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      cond2 <- fields_NUSEDS_asso[fields_NUSEDS_asso$fields_1 == "GFE_ID" &
                                    fields_NUSEDS_asso$fields_2 == f,]$association == "single"
      if(all(c(!cond1,!cond2))){
        out <- c(out,f)
      }
    }
    
  }else{
    out <- c('ANALYSIS_YR','NATURAL_ADULT_SPAWNERS',
              'NATURAL_JACK_SPAWNERS','NATURAL_SPAWNERS_TOTAL',
              'ADULT_BROODSTOCK_REMOVALS','JACK_BROODSTOCK_REMOVALS',
              'TOTAL_BROODSTOCK_REMOVALS','OTHER_REMOVALS','TOTAL_RETURN_TO_RIVER',
              'ENUMERATION_METHODS','ADULT_PRESENCE','JACK_PRESENCE','START_DTT',
              'END_DTT','NATURAL_ADULT_FEMALES','NATURAL_ADULT_MALES',
              'EFFECTIVE_FEMALES','WEIGHTED_PCT_SPAWN',
              'STREAM_ARRIVAL_DT_FROM','STREAM_ARRIVAL_DT_TO','START_SPAWN_DT_FROM',
              'START_SPAWN_DT_TO','PEAK_SPAWN_DT_FROM','PEAK_SPAWN_DT_TO',
              'END_SPAWN_DT_FROM','END_SPAWN_DT_TO','ACCURACY','PRECISION',
              'INDEX_YN','RELIABILITY','ESTIMATE_STAGE','ESTIMATE_CLASSIFICATION',
              'NO_INSPECTIONS_USED','ESTIMATE_METHOD','CREATED_DTT','UPDATED_DTT',
              'ACT_ID','Source','Spawners','SpawnersSource',
              'Broodstock','BroodstockSource','Removals','RemovalsSource')
    
    if(newFieldsIncluded){
      out <- c(out,'Returns','MAX_ESTIMATE')
      out[out == "ANALYSIS_YR"] <- 'Year'
      out <- unique(out)
    }
  }
  NUSEDS_l[[4]] <- out
  
  #
  names(NUSEDS_l) <- c('IndexId','GFE_ID','both','none')
  
  out <- list(NUSEDS_l,CUSS_l)
  names(out) <- c("NUSEDS","CUSS")
  
  return(out)
}

#' Function to return a new row for conservation_unit_system_sites (CUSS) with a
#' series not present (but present in all_areas_nuseds (NUSEDS)), i.e. with a 
#' IndexId/POP_ID and GFE_ID association not present in CUSS.
#' The function fills the different fields using the information associated to 
#' IndexId and GFE_ID in conservation_unit_system_sites and all_areas_nuseds.
#' - In case the GFE_ID of the new series is not present in CUSS, the fields are 
#' filled using the GFE_ID - related information that is present in NUSEDS. 
#' - In case the IndexId is not present in CUSS: not implemented yet (not sure)
#' if that's possible).
# IndexId <- "CN_39983"
# GFE_ID <- 1204
CUSS_newRow_fun <- function(IndexId,GFE_ID,
                            conservation_unit_system_sites,
                            all_areas_nuseds){
  
  # make sure the series does not already exist in CUSS
  cond <- conservation_unit_system_sites$IndexId == IndexId & 
    conservation_unit_system_sites$GFE_ID == GFE_ID
  
  if(sum(cond) != 0){
    print("The series is already in CUSS:")
    print(conservation_unit_system_sites[cond,])
    
  }else{
    
    # return the fields in NUSEDS and CUSS associated to indexId and GFE_ID:
    fields_l <- fields_IndexId_GFE_ID_fun(all_areas_nuseds = all_areas_nuseds,
                                          conservation_unit_system_sites = conservation_unit_system_sites)
    
    # create an empty row and start filling it
    cuss_new <- conservation_unit_system_sites[NA,][1,]
    cuss_new$IndexId <- IndexId
    cuss_new$GFE_ID <- GFE_ID
    
    # Fill with IndexId related fields
    cond_cuss_iid <- conservation_unit_system_sites$IndexId == IndexId
    if(sum(cond_cuss_iid) > 0){ # if IndexId is already present in CUSS:
      
      for(f in fields_l$CUSS$IndexId){
        cuss_new[,f] <- unique(conservation_unit_system_sites[,f][cond_cuss_iid])
      }
      
    }else{  # if IndexId is not already present in CUSS:
      # use fields_l$NUSEDS$IndexId
      
      cond_nuseds_iid <- all_areas_nuseds$IndexId == IndexId
      
      field_comm <- fields_l$CUSS$IndexId[fields_l$CUSS$IndexId %in% fields_l$NUSEDS$IndexId]
      field_comm <- field_comm[field_comm != "IndexId"]
      
      
      for(f in field_comm){
        cuss_new[,f] <- unique(all_areas_nuseds[,f][cond_nuseds_iid])
      }
      
      # add SPECIES_QUALIFIED
      cuss_new$SPECIES_QUALIFIED <- cuss_new$species_acronym_ncc
      if(cuss_new$species_acronym_ncc == "CN"){
        cuss_new$SPECIES_QUALIFIED <- "CK"
        
      }else if(cuss_new$species_acronym_ncc %in% c("SEL","SER")){
        cuss_new$SPECIES_QUALIFIED <- "SX"
        
      }
    }
    
    # Fill with GFE_ID related fields
    cond_cuss_gfeid <- conservation_unit_system_sites$GFE_ID == GFE_ID
    if(sum(cond_cuss_gfeid) > 0){
      
      for(f in fields_l$CUSS$GFE_ID){
        cuss_new[,f] <- unique(conservation_unit_system_sites[,f][cond_cuss_gfeid])
      }
      
    }else{
      # use fields_l$NUSEDS$GFE_ID
      field_comm <- fields_l$CUSS$GFE_ID[fields_l$CUSS$GFE_ID %in% fields_l$NUSEDS$GFE_ID]
      field_comm <- field_comm[field_comm != "GFE_ID"]
      cond_nuseds_gfeid <- all_areas_nuseds$GFE_ID == GFE_ID
      
      for(f in field_comm){
        cuss_new[,f] <- unique(all_areas_nuseds[,f][cond_nuseds_gfeid])
      }
      
      cuss_new$SYSTEM_SITE <- unique(all_areas_nuseds$WATERBODY[cond_nuseds_gfeid])
    }
  }
  return(cuss_new)
}

#' Function to update the fields associated to IndexId or GFE_ID in CUSS or NUSEDS.
# edit_CUSS = F
# edit_NUSEDS = T
# IndexId_focal = "CO_46835"
# IndexId_alter = "CO_46835"
# GFE_ID_focal = 2463
# GFE_ID_alter = 285
fields_edit_NUSEDS_CUSS_fun <- function(IndexId_focal = NA, IndexId_alter = NA,
                                        GFE_ID_focal = NA, GFE_ID_alter = NA,
                                        edit_NUSEDS = F,  edit_CUSS = F, 
                                        all_areas_nuseds = all_areas_nuseds,
                                        conservation_unit_system_sites = conservation_unit_system_sites){
  
  require(dplyr)
  
  #' Import list for the fields in NUSEDS and CUSS that are associated to unique
  #' IndexId and GFE_ID
  fields_l <- fields_IndexId_GFE_ID_fun(all_areas_nuseds = all_areas_nuseds,
                                        conservation_unit_system_sites = conservation_unit_system_sites)
  
  if(!edit_CUSS & !edit_NUSEDS){
    print("Please choose a dataset to edit.")
    
  }else{
    
    varAlter_presentInSameDataset <- T # for the end in case WATERBODY is used for SYSTEM_SITE and vice versa
    
    # check what has to be updated: Index_ID, GFE_ID or both
    iid_diff <- IndexId_focal != IndexId_alter
    gfeid_diff <- GFE_ID_focal != GFE_ID_alter
    
    if(iid_diff & gfeid_diff){
      print("IndexIds and GFE_IDs are the same.")
      
    }else{
      
      if(iid_diff & !gfeid_diff){       # if IndexId is to change
        var <- "IndexId"
        
      }else if(!iid_diff & gfeid_diff){ # if GFE_ID is to change
        var <- "GFE_ID"
        
      }else if(!iid_diff & !gfeid_diff){
        var <- c("IndexId","GFE_ID")
        
      }
      
      # 
      if(!edit_CUSS & edit_NUSEDS){ # edit NUSEDS
        
        datset_name <- "all_areas_nuseds"
        
        dataset_focal <- all_areas_nuseds
        dataset_alter <- all_areas_nuseds
        
        # check if the alternative series is in NUSEDS
        if(length(var) == 2){
          cond_alter <- dataset_alter$IndexId == IndexId_alter & 
            dataset_alter&GFE_ID == GFE_ID_alter
          # to finish eventually but might not be needed
          print("Write code for when the fields for both IndexId and GFE_ID have to be edited.")
          
        }else if(var == "IndexId"){
          fields <- fields_l$NUSEDS$IndexId
          cond_alter <- dataset_alter$IndexId == IndexId_alter
          # if IndexId_alter is not in NUSEDS --> look for Index_Id - related fields in CUSS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- conservation_unit_system_sites
            cond_alter <- dataset_alter$IndexId == IndexId_alter
            # find the fields in common for IndexId in NUSEDS and CUSS
            fields <- fields_l$NUSEDS$IndexId[fields_l$NUSEDS$IndexId %in% fields_l$CUSS$IndexId]
          }
          
        }else if(var == "GFE_ID"){
          fields <- fields_l$NUSEDS$GFE_ID
          cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
          # if GFE_ID_alter is not in NUSEDS --> look for GFE_ID - related fields in CUSS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- conservation_unit_system_sites
            cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
            # find the fields in common for GFE_ID in NUSEDS and CUSS
            fields <- fields_l$NUSEDS$GFE_ID[fields_l$NUSEDS$GFE_ID %in% fields_l$CUSS$GFE_ID]
            # add SYSTEM_SITE, which is WATERSHED in NUSEDS
            fields <- c(fields,"SYSTEM_SITE")
          }
        }
        
      }else if(edit_CUSS & !edit_NUSEDS){ # edit CUSS
        # print("Write code for when The fields in CUSS have to be edited.")
        
        datset_name <- "conservation_unit_system_sites"
        
        dataset_focal <- conservation_unit_system_sites
        dataset_alter <- conservation_unit_system_sites
        
        # check if the alternative series is in CUSS
        if(length(var) == 2){
          cond_alter <- conservation_unit_system_sites$IndexId == IndexId_alter & 
            conservation_unit_system_sites&GFE_ID == GFE_ID_alter
          # to finish eventually but might not be needed
          print("Write code for when the fields for both IndexId and GFE_ID have to be edited.")
          
        }else if(var == "IndexId"){
          fields <- fields_l$CUSS$IndexId
          cond_alter <- conservation_unit_system_sites$IndexId == IndexId_alter
          
          # if IndexId_alter is not in CUSS --> look for Index_Id - related fields in NUSEDS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- all_areas_nuseds
            cond_alter <- dataset_alter$IndexId == IndexId_alter
            # find the fields in common for IndexId in NUSEDS and CUSS
            fields <- fields_l$CUSS$IndexId[fields_l$CUSS$IndexId %in% fields_l$NUSEDS$IndexId]
          }
          
        }else if(var == "GFE_ID"){
          fields <- fields_l$CUSS$GFE_ID
          cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
          # if GFE_ID_alter is not in CUSS --> look for GFE_ID - related fields in NUSEDS
          if(sum(cond_alter) == 0){
            varAlter_presentInSameDataset <- F
            dataset_alter <- all_areas_nuseds
            cond_alter <- dataset_alter$GFE_ID == GFE_ID_alter
            # find the fields in common for GFE_ID in NUSEDS and CUSS
            fields <- fields_l$CUSS$GFE_ID[fields_l$CUSS$GFE_ID %in% fields_l$NUSEDS$GFE_ID]
            # add WATERSHED , which is SYSTEM_SITE in CUSS
            fields <- c(fields,"WATERSHED")
          }
        }
      }
      
      # update dataset_focal
      cond_focal <- dataset_focal$IndexId == IndexId_focal & 
        dataset_focal$GFE_ID == GFE_ID_focal
      
      print(paste(sum(cond_focal),"rows were edited in",datset_name,"at the following fields:",
                  paste(fields, collapse = ", ")))
      
      for(f in fields){
        f_focal <- f
        if(f == "SYSTEM_SITE" & !varAlter_presentInSameDataset){  # add SYSTEM_SITE WATERSHED in NUSEDS
          f_focal <- "WATERBODY"
        }else if(f == "WATERBODY" & !varAlter_presentInSameDataset){
          f_focal <- "SYSTEM_SITE"
        }
        dataset_focal[cond_focal,f_focal] <- unique(dataset_alter[cond_alter,f])
      }
      
      # check there are duplicated year for a same indexId & GFE_ID series:
      if(!edit_CUSS & edit_NUSEDS){ # edit NUSEDS
        
        dupli <- dataset_focal %>%
          dplyr::group_by(IndexId, GFE_ID, Year) %>%
          dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(n > 1L)
        
        if(nrow(dupli) > 0){
          print("There are duplicated rows in NUSEDS:")
          print(dupli)
        }
      }

      return(dataset_focal)
    } 
  }
}


#' Function to returns a list of FULL_CU_IN value to update certain POP_IDs
update_for_FULL_CU_IN_l <- function(){
  
  out <- list()
  i <- 1
  # babine/onerka
  out[[i]] <- c(45452,45462,48064,48069,48074,48094,48099,48599,48674,48684,49354,
                49379,49384,49389,49394,49399,49404,49419,49424,49434,49439)
  names(out)[i] <- "SEL-21-02-EW"
  
  # nilkitkwa
  i <- i + 1
  out[[i]] <- c(49359,49364,49369,49374,49457)
  names(out)[i] <-  "SEL-21-02-LW"
  
  # tahlo/morrison
  i <- i + 1
  out[[i]] <- c(49409,49414)
  names(out)[i] <- "SEL-21-02-MW"
  
  #babine enhanced
  i <- i + 1
  out[[i]] <-  c(3237,45467,45472,3238,45482)
  names(out)[i] <- "SEL-21-02-F"
  
  ### bella coola chum
  i <- i + 1
  out[[i]] <- c(3119,51771,51772,3143,3122,3125,3138,3128,51778)
  names(out)[i] <- "CM-16"
  
  return(out)
}



