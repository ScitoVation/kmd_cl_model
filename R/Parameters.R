#Chemical Database for the KMD Model 
# chemical data obtained from the IndusChemfate Chemical Database

getChemicalParams <- function(chem_name,conn){
  conn <- dbConnect(SQLite(),"Database/ModelDb.sqlite")
  query <- sprintf("Select mw,lkow,vmaxc,km from ChemParams where Chemid = '%s'",chem_name)
  res <- dbSendQuery(conn,query)
  resDF <- dbFetch(res)
  
  if(chem_name == "ChemA"){ #1,2 Dichloroethane
    mw <- 98.96
    lkow <-1.48
    vmaxc <- 450
    km <- 2.5
  }else if(chem_name == "ChemB"){ #Heptane
    mw <- 100.21
    lkow <-4.66
    vmaxc <- 14000
    km <- 1100
  }else if(chem_name == "ChemC"){ #Benzene(2)
    mw <- 78.11
    lkow <-2.13
    vmaxc <- 122
    km <- 40
  }else if(chem_name == "ChemD"){ #BPA
    mw <- 228.29
    lkow <-3.32
    vmaxc <- 6900
    km <- 7.7
  }else if(chem_name == "ChemE"){ #aniline
    mw <- 93.13
    lkow <- 0.94
    vmaxc <- 39000
    km <- 180
  }
  return(list("MW"=resDF$mw,"lkow"=resDF$lkow,"vmaxc"=resDF$vmaxc,"km"=resDF$km))
}

getModelParams <- function(all_input_vals,conn){
  scaled_initial_values <- within(all_input_vals,{
    qc = (qcc *bw ** 0.75) *(1-hct)
    vbld = vbldc * bw * (0.85/(vbldc+vlivc+vrpfc+vspfc))
    vpls = vbld * (1-hct)
    vrbc = vbld * hct
    vliv = vlivc * bw * (0.85/(vbldc+vlivc+vrpfc+vspfc))
    vspf = vspfc * bw * (0.85/(vbldc+vlivc+vrpfc+vspfc))
    vrpf = vrpfc * bw * (0.85/(vbldc+vlivc+vrpfc+vspfc))
    qliv = qlivc * qc * (1/(qlivc+qspfc+qrpfc))
    qspf = qspfc * qc * (1/(qlivc+qspfc+qrpfc))
    qrpf = qrpfc * qc * (1/(qlivc+qspfc+qrpfc))
  }) 
  query <- "Select Variable from Params where InModel = 1;"
  res <- dbSendQuery(conn,query)
  resDF <- dbFetch(res,n=-1)$Variable
  dbClearResult(res)
  valid_indices <- which(names(scaled_initial_values) %in% resDF)
  scaled_initial_values <- scaled_initial_values[valid_indices]
  
  return(scaled_initial_values)
}

getChemicalFa <- function(chem,dose){
  conn <- dbConnect(SQLite(),"Database/ModelDb.sqlite")
  query <- "Select * from ChemFa"
  res <- dbSendQuery(conn,query)
  fa_tble <- dbFetch(res)
  if(chem %in% colnames(fa_tble)){
    fa <- approx(fa_tble$Exposures,fa_tble[[chem]],dose)$y
  }else{
    fa <- 1
  }
  return(fa)
}