calculatePartitions <- function(chem_params,organism){
  organism <- ifelse(is.null(organism),"ra",organism)
  lkow <- chem_params[["lkow"]]
  mw <- chem_params[["MW"]]
  temp <- 25
  VPa <- 10
  wsol <- 1
  # log of henry's coeff
  lhen <- log(VPa*mw/wsol/8.314/(temp+273.15))/log(10)
  lkoair <- lkow-lhen
  
  if(organism =="ha"){
    kow = 10^lkow
    if (kow< 0.1){
      kow <- 0.1
    }
    # Using the partition coefficient equation for fat for lumped slowly perfused tissue comparment
    pspf <- (0.8 * kow^1.03 + 0.2) / (0.0056 * kow^1.03 + 0.83) - 0.38
    if (pspf < 0.1){
      pfat <- 0.1
    }
    pliv  <- (0.049 * kow^0.81 + 0.711) / (0.0056 * kow^0.81 + 0.83) - 0.35
    prpf <- pliv #using liver partition coefficient for lumped rapidly perfused tissue
  }else if(organism == "ra"){
    # pbair calculation
    if (lhen< -0.7){
      pbair <- 0.19338*(1/10^lhen)+(0.002592*10^lkoair)
    }else{
      pbair <- 2.32245*(1/10^lhen)+(0.0067389*10^lkoair)
    }
    #Limit lkow to range where  qsar model works
    if (lkow >5){
      lkow <- 5
    }
    if (lkow < -1){
      lkow <- -1
    }
    pspf <- max(10^(1.106*lkow - 0.1253*lkow^2 - 0.451),0.05)
    pliv  <- 10^(0.15094 * lkow - 0.1111)
    prpf <- pliv
  }
    partCoeff <- lapply(list("pspf"=pspf,"pliv"=pliv,"prpf"=prpf),signif,4)
    return(partCoeff)
  
  
}