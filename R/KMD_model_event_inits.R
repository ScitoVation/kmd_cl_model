initParms <- function(newParms = NULL) {
  parms <- c(
    bw = 0.0,
    qc = 0.0,
    vliv = 0.0,
    vpls = 0.0,
    vrbc = 0.0,
    hct = 0.42,
    vspf = 0.0,
    vrpf = 0.0,
    rvurine = 0,
    vurinec = 0.0,
    vkep1 = 0.0,
    vmaxpu = 0.0,
    kmpu = 0.0,
    vkem1 = 0.0,
    vmaxmu = 0.0,
    kmmu = 0.0,
    qliv = 0.0,
    qspf = 0.0,
    qrpf = 0.0,
    pliv = 0.0,
    prpf = 0.0,
    pspf = 0.0,
    vmax = 0.0,
    km = 0.0,
    vkm1 = 0.0,
    MW = 0.0,
    ivdose = 0.0,
    boral = 0.0,
    ka = 0.0,
    fa = 0.0,
    vmax0 = 0.0,
    km0 = 0.0
  )

  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }

  parms <- within(as.list(parms), {
  })
  out <- .C("getParms",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

Outputs <- c(
    "cpls",
    "cliv",
    "cspf",
    "crpf",
    "mbal",
    "ramet",
    "raumet",
    "raoral",
    "rauprnt",
    "cumet",
    "cuprnt",
    "cmet",
    "vbal",
    "qbal",
    "riv"
)

initStates <- function(parms, newStates = NULL) {
  Y <- c(
    apls = 0.0,
    aliv = 0.0,
    aspf = 0.0,
    arpf = 0.0,
    amet = 0.0,
    aumet = 0.0,
    auprnt = 0.0,
    totiv = 0.0,
    ivswtch = 0.0,
    totodose = 0.0,
    aoral = 0.0,
    auc_cprnt = 0.0,
    auc_cmet = 0.0,
    auc_ctot = 0.0
  )

  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }

.C("initState", as.double(Y));
Y
}
