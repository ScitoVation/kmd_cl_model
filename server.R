#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinybusy)
library(NonCompart)
library(RSQLite)
library(deSolve)
library(pracma)
library(magrittr)
library(plotly)
source("R/KMD_model_event_inits.R")
source("R/QSAR Models.R")
source("R/Parameters.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    #create database connection
    conn <- dbConnect(SQLite(),"Database/ModelDb.sqlite")
    #Identify the model to run
    C_mName <- file.path("Model","KMD_model_event.c")
    system(paste("R CMD SHLIB ", C_mName, sep = ""))
    dll_mName <- file.path("Model",
                           paste("KMD_model_event",.Platform$dynlib.ext,sep="")
    )
    
    results <- reactiveValues(pbpk=NULL,dr=NULL)
    observeEvent(input$sel_chem,{
        chem <- input$sel_chem
        vliv <- input$num_vlivc*input$num_bw
        
        chem_params <- getChemicalParams(chem,conn)
        updateNumericInputIcon(session,"num_MW",value= chem_params[["MW"]])
        updateNumericInput(session,"num_lkow",value = chem_params[["lkow"]])
        updateNumericInputIcon(session,"num_vmax",value = chem_params[["vmaxc"]]*vliv)
        updateNumericInputIcon(session,"num_km",value = chem_params[["km"]])
        updateNumericInputIcon(session,"num_vkm1",value = chem_params[["vmaxc"]]*vliv/chem_params[["km"]])
        
        updateNumericInputIcon(session,"num_vmaxpu",value = chem_params[["vmaxc"]]*vliv)
        updateNumericInputIcon(session,"num_kmpu",value = chem_params[["km"]])
        updateNumericInputIcon(session,"num_vkep1",value = chem_params[["vmaxc"]]*vliv/chem_params[["km"]])
        
        updateNumericInputIcon(session,"num_vmaxmu",value = chem_params[["vmaxc"]]*vliv)
        updateNumericInputIcon(session,"num_kmmu",value = chem_params[["km"]])
        updateNumericInputIcon(session,"num_vkem1",value = chem_params[["vmaxc"]]*vliv/chem_params[["km"]])
        #update Partitions
        org <- input$sel_org
        part_coeffs <- calculatePartitions(chem_params,org)
        lapply(names(part_coeffs),function(x,coeffs){
            updateNumericInput(session,paste0("num_",x),value = coeffs[[x]])
        },part_coeffs)
    },ignoreNULL = T)
    observeEvent(input$sel_org,{
        org <- input$sel_org
        query <- sprintf("Select var,val from Physiology where org = '%s'",org)
        res <- dbSendQuery(conn,query)
        resDF <- dbFetch(res,n=-1)
        dbClearResult(res)
        resDF <- setNames(resDF$val,resDF$var)
        lapply(names(resDF),function(x,vals){
            updateNumericInputIcon(session,paste0("num_",x),value = vals[[x]])
        },resDF)
        #Update Paritions
        chem <- input$sel_chem
        chem_params <- getChemicalParams(chem)
        part_coeffs <- calculatePartitions(chem_params,org)
        lapply(names(part_coeffs),function(x,coeffs){
            updateNumericInput(session,paste0("num_",x),value = coeffs[[x]])
        },part_coeffs)
    })
    observeEvent({input$num_bw
        input$num_vlivc},{
            chem <- input$sel_chem
            vliv <- input$num_vlivc*input$num_bw
            chem_params <- getChemicalParams(chem)
            updateNumericInputIcon(session,"num_vmax",value = chem_params[["vmaxc"]]*vliv)
            updateNumericInputIcon(session,"num_km",value = chem_params[["km"]])
            updateNumericInputIcon(session,"num_vkm1",value = chem_params[["vmaxc"]]*vliv/chem_params[["km"]])
            
            updateNumericInputIcon(session,"num_vmaxu",value = chem_params[["vmaxc"]]*vliv)
            updateNumericInputIcon(session,"num_kmu",value = chem_params[["km"]])
            updateNumericInputIcon(session,"num_vke1",value = chem_params[["vmaxc"]]*vliv/chem_params[["km"]])
        })
    
    
    observeEvent(input$btn_runSim,{
      tstart <- 0
      tstop <- input$num_testSimDuration
      times <- seq(0,tstop,0.1)
      hep_metab_type <- input$sel_metabType
      metab_utype <- input$sel_metab_utype
      prnt_utype <- input$sel_prnt_utype
      expo_route <- input$sel_expo_type
      oral_expo_type <- input$sel_oral_expo_type
      chem <- input$sel_chem
      inputs <- reactiveValuesToList(input)
      names(inputs)<- sub("num_","",names(inputs))
      scaled_initial_values <- getModelParams(inputs,conn)
      
      #Hepatic Metabolism
      if(hep_metab_type == "sat"){
        scaled_initial_values[["vkm1"]]<-0
      }else if(hep_metab_type=="lin"){
        scaled_initial_values[["vmax"]]<-0
        scaled_initial_values[["km"]]<- 1
      }else{
        scaled_initial_values[["vmax"]]<-0
        scaled_initial_values[["km"]]<- 1
        scaled_initial_values[["vkm1"]]<-0
      }
      #Urinary Clearance
      #Parent
      if(prnt_utype == "sat"){
        scaled_initial_values[["vkep1"]]<- 0
      }else if(prnt_utype=="lin"){
        scaled_initial_values[["vmaxpu"]]<- 0
        scaled_initial_values[["kmpu"]]<- 1
      }else{
        scaled_initial_values[["vkep1"]]<- 0
        scaled_initial_values[["vmaxpu"]]<- 0
        scaled_initial_values[["kmpu"]]<- 1
      }
      
      #Metaoblite
      if(metab_utype == "sat"){
        scaled_initial_values[["vkem1"]]<- 0
      }else if(metab_utype=="lin"){
        scaled_initial_values[["vmaxmu"]]<- 0
        scaled_initial_values[["kmmu"]]<- 1
      }else{
        scaled_initial_values[["vkem1"]]<- 0
        scaled_initial_values[["vmaxmu"]]<- 0
        scaled_initial_values[["kmmu"]]<- 1
      }
      
      
      if(input$sel_sim_type=="time"){
        # Run Time course simulation
        shinybusy::show_modal_spinner()
        if(expo_route=="iv"){
          scaled_initial_values[["boral"]]<-0
          scaled_initial_values[["ivdose"]] <- input$num_testIVDose
          ivlen <- input$num_testIVLen
          if(ivlen == 24){
            event_times <- c(0)
          }else{
            # event_days <- 1:(tstop/24)
            # print(event_days)
            # event_times1 <- unlist(lapply(event_days,function(x){0+24.0*(x-1)}))
            # event_times2 <- unlist(lapply(event_days,function(x){ivlen+24.0*(x-1)}))
            # event_times <-sort(c(event_times1,event_times2))
            # print(event_times)
            #Number of replications of the event
            Nrep <- ceiling(max(times) / 24)
            #Find start and end times
            event_times <- rep(c(0, ivlen), Nrep) + rep(24 * (0:(Nrep - 1)), rep(2, Nrep))
            
            
          }
          
          
        }else{
          boral <- input$num_testOralDose
          
          scaled_initial_values[["boral"]] <- boral
          scaled_initial_values[["ivdose"]]<-0
          event_times <- head(seq(0,tstop,24),-1)
          if(oral_expo_type=="fa"){ # dose dependent fa
            scaled_initial_values[["fa"]]<- getChemicalFa(chem,boral)
            scaled_initial_values[["vmax0"]] <- 0
            scaled_initial_values[["km0"]]<- 1
            
          }else if(oral_expo_type=="sat"){ # Saturable Absorption
            scaled_initial_values[["ka"]]<-0
          }else{    #Dose independent Fa
            scaled_initial_values[["vmax0"]] <- 0
            scaled_initial_values[["km0"]]<- 1
          }
        }
        dyn.load(dll_mName)
        parms <-initParms(scaled_initial_values)
        y <- initStates(params)
        times <- sort(c(times,cleanEventTimes(times,event_times)))
        
        out <- ode(y,times,func= 'derivs',parms = parms,
                   dllname = "KMD_model_event",method = "lsodes",
                   initfunc = "initmod",
                   events = list(func = "event",time = event_times),
                   nout = length(Outputs),
                   outnames = Outputs)
        results$pbpk <- as.data.frame(out)
        dyn.unload(dll_mName)
        shinybusy::remove_modal_spinner()
        
      }else{
        shinybusy::show_modal_progress_line(value = 0,text = "Starting Simulation")
        #Get a vector of exposures to run dose response sim
        expo_range <- input$numrange_expo
        num_expos <- input$num_numexpos
        expo_vector <- pracma::logseq(expo_range[1],
                                      expo_range[2],
                                      num_expos)
        #setup empty vectors for dose response plots
        ramets = c(rep(NA,num_expos))
        raumets = c(rep(NA,num_expos))
        rauprnts = c(rep(NA,num_expos))
        raoral = c(rep(NA,num_expos))
        auc_prnts = c(rep(NA,num_expos))
        auc_mets = c(rep(NA,num_expos))
        cmax_prnts = c(rep(NA,num_expos))
        cmax_mets = c(rep(NA,num_expos))
        auc_tots = c(rep(NA,num_expos))
        for(idx in seq_along(expo_vector)){
          update_modal_progress(idx/num_expos,sprintf("Running Exposure %i of %i",idx,num_expos))
          each_dose <- expo_vector[[idx]]
          #Setup exposure
          if(expo_route=="iv"){
            scaled_initial_values[["boral"]]<-0
            scaled_initial_values[["ivdose"]] <- each_dose
            ivlen <- input$num_testIVLen
            if(ivlen == 24){
              event_times <- c(0)
            }else{
              #Number of replications of the event
              Nrep <- ceiling(max(times) / 24)
              #Find start and end times
              event_times <- rep(c(0, ivlen), Nrep) + rep(24 * (0:(Nrep - 1)), rep(2, Nrep))
              
            }
            
            
          }else{
            
            scaled_initial_values[["boral"]] <- each_dose
            scaled_initial_values[["ivdose"]]<-0
            event_times <- head(seq(0,tstop,24),-1)
            if(oral_expo_type=="fa"){
              scaled_initial_values[["fa"]]<- getChemicalFa(chem,each_dose)
              scaled_initial_values[["vmax0"]] <- 0
              scaled_initial_values[["km0"]]<- 1
              
            }else if(oral_expo_type=="sat"){
              scaled_initial_values[["ka"]]<-0
            }else{
              scaled_initial_values[["vmax0"]] <- 0
              scaled_initial_values[["km0"]]<- 1
            }
            
          }
          #run each simulation
          dyn.load(dll_mName)
          parms <-initParms(scaled_initial_values)
          
          y <- initStates(params)
          out <- ode(y,times,func= 'derivs',parms = parms,
                     dllname = "KMD_model_event",method = "lsodes",
                     initfunc = "initmod",
                     events = list(func = "event",time = event_times),
                     nout = length(Outputs),
                     outnames = Outputs)
          out <- as.data.frame(out)
          dyn.unload(dll_mName)
          #Last time index
          tlast24h_idx <- which(out$time==(tstop-24))
          
          tlast_idx<- length(out$time)
          tlast24h_idx <- tlast_idx-(24/0.1)
          last24_time_array <- out$time[tlast24h_idx:tlast_idx]
          last24_cpls_array <- out$cpls[tlast24h_idx:tlast_idx]
          last24_cmet_array <- out$cmet[tlast24h_idx:tlast_idx]
          
          # get the values as needed by for dose response plots
          ramets[idx]<- max(out$ramet)
          raumets[idx]<- max(out$raumet)
          rauprnts[idx]<- max(out$rauprnt)
          raoral[idx]<- max(out$raoral)
          cmax_mets[idx]<- max(out$cmet)
          cmax_prnts[idx]<- max(out$cpls)
          auc_mets[idx]<- max(out$auc_cmet)-out$auc_cmet[tlast24h_idx]
          auc_prnts[idx]<- max(out$auc_cprnt)-out$auc_cprnt[tlast24h_idx]
          auc_tots[idx]<- max(out$auc_ctot)-out$auc_ctot[tlast24h_idx]
        }
        
        shinybusy::remove_modal_progress()
        results$dr <- data.frame("expos" = expo_vector,
                                 "ramet" = ramets,
                                 "raumet"=raumets,
                                 "rauprnt"=rauprnts,
                                 "raoral"=raoral,
                                 "cpls" = cmax_prnts,
                                 "cmet" = cmax_mets,
                                 "auc_prnt" = auc_prnts,
                                 "auc_met"= auc_mets,
                                 "auc_tot"=auc_tots)
        
      }
      
      
      
        
        
        
      
    })
    
    
    
    
    tc_plt_data <- reactive({
        validate(need(results$pbpk,"Time Course Simulation not Run"))
        return(results$pbpk)
        
    })
    
    tc_conc_data <- reactive({
      validate(need(results$pbpk,"Time Course Simulation not Run"))
      col_names <- c("time","cpls","cmet","crpf","cspf","cliv")
      simulation_data <- results$pbpk
      return_data <- simulation_data[which(names(simulation_data) %in% col_names)]
      return_data <- return_data[,col_names]
      return(return_data)
      
    })
    
    output$plt_tc_conc<- renderPlotly({
        plot_ly(data = tc_plt_data(),x = ~time)%>%
            add_trace(y= ~cpls,mode= "lines",type = "scatter",name = "Plasma Parent Concentration")%>%
            add_trace(y= ~cmet, mode = "lines",type = "scatter",name = "Plasma Metabolite Concentration")%>%
        add_trace(y= ~crpf, mode = "lines",type = "scatter",name = "RPF Concentration")%>%
        add_trace(y= ~cspf, mode = "lines",type = "scatter",name = "SPF Concentration")%>%
        add_trace(y= ~cliv, mode = "lines",type = "scatter",name = "Liver Concentration")%>%
            layout(yaxis = list(title = "Concentration (\U00B5M)"),
                   xaxis = list(title = "Time (h)"),
                   legend=list(orientation = "h",y = 100)
                   )
        })
    
    
    output$tble_tc_conc<-renderDT(DT::datatable(tc_conc_data(),rownames = F,
                                              colnames = c("Time(h)",
                                                           "Parent Plasma Concentration(\U00B5M)",
                                                           "Metabolite Plasma Concentration(\U00B5M)",
                                                           "Parent RPF Tissue Concentration(\U00B5M)",
                                                           "Parent SPF Tissue Concentration(\U00B5M)",
                                                           "Parent Liver Concentration(\U00B5M)"),
                                              extensions = "Buttons",
                                              options = list(
                                                dom = 'Bfrtip',
                                                buttons = c('csv', 'excel')
                                              )
    ),server = T
    )
    
    tc_amt_data <- reactive({
      validate(need(results$pbpk,"Time Course Simulation not Run"))
      col_names <- c("time","apls","amet","auprnt","aumet","arpf","aspf","aliv")
      simulation_data <- results$pbpk
      return_data <- simulation_data[which(names(simulation_data) %in% col_names)]
      return_data <- return_data[,col_names]
      return(return_data)
      
      
    })
    
    output$plt_tc_amt<- renderPlotly({
      plot_ly(data = tc_plt_data(),x = ~time)%>%
        add_trace(y= ~apls,mode= "lines",type = "scatter",name = "Parent Plasma")%>%
        add_trace(y= ~amet, mode = "lines",type = "scatter",name = "Metabolite Plasma")%>%
        add_trace(y= ~auprnt, mode = "lines",type = "scatter",name = "Parent Excreted in Urine")%>%
        add_trace(y= ~aumet, mode = "lines",type = "scatter",name = "Metabolite Excreted in Urine")%>%
        add_trace(y= ~arpf, mode = "lines",type = "scatter",name = "Parent RPF")%>%
        add_trace(y= ~aspf, mode = "lines",type = "scatter",name = "Parent SPF")%>%
        add_trace(y= ~aliv, mode = "lines",type = "scatter",name = "Parent Liver")%>%
        layout(yaxis = list(title = "Amount (\U00B5mol)"),
               xaxis = list(title = "Time (h)"),
               legend=list(orientation = "h",y = 100)
        )
    })
    
    output$tble_tc_amt<-renderDT(DT::datatable(tc_amt_data(),rownames = F,
                                                colnames = c("Time(h)",
                                                             "Parent Plasma Amount(\U00B5moles)",
                                                             "Metabolite Plasma Amount(\U00B5moles)",
                                                             "Parent Excreted in Urine(\U00B5moles)",
                                                             "Metabolite Excreted in Urine(\U00B5moles)",
                                                             "Parent RPF Tissue Amount(\U00B5moles)",
                                                             "Parent SPF Tissue Amount(\U00B5moles)",
                                                             "Parent Liver Amount(\U00B5moles)"),
                                                extensions = "Buttons",
                                                options = list(
                                                  dom = 'Bfrtip',
                                                  buttons = c('csv', 'excel')
                                                )
    ),server = T
    )
    
    tc_expo_data <- reactive({
      validate(need(results$pbpk,"Time Course Simulation not Run"))
      col_names <- c("time","aoral","totodose","totiv")
      simulation_data <- results$pbpk
      return_data <- simulation_data[which(names(simulation_data) %in% col_names)]
      return_data <- return_data[,col_names]
      return(return_data)
      
      
    })
    
    output$plt_tc_expo<- renderPlotly({
      plot_ly(data = tc_plt_data(),x = ~time)%>%
        add_trace(y= ~aoral,mode= "lines",type = "scatter",name = "Amount in gut lumen")%>%
        add_trace(y= ~totodose, mode = "lines",type = "scatter",name = "Total Oral Exposure")%>%
        add_trace(y= ~totiv, mode = "lines",type = "scatter",name = "Total IV Exposure")%>%
        layout(yaxis = list(title = "Amount (\U00B5mol)"),
               xaxis = list(title = "Time (h)"),
               legend=list(orientation = "h",y = 100)
        )
    })
    
    output$tble_tc_expo<-renderDT(DT::datatable(tc_expo_data(),rownames = F,
                                               colnames = c("Time(h)",
                                                            "Amount in Gut Lumen(\U00B5moles)",
                                                            "Total Oral Exposure(\U00B5moles)",
                                                            "Total IV Exposure(\U00B5moles)"),
                                               extensions = "Buttons",
                                               options = list(
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel')
                                               )
    ),server = T
    )
    
    
    output$plt_tc_QC<- renderPlotly({
      plot_ly(data = tc_plt_data(),x = ~time)%>%
        add_trace(y= ~mbal,mode= "lines",type = "scatter",name = "Mass Balance (\U00B5mol)")%>%
        add_trace(y= ~qbal, mode = "lines",type = "scatter",name = "Blood Flow Balance (L/h)")%>%
        add_trace(y= ~vbal, mode = "lines",type = "scatter",name = " Perfused Tissue Volume Balance (L)")%>%
       
        layout(xaxis = list(title = "Time (h)"),
               legend=list(orientation = "h",y = 100)
        )
    })
    
    dr_plt_data <- reactive({
        validate(need(results$dr,"Dose Response Simulation Not Run"))
      ret_data <- results$dr
      ret_data$cpls <- NULL
      ret_data$cmet <- NULL
      ret_data$expos <- results$dr$expos
      ret_data$ramet <- results$dr$ramet
      ret_data$raumet <- results$dr$raumet
      ret_data$rauprnt <- results$dr$rauprnt
      ret_data$auc_prnt <- results$dr$auc_prnt
      ret_data$auc_met <- results$dr$auc_met
      ret_data$auc_tot <- results$dr$auc_tot
      return(ret_data)
        
    })
    output$plt_DRSim<- renderPlotly({
      plot_ly(data = dr_plt_data(),x = ~expos)%>%
        add_trace(y = ~ramet,type = "scatter",mode = "lines",name = "Metabolite generation",yaxis = "y2")%>%
        add_trace(y = ~rauprnt,type = "scatter",mode = "lines",name = "Urinary Excretion of Parent",yaxis = "y2")%>%
        add_trace(y = ~raumet,type = "scatter",mode = "lines",name = "Urinary Excretion of Metabolite",yaxis = "y2")%>%
        add_trace(y = ~raoral,type = "scatter",mode = "lines",name = "Absorption of parent",yaxis = "y2")%>%
        add_trace(y = ~auc_prnt, type = "scatter",mode = "markers",name = "Parent Plasma AUC")%>%
        add_trace(y= ~auc_met,type = "scatter",mode = "markers",name="Metabolite Plasma AUC")%>%
        add_trace(y= ~auc_tot,type = "scatter",mode = "markers",name="Total Chemical AUC")%>%
        layout(yaxis = list(side = "left",type= input$sel_yaxis,
                            title = "24h Concentration AUC at Steady State(\U00B5M.h)"),
               yaxis2 = list(side = "right",automargin=TRUE,
                             overlaying = "y",type = input$sel_yaxis,
                             title = "Maximum Rate of Change of Amount (\U00B5mol/h)"),
               xaxis = list(title= ifelse(input$sel_expo_type=="iv",
                                          "IV Infusion (mg/h)",
                                          "Oral Bolus (mg/kg Bw/day)"),
                            type = "log"),
               legend = list(orientation = 'h',
                             y = 100)
        )
    })
    
    output$tble_DRSim<-renderDT(DT::datatable(dr_plt_data(),rownames = F,
                                              colnames = c("Exposure",
                                                           "Maxmimum Urinary Clearance Rate for Parent",
                                                           "Maximum Urinary Clearance Rate for Metabolite",
                                                           "24h AUC for Parent",
                                                           "24h AUC for Metabolite",
                                                           "24h AUC for Parent and Metabolite"),
                                              extensions = "Buttons",
                                              options = list(
                                                dom = 'Bfrtip',
                                                buttons = c('csv', 'excel')
                                              )
    ),server = F
    )
    
    auc_dose_norm_plt_data <- reactive({
      validate(need(results$dr,"Dose Response Simulation Not Run"))
    
      ret_data <- results$dr
      ret_data$cpls <- NULL
      ret_data$cmet <- NULL
      ret_data$auc_prnt<- results$dr$auc_prnt/results$dr$expos
      ret_data$auc_met<- results$dr$auc_met/results$dr$expos
      ret_data$auc_tot<- results$dr$auc_tot/results$dr$expos
      return(ret_data)
    })
    output$plt_auc_dose_norm<- renderPlotly({
      plot_ly(data = auc_dose_norm_plt_data(),x = ~expos)%>%
        add_trace(y = ~ramet,type = "scatter",mode = "lines",name = "Rate of Metabolism",yaxis = "y2")%>%
        add_trace(y = ~rauprnt,type = "scatter",mode = "lines",name = "Rate Urinary Excretion of Parent",yaxis = "y2")%>%
        add_trace(y = ~raumet,type = "scatter",mode = "lines",name = "Rate Urinary Excretion of Metabolite",yaxis = "y2")%>%
        add_trace(y = ~raoral,type = "scatter",mode = "lines",name = "Rate of Absorption of the Parent",yaxis = "y2")%>%
        add_trace(y = ~auc_prnt, type = "scatter",mode = "markers",name = "Parent Plasma AUC")%>%
        add_trace(y= ~auc_met,type = "scatter",mode = "markers",name="Metabolite Plasma AUC")%>%
        add_trace(y= ~auc_tot,type = "scatter",mode = "markers",name="Total Chemical AUC")%>%
        layout(yaxis = list(side = "left",type= input$sel_yaxis2,
                            title = "24h Concentration Dose normalized AUC at Steady State(\U00B5M.h)"),
               yaxis2 = list(side = "right",automargin=TRUE,
                             overlaying = "y",type = input$sel_yaxis2,
                             title = "Maximum Rate of Change of Amount(\U00B5mol/h)"),
               xaxis = list(title= ifelse(input$sel_expo_type=="iv",
                                          "IV Infusion (mg/h)",
                                          "Oral Bolus (mg/kg Bw/day)"),
                            type = "log"),
               legend = list(orientation = 'h',
                             y = 100)
        )
    })
    
    output$tble_auc_dose_norm<-renderDT(DT::datatable(auc_dose_norm_plt_data(),rownames = F,
                                              colnames = c("Exposure","Maxmimum Metabolic Rate",
                                                           "Maximum Urinary Clearance Rate for Metabolite",
                                                           "Maximum Urinary Clearance Rate for Parent",
                                                           "24h AUC for Parent",
                                                           "24h AUC for Metabolite",
                                                           "24h AUC for Parent and Metabolite"),
                                              extensions = "Buttons",
                                              options = list(
                                                dom = 'Bfrtip',
                                                buttons = c('csv', 'excel')
                                              )
    ),server = F
    )
    
    cmax_dose_plt_data <- reactive({
      validate(need(results$dr,"Dose Response Simulation Not Run"))
      ret_data <- results$dr
      ret_data$auc_prnt <- NULL
      ret_data$auc_met <- NULL
      ret_data$auc_tot <- NULL
      ret_data$expos <- results$dr$expos
      ret_data$ramet <- results$dr$ramet
      ret_data$raumet <- results$dr$raumet
      ret_data$rauprnt <- results$dr$rauprnt
      ret_data$cpls <- results$dr$cpls
      ret_data$cmet <- results$dr$cmet
      return(ret_data)
    })
    output$plt_cmax_dose<- renderPlotly({
      plot_ly(data = cmax_dose_plt_data(),x = ~expos)%>%
        add_trace(y = ~ramet,type = "scatter",mode = "lines",name = "Rate of Metabolism",yaxis = "y2")%>%
        add_trace(y = ~rauprnt,type = "scatter",mode = "lines",name = "Rate Urinary Excretion for Parent",yaxis = "y2")%>%
        add_trace(y = ~raumet,type = "scatter",mode = "lines",name = "Rate Urinary Excretion for Metabolite",yaxis = "y2")%>%
        add_trace(y = ~cpls, type = "scatter",mode = "markers",name = "Parent in Plasma")%>%
        add_trace(y= ~cmet,type = "scatter",mode = "markers",name="Metabolite in Plasma")%>%
        layout(yaxis = list(side = "left",type= input$sel_yaxis,
                            title = "Maximum concentration at Steady State(\U00B5M)"),
               yaxis2 = list(side = "right",automargin=TRUE,
                             overlaying = "y",type = input$sel_yaxis,
                             title = "Maximum Rate of Change of Amount (\U00B5mol/h)"),
               xaxis = list(title= ifelse(input$sel_expo_type=="iv",
                                          "IV Infusion (mg/h)",
                                          "Oral Bolus (mg/kg Bw/day)"),
                            type = "log"),
               legend = list(orientation = 'h',
                             y = 100)
        )
    })
    
    output$tble_cmax_dose<-renderDT(DT::datatable(cmax_dose_plt_data(),rownames = F,
                                              colnames = c("Exposure","Maxmimum Metabolic Rate",
                                                           "Maximum Urinary Clearance Rate for Metabolite",
                                                           "Maxmimum Urinary Clearance Rate for Parent",
                                                           "Maxmimum Parent Concentration",
                                                           "Maximim Metabolite Concentration"),
                                              extensions = "Buttons",
                                              options = list(
                                                dom = 'Bfrtip',
                                                buttons = c('csv', 'excel')
                                              )
    ),server = F
    )
    
    
    
    observeEvent(input$rdo_setup_tab,{
        
        updateTabsetPanel(session,"setup_tabs",selected = input$rdo_setup_tab)
        
    })
    observeEvent(input$page,{
        if (input$page == "off"){
            stopApp()
        }
    })

})
