library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinybusy)

# Define UI for application that draws a histogram
shinyUI(
  
  shiny::navbarPage("KMD Modeling Case Study",id = "page",
                    
                      header = tagList(
                          conditionalPanel("input.page == 'setup'",
                                           fluidRow(
                                               column(10, offset = 1,
                                                      radioGroupButtons("rdo_setup_tab",NULL,
                                                                        choices = list(
                                                                            "Chemical and Physiology"="chem_physio",
                                                                            "Absorption"="absorb",
                                                                            "Metabolism Parameters"="metab",
                                                                            "Urinary Clearance Parameters"="urinary",
                                                                            "Simulation Setup"="sim"
                                                                        ),
                                                                        selected = "chem_physio",
                                                                        width = "100%"
                                                      )
                                               )
                                           )
                                           ),
                          
                          fluidRow(
                            shinybusy::add_busy_spinner(spin = "orbit",
                                                        timeout = 100,
                                                        position="bottom-right",
                                                        margins = c(100,250),
                                                        height = "200px",
                                                        width = "200px"),
                              tags$br()
                          )
                          
                      ),
                    
                      tabPanel(title = NULL,icon = icon("home"),
                               includeHTML("www/home.html")
                               ),
                      tabPanel(title = "Setup",value ="setup",
                               tabsetPanel(id = "setup_tabs",type= "hidden",
                                           tabPanelBody(value = "chem_physio",
                                                        fluidRow(
                                                            column(3,offset = 1,
                                                                   selectInput("sel_chem","Select Chemical",
                                                                               choices =c("Chemical A"="ChemA","Chemical B"="ChemB",
                                                                                          "Chemical C"="ChemC","Chemical D"="ChemD",
                                                                                          "Chemical E"="ChemE","Chemical F"="ChemF",
                                                                                          "Chemical G"="ChemG","Chemical H"="ChemH"),width = "100%")
                                                            ),
                                                            column(3,
                                                                   selectInput("sel_org","Select Physiology",
                                                                               choices =c("Rat"="ra","Human"="ha"),width = "100%")
                                                            )
                                                        ),
                                                        fluidRow(
                                                          tags$h3("Chemical Properties")
                                                        ),
                                                        fluidRow(
                                                            column(3,offset = 1,
                                                                   numericInputIcon("num_MW","Molecular Weight",
                                                                                    icon = list("g/mol"),value = 0,
                                                                                    width= "100%")
                                                                   ),
                                                            column(3,
                                                                   numericInput("num_lkow","Log10 Octanol Water Partition",
                                                                                value = 1,width = "100%")
                                                                   )
                                                        ),
                                                        fluidRow(
                                                          tags$h3("Physiology")
                                                        ),
                                                        fluidRow(
                                                            column(3,offset = 1,
                                                                   numericInputIcon("num_bw","Body Weight",icon = list("kg"),
                                                                                    value = 0.35,min = 0.0001, 
                                                                                    help_text ="Weight should be greater than 0" ,width = "100%")
                                                                   ),
                                                            column(3,
                                                                   numericInputIcon("num_qcc","Cardiac Output",icon= list("L/h/ kg.BW \U00BE"),
                                                                                    value = 15.2,width = "100%")
                                                                   ),
                                                            column(3,
                                                                   numericInput("num_hct","Hematocrit Factor",value = 0.42,min = 0, max = 1, width = "100%")
                                                        )
                                                        ),
                                                        fluidRow(
                                                            column(3,
                                                                   numericInput("num_vbldc","Fractional Blood Volume",
                                                                                value= 0.074, min = 0, max =1 , width = "100%")
                                                                   ),
                                                            column(3,
                                                                   numericInput("num_vlivc","Fractional Liver Volume",
                                                                                value= 0.0387, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInput("num_vrpfc","Fractional Rapidly Perfused Tissue Volume",
                                                                                value= 0.0647, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInput("num_vspfc","Fractional Slowly Perfused Tissue Volume",
                                                                                value= 0.6925, min = 0, max =1 , width = "100%")
                                                            )
                                                        ),
                                                        fluidRow(
                                                            column(3,
                                                                   numericInput("num_qlivc","Fractional Liver Blood Flow",
                                                                                value= 0.183, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInput("num_qrpfc","Fractional Rapidly Perfused Tissue Blood Flow",
                                                                                value= 0.58, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInput("num_qspfc","Fractional Slowly Perfused Tissue Blood Flow",
                                                                                value= 0.23, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInputIcon("num_vurinec","Urine Production",icon = list("L/kg BW/day"),
                                                                                value= 0.012, min = 0, width = "100%")
                                                            )
                                                        ),
                                                        fluidRow(
                                                          tags$h3("Partitions")
                                                        ),
                                                        fluidRow(
                                                            column(3,offset = 1,
                                                                   numericInput("num_pliv","Liver Parition Coefficient",
                                                                                value= 0.183, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInput("num_prpf","Rapidly Perfused Tissue Partition Coefficient",
                                                                                value= 0.58, min = 0, max =1 , width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInput("num_pspf","Slowly Perfused Tissue Parition Coefficient",
                                                                                value= 0.23, min = 0, max =1 , width = "100%")
                                                            )
                                                        )
                                                        ),
                                           tabPanelBody(value = "absorb",
                                                        fluidRow(
                                                          column(3,offset= 1,
                                                                 numericInputIcon("num_ka","Non Saturable Oral Absorption Rate",icon= list("/h"),
                                                                                  value = 5,min = 0,help_text = "Value should not be less than 0",width = "100%"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 numericInputIcon("num_fa",icon = list("fraction"),
                                                                                  "Fraction Absorped", 
                                                                                  value = 1, min = 0, max = 1,
                                                                                  help_text = "Value should be between 0 and 1",width = "100%")
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(3,offset =1,
                                                                 numericInputIcon("num_vmax0","Maximum Saturable Oral Absorption Rate",
                                                                                  icon=list("\U00B5mol/h"),value =10, min = 0,
                                                                                  help_text = "Value should not be less than zero",width = "100%")
                                                                 ),
                                                          column(3,
                                                                 numericInputIcon("num_km0","Michaelis Menten Constant for Absorption",
                                                                                  icon = list("\U00B5mol"),
                                                                                  value = 2, min = 0.00001,
                                                                                  help_text = "Value should be greater than 0",width = "100%")
                                                          )
                                                        )
                                                        ),
                                           tabPanelBody(value = "metab",
                                                        fluidRow(
                                                            column(3,offset = 1,
                                                                   numericInputIcon("num_vmax",
                                                                                    "Maximum Metabolic Rate",
                                                                                    value = 0,
                                                                                    icon = list("\U00B5mol/h"),width = "100%")
                                                            ),
                                                            column(3,
                                                                   numericInputIcon("num_km",
                                                                                    "Michaelis Menten Constant",
                                                                                    icon = list("\U00B5M"),
                                                                                    value = 1,width = "100%")
                                                                   
                                                            ),
                                                            column(3,
                                                                   numericInputIcon("num_vkm1","Hepatic Clearence",
                                                                                    icon = list("L/h"),
                                                                                    value = 0,width = "100%")
                                                            )
                                                        )
                                                        ),
                                           tabPanelBody(value = "urinary",
                                                        fluidRow(
                                                          column(3,offset = 1,
                                                                 tags$h3("Parent")
                                                                 )
                                                          
                                                          
                                                        ),
                                                        fluidRow(
                                                          column(3,offset = 1,
                                                                 numericInputIcon("num_vmaxpu","Maximum Urinary Excretion Rate",
                                                                                  icon = list("\U00B5mol/h"),
                                                                                  value = 0,width = "100%")
                                                          ),
                                                          column(3,
                                                                 numericInputIcon("num_kmpu","Michaelis Menten Constant",
                                                                                  icon = list("\U00B5M"),
                                                                                  value = 1,width = "100%")
                                                                 
                                                          ),
                                                          column(3,
                                                                 numericInputIcon("num_vkep1","First-order Urinary Clearance Rate",
                                                                                  icon = list("L/h"),
                                                                                  value = 0,width = "100%")
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(3,offset = 1,
                                                                 tags$h3("Metabolite")
                                                          )
                                                          
                                                        ),
                                                        
                                                        fluidRow(
                                                          column(3,offset = 1,
                                                                 numericInput("num_vmaxmu","Maximum Urinary Excretion Rate (\U00B5mol/h)",
                                                                              value = 0,width = "100%")
                                                          ),
                                                          column(3,
                                                                 numericInput("num_kmmu","Michaelis Menten Constant (\U00B5M)",
                                                                              value = 1,width = "100%")
                                                                 
                                                          ),
                                                          column(3,
                                                                 numericInput("num_vkem1","First-order Urinary Clearance Rate (L/h)",
                                                                              value = 0,width = "100%")
                                                          )
                                                        )
                                                        
                                                        ),
                                           tabPanelBody(value = "sim",
                                                        fluidRow(
                                                          column(2,offset = 1, 
                                                                 selectInput("sel_sim_type","Simulation Type",
                                                                             choices = c("Time Course"="time","Dose Response"="dose"),
                                                                             width = "100%"
                                                                             )
                                                                 ),
                                                          column(2,
                                                                 selectInput("sel_expo_type","Exposure Type",
                                                                             choices = c("IV infusion"="iv","Oral Bolus Dose"="boral"),
                                                                             width = "100%"
                                                                             )
                                                                 ),
                                                          column(2,
                                                                 numericInputIcon("num_testSimDuration","Simulation Duration",
                                                                                  icon = list("h"),
                                                                                  value= 2160, 
                                                                                  min = 0.1,
                                                                                  help_text = "Value has to be greater than 0",
                                                                                  width = "100%"
                                                                 )
                                                          )
                                                        ),
                                                        fluidRow(
                                                          conditionalPanel("input.sel_expo_type=='iv'",
                                                                           column(3,offset = 1,
                                                                                  numericInputIcon("num_testIVLen",
                                                                                                   "Length of IV Infusion",
                                                                                                   icon = list("h/day"),
                                                                                                   value = 24,
                                                                                                   min = 0.1, 
                                                                                                   max = 24,
                                                                                                   help_text = "Value should be between 0.1 and 24",
                                                                                                   width = "100%"
                                                                                  )
                                                                           )
                                                                           ),
                                                          conditionalPanel("input.sel_expo_type == 'boral' ",
                                                                           column(3, offset = 1,
                                                                                  selectInput("sel_oral_expo_type",
                                                                                              "Select Oral Exposure Model",
                                                                                              choices = c("Dose Independent Fa"="lin",
                                                                                                          "Dose Dependent Fa"="fa",
                                                                                                          "Saturable Absorption"="sat"))
                                                                           )
                                                                           )
                                                          ),
                                                        fluidRow(
                                                          conditionalPanel("input.sel_sim_type == 'time' && input.sel_expo_type == 'iv'",
                                                                           column(3,offset = 1,
                                                                                  numericInputIcon("num_testIVDose",
                                                                                                   "IV Exposure for Time Course Simulation",icon = list("mg/h"),
                                                                                                   value = 10,
                                                                                                   width = "100%")
                                                                                  )
                                                                           ),
                                                          conditionalPanel("input.sel_sim_type == 'time' && input.sel_expo_type == 'boral'",
                                                                           column(3,offset = 1,
                                                                                  numericInputIcon("num_testOralDose",
                                                                                                   "Bolus Exposure for Time Course Simulation",
                                                                                                   icon = list("mg/kg BW/day"),
                                                                                                   value = 10,
                                                                                                   width= "100%")
                                                                                  )
                                                                           )
                                                          ),
                                                        fluidRow(
                                                          column(6,offset = 1,
                                                                 conditionalPanel("input.sel_sim_type == 'dose' ",
                                                                                  numericRangeInput("numrange_expo",
                                                                                                    "Select Exposure Range for Dose Responses Plots",
                                                                                                    value = c(0.1,10),separator = "to",
                                                                                                    width = "100%")
                                                                                  )
                                                                 ),
                                                          column(3,
                                                                 conditionalPanel("input.sel_sim_type == 'dose' ",
                                                                                  numericInput("num_numexpos","Number of Exposures",
                                                                                               value = 50,width = "100%")
                                                                                  )
                                                                 )
                                                          ),
                                                        fluidRow(
                                                          column(3,offset = 1,
                                                                 selectInput("sel_metabType","Select Metabolism Type",
                                                                             c("Saturable"="sat",
                                                                               "Linear First Order"="lin",
                                                                               "None"="none"),
                                                                             selected = "lin",
                                                                             width = "100%"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 selectInput("sel_metab_utype","Select Metabolite Urinary Clearance Type",
                                                                             c("Saturable"="sat",
                                                                               "Linear First Order"="lin",
                                                                               "None"="none"),selected = "none",
                                                                             width = "100%"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 selectInput("sel_prnt_utype","Select Parent Urinary Clearance Type",
                                                                             c("Saturable"="sat",
                                                                               "Linear First Order"="lin",
                                                                               "None"="none"),selected = "none",
                                                                             width = "100%"
                                                                 )
                                                          )
                                                          
                                                          
                                                        ),
                                                        
                                                        fluidRow(
                                                          column(4, offet = 2, 
                                                                 bsButton("btn_runSim","Run Simulation",style = "primary",width = "100%")
                                                          )
                                                        ),
                                                        bsCollapse(id="sim_panels",
                                                                   bsCollapsePanel("Time Course Plot",value = "test_sims",
                                                                                   tabsetPanel(
                                                                                     tabPanel("Concentrations",
                                                                                              tabsetPanel(type = "pills",
                                                                                                          tabPanel("Plot",
                                                                                                                   
                                                                                                                   column(10,offset = 1,
                                                                                                                          plotlyOutput("plt_tc_conc")
                                                                                                                          )
                                                                                                                   ),
                                                                                                          tabPanel("Data",
                                                                                                                   column(10,offset = 1,
                                                                                                                          DTOutput("tble_tc_conc")
                                                                                                                          )
                                                                                                                   )
                                                                                                          )
                                                                                              ),
                                                                                     tabPanel("Amounts",
                                                                                              tabsetPanel(type = "pills",
                                                                                                          tabPanel("Plot",
                                                                                                                   column(10,offset = 1,
                                                                                                                          plotlyOutput("plt_tc_amt")
                                                                                                                   )
                                                                                                          ),
                                                                                                          tabPanel("Data",
                                                                                                                   column(10,offset = 1,
                                                                                                                          DTOutput("tble_tc_amt")
                                                                                                                   )
                                                                                                          )
                                                                                              )
                                                                                     ),
                                                                                     tabPanel("Exposure",
                                                                                              tabsetPanel(type = "pills",
                                                                                                          tabPanel("Plot",
                                                                                                                   column(10,offset = 1,
                                                                                                                          plotlyOutput("plt_tc_expo")
                                                                                                                   )
                                                                                                          ),
                                                                                                          tabPanel("Data",
                                                                                                                   column(10,offset = 1,
                                                                                                                          DTOutput("tble_tc_expo")
                                                                                                                   )
                                                                                                          )
                                                                                              )
                                                                                     ),
                                                                                     tabPanel("QC",
                                                                                              tabsetPanel(type = "pills",
                                                                                                          tabPanel("Plot",
                                                                                                                   column(10,offset = 1,
                                                                                                                          plotlyOutput("plt_tc_QC")
                                                                                                                   )
                                                                                                          ),
                                                                                                          tabPanel("Data",
                                                                                                                   column(10,offset = 1,
                                                                                                                          DTOutput("tble_tc_QC")
                                                                                                                   )
                                                                                                          )
                                                                                              )
                                                                                     )
                                                                                   )
                                                                                   ),
                                                                   bsCollapsePanel("Dose Response Simulation Plots", value = "DR_sims",
                                                                                   tabsetPanel(
                                                                                     tabPanel("AUC",
                                                                                              tabsetPanel(type = "pills",
                                                                                                tabPanel("Plot",
                                                                                                         fluidRow(
                                                                                                           column(1,
                                                                                                                  selectInput("sel_yaxis",NULL,
                                                                                                                              choices = c("log","linear"))
                                                                                                           ),
                                                                                                           column(10,offset = 1,
                                                                                                                  plotlyOutput("plt_DRSim")
                                                                                                           )
                                                                                                           
                                                                                                         )
                                                                                                ),
                                                                                                tabPanel("Data",
                                                                                                         fluidRow(
                                                                                                           column(10,offset = 1,
                                                                                                                  DTOutput("tble_DRSim")
                                                                                                           )
                                                                                                         )
                                                                                                )
                                                                                              )
                                                                                              ),
                                                                                     tabPanel("Dose Normalized AUC",
                                                                                              tabsetPanel(type = "pills",
                                                                                                          tabPanel("Plot",
                                                                                                                   fluidRow(
                                                                                                                     column(1,
                                                                                                                            selectInput("sel_yaxis2",NULL,
                                                                                                                                        choices = c("log","linear"))
                                                                                                                     ),
                                                                                                                     column(10,offset = 1,
                                                                                                                            plotlyOutput("plt_auc_dose_norm")
                                                                                                                     )
                                                                                                                     
                                                                                                                   )
                                                                                                          ),
                                                                                                          tabPanel("Data",
                                                                                                                   fluidRow(
                                                                                                                     column(10,offset = 1,
                                                                                                                            DTOutput("tble_auc_dose_norm")
                                                                                                                     )
                                                                                                                   )
                                                                                                          )
                                                                                              )),
                                                                                     tabPanel("Maxmimum Concentrations",
                                                                                              tabsetPanel(type = "pills",
                                                                                                          tabPanel("Plot",
                                                                                                                   fluidRow(
                                                                                                                     column(1,
                                                                                                                            selectInput("sel_yaxis2",NULL,
                                                                                                                                        choices = c("log","linear"))
                                                                                                                     ),
                                                                                                                     column(10,offset = 1,
                                                                                                                            plotlyOutput("plt_cmax_dose")
                                                                                                                     )
                                                                                                                     
                                                                                                                   )
                                                                                                          ),
                                                                                                          tabPanel("Data",
                                                                                                                   fluidRow(
                                                                                                                     column(10,offset = 1,
                                                                                                                            DTOutput("tble_cmax_dose")
                                                                                                                     )
                                                                                                                   )
                                                                                                          )
                                                                                              ))
                                                                                   )
                                                                                   
                                                                                   
                                                                                   )
                                                                       
                                                         
                                                            
                                                        )
                                                        )
                                           )
                               ),
                      tabPanel(NULL,value = "off",icon = icon("power-off"))
                      )
    )

