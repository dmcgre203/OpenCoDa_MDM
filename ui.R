library(shiny)

ui <- fluidPage(

  # App title ----
  titlePanel("CODA Cox Regression Model"),
  
  shinyUI(navbarPage("Open CoDa",
                     tabPanel("Read_Data",

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
    
      # Sidebar panel for inputs ----
      sidebarPanel(
      
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
        # Horizontal line ----
        tags$hr(),
      
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
      
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
        # Horizontal line ----
        tags$hr(),
      
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
        
        # Input: Select response variables ----
        uiOutput("choose_TimeToEvent"),
        uiOutput("choose_EventType"),
        uiOutput("choose_DeathCode")
        
        ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        tags$h3(textOutput("rawData_Header")), 
        tableOutput("contents"),
        
        # Horizontal line ----
        tags$hr(),        
        
        tags$h3(textOutput("rawDataSummary_Header")), 
        tableOutput("rawSummary")
        )
      
    )),
    
    tabPanel("Covariates",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
        
                 # Input: Additional parameters
                 uiOutput("choose_covariates"),
                 uiOutput("choose_cofactors")
                 
               ),

                mainPanel(
                  # Output: Model spec ----
                  textOutput("modelText"),
                  
                  tags$br(),
                  
                  # Output: Drop1 Table ----
                  tableOutput("regTab"), 

                  # Output: Model summary ----
                  tags$h3(textOutput("regSummaryHeader")),                 
                  verbatimTextOutput("regSummary"),
                  
                  # Output: Factor summary ----
                  tags$h3(textOutput("cofactorSummary_Header")),
                  verbatimTextOutput("cofactorSummary")
                  
                )
              )),
    
    tabPanel("ILR Coordinates",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
                 
                 # Input: Additional parameters
                 uiOutput("choose_CODA"),
                 uiOutput("choose_useImpute"),
                 textOutput("choose_detectLimits_Header"),
                 uiOutput("choose_detectLimits"),
                 uiOutput("choose_ilr")
                 
               ),
               
               mainPanel(

                  # Output: zPatterns
                  tags$h3(textOutput("zPatterns_Header")),
                  plotOutput("zPatterns"),
                  
                  # Output: Imputation report
                  tags$h3(textOutput("imputeTable_Header")),
                  tableOutput("imputeTable"),  
                  
                  # Output: CODA Average
                  tags$h3(textOutput("avgCODA_Table_Header")),
                  tableOutput("avgCODA_Table"), 
                  
                  # Output: CODA ----
                  tags$h3(textOutput("ilrCoords_Header")),
                  tableOutput("ilrCoords"),  
                          
                  # Output: Model LR Test ----
                  tags$h3(textOutput("fullRegTab_Header")),
                  tableOutput("fullRegTab"),
                 
                  # Output: Model summary ----
                  tags$h3(textOutput("fullRegCoef_Header")),
                  tableOutput("fullRegCoef")
                  
               )
             )
          ),
    
    tabPanel("Diagnostics",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
                 
                 # Input: Which parameter
                 uiOutput("choose_KMVar"),
                 
                 # Input: Which parameter
                 uiOutput("choose_DiagVar")

               ),
               
               mainPanel(
                 
                 # Output: ZPH Plot ----
                 tags$h3(textOutput("fullRegKMCurves_Header")), 
                 plotOutput("fullRegKMCurves"),
                 
                 # Output: ZPH Plot ----
                 tags$h3(textOutput("fullRegcoxZPH_Table_Header")), 
                 tableOutput("fullRegcoxZPH_Table"),
                 
                 # Output: ZPH Plot ----
                 tags$h3(textOutput("fullRegcoxZPH_Header")), 
                 plotOutput("fullRegcoxZPH"),
                 
                 # Output: dfBeta Plot ----
                 tags$h3(textOutput("fullRegDfBetas_Header")), 
                 plotOutput("fullRegDfBetas"),
                 
                 # Output: Deviances Plot ----
                 tags$h3(textOutput("fullRegDeviances_Header")), 
                 plotOutput("fullRegDeviances"),
                 
                 # Output: Model summary ----
                 tags$h3(textOutput("regSummaryHeader2")),
                 verbatimTextOutput("fullRegSummaryOut")
                 
               ) # end MainPanel
             )  #end SidebarLayout
    ),  #end tabPanel
    
    tabPanel("Comparator / Null Hypothesis",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
                 
                 # Input: Additional parameters
                 uiOutput("choose_covariates_H0"),
                 uiOutput("choose_cofactors_H0"),
                 uiOutput("choose_ilr_H0")
                 
               ),
               
               mainPanel(
                 # Output: Model spec ----
                 textOutput("modelText_H0"),
                 verbatimTextOutput("fullRegSummary_H0"),
                 verbatimTextOutput("fullRegComparisonOut")
                 
               )
             )
    ),
    
    tabPanel("Forecasting",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
                 
                 # Input: Illustration cofactors
                 uiOutput("illustrationCofactors"),
                 
                 # Input: Illustration covariates
                 uiOutput("illustrationCovariates"),
                 
                 # Input: Illustration covariates
                 uiOutput("choose_extrapolate"),
                 
                 # Input: Illustration compositional
                 uiOutput("choose_illustrationX"),
                 
                 # Input: Illustration compositional
                 uiOutput("choose_illustrationY"),
                 
                 # Input: Illustration compositional
                 uiOutput("choose_illustrationZ"),
                 
                 # Input: Illustration fixed compositional
                 tags$h3(textOutput("illustrationComp_header")),
                 uiOutput("illustrationComp")
                 
               ),
               
               mainPanel(
                 
                 plotOutput("illustrativePlot"),
                 tableOutput("illustrativeData")
                
               ) # end MainPanel
             )  #end SidebarLayout
    ),  #end tabPanel
    
    tabPanel("Isotemporal",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
                 
                 # Input: Illustration compositional
                 uiOutput("choose_isotemporalX"),
                 
                 # Input: Illustration covariates
                 uiOutput("choose_extrapolate2"),
                 
                 # Input: Illustration compositional minimum
                 uiOutput("choose_isoLimits"),
                 
                 # Input: Illustration fixed compositional
                 tags$h3(textOutput("set_RefPoint_header")),
                 uiOutput("set_RefPoint"),
                 
                 # Input: Graph - label numbers x-axis
                 tags$h3(textOutput("choose_close_header")),
                 uiOutput("choose_closeTotal"),
                 
                 # Input: Graph - label units x-axis
                 uiOutput("choose_closeTotalUnits"),
                 
                 # Input: Graph - max x-axis
                 tags$h3(textOutput("choose_graph_limits")),
                 uiOutput("choose_minXDisplay"),
                 
                 # Input: Graph - max x-axis
                 uiOutput("choose_maxXDisplay"),
                 
                 # Input: Graph - max y-axis
                 uiOutput("choose_maxYDisplay")
                 
               ),
               
               mainPanel(
                 
                # tableOutput("testCODADat"),
                 plotOutput("illustrativeISOPlot"),
                 tags$h3(textOutput("RefPoint_header")),
                 tableOutput("test_refCODADat"),
                 tags$h3(textOutput("illustrativeISOPlot_Data_header")),
                 tableOutput("illustrativeISOPlot_Data"),
                 tableOutput("test_isoCODADat")
                 
               ) # end MainPanel
             )  #end SidebarLayout
    ),  #end tabPanel
    
    
    tabPanel("Meta Analysis",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(   
                 
                 # Input: Illustration cofactors
                 downloadButton("report", "Generate report")
                 
               ),
               
               mainPanel(
                 tableOutput("zzTableCheckOut"),
                 tableOutput("extractListOut"),
                 tableOutput("extractListOut2")
                 
               ) # end MainPanel
             )  #end SidebarLayout
    )  #end tabPanel
  
    
    
      )
  )
)
