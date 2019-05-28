library(shiny)

server <- function(input, output) {

  require(survival)
  require(survminer)
  require(compositions)
  require(robCompositions)
  require(zCompositions)
  require(ggplot2)
  require(ggtern)
  require(rmarkdown)
  require(knitr)
  require(lpSolve)
  
  # output$rawData_Header             - header text
  # output$rawDataSummary_Header      - header text
  # dat()                             - holder for raw data
  # output$contents                   - display raw data as table
  # output$rawSummary                 - basic summary table of raw data
  # output$choose_covariates          - UI to select continuous covariates
  #   - input$model_covars
  # output$choose_cofactors           - UI to select discrete covariates 
  #   - input$model_cofactors
  # output$choose_covariates_H0       - UI to select continuous covariates for comparator model
  #   - input$model_covars_H0
  # output$choose_cofactors_H0        - UI to select discrete covariates for comparator model
  #   - input$model_cofactors_H0
  # output$choose_CODA                - UI to select compositional variables
  #   - input$model_CODAvars
  # output$choose_useImpute           - UI to confirm whether to impute zero values
  #   - input$useImpute
  # output$choose_detectLimits        - UI to set detection limits for imputation
  #   - input$detectLimit_1,2,3...
  # output$choose_detectLimits_Header - header text
  # output$zPatterns                  - Plot of zero patterns in compositional data
  # output$zPatterns_Header           - header text
  # output$choose_TimeToEvent         -  UI to select time to event
  #   - input$TimeToEvent
  # output$choose_EventType           -  UI to select event types
  #   - input$EventType
  # output$choose_DeathCode           -  UI to specify which event types are deaths
  #   - input$input$deathCodes
  # output$modelText                  - renders specified model into R syntax as text
  # output$cofactorSummary_Header     - header text
  # confounderSummary()               - summary of model covariates (allows for discrete)
  # output$cofactorSummary            - display summary of model covariates (allows for discrete)
  # runRegression()                   - Cox regression model, no physical activity
  # output$regTab                     - Display drop1 table based on RunRegression()
  # output$regSummary                 - Display R summary of RunRegression()
  # output$regSummaryHeader           - header text 
  # output$regSummaryHeader2          - header text 
  # catvar                            - function, converts cts to quantiles, default x3
  # CODAdat()                         - ilr coordinates constructed using Pivot transform
  # CODAList()                        - list of CoDavars for use in automatic output
  # zzTableCheck()                    - Convert CODAList() to dataframe
  # output$zzTableCheckOut            - Display zzTableCheck
  # extractList()                     - Generate output for automatic output
  # output$extractListOut             - Display generated output
  # avgCODA()                         - CoDa average composition of imputed data
  # output$avgCODA_Table              - Display CoDa average of imputed data
  # output$avgCODA_Table_Header       - header text  
  # VariationMatrix()                 - Variation matrix of data
  # output$imputeTable                - Display sumary of Imputation
  # output$imputeTable_Header         - 
  # CODAcats                          - tertiles of ilrs (not used?)
  # output$choose_ilr                 - UI to select ilrs to use in model
  #   - input$model_ilr_coords
  # output$choose_ilr_H0              - UI to select ilrs to use in comparator model (default none)
  #   - input$model_ilr_coords_H0
  # Fulldat()                         - Combined table dat(),CODAdat()
  # runFullRegression()               - Cox regression model with physical activity
  # runRegression_H0()                - Cox regression model, no physical activity, comparator
  # runFullRegression_H0()            - Cox regression model with physical activity, comparator
  # output$modelText_H0               - renders specified comparator model into R syntax as text
  # output$fullRegTab                 - Display drop1 table based on RunFullRegression()
  # output$fullRegCoef                - Display regression coefficients for RunFullRegression()
  # fullRegSummary()                  - R summary of RunFullRegression()
  # output$fullRegSummaryOut          - Display R summary of RunFullRegression()
  # output$fullRegSummary_H0          - Display R summary of RunFullRegression_H0()
  # fullRegComparison()               - Calculate anova comparison between H0 and H1
  # output$fullRegComparisonOut       - Display anova comparison between H0 and H1
  # output$choose_DiagVar             - UI for selecting variable to view cox.zph diagnostics on 
  # output$choose_KMVar               - UI for selecting variable to view KM diagnostics on 
  # output$fullRegKMCurves            - Display Kaplan Meier Curves
  # fullRegcoxZPH_Data                - Calculate Schoenfeld residuals (cox.zph)
  # output$fullRegcoxZPH_Table        - Display Cox.zph table
  # output$fullRegcoxZPH              - Display plot of cox.zph residuals
  # output$fullRegDfBetas             - Calculate dfBetas
  # output$fullRegDeviances           - Calculate deviances
  # output$fullRegTab_Header          - header text
  # output$fullRegCoef_Header         - header text
  # output$fullRegKMCurves_Header     - header text
  # output$fullRegcoxZPH_Table_Header - header text
  # output$fullRegcoxZPH_Header       - header text
  # output$fullRegDfBetas_Header      - header text
  # output$fullRegDeviances_Header    - header text
   
  
  output$rawData_Header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Raw Data"
  })

  output$rawDataSummary_Header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Raw Data Summary Table"
  })
  
  dat <- reactive({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
  })
    
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    df <- dat()
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })

  output$rawSummary <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    df   <- dat()
    out1 <- do.call(cbind, lapply(df, summary))
    out  <- data.frame(rownames(out1),out1)
    colnames(out)[1] <- "Statistic"
    out
    
  })

  #UI to select input$model_covars
  output$choose_covariates <- renderUI({           
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_covars", "Choose continuous covariates",
                       choices = col_names,
                       selected = NULL)
  })
 
  #UI to select input$model_cofactors
  output$choose_cofactors <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_cofactors", "Choose categorical covariates",
                       choices = col_names,
                       selected = NULL)
  })
  
  #UI to select input$model_covars_H0
  output$choose_covariates_H0 <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_covars_H0", "Choose continuous covariates",
                       choices = col_names,
                       selected = input$model_covars)
  })
  
  #UI to select input$model_cofactors_H0
  output$choose_cofactors_H0 <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_cofactors_H0", "Choose categorical covariates",
                       choices = col_names,
                       selected = input$model_cofactors)
  })
  
  #UI to select input$model_CODAvars
  output$choose_CODA <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select none by default
    checkboxGroupInput("model_CODAvars", "Identify physical activity compositional variables",
                       choices = col_names,
                       selected = NULL)
  })
  
  #UI to confirm whether to impute zero values
  output$choose_useImpute <- renderUI({ 
  
    # UI prompts whether need to impute zero values
  
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()   
    
    col_names <- c("Yes","No")
     
    radioButtons("useImpute","Do you require to impute zero-values?",choices=col_names,
                 selected = col_names[2])
    
  })
  
  #UI to set detection limits for imputation
  output$choose_detectLimits <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()      
    if(is.null(input$useImpute)) return()
    if(input$useImpute=="No") return()
        
    D  <- length(input$model_CODAvars)
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    CODA.min <- min(df.CODA[df.CODA>0])
           
    lapply(seq(D),function(i){
      numericInput(paste0("detectLimit_",i), input$model_CODAvars[i], val=CODA.min)
    })
  })
  
  output$choose_detectLimits_Header <- renderText({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()      
    if(is.null(input$useImpute)) return()
    if(input$useImpute=="No") return()
    
    "Specify detection limits by component"
    
  })
  
  output$zPatterns <- renderPlot({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()
 
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])    
    
    if(length(which(df.CODA==0))>0) {
       zPatterns(df.CODA,label=0, plot=T)
       output <- recordPlot()
       return(output)
    } else{
      return()
    }
    
  })
  
  output$zPatterns_Header <- renderText({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()      
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars]) 
    
    if(length(which(df.CODA==0))>0) {
      "Patterns of Zeroes in Data"
    } else {
      "No Zeroes in Data"
    }
    
  })

  # Setup UI for identifying input$TimeToEvent
  output$choose_TimeToEvent <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    
    radioButtons("TimeToEvent","Identify time to event",choices=col_names,selected = col_names[1])
    
  })  
  
  # Setup UI for identifying input$EventType
  output$choose_EventType <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    
    radioButtons("EventType","Identify event type",choices=col_names,selected = col_names[1])
    
  })  
  
  # Setup UI for identifying input$deathCodes
  output$choose_DeathCode <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate values
    
    ETypes <- sort(unique(df[,input$EventType]))
                 
    # Create the checkboxes and select them all by default
    checkboxGroupInput("deathCodes", "Choose death codings",
                       choices = ETypes,
                       selected = NULL)
    
  })  
  
  # Create Surival Object
  SurvObj <- reactive({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1))       return()
    if(is.null(dat()))             return()  
    if(is.null(input$EventType))   return()
    if(is.null(input$TimeToEvent)) return()
    if(is.null(input$deathCodes))  return()
    
    TTE <- dat()[,input$TimeToEvent]
    ET  <- dat()[,input$EventType]
    VD  <- dat()[,input$EventType] %in% input$deathCodes
    
    SO <- Surv(TTE,VD)
    SO
    
  })  
  
  # Print model formula used
  output$modelText <- renderText({
    
    if(is.null(input$model_cofactors)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")",sep="") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    
    paste(" ~ ",paste(covars2,collapse=" + "))
  })
  
  output$cofactorSummary_Header <- renderText({
    if(is.null(dat())) return()
    if(is.null(input$model_cofactors)) return()
    "Summary of Included Categorical Variables"
  })

  confounderSummary <- reactive({
    
    if(is.null(dat())) return()
    if(is.null(input$model_cofactors)) return()
    if(is.null(input$model_covars)) return()
    
    df     <- dat()
    dat1   <- data.frame(df[,input$model_cofactors])
    dat1[] <- lapply(dat1, factor)
    colnames(dat1) <- input$model_cofactors
    dat2   <- data.frame(df[,input$model_covars])
    colnames(dat2) <- input$model_covars
    dat3   <- data.frame(dat1,dat2)
    
    out<-lapply(dat3, function(x) {
      if (is.numeric(x)) return(summary(x))
      if (is.factor(x))  return( rbind(levels(x),paste0(prop.table(table(x))*100,"%")) )
    })
    out
    
  })  
   
  output$cofactorSummary <- renderPrint({
    
    if(is.null(dat())) return()
    if(is.null(input$model_cofactors)) return()
    
    df   <- dat()
    dat1 <- data.frame(df[,input$model_cofactors])
    zfac <- lapply(dat1,as.factor)
    
    out<-lapply(zfac, function(x) {
      if (is.numeric(x)) return(summary(x))
      if (is.factor(x))  return( rbind(levels(x),paste0(prop.table(table(x))*100,"%")) )
    })
    out
    
  })
  
  # Setup Cox model for use in other routines
  runRegression <- reactive({

    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) return()
    if(is.null(SurvObj())) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2), collapse="+")
    
    SO <- SurvObj()
    
    if(is.null(input$model_cofactors)) {
      coxph(as.formula(paste("SO ~ ",covars1)),data=dat())
    } else if(is.null(input$model_covars)) {
      coxph(as.formula(paste("SO ~ ",cofacs2)),data=dat())
    } else {
      coxph(as.formula(paste("SO ~ ",covars2)),data=dat())
    }

  })
    
  # Display drop1 table (to assist users in selecting covariates)
  output$regTab <- renderTable({
    if(!(is.null(input$model_cofactors) && is.null(input$model_covars))){
      model.drop       <- drop1(runRegression(),test="Chisq")
      model.drop.table <- cbind(rownames(model.drop),as.data.frame(model.drop))
      colnames(model.drop.table)[1] <- "Covariate"
      model.drop.table
      #coef(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  
  # Show cox regression summary for selecting covariates
  output$regSummary <- renderPrint({
    if(!(is.null(input$model_cofactors) && is.null(input$model_covars))){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }    
  })
  
  output$regSummaryHeader <- renderText({"Model Summary"})
  output$regSummaryHeader2 <- renderText({"Model Summary"})
  
  # ***********************************************************
  
  catvar <- function(xvec,qvec=c(0.333333,0.666667)) {
    
    if(is.null(xvec)) return()
    
    qs  <- quantile(xvec,qvec,na.rm=T)
    
    catvar <- 0 * xvec
    catvar <- catvar + 1*(xvec  < qs[1])
    catvar <- catvar + (length(qs)+1)*(xvec >= qs[length(qs)])
    
    for (i in 2:(length(qs))) {
      catvar <- catvar + i *( (xvec >= qs[i-1])&(xvec < qs[i]) ) 
    }
    
    catvar[which(is.na(xvec))] <- 0
    catvar <- as.factor(catvar)
    
    return(catvar)
    
  }
  
  get_ilrCoord <- function(df.CODA) {
      D <- dim(df.CODA)[2]
      
      if(D==3) {
        z1      <- pivotCoord(df.CODA,pivotvar=1)
        z2      <- pivotCoord(df.CODA,pivotvar=2)
        z3      <- pivotCoord(df.CODA,pivotvar=3)
        
        zz      <- cbind(z1,z2,z3)
        df1      <- data.frame(zz)
        
      } else if(D==4) {
        z1      <- pivotCoord(df.CODA,pivotvar=1)
        colnames(z1) <- gsub("-", ".", colnames(z1))
        z11     <- pivotCoord(df.CODA[,-1],pivotvar=1)
        z12     <- pivotCoord(df.CODA[,-1],pivotvar=2)
        z13     <- pivotCoord(df.CODA[,-1],pivotvar=3)
        z2      <- pivotCoord(df.CODA,pivotvar=2)
        colnames(z2) <- gsub("-", ".", colnames(z2))
        z21     <- pivotCoord(df.CODA[,-2],pivotvar=1)
        z22     <- pivotCoord(df.CODA[,-2],pivotvar=2)
        z23     <- pivotCoord(df.CODA[,-2],pivotvar=3)
        z3      <- pivotCoord(df.CODA,pivotvar=3)
        colnames(z3) <- gsub("-", ".", colnames(z3))
        z31     <- pivotCoord(df.CODA[,-3],pivotvar=1)
        z32     <- pivotCoord(df.CODA[,-3],pivotvar=2)
        z33     <- pivotCoord(df.CODA[,-3],pivotvar=3)
        z4      <- pivotCoord(df.CODA,pivotvar=4)
        colnames(z4) <- gsub("-", ".", colnames(z4))
        z41     <- pivotCoord(df.CODA[,-4],pivotvar=1)
        z42     <- pivotCoord(df.CODA[,-4],pivotvar=2)
        z43     <- pivotCoord(df.CODA[,-4],pivotvar=3)
        
        zzA      <- cbind(z1[,1],z11,z12,z13,z2[,1],z21,z22,z23,
                          z3[,1],z31,z32,z33,z4[,1],z41,z42,z43)
        zzB      <- data.frame(zzA)
        colnames(zzB)[c(1,8,15,22)] <- c(colnames(z1)[1],colnames(z2)[1],
                                         colnames(z3)[1],colnames(z4)[1])
        df1 <- zzB
        
      } else {
        
        zList <- vector("list",D)
        
        for (i in 1:D) {
          zList[[i]] <- pivotCoord(df.CODA,pivotvar=i)
        }
        
        df1 <- do.call("cbind", zList)
        colnames(df1) <- gsub("-", ".", colnames(df1))
      }
      return(df1)
  }
  
  # ilr transform of compositional variables 
  CODAdat <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
        
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    D <- length(input$model_CODAvars)
    if(D<3) return()  
    
    if(input$useImpute=="Yes") {
      if(length(which(df.CODA==0))>0) {
        dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1     <- as.numeric(dl1)
        df.CODA <- lrEM(df.CODA,label=0,dl=dl1,ini.cov="multRepl")
      }
    }
    
    df1 <-  get_ilrCoord(df.CODA)
    df1
  })
  
  # Create list of CoDavars for use in automatic output
  CODAList <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])    
    
    D <- length(df.CODA)
    
    if(input$useImpute=="Yes") {
      if(length(which(df.CODA==0))>0) {
        dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1     <- as.numeric(dl1)
        df.CODA <- lrEM(df.CODA,label=0,dl=dl1,ini.cov="multRepl")
      }
    }
    
    if(D<3) return()  
    
    if(D==3) {
      z1      <- colnames(pivotCoord(df.CODA,pivotvar=1))
      z2      <- colnames(pivotCoord(df.CODA,pivotvar=2))
      z3      <- colnames(pivotCoord(df.CODA,pivotvar=3))
      
      zzList   <- list(z1,z2,z3)
      
    } else if(D==4) {
      
      z1      <- pivotCoord(df.CODA,pivotvar=1)
      z2      <- pivotCoord(df.CODA,pivotvar=2)
      z3      <- pivotCoord(df.CODA,pivotvar=3)
      z4      <- pivotCoord(df.CODA,pivotvar=4)
      
      z10 <- colnames(z1)[1]
      z11 <- (colnames(pivotCoord(df.CODA[,-1],pivotvar=1)))
      z12 <- (colnames(pivotCoord(df.CODA[,-1],pivotvar=2)))
      z13 <- (colnames(pivotCoord(df.CODA[,-1],pivotvar=3)))
      z20 <- colnames(z2)[1]
      z21 <- (colnames(pivotCoord(df.CODA[,-2],pivotvar=1)))
      z22 <- (colnames(pivotCoord(df.CODA[,-2],pivotvar=2)))
      z23 <- (colnames(pivotCoord(df.CODA[,-2],pivotvar=3)))
      z30 <- colnames(z3)[1]
      z31 <- (colnames(pivotCoord(df.CODA[,-3],pivotvar=1)))
      z32 <- (colnames(pivotCoord(df.CODA[,-3],pivotvar=2)))
      z33 <- (colnames(pivotCoord(df.CODA[,-3],pivotvar=3)))
      z40 <- colnames(z4)[1]
      z41 <- (colnames(pivotCoord(df.CODA[,-4],pivotvar=1)))
      z42 <- (colnames(pivotCoord(df.CODA[,-4],pivotvar=2)))
      z43 <- (colnames(pivotCoord(df.CODA[,-4],pivotvar=3)))
      
      zzList  <- list( c(z10,z11),
                       c(z10,z12),
                       c(z10,z13),
                       c(z20,z21),
                       c(z20,z22),
                       c(z20,z23),
                       c(z30,z31),
                       c(z30,z32),
                       c(z30,z33),
                       c(z40,z41),
                       c(z40,z42),
                       c(z40,z43)  )  
      
    } else{
      return()
    }
    
    zzList2 <- lapply(zzList, gsub,pattern="-",replacement= ".")
    zzList2
    
  })
  
  #Convert CODAList() to dataframe
  zzTableCheck <- reactive({
    if(is.null(CODAList())) return()
    
    do.call("rbind", CODAList())
    
    
  })
  
  #Display zzTableCheck
  output$zzTableCheckOut <- renderTable({
    zzTableCheck()
  })
  
  #Generate output for automatic output
  extractList <- reactive({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    outputList <- lapply(CODAList(),
                  RegressionFunction,
                  SO = SurvObj(),
                  df = Fulldat(),
                  model_cofactors = input$model_cofactors,
                  model_covars = input$model_covars)
    
    df <- do.call("rbind", outputList)
    
    df
    
  })
  
  #Generate output for automatic output
  extractList2 <- reactive({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    outputList <- lapply(CODAList(),
                         RegressionVCov,
                         SO = SurvObj(),
                         df = Fulldat(),
                         model_cofactors = input$model_cofactors,
                         model_covars = input$model_covars)
    
    df <- do.call("rbind", outputList)

    NC <- length(colnames(df))
    D  <- length(input$model_ilr_coords)
    
    colnames(df)[seq(from=NC-D+1,NC)] <- paste0("z",seq(D))
    df
    
  })
  
  #Display generated output
  output$extractListOut <- renderTable({
    extractList()
    
  })
  
  #Display generated output
  output$extractListOut2 <- renderTable({
    extractList2()
    
  },digits=4)
  
  #Calculate CoDa average of CoDa variables in Dat()
  avgCODA <- reactive({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()  
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    D <- length(df.CODA)

    if(length(which(df.CODA==0))>0) {
        if(input$useImpute=="Yes") {
          dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
          dl1     <- as.numeric(dl1)
          df.CODA <- lrEM(df.CODA,label=0,dl=dl1,ini.cov="multRepl")
        } else{
          return()
        }
    }
    
    gmall  <- apply(df.CODA,2,gm)
    gmall  <- gmall/sum(gmall)
    
    gmall  <- as.data.frame(t(gmall))
    gmall
    
  })
  
  #Display CoDa average
  output$avgCODA_Table <- renderTable({
    if(is.null(avgCODA())) return()
      
    return(avgCODA())
  })
  
  output$avgCODA_Table_Header <- renderText({
    if(is.null(avgCODA())) return()
    
    "CoDa (geometric) Average Composition"
  })
  
  VariationMatrix <-  reactive({
    if(is.null(input$file1))          return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    
    df      <- dat()[,input$model_CODAvars]
    Cmat    <- acomp(df)
    VarMat  <- variation(Cmat)
    row_nam <- as.data.frame(rownames(VarMat))
    
    V       <- data.frame(row_nam,VarMat)
    colnames(V)[1] <- "_"
    
    V
  })
    
  # Display sumary of Imputation
  output$imputeTable <- renderTable({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    D <- length(df.CODA)
    
    if(input$useImpute=="Yes") {
      if(length(which(df.CODA==0))>0) {
        dl1      <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1      <- as.numeric(dl1)
        df.Fixed <- lrEM(df.CODA,label=0,dl=dl1,ini.cov="multRepl")
        t1       <- which(!apply(df.Fixed == df.CODA,1,all))
        output   <- data.frame(df.CODA[t1,],"TO________"="",df.Fixed[t1,])
        return(output)
      } else{
        return()
      }
    } else{
      return()
    }
  })
  
  output$imputeTable_Header <- renderText({
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(input$useImpute)) return()
    if(input$useImpute=="No") return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()   
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars]) 
    
    if(length(which(df.CODA==0))>0) {
      "Individuals where zero values were imputed"
    } else {
      "No Zeroes in Data"
    }
    
  })
  
  # Calculate tertiles of ilr transform of compositional variables 
  CODAcats <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return()
    if(is.null(CODAdat())) return()
    
    df1             <- CODAdat()
    zCats           <- apply(df1, MARGIN =2, catvar)
    colnames(zCats) <- paste0(colnames(zCats),"_Cat")
    
    df <- as.data.frame(zCats)
    df
    
  })
  
  # Setup UI for selecting compositional variables input$model_ilr_coords from CODADat()
  output$choose_ilr <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(CODAdat())) return()
    
    # Get the data set with the appropriate name
    col_names2 <- colnames(CODAdat())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_ilr_coords", "Choose ilr coordinates",
                       choices = col_names2,
                       selected = NULL)
  })  
  
  # Setup UI for selecting compositional variables in null hypothesis input$model_ilr_coords_H0
  output$choose_ilr_H0 <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(CODAdat())) return()
    
    # Get the data set with the appropriate name
    col_names2 <- colnames(CODAdat())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_ilr_coords_H0", "Choose ilr coordinates",
                       choices = col_names2,
                       selected = NULL)
  }) 

  # setup combined table dat(),CODAdat()
  Fulldat <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(CODAdat())) return()
    
    data.frame(dat(),CODAdat())

  })  
  
  # fitted linear model SurvObj() ~ input$model_cofactors+input$model_covars+input$model_ilr_coords
  runFullRegression <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(input$model_ilr_coords,collapse="+") 
    
    if( (is.null(input$model_cofactors)) && (is.null(input$model_covars))) {
      coda2   <- coda1
    } else if(is.null(input$model_cofactors)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(input$model_covars)) {
      coda2   <- paste(c(cofacs2,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    SO <- SurvObj()
    df <- Fulldat()
    
    coxph(as.formula(paste("SO ~ ",coda2)),data=df)
  }) 
  
  # Setup model for H0 with no ilr coordinates SurvObj() ~ input$model_cofactors+input$model_covars
  runRegression_H0 <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(input$model_cofactors_H0) && is.null(input$model_covars_H0)) return()
    if(is.null(SurvObj())) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors_H0,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars_H0, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2), collapse="+")
    
    SO <- SurvObj()
    df <- dat()
    
    if(is.null(input$model_cofactors_H0)) {
      coxph(as.formula(paste("SO ~ ",covars1)),data=df)
    } else if(is.null(input$model_covars_H0)) {
      coxph(as.formula(paste("SO ~ ",cofacs1)),data=df)
    } else {
      coxph(as.formula(paste("SO ~ ",covars2)),data=df)
    }
    
  })
  
  # fit linear model for SurvObj() ~ input$model_cofactors+input$model_covars+input$model_ilr_coords_H0
  runFullRegression_H0 <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords_H0)) return(runRegression_H0())
    
    cofacs1 <- paste("as.factor(",input$model_cofactors_H0,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars_H0, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(input$model_ilr_coords_H0,collapse="+") 
    
    if( (is.null(input$model_cofactors_H0)) && (is.null(input$model_covars_H0))) {
      coda2   <- coda1
    } else if(is.null(input$model_cofactors_H0)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(input$model_covars_H0)) {
      coda2   <- paste(c(cofacs2,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    SO <- SurvObj()
    df <- Fulldat()
    
    coxph(as.formula(paste("SO ~ ",coda2)),data=df)
  })
  
  # Print model formula used
  output$modelText_H0 <- renderText({
    
    if(is.null(input$model_cofactors)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors_H0,")",sep="") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars_H0, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    
    paste(" ~ ",paste(covars2,collapse=" + "))
  })
  
  # Show drop1 table for selecting covariates drop1(runFullRegression(),test="Chisq")
  output$fullRegTab <- renderTable({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()

    model.drop       <- drop1(runFullRegression(),test="Chisq")
    model.drop.table <- cbind(rownames(model.drop),as.data.frame(model.drop))
    colnames(model.drop.table)[1] <- "Covariate"
    model.drop.table

  })  
  
  # Show regression coefficients coef(runFullRegression())
  output$fullRegCoef <- renderTable({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    #coef(runFullRegression())
    model.coef       <- coef(runFullRegression())
    model.coef.table <- cbind(names(model.coef),as.data.frame(model.coef))
    colnames(model.coef.table)[1] <- "Covariate"
    model.coef.table

  }) 
  
  # Calculate cox regression summary for selecting covariates: summary(runFullRegression())
  fullRegSummary <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    summary(runFullRegression())
    
  })
  
  # Display cox regression summary for selecting covariates: summary(runFullRegression())
  output$fullRegSummaryOut <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    summary(runFullRegression())
    
  })
  
  # Show cox regression summary for selecting covariates for null: summary(runFullRegression_H0())
  output$fullRegSummary_H0 <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    
    summary(runFullRegression_H0())
    
  })

  # Calculate anova comparison between H0 and H1
  fullRegComparison <- reactive({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(runFullRegression_H0())) return()
    
    anova(runFullRegression(),runFullRegression_H0(),test="Chisq")
    
  })
    
  # Display anova comparison between H0 and H1
  output$fullRegComparisonOut <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(runFullRegression_H0())) return()
    
    anova(runFullRegression(),runFullRegression_H0(),test="Chisq")
    
  })
  
  # DIAGNOSTICS
  # ===========

  # Setup UI for selecting variable to view cox.zph diagnostics on 
  output$choose_DiagVar <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    # Read file
    model               <- runFullRegression()
    model.z             <- cox.zph(model)
    
    # Get the data set with the appropriate name
    row_names <- rownames(model.z$table)
    d         <- length(row_names)
    var_names <- row_names[-d]
    
    # Create the checkboxes and select them all by default
    
    radioButtons("DiagVar","Select variable to view CoxZPH plot on",
                 choiceNames=var_names, choiceValues=seq(d-1))
    
  })  

  # Setup UI for selecting variable to view KM diagnostics on 
  output$choose_KMVar <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$model_ilr_coords)) return()

    var_names <- c(input$model_cofactors,input$model_covars,input$model_ilr_coords)
    
    # Create the checkboxes and select them all by default
    
    radioButtons("KMVar","Select variable to view KM Category/Tertile Plot for",
                 choiceNames=var_names, choiceValues=var_names)
    
  })  
  
  # Display Kaplan Meier Curves
  output$fullRegKMCurves <- renderPlot({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(runFullRegression())) return()
    if(is.null(input$KMVar)) return()
    
    df       <- Fulldat()
    KM_vals  <- df[,input$KMVar]
    
    if ( input$KMVar %in% input$model_cofactors ) {
      KM_cat   <- as.factor(KM_vals)
    } else {
      KM_cat   <- catvar(KM_vals)
    }
      
    SO       <- SurvObj()
    dfK      <- data.frame(SO,KM_cat)
    fit      <- survfit(SO ~ KM_cat, data=dfK)
    plot     <- ggsurvplot(fit, data = dfK, risk.table = TRUE)
    
    print(plot)
    
  })
  
  # Calculate Schoenfeld residuals (cox.zph)
  fullRegcoxZPH_Data <- reactive({

    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model               <- runFullRegression()
    model.z             <- cox.zph(model)
    model.z.tab         <- as.data.frame(model.z$table)
    
    model.z.table <- cbind(rownames(model.z.tab),as.data.frame(model.z.tab))
    colnames(model.z.table)[1] <- "Covariate"
    model.z.table
    
  })
  
  # Display Cox.zph table
  output$fullRegcoxZPH_Table <- renderTable({
    if(is.null(fullRegcoxZPH_Data())) return() 
    fullRegcoxZPH_Data()
  })  
  
  # Display plot of cox.zph residuals
  output$fullRegcoxZPH <- renderPlot({
  
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(runFullRegression())) return()
    if(is.null(input$DiagVar)) return()
      
    model               <- runFullRegression()
    model.z             <- cox.zph(model)
    ggcoxzph(model.z)[[input$DiagVar]]
    
  })
  
  # Calculate dfBetas
  output$fullRegDfBetas <- renderPlot({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model               <- runFullRegression()
    ggcoxdiagnostics(model, type = "dfbeta",
                     linear.predictions = FALSE)
    
  })

  #Calculate deviances
  output$fullRegDeviances <- renderPlot({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model               <- runFullRegression()
    ggcoxdiagnostics(model, type = "deviance",
                     linear.predictions = FALSE)
    
  })

  output$fullRegTab_Header <- renderText({
    "Full Regression Model - AIC and likelihood ratio tests for dropping each variable in isolation"
  })
  output$fullRegCoef_Header <- renderText({
    "Full Regression Model - regression coefficients"
  })
  output$fullRegKMCurves_Header <- renderText({
    "KM Category/Tertile Curves"
  })  
  output$fullRegcoxZPH_Table_Header <- renderText({
    "Schoenfeld Residuals"
  })  
  output$fullRegcoxZPH_Header <- renderText({
    "Schoenfeld Residuals - test proportional hazards assumption"
    })
  output$fullRegDfBetas_Header <- renderText({
    "dfBetas (examine for influential data points)"
    })
  output$fullRegDeviances_Header <- renderText({
    "Deviances (examine for influential data points)"
    })
  
  #########################
  # Produce Illustrations #
  #########################
 
  # output$illustrationCofactors  - UI for selecting cofactors for ternary
  #   - input$factor_1,2,3...
  # output$illustrationCovariates - UI for setting covariates for ternary
  #   - input$covariate_1,2,3,...
  # output$choose_extrapolate     - UI for specifying if extrapolate in ternary
  #   - input$allowExtrapolate
  # output$choose_extrapolate2    - UI for specifying if extrapolate in isotemporal
  #   - input$allowExtrapolate2
  # illCODADat2()                 - data frame of compositional variables, ilrs, ilr_cats, and fixed covariates
  # baselineVal()                 - model prediction based on avgCODA()
  # illustration2                 - model predictions based on illCODADat2()
  # output$illustrativeData       - illCODADat2() and HR output
  # output$choose_illustrationX   - UI for identifying plot variable x
  #   - input$VarX
  # output$choose_illustrationY   - UI for identifying plot variable y
  #   - input$VarY
  # output$choose_illustrationZ   - UI for identifying plot variable z
  #   - input$VarZ
  # output$illustrationComp       - UI for setting fixed compositional variables for 3d plot
  #   - input$fixedCODA_1,2,3,...
  # output$illustrationComp_header - header text
  # output$illustrativePlot       - ternary plot
  
  
  # Setup UI for selecting discrete cofactors for ternary
  output$illustrationCofactors <- renderUI({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model.cofactors        <- input$model_cofactors
    model.cofactors.seq    <- seq(length(model.cofactors))
    df                     <- data.frame(dat())
    
    model.cofactors.vals   <- data.frame(df[,model.cofactors])
    model.cofactors.vals   <- data.frame(apply(model.cofactors.vals,2,as.factor))
    model.cofactors.levels <- lapply(model.cofactors.vals,levels)
    
      # Get the data set value for variable name
      lapply(model.cofactors.seq,function(i){
         radioButtons(paste0("factor_",i), model.cofactors[i], model.cofactors.levels[[i]])
      }) 
    })
 
  # Setup UI for setting cts covariates for ternary
  output$illustrationCovariates <- renderUI({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$model_covars)) return()     
    
    model.covars           <- input$model_covars
    model.covars.seq       <- seq(length(model.covars))
    df                     <- data.frame(dat())
    
    model.covars.vals   <- data.frame(df[,model.covars])
    model.covars.mins   <- lapply(model.covars.vals,quantile,0.25)
    model.covars.maxs   <- lapply(model.covars.vals,quantile,0.75)
    model.covars.mean   <- lapply(model.covars.vals,mean)
    
    # Get the data set value for variable name
    lapply(model.covars.seq,function(i){
      numericInput(paste0("covariate_",i), model.covars[i], min=model.covars.mins[[i]],
                   max=model.covars.maxs[[i]], val=model.covars.mean[[i]])
    }) 
  })

  # Setup UI for specifying whether to extrapolate beyond data in ternary
  output$choose_extrapolate <- renderUI({ 
    
    # UI prompts whether to restrict graph to supported region of simplex
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()   
    
    col_names <- c("Yes","No")
    
    radioButtons("allowExtrapolate","Allow illustration to extrapolate beyond data region?",choices=col_names,
                 selected = col_names[2])
    
  })

  # Setup UI for specifying whether to extrapolate beyond data in isotemporal
  output$choose_extrapolate2 <- renderUI({ 
    
    # UI prompts whether to restrict graph to supported region of simplex in isotemporal
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()   
    
    col_names <- c("Yes","No")
    
    radioButtons("allowExtrapolate2","Allow illustration to extrapolate beyond data region?",choices=col_names,
                 selected = col_names[2])
    
  })
  
  # ilr transform of compositional variables 
  illCODADat2 <- reactive({
    
    ###################
    # Data Validation #
    ###################
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(input$model_CODAvars)) return()  
    if(is.null(input$VarX)) return()
    if(is.null(input$VarY)) return()
    if(is.null(input$VarZ)) return()
    if( (is.null(input$fixedCODA_1)) && length(input$model_CODAvars) >3 ) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
    
    not_loaded_cofactors  <- (!is.null(input$model_cofactors)) && (is.null(input$factor_1))
    not_loaded_covariates <- (!is.null(input$model_covars)) && (is.null(input$covariate_1))
    
    if(not_loaded_cofactors)  return()
    if(not_loaded_covariates) return()
    
    ###########################

    col_names_all <- input$model_CODAvars    
    D             <- length(col_names_all)
    
    if(D<3) return()  # Ternary plot requires 3+ CoDa variables 
    
    xnam  <- input$VarX
    ynam  <- input$VarY
    znam  <- input$VarZ
    
    col_names_xyz          <- col_names_all[col_names_all %in% c(xnam,ynam,znam)]
    if(D>3) col_names_off  <- col_names_all[!(col_names_all %in% c(xnam,ynam,znam))]    
    
    # Load data, restrict to CODA vars, impute zero values, convert to propns.
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    if(input$useImpute=="Yes") {
      if(length(which(df.CODA==0))>0) {
        dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
        dl1     <- as.numeric(dl1)
        df.CODA <- lrEM(df.CODA,label=0,dl=dl1,ini.cov="multRepl")
      }
    }   
    prop.CODA <- df.CODA / rowSums(df.CODA)
    
    df.xyz  <- data.frame(prop.CODA[,col_names_xyz])
    
    if(input$allowExtrapolate=="No"){
      grid1 <- generateCODASims(df.xyz)
    } else {
      grid1 <- generateCODASims2(df.xyz)
    }
    
    colnames(grid1) <- col_names_xyz
    
    if(D>3) {
      cvc       <- "input$fixedCODA_"
      cvc_seq   <- seq((D-3))
      fixcomp1  <- lapply(cvc_seq, function(i) {eval(parse(text=paste0(cvc,i)))})    
      grid2     <- data.frame(do.call("cbind", fixcomp1))
      colnames(grid2) <- col_names_off
      grid1 <- (1-rowSums(grid2))*grid1
      gridX <- data.frame(grid1,grid2)
    } else {
      gridX <- grid1
    }
    
    gridX <- data.frame(gridX[,input$model_CODAvars])
    df1 <-  get_ilrCoord(gridX)
    
    # Covariates
    
    cvt <- "input$covariate_"
    cvf <- "input$factor_"
    
    cvt_seq <- seq(length(input$model_covars))
    cvf_seq <- seq(length(input$model_cofactors))
    
    covars1 <- lapply(cvt_seq, function(i) {eval(parse(text=paste0(cvt,i)))})    
    cofacs1 <- lapply(cvf_seq, function(i) {eval(parse(text=paste0(cvf,i)))}) 
    
    df_cov           <- data.frame(covars1,cofacs1)
    colnames(df_cov) <- c(input$model_covars,input$model_cofactors) 
    
    dfOut <- as.data.frame(cbind(gridX,df1,df_cov))
    dfOut
    
  })
  
  # ilr transform of compositional variables 
  baselineVal <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(CODAdat())) return() 
    if(is.null(illCODADat2())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$model_CODAvars)) return()
    
    col_names_all <- input$model_CODAvars    
    D             <- length(col_names_all)
    model         <- runFullRegression()               # point at model

    if(D<3) return()  # Ternary plot requires 3+ CoDa variables 
    
    xnam  <- input$VarX
    ynam  <- input$VarY
    znam  <- input$VarZ
    
    col_names_xyz          <- col_names_all[col_names_all %in% c(xnam,ynam,znam)]
    if(D>3) col_names_off  <- col_names_all[!(col_names_all %in% c(xnam,ynam,znam))]
    
    data                           <- illCODADat2()[1:2,]
    data[,input$model_CODAvars]    <- avgCODA()
    grid1                          <- data[,col_names_xyz]/ rowSums(data[,col_names_xyz])
    
    if(D>3) {
      cvc       <- "input$fixedCODA_"
      cvc_seq   <- seq((D-3))
      fixcomp1  <- lapply(cvc_seq, function(i) {eval(parse(text=paste0(cvc,i)))})    
      grid2     <- data.frame(do.call("cbind", fixcomp1))
      colnames(grid2) <- col_names_off
      grid1 <- (1-rowSums(grid2))*grid1
      gridX <- data.frame(grid1,grid2)
    } else {
      gridX <- grid1
    }
    
    gridX <- data.frame(gridX[,input$model_CODAvars])
    df1 <-  get_ilrCoord(gridX)

    # Covariates
    
    cvt <- "input$covariate_"
    cvf <- "input$factor_"
    
    cvt_seq <- seq(length(input$model_covars))
    cvf_seq <- seq(length(input$model_cofactors))
    
    covars1 <- lapply(cvt_seq, function(i) {eval(parse(text=paste0(cvt,i)))})    
    cofacs1 <- lapply(cvf_seq, function(i) {eval(parse(text=paste0(cvf,i)))}) 
    
    df_cov           <- data.frame(covars1,cofacs1)
    colnames(df_cov) <- c(input$model_covars,input$model_cofactors) 
    
    dfOut <- as.data.frame(cbind(gridX,df1,df_cov))
    data <- dfOut[1,]
    
    Prediction <- exp(predict(model,data))
    Prediction
    
  })
   
  # ilr transform of compositional variables 
  illustration2 <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model <- runFullRegression()
    data  <- illCODADat2()
    
    Prediction <- exp(predict(model,data))
    Prediction
    
  })
  
  # Generate data frame containing predicted value and illustrative data
  output$illustrativeData <- renderTable({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(illCODADat2())) return()
    if(is.null(illustration2())) return()
    if(is.null(baselineVal())) return()
    
    df1     <- illCODADat2()
    HR      <- illustration2()/baselineVal()
    df      <- as.data.frame(cbind(HR,df1))
    df
  })  
  
  # Setup UI for identifying plot variable x
  output$choose_illustrationX <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()   
    if(is.null(input$model_CODAvars)) return()  
    
    # Get the data set with the appropriate name
    col_names <- input$model_CODAvars
    
    # Create the checkboxes and select them all by default
    
    radioButtons("VarX","Choose variable x",choices=col_names,selected = col_names[1])
    
  })  
  
  # Setup UI for identifying plot variable y
  output$choose_illustrationY <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()    
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$VarX)) return() 
    
    # Get the data set with the appropriate name
    col_names1 <- input$model_CODAvars
    col_names  <- col_names1[!col_names1 %in% input$VarX]

    # Create the checkboxes and select them all by default
    
    radioButtons("VarY","Choose variable y",choices=col_names,selected = col_names[1])
    
  })  

  # Setup UI for identifying plot variable z
  output$choose_illustrationZ <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()    
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    
    # Get the data set with the appropriate name
    col_names1 <- input$model_CODAvars
    col_names  <- col_names1[!col_names1 %in% c(input$VarX,input$VarY)]
    
    # Create the checkboxes and select them all by default
    
    radioButtons("VarZ","Choose variable z",choices=col_names,selected = col_names[1])
    
  })   
  
  # Setup UI for setting fixed compositional variables for 3d plot
  output$illustrationComp <- renderUI({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    if(is.null(input$VarZ)) return() 

    col_names_all <- input$model_CODAvars
    D             <- length(col_names_all)    

    if(D<4) return() 

    xnam  <- input$VarX
    ynam  <- input$VarY
    znam  <- input$VarZ
    
    col_names  <- col_names_all[!col_names_all %in% c(xnam,ynam,znam)]
    
    df1       <- dat()
    df        <- data.frame(df1[,input$model_CODAvars])
    df        <- df / rowSums(df)
    offvars   <- df[,col_names]
    
    model.covars        <- col_names
    model.covars.seq    <- seq(length(col_names))

    model.covars.vals   <- offvars
    model.covars.mins   <- lapply(model.covars.vals,quantile,0.025)   # lapply to offvars
    model.covars.maxs   <- lapply(model.covars.vals,quantile,0.975)   # lapply to offvars
    model.covars.mean   <- lapply(model.covars.vals,mean)             # lapply to offvars
    
    # Get the data set value for variable name
    lapply(model.covars.seq,function(i){
      numericInput(paste0("fixedCODA_",i), model.covars[i], min=model.covars.mins[[i]],
                   max=model.covars.maxs[[i]], val=model.covars.mean[[i]])
    }) 
  })
  
  # Setup UI for selecting fixed compositional variables for 3d plot
  output$illustrationComp_header <- renderText({ 
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    if(is.null(input$VarZ)) return() 
    
    D <- length(input$model_CODAvars)
    
    if(D<4) return()
    
    "Define remaining components (as proportion of total time)"
    
  })
  
  
  
  output$illustrativePlot <- renderPlot({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    if(is.null(illCODADat2())) return() 
    if(is.null(illustration2())) return() 
    if(is.null(baselineVal())) return()
    if(is.null(avgCODA())) return()
    if(is.null(input$VarX)) return() 
    if(is.null(input$VarY)) return() 
    if(is.null(input$VarZ)) return() 
    
    
    df <- illCODADat2()
    
    x  <- df[,input$VarX] 
    y  <- df[,input$VarY]
    z  <- df[,input$VarZ]
    EE <- illustration2()/baselineVal()
    
    baseloc <- avgCODA()
    baseX <- baseloc[,input$VarX]
    baseY <- baseloc[,input$VarY]
    baseZ <- baseloc[,input$VarZ]
    
    limitssol <- ternLimits(data.frame(x,y,z))
    
    predict.data.graph      <- data.frame(cbind(x,y,z,EE))
    baseline.data.graph     <- predict.data.graph[1,]
    baseline.data.graph[1,1] = baseX
    baseline.data.graph[1,2] = baseY
    baseline.data.graph[1,3] = baseZ
    baseline.data.graph[1,4] = baselineVal()
    
    plot1<-ggtern(data=predict.data.graph,aes(x=x,y=y,z=z,value=EE))+
      tern_limit(T=limitssol[2],L=limitssol[1],R=limitssol[3]) +
      geom_point(aes(colour=EE),size=2) +
      scale_colour_gradient2(midpoint=1,low="green",mid="white",high="red")+
      geom_point(data=baseline.data.graph,col="blue",size=2) +
      theme_clockwise() +
      labs( title = 'Hazard Ratio',
            fill  = 'Hazard Ratio',
            color = 'Hazard Ratio')+ 
      labs(x = input$VarX,y=input$VarY,z=input$VarZ)
    
    print(plot1)
    NULL
    
  })
  
  #####################################################################
  isoArray1 <- function(xid,xmin,Cref,data,lenx=200){
    
    # xid  = identifier for x-coordinate
    # xmin = minimum value allowed for any component
    # Cref = reference / baseline composition
    # lenx = mesh size
    # data = raw compositional data
    
    D      <- length(Cref)
    xR     <- Cref[,-which(names(Cref) == xid)] 
    xList  <- list()
    
    zdat   <- (1/sqrt(2))*log( (data[,xid]) * (1/data[,-which(names(Cref) == xid)]))
    mu     <- apply(zdat,2,mean)
    sigma  <- apply(zdat,2,sd)
    
    zmin   <- mu - sigma * qnorm(0.995) 
    zmax   <- mu + sigma * qnorm(0.995)
    
    cat(zmin)
    cat(zmax)
    
    for (i in 1:(D-1)) {
      xnam          <- colnames(xR)[i]
      x1max         <- 1 - xmin - sum(xR) + xR[,i]
      x1            <- data.frame(x1=seq(from=xmin,to=x1max,length.out=lenx))
      xx            <- data.frame(x1,xR[,-i])
      x2            <- 1-rowSums(xx)
      xxx           <- data.frame(xx,x2,xnam)
      
      zilr          <- (1/sqrt(2))*log(x1/x2)
      zbig          <- which(zilr>zmax[i])
      #zbig          <- 1
      zsmall        <- which(zilr<zmin[i])
      #zsmall        <- 2
      xxx           <- xxx[-c(zbig,zsmall),]    
      
      colnames(xxx) <- c(xid,colnames(xR)[-i],xnam,"xnam")
      xList[[i]]    <- xxx 
    }
    
    xdf <- do.call("rbind", xList)
    
    return(xdf)
    
  }
  
  isoArray2 <- function(xid,xmin,Cref,lenx=200){
    
    # xid  = identifier for x-coordinate
    # xmin = minimum value allowed for any component
    # Cref = reference / baseline composition
    # lenx = mesh size
    
    D      <- length(Cref)
    xR     <- Cref[,-which(names(Cref) == xid)] 
    xList  <- list()
    
    for (i in 1:(D-1)) {
      xnam          <- colnames(xR)[i]
      x1max         <- 1 - xmin - sum(xR) + xR[,i]
      x1            <- data.frame(x1=seq(from=xmin,to=x1max,length.out=lenx))
      xx            <- data.frame(x1,xR[,-i])
      x2            <- 1-rowSums(xx)
      xxx           <- data.frame(xx,x2,xnam)
      colnames(xxx) <- c(xid,colnames(xR)[-i],xnam,"xnam")
      xList[[i]]    <- xxx 
    }
    
    xdf <- do.call("rbind", xList)
    
    return(xdf)
    
  }
  
  # ilr transform of compositional variables 
  baselineVal2 <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(CODAdat())) return() 
    if(is.null(isoCODADat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$model_CODAvars)) return()
    
    model                         <- runFullRegression()
    data                          <- isoCODADat()[1,]
    data[,input$model_CODAvars]   <- refCODADat()
    
    gridX <- data[c(1,1),input$model_CODAvars]
    
    D <- length(input$model_CODAvars)
    
    df1 <-  get_ilrCoord(gridX)
    
    data[,input$model_ilr_coords] <- df1[1,input$model_ilr_coords]
    
    Prediction <- exp(predict(model,data))
    Prediction
    
  })
  
  output$illustrativeISOPlot <- renderPlot({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$closureTot)) return()
    if(is.null(input$closureTotUnits)) return()    
    
    if(is.null(isoCODADat())) return() 
    if(is.null(refCODADat())) return() 
    if(is.null(illustrationISO())) return() 
    if(is.null(illIntervalISO())) return()
    if(is.null(baselineVal2())) return() 
    if(is.null(input$iVarX)) return() 
    if(is.null(input$maxYDisplay)) return() 

    df    <- isoCODADat()
    Cref  <- refCODADat()
    
    x        <- df[,input$iVarX] * input$closureTot
    y        <- illustrationISO()/baselineVal2()
    ylow     <- illIntervalISO()[,1]/baselineVal2()
    ycheck   <- illIntervalISO()[,2]/baselineVal2()
    yup      <- illIntervalISO()[,3]/baselineVal2()
    
    cat      <- as.factor(df[,"xnam"])
    xlabel   <- paste0(input$iVarX," (",input$closureTotUnits,")")
    catlabel <- unique(cat)
    
    plot_data <- data.frame(x1=x,y1=y,cat1=as.factor(cat),ylow=ylow,yup=yup)
    
    plot3   <-ggplot(plot_data,aes(x1,y1,colour=cat1)) +
#    geom_ribbon(aes(ymin = ylow, ymax = yup, fill=cat1))+
      geom_errorbar(aes(ymin = ylow, ymax = yup), width=.2,
                    position=position_dodge(0.05))+
      geom_line(size=2) + theme_bw()+
      labs(x=xlabel,y="Hazard Ratio") + 
      scale_x_continuous(limits=c(input$minXDisplay,input$maxXDisplay),
                         breaks=seq(from=input$minXDisplay,to=input$maxXDisplay,
                                    length.out=(input$maxXDisplay-input$minXDisplay+1)))+
      scale_y_continuous(limits=c(0,input$maxYDisplay))+
      scale_colour_discrete(name="Replaced\nBehavior",labels=catlabel)
    #print(plot3 + theme1)
    print(plot3)
    
#    plot3   <- ggplot(plot_data,aes(x=x1,y=y1,colour=cat1))+geom_line(size=2)+theme_bw()
#    plot3   <- ggplot(plot_data,aes(x=x1,y=y1))+geom_line(size=2)+theme_bw()
#    print(plot3)
    
    #interaction.plot(plot_data$x1,plot_data$cat1,plot_data$y1)
    
  })

  # ilr transform of compositional variables 
  illustrationISO <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(runFullRegression())) return()
    if(is.null(isoCODADat())) return()
    if(is.null(input$model_ilr_coords)) return()
    
    model <- runFullRegression()   # Retain
    data  <- isoCODADat()          # Revise to list
    
    # Cycle through list
    
    Prediction <- exp(predict(model,data))   #Retain
    Prediction                               #Retain
    
  })
  
  # ilr transform of compositional variables 
  illIntervalISO <- reactive({  
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(refCODADat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    model   <- runFullRegression()   # Retain
    data    <- isoCODADat()          # Revise to list
    ilrvars <- input$model_ilr_coords
    
    # Cycle through list
    
    Prediction <- predict(model,data)  #Retain
    Prediction                         #Retain    
    
    dataB  <- refCODADat()[c(1,1),]
    ilrB   <- get_ilrCoord(dataB)[1,]
    delta  <- data[,ilrvars]-ilrB[rep(1,dim(data)[1]),ilrvars]
    
    errors <- numeric(length=dim(delta)[1])
    for (i in 1:(dim(delta)[1])) {
      errors[i] = sqrt((as.matrix(delta[i,])) %*% 
                   vcov(model)[ilrvars,ilrvars] %*% 
                   t(as.matrix(delta[i,])))*1.96
    }
    
    exp(cbind(Prediction - errors, Prediction, Prediction + errors))
    
  })

  # Setup UI for identifying plot variable x
  output$choose_isotemporalX <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()   
    if(is.null(input$model_CODAvars)) return()  
    
    # Get the data set with the appropriate name
    col_names <- input$model_CODAvars
    
    # Create the checkboxes and select them all by default
    
    radioButtons("iVarX","Choose variable x",choices=col_names,selected = col_names[1])
    
  })
  
  #UI to set detection limits for imputation
  output$choose_isoLimits <- renderUI({ 
    
    numericInput("isoLimit", 
                 label="Minimum compositional component", 
                 value=0.001)
    
  })
  
  #UI to set closure total
  output$choose_closeTotal <- renderUI({ 
    
    numericInput("closureTot", 
                 label="Total time", 
                 value=24)
    
  })
  
  #UI to set closure total units
  output$choose_closeTotalUnits <- renderUI({ 
    
    textInput("closureTotUnits", 
                 label="Total time units", 
                 value="hours")
    
  })  
  
  #UI to set closure total units
  output$choose_minXDisplay <- renderUI({ 
    
    numericInput("minXDisplay", 
                 label="Minimum x to display", 
                 value=0)
    
  })  
  
  #UI to set closure total units
  output$choose_maxXDisplay <- renderUI({ 
    
    numericInput("maxXDisplay", 
                 label="Maximum x to display", 
                 value=24)
    
  })  
  
  #UI to set closure total units
  output$choose_maxYDisplay <- renderUI({ 
    if(is.null(illustrationISO())) return() 
    if(is.null(baselineVal2())) return() 
    
    y        <- illustrationISO()/baselineVal2()
    maxy     <- max(y)
    
    numericInput("maxYDisplay", 
                 label="Maximum HR to display", 
                 value=maxy)
    
  })
  
  #UI to set detection limits for imputation
  output$set_RefPoint <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()  
    if(is.null(input$model_CODAvars)) return()      

    D  <- length(input$model_CODAvars)
    
    lapply(seq(D),function(i){
      numericInput(paste0("RefPoint_",i), 
                   input$model_CODAvars[i], 
                   val=avgCODA()[i])
    })
  })
  
  output$set_RefPoint_header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Reference point for Hazard ratio calculation"
  })
  
  output$RefPoint_header <- renderText({
    if(is.null(input$RefPoint_1)) return()
    "Reference point for Hazard ratio calculation"
  })
  
  output$illustrativeISOPlot_Data_header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$closureTot)) return()
    if(is.null(input$closureTotUnits)) return()
    
    if(is.null(isoCODADat())) return() 
    if(is.null(refCODADat())) return() 
    if(is.null(illustrationISO())) return() 
    if(is.null(baselineVal2())) return() 
    if(is.null(input$iVarX)) return() 
    
    "Dataframe underlying Hazard ratio plot"
  })
  
  
  
  output$choose_close_header <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Which compositional units should the graph use?"
  })
 
  output$choose_graph_limits <- renderText({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return()
    "Specify x and y range of plot"
  })
  
  refCODADat <- reactive({
    if(is.null(input$RefPoint_1)) return()
    if(is.null(input$model_CODAvars)) return() 
    
    df <- avgCODA()
    D  <- length(input$model_CODAvars)
    
    for (i in seq(D)) {
      df[,i] = eval(parse(text=paste0("input$RefPoint_",i)))
    }
    
    colnames(df) <- input$model_CODAvars
    
    df
    
  })
  
  output$test_refCODADat <- renderTable({
    refCODADat() * input$closureTot
  })
  
  output$test_isoCODADat <- renderTable({
    isoCODADat()
  })

  output$illustrativeISOPlot_Data <- renderTable({
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(input$closureTot)) return()
    if(is.null(input$closureTotUnits)) return()
    
    if(is.null(isoCODADat())) return() 
    if(is.null(refCODADat())) return() 
    if(is.null(illustrationISO())) return() 
    if(is.null(illIntervalISO())) return()     
    if(is.null(baselineVal2())) return() 
    if(is.null(input$iVarX)) return() 
    
    df    <- isoCODADat()
    Cref  <- refCODADat()
    
    x        <- df[,input$iVarX] * input$closureTot
    y        <- illustrationISO()/baselineVal2()
    ylow     <- illIntervalISO()[,1]/baselineVal2()
    ycheck   <- illIntervalISO()[,2]/baselineVal2()
    yup      <- illIntervalISO()[,3]/baselineVal2()
    cat      <- df[,"xnam"]
    xlabel   <- paste0(input$iVarX," (",input$closureTotUnits,")")
    catlabel <- colnames(Cref[,-which(names(Cref) == input$iVarX)])
    
    plot_data <- data.frame(Behavior=x,HR=ycheck,HR_lower=ylow,HR_upper=yup,Replace=cat)
    plot_data
  
  },digits=4)
  
  
  # ilr transform of compositional variables 
  output$testCODADat <- renderTable({
    
    ###################
    # Data Validation #
    ###################
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(input$model_CODAvars)) return()  
    if(is.null(input$iVarX)) return()
    if(is.null(input$isoLimit)) return()
    if(is.null(refCODADat())) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
    
    not_loaded_cofactors  <- (!is.null(input$model_cofactors)) && (is.null(input$factor_1))
    not_loaded_covariates <- (!is.null(input$model_covars)) && (is.null(input$covariate_1))
    
    if(not_loaded_cofactors)  return()
    if(not_loaded_covariates) return()
    
    ###########################
    
    xid  <- input$iVarX
    xmin <- input$isoLimit
    Cref <- refCODADat()
    
    
    
    if(input$allowExtrapolate2=="No"){
      data <- dat()[,input$model_CODAvars] 
      grid <- isoArray1(xid,xmin,Cref,data)
    } else{
      grid  <- isoArray2(xid,xmin,Cref)
    }
    
    grid
    
  })

  # ilr transform of compositional variables 
  isoCODADat <- reactive({
    
    ###################
    # Data Validation #
    ###################
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(Fulldat())) return() 
    if(is.null(input$model_ilr_coords)) return()  
    if(is.null(input$model_CODAvars)) return()  
    if(is.null(input$iVarX)) return()
    if(is.null(input$isoLimit)) return()
    if(is.null(refCODADat())) return()
    if( (input$useImpute=="Yes") && (is.null(input$detectLimit_1))) return()
    
    not_loaded_cofactors  <- (!is.null(input$model_cofactors)) && (is.null(input$factor_1))
    not_loaded_covariates <- (!is.null(input$model_covars)) && (is.null(input$covariate_1))
    
    if(not_loaded_cofactors)  return()
    if(not_loaded_covariates) return()
    
    ###########################
    
    xid  <- input$iVarX
    xmin <- input$isoLimit
    Cref <- refCODADat()
    
    if(input$allowExtrapolate2=="No"){
       data <- dat()[,input$model_CODAvars]
       D <- length(input$model_CODAvars)
       if(input$useImpute=="Yes") {
         if(length(which(data==0))>0) {
           dl1     <- lapply(seq(D), function(i) {eval(parse(text=paste0("input$detectLimit_",i)))}) 
           dl1     <- as.numeric(dl1)
           data <- lrEM(data,label=0,dl=dl1,ini.cov="multRepl")
         }
       }
       grid <- isoArray1(xid,xmin,Cref,data)
    } else{
       grid <- isoArray2(xid,xmin,Cref)
    }
    
    gridX <- grid[,-dim(grid)[2]] 
    gridX <- gridX[,input$model_CODAvars]
    
    D <- length(input$model_CODAvars)
    
    df1 <-  get_ilrCoord(gridX)
    
    # Covariates
    
    cvt <- "input$covariate_"
    cvf <- "input$factor_"
    
    cvt_seq <- seq(length(input$model_covars))
    cvf_seq <- seq(length(input$model_cofactors))
    
    covars1 <- lapply(cvt_seq, function(i) {eval(parse(text=paste0(cvt,i)))})    
    cofacs1 <- lapply(cvf_seq, function(i) {eval(parse(text=paste0(cvf,i)))}) 
    
    df_cov           <- data.frame(covars1,cofacs1)
    colnames(df_cov) <- c(input$model_covars,input$model_cofactors) 
    
    dfOut <- as.data.frame(cbind(grid,df1,df_cov))
    dfOut
    
  })

  ####################################################
    
  generateCODASims <- function(data) {
    
    ilr_data <- pivotCoord(data,1)
    mu       <- apply(ilr_data,2,mean)
    Sigma    <- cov(ilr_data)
    ilr_D    <- mahalanobis(ilr_data, center=mu, cov=Sigma)
    
    q995      <- qchisq(df=1,0.995)
    ilr_cut  <- ilr_data[-(which(ilr_D>q995)),]
    
    Xpca     <- prcomp(ilr_cut)
    
    min1 <- min(Xpca$x[,1])
    min2 <- min(Xpca$x[,2])
    max1 <- max(Xpca$x[,1])
    max2 <- max(Xpca$x[,2])
    
    seq1 <- seq(from=min1,to=max1,length.out=50)
    seq2 <- seq(from=min2,to=max2,length.out=50)
    grid <- expand.grid(seq1,seq2)
    
    Sigma_PCA  <- cov(Xpca$x)
    grid_MD2   <- (mahalanobis(grid, center=c(0,0), cov=Sigma_PCA))^2
    grid_cut   <- grid[-(which(grid_MD2>q995)),]
    
    Xhat <- as.matrix(grid_cut) %*% t(Xpca$rotation)
    Xhat <- scale(Xhat, center = -mu, scale = FALSE)
    
    behaviors <- pivotCoordInv(Xhat)
    behaviors <- data.frame(behaviors)
    colnames(behaviors) <- colnames(data)
    
    return(behaviors)
    
  }
  
  generateCODASims2 <- function(data) {
    
    ilr1 <- seq(from=-3,to=3,length.out=101)
    ilr2 <- seq(from=-3,to=3,length.out=101)
    Xhat <- expand.grid(ilr1,ilr2)

    behaviors <- pivotCoordInv(Xhat)
    behaviors <- data.frame(behaviors)
    colnames(behaviors) <- colnames(data)
    
    return(behaviors)
    
  }
  
  ternLimits <- function(df) {
    
    a     <- ceiling(max(df[,1])*100)/100
    b     <- ceiling(max(df[,2])*100)/100
    c     <- ceiling(max(df[,3])*100)/100
    d     <- floor(min(df[,1])*100)/100
    e     <- floor(min(df[,2])*100)/100
    f     <- floor(min(df[,3])*100)/100
    
    # Setup problem: minimize
    # tmax + lmax + rmax - tmin - lmin - rmin  subj. to
    #
    # tmax <= 1
    # lmax <= 1
    # rmax <= 1
    # tmin >= 0
    # lmin >= 0
    # rmin >= 0
    # tmax >= a
    # lmax >= b
    # rmax >= c
    # tmin <= d
    # lmin <= e
    # rmin <= f
    # tmax + lmin + rmin = 1
    # tmin + lmax + rmin = 1
    # tmin + lmin + rmax = 1
    f.obj <- c(1, 1, 1,-1,-1,-1)
    f.con <- matrix (c(1, 0, 0, 0, 0, 0,
                       0, 1, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0,
                       0, 0, 0, 1, 0, 0,
                       0, 0, 0, 0, 1, 0,
                       0, 0, 0, 0, 0, 1,
                       1, 0, 0, 0, 0, 0,
                       0, 1, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0,
                       0, 0, 0, 1, 0, 0,
                       0, 0, 0, 0, 1, 0,
                       0, 0, 0, 0, 0, 1,
                       1, 0, 0, 0, 1, 1,
                       0, 1, 0, 1, 0, 1,
                       0, 0, 1, 1, 1, 0), nrow=15, byrow=TRUE)
    f.dir <- c("<=", "<=", "<=", 
               ">=", ">=", ">=",
               ">=", ">=", ">=",
               "<=", "<=", "<=",
               "==", "==", "==")
    f.rhs     <- c(1, 1, 1, 0, 0, 0, a, b, c, d, e, f, 1, 1, 1)
    lp_test   <- lp ("min", f.obj, f.con, f.dir, f.rhs)
    limitssol <- lp_test$solution
    
    return(limitssol)
  }
  
  #calculate geometric average
  gm <- function(x) {
    exp(mean(log(x)))
  }
  
  RegressionFunction <- function(SO,df,model_cofactors,model_covars,model_ilr_coords) {
    # Inputs
    # ======
    # model_cofactors  : input$model_cofactors
    # model_covars     : input$model_covars
    # model_ilr_coords : input$model_ilr_coords
    # SO               : SurvObj()
    # df               : Fulldat()
    
    if(is.null(SO))               return() 
    if(is.null(df))               return() 
    if(is.null(model_cofactors))  return()
    if(is.null(model_covars))     return()
    if(is.null(model_ilr_coords)) return()
    
    cofacs1 <- paste("as.factor(",model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(model_ilr_coords,collapse="+") 
    
    if( (is.null(model_cofactors)) && (is.null(model_covars))) {
      coda2   <- coda1
    } else if(is.null(model_cofactors)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(model_covars)) {
      coda2   <- paste(c(cofacs2,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    SO <- SurvObj()
    df <- Fulldat()
    
    model <- coxph(as.formula(paste("SO ~ ",coda2)),data=df)
    
    cox_coef <- summary(model)$coefficients
    cox_vars <- rownames(cox_coef)
    cox_conf <- summary(model)$conf.int[,c(3,4)] 
    
    output <- cbind(cox_vars,cox_coef,cox_conf)
    colnames(output)[1] <- "Variable"
    
    return(output)
    
  }
  
  RegressionVCov <- function(SO,df,model_cofactors,model_covars,model_ilr_coords) {
    # Inputs
    # ======
    # model_cofactors  : input$model_cofactors
    # model_covars     : input$model_covars
    # model_ilr_coords : input$model_ilr_coords
    # SO               : SurvObj()
    # df               : Fulldat()
    
    if(is.null(SO))               return() 
    if(is.null(df))               return() 
    if(is.null(model_cofactors))  return()
    if(is.null(model_covars))     return()
    if(is.null(model_ilr_coords)) return()
    
    cofacs1 <- paste("as.factor(",model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste(model_ilr_coords,collapse="+") 
    
    if( (is.null(model_cofactors)) && (is.null(model_covars))) {
      coda2   <- coda1
    } else if(is.null(model_cofactors)) {
      coda2   <- paste(c(covars1,coda1),collapse="+")
    } else if(is.null(model_covars)) {
      coda2   <- paste(c(cofacs2,coda1),collapse="+")
    } else {
      coda2   <- paste(c(covars2,coda1),collapse="+")
    }
    
    SO <- SurvObj()
    df <- Fulldat()
    
    model <- coxph(as.formula(paste("SO ~ ",coda2)),data=df)
    
    cox_covars <- vcov(model)
    cox_nams <- rownames(cox_covars)
    
    output <- cbind(cox_nams,cox_covars)
    colnames(output)[1] <- "Variable"
    
    return(output)
    
  }
 
  params1 <- reactive ({
    
      list(n              = runFullRegression()$n,
           dataSummary    = confounderSummary(),
           avgCODA        = avgCODA(),
           varCODA        = VariationMatrix(),
           schoenfeldTest = fullRegcoxZPH_Data(),
           regSummary1    = fullRegSummary(),
           hypTest1       = fullRegComparison(),
           zList1         = zzTableCheck(),
           regTable1      = extractList(),
           regTable2      = extractList2()
      )
  }) 
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- params1()
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
    
# End server  
}

