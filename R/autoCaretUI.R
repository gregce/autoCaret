##' autoCaret Add-in
##'
##' This function launches a shiny app in a web browser in order to
##' run the autoCaret UI
##'
##' @return
##' The function launches a shiny app in the system web browser.
##' @import shiny
##' @import rstudioapi
##' @import miniUI
##' @importFrom highr hi_html
##' @export

autoCaretUI <- function(obj = NULL, var_name = NULL) {
  
  run_as_addin <- ifunc_run_as_addin()
  
  ######### leftover from iorder from questionr package. ###########
  if (is.null(obj)) {
    if (ifunc_run_as_addin()) {
      context <- rstudioapi::getActiveDocumentContext()
      obj <- context$selection[[1]]$text
      if (obj == "") obj <- NULL
    }
    obj_name <- NULL
    var_name <- NULL
  }
  if (!is.null(obj)) {
    ## If first arg is a string
    if (is.character(obj) && length(obj) == 1) {
      obj_name <- obj
      try({
        obj <- get(obj_name, envir = sys.parent())
      }, silent = TRUE)
    }
    else {
      obj_name <- deparse(substitute(obj))
    }
    ## If first arg is of the form d$x
    if (grepl("\\$", obj_name)) {
      s <- strsplit(obj_name, "\\$")
      obj_name <- gsub("^\\s*", "", s[[1]][1])
      var_name <- gsub("\\s*$", "", s[[1]][2])
      var_name <- gsub("`", "", var_name)
      obj <- get(obj_name, envir = sys.parent())
    }
    if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
    
    ## Check if obj is a data frame or a vector
    if (!is.data.frame(obj) && !is.vector(obj) && !is.factor(obj)) {
      stop(sQuote(paste0(obj_name, ' must be a vector, a factor or a data frame.')))
    }
    
    ## If obj is a data.frame
    if (is.data.frame(obj)) {
      ## If var_name is not a character string, deparse it
      is_char <- FALSE
      is_null <- FALSE
      try({
        if (is.character(var_name)) is_char <- TRUE
        if (is.null(var_name)) is_null <- TRUE
      }, silent = TRUE)
      if (!is_char && !is_null) {
        var_name <- deparse(substitute(var_name))
      }
      ## Check if var_name is a column of robject
      if (!is.null(var_name) && !(var_name %in% names(obj))) {
        stop(sQuote(paste0(var_name, ' must be a column of ', obj_name, '.')))
      }
    }
  }
  ############################################################################
  
  ## Gadget UI
  ui <- miniUI::miniPage(
    ## Page title
    miniUI::gadgetTitleBar(gettext("autoCaret", domain="R-autoCaret")),
    
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        gettext("Setup", domain="R-autoCaret"), icon = shiny::icon("sliders"),
        miniUI::miniContentPanel(
          
          # ifunc_show_alert(run_as_addin),
          shiny::tags$h2(shiny::icon("columns"), gettext("Select Data from R Environment", domain="R-autoCaret")),
          shiny::wellPanel(
            shiny::fluidRow(shiny::column(6, shiny::uiOutput("dfInput")),
                            shiny::column(6, shiny::uiOutput("varInput")))
          ),
          ## First panel : new variable name
          shiny::tags$h4(gettext("or", domain="R-questionr"),align="center"),
          
          shiny::tags$h2(shiny::icon("columns"), gettext("Upload New Dataset", domain="R-autoCaret")),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(6,
                            shiny::fileInput('Load_Data', shiny::h5('Upload *.csv or *.txt File'),accept=c('text/csv','text/comma-separated-values,text/plain', '.csv'))
              ),
              shiny::column(6, shiny::uiOutput("varInput_file"))
            )
          )
          ,shiny::fluidRow("")
          ,shiny::actionButton("runautoCaret", "Run autoCaret",width="100%",icon = shiny::icon("sitemap"))
          ,shiny::textOutput("Loading")
        )
      ),
      miniUI::miniTabPanel( #add a new tab panel
        gettext("Data Preview", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          
          shiny::dataTableOutput("tablePreview") #output the value of the reactive  function tablePreview "output$tablePreview"
          
        )
      ),
      miniUI::miniTabPanel(
        gettext("Results - Preprocessing", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          #####################################################
          ###Rock, Add code here for pre-processing output#####
          #####################################################
        )
      ),
      miniUI::miniTabPanel(
        gettext("Results - Graph", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          #####################################################
          ###Rock, Add code here for graph output#####
          #####################################################
        )
      ),
      miniUI::miniTabPanel(
        gettext("Results - Variable Importance", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          wellPanel(
            shiny::column(6, shiny::uiOutput("VariableImportance"))
          ),
          shiny::fluidRow(tableOutput("VariableImportanceTable"))
        )
      ),
      miniUI::miniTabPanel(
        gettext("Results - Summary", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::tags$h4(gettext("Details for each model attempted", domain="R-autoCaret")),
          shiny::fluidRow(tableOutput("BestModelResults")),
          shiny::column(6, shiny::uiOutput("Wikipedia_Pages")),
          shiny::textOutput("Wikipedia_Output")
          
          
        )
      ),
      miniUI::miniTabPanel(
        gettext("Results - Details", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          wellPanel(
            shiny::column(6, shiny::uiOutput("autoModelListNames"))
          ),
          shiny::fluidRow(tableOutput("ResultsText"))
        )
      )
      
    )
  )
  
  #create flag for when the autoModel function is complete
  
  
  server <- function(input, output) {
    autoModelComplete <- 0
    ##Reactive function for when user uploads data. Returns df. This could probably use some error checking.
    Uploaded_Data <- shiny::reactive({
      inFile <-  input$Load_Data
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath)
    })
    
    ## reactive first level object (vector or data frame)
    robj <- shiny::reactive({
      obj <- get(req(input$obj_name), envir = .GlobalEnv)
      if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
      obj
    })
    
    ## reactive variable object (vector or data frame column)
    rvar <- shiny::reactive({
      invisible(input$obj_name)
      if (is.data.frame(robj())) {
        return(robj()[[req(input$var_name)]])
      }
      if (is.vector(robj()) || is.factor(robj())) {
        return(robj())
      }
      return(NULL)
    })
    
    #re-render UI.
    output$dfInput <- shiny::renderUI({
      selectizeInput(
        "obj_name",
        gettext("Select Data frame", domain="R-questionr"),
        choices = Filter(
          function(x) {
            inherits(get(x, envir = sys.parent()), "data.frame") ||
              is.vector(get(x, envir = sys.parent())) ||
              is.factor(get(x, envir = sys.parent()))
          }, ls(.GlobalEnv)),
        selected = obj_name, multiple = FALSE)
    })
    
    ## R Environment. Column to predict selection.
    ## If obj from R environment selected is a dataframe, display all the column names in varInput drop-down list.
    output$varInput <- shiny::renderUI({
      if (is.data.frame(robj())) {
        selectizeInput("var_name",
                       gettext("Variable You'd Like to Predict", domain="R-questionr"),
                       choices = names(robj()),
                       selected = var_name,
                       multiple = FALSE)
      }
    })
    
    ## File Upload. Column to predict selection.
    ## If file uploaded is a dataframe, display all the column names in varInput drop-down list.
    output$varInput_file <- shiny::renderUI({
      if (is.data.frame(Uploaded_Data())) {
        selectizeInput("var_names_file",
                       gettext("Variable You'd Like to Predict", domain="R-questionr"),
                       choices = names(Uploaded_Data()),
                       multiple = FALSE)
      }
    })
    
    #Data preview tab output.
    output$tablePreview <- shiny::renderDataTable({
      #if a file hasn't been uploded, display the dataframe selected from the R Environment.
      if(is.null(Uploaded_Data())){
        robj() #the df selected from the R Environment.
      }else{
        Uploaded_Data() #the uploaded data
      }
    })
    shiny::observeEvent(input$runautoCaret,{print("running automodel")
      if(input$runautoCaret>0){
        #create function to get name of a dataframe from the environment
        df_name <- function(v1) {
          deparse(substitute(v1))
        }
        #Determine df and y  to be sent to autoModel
        if(is.null(Uploaded_Data())){
          print("env")
          y <- input$var_name
          df <- robj() #the df selected from the R Environment.
          df_string <- df_name(df) #the variable name as a string
          # y_string <- paste(df_string,"$",y,sep="") #concatenate df name and y var -> "df$y"
          autoModel_call_string <- paste("autoModel(",df_string,",",y,")",sep="") #create a string of the call to autoModel
          
        }else{
          print("uploaded")
          
          y <- input$var_names_file
          Uploaded_df <<- Uploaded_Data() #the uploaded data
          print(Uploaded_df)
          df_string <- df_name(Uploaded_df) #the variable name as a string
          # y_string <- paste(df_string,"$",y,sep="") #concatenate df name and y var -> "df$y"
          autoModel_call_string <- paste("autoCaret::autoModel(",df_string,",",y,")",sep="")  #create a string of the call to autoModel
        }
        print(autoModel_call_string)
        #mod1 <- eval(parse(text = "mean(seq(1,10000))"))
        
        mod1 <- eval(parse(text = autoModel_call_string))
      }
      print("autoCaret Complete")
      print(mod1)
      autoModelList <<- mod1
      autoModelComplete <<- 1
    },priority = 1
    )
    output$autoModelListNames <- shiny::renderUI({
      input$Results
      if(autoModelComplete == 1){
        print("autoModelListNamesInput")
        selectizeInput(
          "autoModelListNamesInput",
          gettext("Output to Display", domain="R-questionr"),
          choices = names(autoModelList),
          selected = "model_list", multiple = FALSE)
      }
    })
    
    
    
    # output$caretListNames <- shiny::renderUI({
    #   input$Results
    #   selectedClass <- class(input$caretListNames)
    #   if(selectedClass == "caretList"){
    #     print("autoModelListNamesInput")
    #     selectizeInput(
    #       "caretListNamesInput",
    #       gettext("Output to Display", domain="R-questionr"),
    #       choices = names(input$autoModelListNamesInput),
    #       selected = "caret_list", multiple = FALSE)
    #   }
    # })
    
    output$ResultsText <- renderTable({
      if(autoModelComplete == 1){
        print_string <- paste("autoModelList$",input$autoModelListNamesInput,sep="") #concatenate df name and y var -> "df$y"
        eval(parse(text = print_string))
      }else{
        "Run autoCaret in the setup tab to see results"
      }
    })
    output$VariableImportance <- shiny::renderUI({
      input$Results
      if(autoModelComplete == 1){
        selectizeInput(
          "VarImpNameInput",
          gettext("Model", domain="R-questionr"),
          choices = names(autoModelList$variable_importance),
          selected = "overall", multiple = FALSE)
      }
    })
    output$VariableImportanceTable <- renderTable({
      if(autoModelComplete == 1){
        selected_column <- ifelse(is.null(input$VarImpNameInput),"overall",input$VarImpNameInput)
        Rank <- seq(1,nrow(autoModelList$variable_importance))
        return_df <- autoModelList$variable_importance[c(selected_column,"variable")]
        return_df_string1 <- paste("return_df[order(return_df$",selected_column,",decreasing=TRUE),]",sep='')
        return_df <- eval(parse(text = return_df_string1))
        cbind(Rank,return_df)
      }else{
        "Run autoCaret in the setup tab to see variable importance"
      }
    })
    
    output$BestModelResults <- renderTable({
      if(autoModelComplete == 1){
        summary(autoModelList)$best_model_results
      }else{
        "Run autoCaret in the setup tab to see model results"
      }
    })
    output$Wikipedia_Pages <- shiny::renderUI({
      input$Results
      page_names <- c("Random Forest", "Linear predictor function")
      if(autoModelComplete == 1){
        selectizeInput(
          "Wikipedia_Page_Select",
          gettext("Choose a model for more information", domain="R-questionr"),
          choices = page_names,
          selected = NULL, multiple = FALSE)
      }
    })
    output$Wikipedia_Output <- shiny::renderText({
      input$Wikipedia_Page_Select
      page_names <- c("Random Forest", "Linear predictor function")
      if(autoModelComplete == 1){
        connector <- wiki_con(language = "en", project = "wikisource", w_timeout = 10)
        content <- wiki_page(con = connector,
                             page = input$Wikipedia_Page_Select,
                             properties = "text")
        content
      }
    })
    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
    
  }
  
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("autoCaret", width = 1100, height = 900))
  
}

####################################
# ##         Testing Area           ##
# ####################################
# source('R/shiny-utils.R')
# source('R/autoCaret.R')
# source('R/helperFunctions.R')
# library(shiny)
# library(rstudioapi)
# library(miniUI)
# library(highr)
# library(questionr)
#
# #create a couple test dataframe
# df <- data.frame(a=seq(1,100),b=seq(1,100),c = seq(1,100))
# df2 <- data.frame(x=rep(c("cat1","cat2"),50),y=seq(11,110),z = seq(453,552))

# autoCaretUI() #main function. Run this to test add-in

####################################
##       End Testing Area         ##
####################################