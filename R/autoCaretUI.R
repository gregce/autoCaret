##' autoCaret Add-in
##'
##' This function launches a shiny app in a web browser in order to
##' run the autoCaret UI
##'
##' @param
##' @param
##' @return
##' The function launches a shiny app in the system web browser.
##' @examples
##' @import shiny
##' @import rstudioapi
##' @import miniUI
##' @importFrom highr hi_html
##' @export


model_descriptions <- read.csv("data/model_descriptions.csv",stringsAsFactors = TRUE)

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
        gettext("Ways to Improve", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
              #####################################################
              ###Rock, Add code here for pre-processing output#####
              #####################################################
          shiny::tags$h3(gettext("Possible ways to improve accuracy of your model", domain = "R-autoCaret"))
          ,shiny::tags$h4(shiny::textOutput("nzvHeader"))
          ,shiny::textOutput("nzvDescOutput")
          ,shiny::tags$ul(shiny::tags$li(shiny::tags$em(shiny::textOutput("nzvOutput"))))
          ,shiny::tags$br()
          ,shiny::tags$h4(shiny::textOutput("HighCorHeader"))
          ,shiny::textOutput("HighCorDescOutput")
          ,shiny::tags$ul(shiny::tags$li(shiny::tags$em(shiny::textOutput("HighCorOutput"))))
          ,shiny::tags$br()
          ,shiny::tags$h4(shiny::textOutput("LinearDepHeader"))
          ,shiny::textOutput("LinearDepDescOutput")
          ,shiny::tags$ul(shiny::tags$li(shiny::tags$em(shiny::textOutput("LinearDepOutput"))))
          )

      ),
      #miniUI::miniTabPanel(
      #  gettext("Results - Graph", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
#
#        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          #####################################################
          ###Rock, Add code here for graph output#####
          #####################################################
          #shiny::plotOutput("GraphOutput")
#          )
#      ),
      miniUI::miniTabPanel(
        gettext("Results - Variable Importance", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::wellPanel(
            shiny::column(6, shiny::uiOutput("VariableImportance"))
          ),
          shiny::fluidRow(shiny::tableOutput("VariableImportanceTable"))
          ,shiny::plotOutput("GraphVarImp")
        )
      ),
      miniUI::miniTabPanel(
        gettext("Results - Summary", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::tags$h4(gettext("Details for each model attempted", domain="R-autoCaret")),
          shiny::fluidRow(shiny::tableOutput("BestModelResults")),
          shiny::column(6, shiny::uiOutput("Model_Information")),
          shiny::tableOutput("Model_Information_Output")
          ,shiny::plotOutput("GraphOutput")


        )
      ),
      #mini tab panel for details of the model
      miniUI::miniTabPanel(
        gettext("Results - Details", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::wellPanel(
            shiny::column(6, shiny::uiOutput("autoModelListNames")) #drop down list for the names in the list object returned by autoModel
          ),
          shiny::fluidRow(shiny::tableOutput("ResultsText"))
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

    #Results - Details
    #Create drop down list for the names in the list object returned by autoModel
    output$autoModelListNames <- shiny::renderUI({
      input$Results
      if(autoModelComplete == 1){
        selectizeInput(
          "autoModelListNamesInput",
          gettext("Output to Display", domain="R-questionr"),
          choices = names(autoModelList),
          selected = "model_list", multiple = FALSE)
      }
    })
    #Results - Details
    #render a table based on the selection from the autoModelListNames dropdown.
    output$ResultsText <- renderTable({
      #check if autoModel has been run. If it hasn't, give user a message.
      if(autoModelComplete == 1){
        print_string <- paste("autoModelList$",input$autoModelListNamesInput,sep="") #concatenate df name and y var -> "df$y"
        eval(parse(text = print_string))
      }else{
        "Run autoCaret in the setup tab to see results"
      }
    })

    #Results - Variable Importance
    #Create drop down list for the names in the list object returned by autoModel$variable_importance. These should be the model types.
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

    #Results - Variable Importance
    #Render a table of the variable importance measures for the selected model in input$VarImpNameInput
    output$VariableImportanceTable <- renderTable({
      #check if autoModel has been run. If it hasn't, give user a message.
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

    #Results - Summary
    #render a table of the best model results. This is an object returned from the summary method on the autoModel object.
    output$BestModelResults <- renderTable({
      #check if autoModel has been run. If it hasn't, give user a message
      if(autoModelComplete == 1){
      summary(autoModelList)$best_model_results
      }else{
        "Run autoCaret in the setup tab to see model results"
      }
    })

    #Results - Summary
    #create a drop down list of the different models from which the user will select for more information.
    output$Model_Information <- shiny::renderUI({
      input$Results
      model_results <- summary(autoModelList)$best_model_results
      model_names <- as.character(model_results$model_name)
      # general_names <- model_descriptions$general_name[model_descriptions$caret_name %in% model_names]
      if(autoModelComplete == 1){
        selectizeInput(
          "Model_Information_Select",
          gettext("Choose a model for more information", domain="R-questionr"),
          choices = model_names,
          selected = NULL, multiple = FALSE)
      }
    })

    #Results - Summary
    #display information from model_descriptions.csv for selected model.
    output$Model_Information_Output <- shiny::renderTable({
      selected_model <- input$Model_Information_Select
      model_descriptions[model_descriptions$caret_name ==selected_model,][-1]

    })
    ###ROCK###
    #Result - Graph
    #output$GraphOutput <- shiny::renderPlot({
    #  selected_model <- input$Model_Information_Select
    #  return_df <- autoModelList$variable_importance[c(selected_model,"variable")]
    #  return_df_string1 <- paste("return_df[order(return_df$",selected_model,",decreasing=TRUE),]",sep='')
    #  return_df <- eval(parse(text = return_df_string1))
    #  plot(eval(parse(text = return_df$variable[1])), eval(parse(text = return_df$variable[2])), xlab = return_df$variable[1], ylab = return_df$variable[2], pch=21, bg=c("red","green3")[unclass(autoModelList$df_processed$y)])
#
#    })
    #Graph Var Imp
    output$GraphVarImp <- shiny::renderPlot({
      selected_model <- ifelse(is.null(input$VarImpNameInput),"overall",input$VarImpNameInput)
      return_df <- autoModelList$variable_importance[c(selected_model,"variable")]
      return_df_string1 <- paste("return_df[order(return_df$",selected_model,",decreasing=TRUE),]",sep='')
      return_df <- eval(parse(text = return_df_string1))
      if(selected_model != "variable"){
        plot(eval(parse(text = return_df$variable[1])), eval(parse(text = return_df$variable[2])), xlab = return_df$variable[1], ylab = return_df$variable[2], pch=21, bg=c("red","green3")[unclass(autoModelList$df_processed$y)])
      }
    })
    #PreProcessing Output
    output$nzvHeader <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$nzv != FALSE){
          "Near Zero Variance Field"
        }
      }
    })
    output$nzvDescOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$nzv != FALSE){
          autoModelList$nzvDesc
        }
      }
    }

    )
    output$nzvOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$nzv != FALSE){
          paste(autoModelList$nzvNames, collapse = ", ")
        }
      }else{
        "Run autoCaret in the setup tab to see result of preprocessing for improving results"
      }
    }

    )

    #HighCor
    output$HighCorHeader <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$HighCor != FALSE){
          "Highly Correlated Fields"
        }
      }
    })

    output$HighCorDescOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$HighCor != FALSE){
          autoModelList$HighCorDesc
        }
      }
    }

    )
    output$HighCorOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$HighCor != FALSE){
          paste(autoModelList$HighCorNames, collapse = ", ")
        }
      }
    }

    )
    ### Linear Dependency
    output$LinearDepHeader <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$LinearDep != FALSE){
          "Linearly Dependent Field"
        }
      }
    })

    output$LinearDepDescOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$LinearDep != FALSE){
          autoModelList$LinearDepDesc
        }
      }
    }

    )
    output$LinearDepOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$LinearDep != FALSE){
          paste(autoModelList$LinearDepNames, collapse = ", ")
        }
      }
    }

    )
    ###

    #Graph Output
    #output$GraphOutput <- shiny::renderPlot({
 #     if(autoModelComplete == 1){
   #     plot(autoModelList$variable_importance[0])
 #     }
 #   })
    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("autoCaret", width = 1100, height = 900))

}

