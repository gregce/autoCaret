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
##' @import ggplot2
##' @importFrom highr hi_html
##' @export

model_descriptions <- read.csv("data/Model_Descriptions.csv",stringsAsFactors = TRUE)
measure_descriptions <- read.csv("data/Measure_Descriptions.csv",stringsAsFactors = TRUE)

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
          # ,shiny::actionButton("runautoCaret", "Run autoCaret",width="100%",icon = shiny::icon("sitemap"))
          ,shiny::textOutput("Loading")
        )
        ,
        miniButtonBlock(
          shiny::actionButton("runautoCaret", "Run autoCaret",width="70%",icon = shiny::icon("sitemap"),
                              class="btn btn-primary")
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
          # shiny::fluidRow(tableOutput("BestModelResults")),
          shiny::fluidRow(shiny::plotOutput("BestModelResults", click = "plot_click")),
          shiny::column(6, shiny::textOutput("Measure_Information_Output")),
          shiny::tableOutput("Model_Information_Output")
        )
      ),
      #mini tab panel for details of the model
      miniUI::miniTabPanel(
        gettext("Results - Details", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          wellPanel(
            shiny::column(6, shiny::uiOutput("autoModelListNames")) #drop down list for the names in the list object returned by autoModel
          ),
          shiny::fluidRow(tableOutput("ResultsText"))
        )#end miniContentPanel
      )#end miniTabPanel
    )#end miniTabstripPanel
  )#end miniPage




  server <- function(input, output) {
  #create flag for when the autoModel function is complete
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

    # #Results - Summary
    # #render a table of the best model results. This is an object returned from the summary method on the autoModel object.
    # output$BestModelResults <- renderTable({
    #   #check if autoModel has been run. If it hasn't, give user a message
    #   if(autoModelComplete == 1){
    #   summary(autoModelList)$best_model_results
    #   }else{
    #     "Run autoCaret in the setup tab to see model results"
    #   }
    # })

    #Results - Summary
    #render a table of the best model results. This is an object returned from the summary method on the autoModel object.
    output$BestModelResults <- shiny::renderPlot({
      #check if autoModel has been run. If it hasn't, give user a message
      if(autoModelComplete == 1){
        best_model_results <<- summary(autoModelList)$best_model_results
        Mean <- tidyr::gather(best_model_results[1:4],key = model_name)
        SD <- tidyr::gather(best_model_results[c(1,5,6,7)],key = model_name)
        Graph_df <<- cbind(Mean,SD[3])
        names(Graph_df) <<- c("model_name","measure","mean","SD")
        Graph_df$model_name <<- factor(Graph_df$model_name, levels = rev(as.character(best_model_results$model_name)))
        Graph_df$measure <<- as.factor(Graph_df$measure)
        Graph_df$measure <<- factor(Graph_df$measure, levels = c("Spec","Sens","ROC"))
        Graph_df$alpha_reactive <<- reactive_plot_vars$alpha #pull in reactive value

        levels(Graph_df$measure) <<- c("Specificity","Sensitivity","ROC")


        Label_df <- Graph_df[Graph_df$model_name == reactive_plot_vars$model_selected,]
        Label_df <- cbind(Label_df[1:2],Label_df$mean-Label_df$SD*1.96)
        names(Label_df)[3]<- 'y_position'

        legend_df <- Graph_df[1,]
        legend_df$mean <- .5
        legend_df$SD <- .1

        legend_graph <- ggplot2::ggplot(legend_df)
        legend_graph <- legend_graph + ggplot2::geom_rect(aes(ymin=mean-SD*2.1, ymax=mean+SD*2.1),xmin=-Inf,xmax=Inf,fill='White',color = "grey80")
        legend_graph <- legend_graph + ggplot2::geom_pointrange(aes(y=mean,x=model_name,ymin=mean-SD*1.96, ymax=mean+SD*1.96,color=measure,shape = measure),size=1.1,stat="identity",position = position_dodge(width = .6))
        legend_graph <- legend_graph + ggplot2::coord_flip(ylim=c(0, 1))
        legend_graph <- legend_graph + ggplot2::geom_text(aes(y=mean,x=model_name,label="Mean"),color="black",fontface="bold",position = position_dodge(width = .6),vjust=-1)
        legend_graph <- legend_graph + ggplot2::geom_text(aes(y=mean,x=model_name,color=measure,label=".95 Confidence Interval"),position = position_dodge(width = .6),vjust=2)

        legend_graph <- legend_graph + ggplot2::theme(axis.ticks = element_line(linetype = "blank"),
                                             axis.title = element_text(colour = NA),
                                             axis.text = element_text(colour = NA),
                                             plot.title = element_text(colour = NA),
                                             panel.background = element_rect(fill = NA),
                                             panel.border = element_blank(),
                                             axis.line = element_blank(),
                                             legend.position = "none")

        graph <- ggplot2::ggplot(Graph_df)
        graph <- graph + ggplot2::geom_hline(yintercept =1,size=1.3)
        graph <- graph + ggplot2::geom_pointrange(aes(y=mean,x=model_name,ymin=mean-SD*1.96, ymax=mean+SD*1.96,color=measure,shape = measure,alpha=alpha_reactive),size=.9,stat="identity",position = position_dodge(width = .6))
        # graph <- graph + coord_cartesian(ylim=c(0, 1))
        graph <- graph + ggplot2::coord_flip(ylim=c(0, 1))
        graph <- graph + ggplot2::scale_y_continuous(breaks=seq(0,1,.2),minor_breaks = seq(.1,9,.2),position="top")
        graph <- graph + ggplot2::scale_x_discrete(position = "top")
        graph <- graph + ggplot2::geom_text(data= Label_df,aes(y=y_position,x=model_name,color=measure,label=measure),fontface="bold",position = position_dodge(width = .6),hjust=1.1)
        graph <- graph + ggplot2::theme(axis.ticks = element_line(linetype = "blank"),
                               panel.grid.major.x = element_line(colour = "gray88",
                                                                 size = 0.7),panel.grid.minor.x = element_line(colour = "gray88"), legend.text = element_text(size = 12),
                               legend.title = element_text(colour = NA),
                               panel.background = element_rect(fill = NA),
                               plot.background = element_rect(fill = "white"),
                               panel.border = element_blank(),
                               axis.line = element_blank(),
                               axis.title = element_blank(),
                               legend.position = "none")
        graph <- graph + ggplot2::theme(axis.text.x = element_text(size = 11),
                               axis.text.y = element_text(size = 15, face = "bold"))
        graph
        #gridExtra::grid.arrange(graph, ncol=1, nrow =1)
      }else{
        "Run autoCaret in the setup tab to see model results"
      }
    })

    reactive_plot_vars <- reactiveValues(
      alpha = rep(.7, nrow(Graph_df)),
      model_selected = "ensemble",
      measure_selected = NULL
    )

    # output$hover_info <-
    observeEvent(input$plot_click,{
      reactive_plot_vars$alpha <- rep(.7, nrow(Graph_df))
      # Mean <- tidyr::gather(best_model_results[1:4],key = model_name)
      # SD <- tidyr::gather(best_model_results[c(1,5,6,7)],key = model_name)
      # Graph_df <- cbind(Mean,SD[3])
      # names(Graph_df) <- c("model_name","measure","mean","SD")
      # Graph_df$model_name <- factor(Graph_df$model_name, levels = rev(as.character(best_model_results$model_name)))
      # Graph_df$measure <- as.factor(Graph_df$measure)
      # Graph_df$measure <- factor(Graph_df$measure, levels = c("Spec","Sens","ROC"))
      # levels(Graph_df$measure) <- c("Specificity","Sensitivity","ROC")

      x <- input$plot_click$y
      y <- input$plot_click$x
      ggbuild <- ggplot_build(graph)
      Graph_df_join <- Graph_df
      names(Graph_df_join)[3] <- "y"
      graph_coordinates <- ggbuild$data[[2]]
      obj_and_coordinates <- dplyr::inner_join(Graph_df_join,graph_coordinates)
      selected_rows <- obj_and_coordinates$xmin < x & obj_and_coordinates$xmax > x
      selected_model <- unique(obj_and_coordinates[c("model_name")][selected_rows,])
      selected_measure <- obj_and_coordinates[c("measure")][selected_rows,]
      model_rows <- obj_and_coordinates$model_name == selected_model
      reactive_plot_vars$alpha[model_rows] <- .85
      reactive_plot_vars$alpha[selected_rows] <- 1
      reactive_plot_vars$model_selected <- as.character(selected_model)
      reactive_plot_vars$measure_selected <- as.character(selected_measure)
      print(reactive_plot_vars$model_selected)
      print("Updated reactive_plot_vars$alpha")
      # print(paste("x=",x,", y=",y,sep=""))
      # print(obj_and_coordinates[c("model_name","measure")][obj_and_coordinates$xmin < x & obj_and_coordinates$xmax > x,])
    })

    #Results - Summary
    output$Measure_Information_Output <- shiny::renderText({
      as.character(measure_descriptions$Description[measure_descriptions$Measure ==reactive_plot_vars$measure_selected])
    })

    #Results - Summary
    #display information from model_descriptions.csv for selected model.
    output$Model_Information_Output <- shiny::renderTable({
      mdl <- reactive_plot_vars$model_selected
      print(mdl)
      model_descriptions[model_descriptions$caret_name ==mdl,][-1]
    })

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("autoCaret", width = 1100, height = 900))

}
