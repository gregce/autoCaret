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
term_descriptions <- read.csv("data/Definitions.csv",stringsAsFactors = FALSE)

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
          shiny::tags$h2(shiny::icon("columns"), gettext("Provide Data to Build Models", domain="R-autoCaret")),
          shiny::radioButtons("InputRadio", label = "Input data by", c("Selecting from R environment" = "r_obj", "Uploading a file" = "file"), inline = TRUE, width = NULL)
          ,shiny::wellPanel(
            shiny::fluidRow(shiny::column(6, shiny::uiOutput("inputUI")),
                            shiny::column(6, shiny::uiOutput("varInput")))
          )

          ,shiny::fluidRow("")
           ,shiny::actionButton("runautoCaret", "Run autoCaret",width="100%",icon = shiny::icon("sitemap"))
          ,shiny::tags$div()
          ,shiny::tags$br()
          ,shiny::dataTableOutput("tablePreviewObj")
          ,shiny::dataTableOutput("tablePreviewFile")
          ,conditionalPanel(condition="$('html').hasClass('shiny-busy')",uiOutput("LoadingImage"))

        )

      )#end miniTabPanel


      ,miniUI::miniTabPanel(
        gettext("Model Summary", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::tags$h4(gettext("autoCaret tried the following models. Click the graph on the left to learn more.", domain="R-autoCaret")),
          shiny::fillCol(
            shiny::fillRow(
              shiny::plotOutput("BestModelResults", click = "plot_click",height = "100%",width="93%"),
              fillCol(
                shiny::tableOutput("Measure_Summary_Output"),shiny::tableOutput("VariableImportanceTable")
              ,flex=c(2,3))
              ,shiny::plotOutput("GraphVarImp",height="60%")
            ,flex=c(10,5,6)),
            shiny::fillRow(
              shiny::textOutput("Measure_Information_Output")
            ),
            shiny::fillRow(
              shiny::tableOutput("Model_Information_Output")
            )
          ,flex=c(10,1,3.5))
        )#end miniContentPanel
      )#end miniTabPanel


      ,miniUI::miniTabPanel(
        gettext("Use Your Model", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style

        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::tags$h2(gettext("Make predict to new data with built autoCaret model", domain="R-autoCaret")),
          shiny::tags$h4(shiny::icon("columns"), gettext("Select new data to apply your model", domain="R-autoCaret")),
          shiny::wellPanel(
            shiny::fluidRow(shiny::column(6, shiny::uiOutput("NewDataInput"))
            )
          ),
          shiny::actionButton("PredictButton", "Predict",width="100%",icon = shiny::icon("sitemap"))
          ,shiny::tags$br()
          ,shiny::tags$div()
          ,shiny::textOutput("Completed")
        )#end miniContentPanel
      )#end miniTabPanel

      ,miniUI::miniTabPanel(
        gettext("Ways to Improve", domain="R-autoCaret"), icon = shiny::icon("table"), #tab "button" style
        miniUI::miniContentPanel( #create the "bucket" for the content of the tab.
          shiny::tags$h3(gettext("Possible ways to improve accuracy of your model", domain = "R-autoCaret"))
          ,shiny::tags$h4(shiny::textOutput("nzvHeader"))
          ,shiny::textOutput("CaretRan")
          ,shiny::textOutput("nzvDescOutput")
          ,shiny::htmlOutput("nzvOutput")
          #,shiny::textOutput("nzvOutput")
          ,shiny::tags$br()
          ,shiny::tags$h4(shiny::textOutput("HighCorHeader"))
          ,shiny::textOutput("HighCorDescOutput")
          ,shiny::htmlOutput("HighCorOutput")
          ,shiny::tags$br()
          ,shiny::tags$h4(shiny::textOutput("LinearDepHeader"))
          ,shiny::textOutput("LinearDepDescOutput")
          ,shiny::htmlOutput("LinearDepOutput")
        )#end miniContentPanel
      )#end miniTabPanel

    )#end miniTabstripPanel
  )#end miniPage




  server <- function(input, output) {
  #create flag for when the autoModel function is complete
  autoModelComplete <- 0
    ##Reactive function for when user uploads data. Returns df. This could probably use some error checking.
    Uploaded_Data <- shiny::reactive({
      if(is.null(input$Load_Data)){
        return(NULL)
      } else {
        inFile <-  input$Load_Data
        read.csv(inFile$datapath)
      }
    })

    ## reactive first level object (vector or data frame)
    robj <- shiny::reactive({
      if(is.null(input$obj_name)){
        return(NULL)
      } else {
        obj <- get(req(input$obj_name), envir = .GlobalEnv)
      if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
      return(obj)
      }
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



    output$inputUI <- shiny::renderUI({
      if (input$InputRadio=="r_obj") {
        selectizeInput(
          "obj_name",
          gettext("Select Data frame", domain="R-questionr"),
          choices = Filter(
            function(x) {
              inherits(get(x, envir = sys.parent()), "data.frame") ||
                is.vector(get(x, envir = sys.parent())) ||
                is.factor(get(x, envir = sys.parent()))
            }, ls(.GlobalEnv)),
          selected = Filter(
            function(x) {
              inherits(get(x, envir = sys.parent()), "data.frame") ||
                is.vector(get(x, envir = sys.parent())) ||
                is.factor(get(x, envir = sys.parent()))
            }, ls(.GlobalEnv))[1], multiple = FALSE)
      } else if (input$InputRadio == "file"){
        fileInput('Load_Data',
                  gettext('Upload *.csv or *.txt File'), accept=c('text/csv','text/comma-separated-values,text/plain', '.csv'))
      }
    })

    ## For data frame selection for prediction (Use your model)
    robjNewData <- shiny::reactive({
      obj <- get(req(input$new_data_name), envir = .GlobalEnv)
      if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
      obj
    })

    ## R Environment. Column to predict selection.
    ## If obj from R environment selected is a dataframe, display all the column names in varInput drop-down list.
    output$varInput <- shiny::renderUI({
      if (input$InputRadio == "r_obj") {
        if (is.data.frame(robj())) {
        selectizeInput("var_name",
                       gettext("Variable You'd Like to Predict", domain="R-questionr"),
                       choices = names(robj()),
                       selected = var_name,
                       multiple = FALSE)
        }
      } else if (input$InputRadio == "file"){
          if (is.data.frame(Uploaded_Data())) {
            selectizeInput("var_names_file",
                           gettext("Variable You'd Like to Predict", domain="R-questionr"),
                           choices = names(Uploaded_Data()),
                           multiple = FALSE)
          }
        }
    })





    output$tablePreviewObj <- shiny::renderDataTable({
      #If new object is selected, show that dataframe
      if(input$InputRadio == "r_obj"){
        get(req(input$obj_name))
      } else {
        NULL
      }
      }
    )
    output$tablePreviewFile <- shiny::renderDataTable({
      #If new file is uploaded, show that file
      if(input$InputRadio == "file"){
        try({read.csv(input$Load_Data$datapath)}, silent = TRUE)
      } else {
        NULL
      }
      #input$obj_name <- NULL
      }
    )


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


    output$LoadingImage <- renderUI({
      Sys.sleep(.1)
      tags$img(src="https://github.com/gregce/autoCaret/raw/master/data/autoCaretLoading.gif",width="600",height="150"
               ,style="display: block; margin-left: auto;margin-right: auto;margin-top: 10px;")
    })

    ####################################################################################################
    ## MODEL SUMMARY
    ####################################################################################################

    #Model Summary - Scatterplot of top two important variables of the selected model.
    #create interactice plotly scatterplot of the top two important variables of the selected model
    output$GraphVarImp <- shiny::renderPlot({
      selected_model <- ifelse(is.null(reactive_plot_vars$model_selected),"ensemble",reactive_plot_vars$model_selected)
      selected_model <- ifelse(selected_model=="ensemble","overall",selected_model)
      return_df <- autoModelList$variable_importance[c(selected_model,"variable")]
      return_df_string1 <- paste("return_df[order(return_df$",selected_model,",decreasing=TRUE),]",sep='')
      return_df <- eval(parse(text = return_df_string1))
      ReDummied_Data <- getDummifiedVariables(autoModelList$df, autoModelList$df_processed)
      var1_name <- return_df$variable[1]
      var2_name <- return_df$variable[2]
      var1_data <- eval(parse(text = paste("ReDummied_Data$",var1_name,sep="")))
      var2_data <- eval(parse(text = paste("ReDummied_Data$",var2_name,sep="")))
      response_var <- ReDummied_Data$y
      var_plot_df_str <- paste("data.frame(",var1_name,"=var1_data,",var2_name,"=var2_data,yvar=response_var)",sep="")
      var_plot_df <<- eval(parse(text = var_plot_df_str))
      if(selected_model != "variable"){
        ggplot_str <-   paste("ggplot(var_plot_df,aes(x=", var1_name,",y= ",var2_name,",color=yvar))+geom_point(size=2)",sep="")

        var_scatterplot <<- eval(parse(text = ggplot_str))
        var_scatterplot <<- var_scatterplot + theme(plot.subtitle = element_text(vjust = 1),
                                                    plot.caption = element_text(vjust = 1),
                                                    panel.grid.major = element_line(colour = "gray89"),
                                                    axis.title = element_text(size = 12,
                                                                              face = "bold"), axis.text = element_text(size = 10),
                                                    legend.text = element_text(size = 12),
                                                    panel.background = element_rect(fill = NA),
                                                    legend.key = element_rect(fill = NA,
                                                                              size = 2.4), legend.background = element_rect(fill = NA,
                                                                                                                            size = 0.9), legend.position = "top",
                                                    legend.direction = "horizontal") +labs(colour = NULL)
        var_scatterplot <<- var_scatterplot + scale_color_manual(values = c("#bea7a7","#3f733f"))
        var_scatterplot <- var_scatterplot + ggplot2::guides(colour = guide_legend(override.aes = list(size=7)))
        # plotly::ggplotly(var_scatterplot)
        var_scatterplot
      }
    })

    #Model Summary
    #Render a table of the variable importance measures for the selected model
    output$VariableImportanceTable <- renderTable({
      #check if autoModel has been run. If it hasn't, give user a message.
      if(autoModelComplete == 1){
        var_imp <- autoModelList$variable_importance
        names(var_imp)[names(var_imp) =="overall"] <- "ensemble" #change 'overall' name to 'ensemble'
        selected_column <- ifelse(is.null(reactive_plot_vars$model_selected),"ensemble",reactive_plot_vars$model_selected)
        Rank <- seq(1,nrow(var_imp))
        return_df <- var_imp[c(selected_column,"variable")]
        return_df_string1 <- paste("return_df[order(return_df$",selected_column,",decreasing=TRUE),]",sep='')
        return_df <- eval(parse(text = return_df_string1))
        head(cbind(Rank,return_df),5)
      }else{
        "Run autoCaret in the setup tab to see variable importance"
      }
    }, caption = "5 Most Important Variables",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))

    #Model Summary
    #render a plot of the best model results. This is an object returned from the summary method on the autoModel object.
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
        graph <- graph + ggplot2::theme(axis.text.x = element_text(size = 14),
                                        axis.text.y = element_text(size = 19, face = "bold"))
        graph <<- graph #move to global environment
        graph
        #gridExtra::grid.arrange(graph, ncol=1, nrow =1)
      }else{
        "Run autoCaret in the setup tab to see model results"
      }
    })

    #Model Summary
    #create reactive values for the top left plot.
    reactive_plot_vars <- reactiveValues(
      alpha = .7,
      model_selected = "ensemble",
      measure_selected = NULL
    )

    #Model Summary
    #code to capture selected model and measure for top left plot click event.
    observeEvent(input$plot_click,{
      reactive_plot_vars$alpha <- rep(.7,nrow(Graph_df))
      x <- input$plot_click$y
      y <- input$plot_click$x
      ggbuild <- ggplot_build(graph)
      Graph_df_join <- Graph_df
      names(Graph_df_join)[3] <- "y"
      graph_coordinates <- ggbuild$data[[2]]
      obj_and_coordinates <- dplyr::inner_join(Graph_df_join,graph_coordinates)
      print(obj_and_coordinates)
      selected_rows <- obj_and_coordinates$xmin < x & obj_and_coordinates$xmax > x
      selected_model <- unique(obj_and_coordinates[c("model_name")][selected_rows,])
      selected_measure <- obj_and_coordinates[c("measure")][selected_rows,]
      model_rows <- obj_and_coordinates$model_name == selected_model
      reactive_plot_vars$alpha[model_rows] <- .85
      reactive_plot_vars$alpha[selected_rows] <- 1
      #check if no model is selected. return default values if none is selected.

      if(length(selected_model) == 0L){
        reactive_plot_vars$alpha <- rep(.7,nrow(Graph_df))
        selected_model <- "ensemble"
      }
      reactive_plot_vars$model_selected <- as.character(selected_model)
      reactive_plot_vars$measure_selected <- as.character(selected_measure)
    })

    #Model Summary
    #Render a table of the measure summary for the selected model
    output$Measure_Summary_Output <- renderTable({
      #check if autoModel has been run. If it hasn't, give user a message.
      if(autoModelComplete == 1){
        model <- reactive_plot_vars$model_selected
        num_models <- nrow(best_model_results)
        #create named vectors of for ranks. This makes cbind easy.
        ROC_Rank <- seq(1,num_models)
        Sens_Rank <- ROC_Rank
        Spec_Rank <- ROC_Rank
        best_model_results <- cbind(best_model_results[order(best_model_results$ROC,decreasing=TRUE),],ROC_Rank)
        best_model_results <- cbind(best_model_results[order(best_model_results$Sens,decreasing=TRUE),],Sens_Rank)
        best_model_results <- cbind(best_model_results[order(best_model_results$Spec,decreasing=TRUE),],Spec_Rank)
        ROC <- round(best_model_results$ROC[best_model_results$model_name == model],3)
        Sens <- round(best_model_results$Sens[best_model_results$model_name == model],3)
        Spec <- round(best_model_results$Spec[best_model_results$model_name == model],3)
        num_suffix <- function(num){
          last_digit <- num%%10
          if(last_digit==1){
            paste(num,"st",sep="")
          }else if(last_digit ==2){
            paste(num,"nd",sep="")
          }else if(last_digit ==3){
            paste(num,"rd",sep="")
          }else{
            paste(num,"th",sep="")
          }
        }
        ROC_Rank_str <- num_suffix(best_model_results$ROC_Rank[best_model_results$model_name == model])
        Sens_Rank_str <- num_suffix(best_model_results$Sens_Rank[best_model_results$model_name == model])
        Spec_Rank_str <- num_suffix(best_model_results$Spec_Rank[best_model_results$model_name == model])
        ROC_String <- paste("ROC: ",ROC ," (",ROC_Rank_str," out of ",num_models,")",sep="")
        Sens_String <- paste("Sensitivity: ",Sens ," (",Sens_Rank_str," out of ",num_models,")",sep="")
        Spec_String <- paste("Specificity: ",Spec ," (",Spec_Rank_str," out of ",num_models,")",sep="")
        return_df <- rbind(ROC_String,Sens_String,Spec_String)
        colnames(return_df) <- model
        return_df
      }else{
        "Run autoCaret in the setup tab to see variable importance"
      }
    },colnames = FALSE)

    #Model Summary
    #display ROC, Sensitivity, or Specificity descriptions based on the model selected.
    output$Measure_Information_Output <- shiny::renderText({
      as.character(measure_descriptions$Description[measure_descriptions$Measure ==reactive_plot_vars$measure_selected])
    })

    #Model Summary
    #display information from model_descriptions.csv for selected model.
    output$Model_Information_Output <- shiny::renderTable({
      mdl <- reactive_plot_vars$model_selected
      colnames(model_descriptions)[colnames(model_descriptions)=="general_name"] <- '' #remove the general_name column name for display purposes.
      model_descriptions[model_descriptions$caret_name ==mdl,][c(-1,-ncol(model_descriptions))]
    })

    ####################################################################################################


    ###########################################
    ##   USE YOUR MODEL
    ##########################################
    output$NewDataInput <- shiny::renderUI({
      selectizeInput(
        "new_data_name",
        gettext("Select new data", domain="R-questionr"),
        choices = Filter(
          function(x) {
            inherits(get(x, envir = sys.parent()), "data.frame") ||
              is.vector(get(x, envir = sys.parent())) ||
              is.factor(get(x, envir = sys.parent()))
          }, ls(.GlobalEnv)),
        multiple = FALSE)
    })
    ### run Prediction ###
    shiny::observeEvent(input$PredictButton,{
      print("running prediction")
      df_name <- function(v1) {
        deparse(substitute(v1))
      }
      df <- robjNewData()
      df_string <- df_name(df)
      Predict_call_string <- paste("predict.autoCaret(object = autoModelList, newdata = ",df_string,")",sep="")
      print(Predict_call_string)
      #print(paste("predict.autoCaret(object = autoModelList, newdata = ",input$new_data_name,")",sep=""))
      result <- eval(parse(text = Predict_call_string))
      result <- as.data.frame(result)
      colnames(result) <- autoModelList$y_name
      try({
        autoModelPredictionResult <<- data.frame(df, result)
      }, silent = TRUE)
      print("Prediction Complete")
      output$Completed <- shiny::renderText(
        "Prediction has been completed and saved in 'autoModelPredictionResult' object"
      )
    },priority = 1
    )
    ###########################################


    ######################################################
    ##     Ways To Improve
    ######################################################
    output$CaretRan <- shiny::renderText({
      if(autoModelComplete != 1){
        "Run autoCaret in the setup tab to see result of preprocessing for improving results"
      }
    })
    #Near Zero Variance - Header
    output$nzvHeader <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$nzv$flag == TRUE){
          "Near Zero Variance Field"
        }
      }
    })

    #Near Zero Variance - Description
    output$nzvDescOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$nzv$flag == TRUE){
          term_descriptions[term_descriptions$Term =="NZV",]$Description

        }
      }
    }
    )
    #Near Zero Variance - Variable Output
    output$nzvOutput <- shiny::renderUI({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$nzv$flag == TRUE){
          shiny::HTML(paste("<ul><li><em>", paste(autoModelList$WaysToImprove$nzv$names, collapse = ", "),"</em></li></ul>", collapse = ""))
        }
      }
    })



    ## High Correlation ##
    #HighCor - Header
    output$HighCorHeader <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$HighCor$flag == TRUE){
          "Highly Correlated Fields"
        }
      }
    })
    #HighCor - Description
    output$HighCorDescOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$HighCor$flag == TRUE){
          term_descriptions[term_descriptions$Term =="HighCor",]$Description
        }
      }
    })
    #HighCor - Variable Output
    output$HighCorOutput <- shiny::renderUI({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$HighCor$flag == TRUE){
          shiny::HTML(paste("<ul><li><em>", paste(autoModelList$WaysToImprove$HighCor$names, collapse = ", "),"</em></li></ul>", collapse = ""))
        }
      }
    })

    ## Linear Dependency ##
    #Linear Dependency - Header
    output$LinearDepHeader <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$LinearDep$flag == TRUE){
          "Linearly Dependent Field"
        }
      }
    })
    #Linear Dependency - Description
    output$LinearDepDescOutput <- shiny::renderText({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$LinearDep$flag == TRUE){
          term_descriptions[term_descriptions$Term =="LinearDep",]$Description
        }
      }
    })
    #Linear Dependency - Variable Output
    output$LinearDepOutput <- shiny::renderUI({
      if(autoModelComplete == 1){
        if(autoModelList$WaysToImprove$LinearDep$flag == TRUE){
          shiny::HTML(paste("<ul><li><em>", paste(autoModelList$WaysToImprove$LinearDep$names, collapse = ", "),"</em></li></ul>", collapse = ""))
        }
      }
    })
    ###########################################################################

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })

  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("autoCaret", width = 1100, height = 900))

}
