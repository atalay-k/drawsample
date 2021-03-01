#'
#' Draw Samples with a Shiny Applications
#'
#' Performing package functions with user friendly 'shiny' interface.
#' @import shinydashboard
#' @import shinycssloaders
#' @import readxl
#' @import moments
#' @import xlsx
#' @import shiny
#' @export
#' @examples

#' # Example data provided with package
#' data(example_data)
#' \dontrun{
#' # if(interactive()){
#' ## Run this code for launching the 'shiny' application
#' #  draw_sample_shiny()
#' #  }
#' # }

draw_sample_shiny<-function(){
  ui <- dashboardPage(skin = "blue",
dashboardHeader(title = "DRAW SAMPLE APPLICATION",titleWidth = 500),
dashboardSidebar(
  sidebarMenu(id="sidebarmenu",
 shinydashboard::  menuItem("About Package",tabName = "info",icon = shiny::icon("info-circle")),
 shinydashboard:: menuItem("Draw Sample",tabName = "draw",icon = shiny::icon("cogs")),
 shinydashboard:: menuItem("Draw Sample (Nearest)",tabName = "nearest",icon = shiny::icon("cogs")),
 shinydashboard:: menuItem("Histograms",tabName = "graph",icon = shiny::icon("table"))


  )),
dashboardBody(
  tabItems(

    tabItem(
      tabName = "graph",
      shiny::fluidRow(shiny::column(12,
          shinydashboard::box(title="Histograms",shiny::plotOutput("graph"),solidHeader = TRUE,status = "info",width = 12))

      )),

    tabItem(tabName = "info",
shiny::fluidRow(
  shinydashboard::box(title = "About Package", solidHeader = TRUE, status = "info",shiny::htmlOutput("info"),width = 8)
)),

    tabItem(tabName = "nearest",
shiny::fluidRow(shiny::column(4,
     shinydashboard::box(title = "UpLoad File",status = "warning",solidHeader = TRUE,width = 12,
shiny::fileInput("datasetnearest", "Please upload your data file in one of .xls, .xslx formats. There should be 2 shiny::columns of data consisting of id and score", placeholder="File",buttonLabel = "Browse",accept = c("xlsx","xls","csv")
),shiny::uiOutput(outputId="datachecknearest")),
     shinydashboard::box(title = "Select Conditions",status = "warning",solidHeader = TRUE,width = 12,
shiny::uiOutput(outputId="samplesizenearest"),
shiny::uiOutput(outputId="checknearest"),
shiny::uiOutput(outputId="skewnearest"),
shiny::uiOutput(outputId="kurtnearest"),
shiny::uiOutput(outputId="location"),
shiny::uiOutput(outputId="delta"),
shiny::submitButton("Submit")

     )

),
shiny::column(8,
       shinydashboard::box(title = "Descriptive Statistics",
shiny::downloadButton('downloadstatsnearest','Download Stats'),
solidHeader = TRUE, status = "info",shiny::dataTableOutput("statsnearest"),width = 12),
       shinydashboard::box(title="Drawed Sample",
shiny::downloadButton('downloadsamplenearest','Download Sample'),
shiny::dataTableOutput("drawnearest")%>%withSpinner(color='#0dc5c1',type=6,size=1.5),solidHeader = TRUE,status = "info",width = 12))),



    ),

    tabItem(tabName = "draw",
shiny::fluidRow(shiny::column(4,
     shinydashboard::box(title = "UpLoad File",status = "warning",solidHeader = TRUE,width = 12,
shiny::fileInput("dataset", "Please upload your data file in one of .xls, .xslx formats. There should be 2 shiny::columns of data consisting of id and score", placeholder="File",buttonLabel = "Browse",accept = c("xlsx","xls","csv")
),shiny::uiOutput(outputId="datacheck")),
     shinydashboard::box(title = "Select Conditions",status = "warning",solidHeader = TRUE,width = 12,
shiny::uiOutput(outputId="samplesize"),
shiny::uiOutput(outputId="check"),
shiny::uiOutput(outputId="skew"),
shiny::uiOutput(outputId="kurt"),
shiny::submitButton("Submit")

     )

),
shiny::column(8,
       shinydashboard::box(title = "Descriptive Statistics",
  shiny::downloadButton('downloadstats','Download Stats'), solidHeader = TRUE, status = "info",shiny::dataTableOutput("stats"),width = 12),
   shinydashboard::box(title="Drawed Sample",
    shiny:: downloadButton('downloadsample','Download Sample'),shiny::dataTableOutput("draw")%>%withSpinner(color='#0dc5c1',type=6,size=1.5),solidHeader = TRUE,status = "info",width = 12))),



    ))

))


  server <- function(input, output,session) {


    data<-reactive({
      inFile <- input$dataset
      if (is.null(inFile))
        return("Please upload data")
      a<- readxl::read_xlsx(inFile$datapath, sheet=1)
      data<-as.data.frame(a)
      data
    })
    ##############################
    datanearest<-reactive({
      inFile <- input$datasetnearest
      if (is.null(inFile))
        return("Please upload data")
      a<- readxl::read_xlsx(inFile$datapath, sheet=1)
      data<-as.data.frame(a)
      data
    })
    #####################################


    output$samplesize<-renderUI({
      textInput(inputId ="samplesize",label = "Desired Sample Size",value = "")
    })

    output$skew<-renderUI({
      textInput(inputId ="skew",label = "Desired Skewness",value = "")
    })

    output$kurt<-renderUI({
      textInput(inputId ="kurt",label = "Desired Kurtosis",value = "")
    })

    output$check<-renderUI({

      checkboxInput(inputId = "check",label = "Use Skewness and Kurtosis Values of Population")
    })


    output$datacheck<-renderUI({

      checkboxInput(inputId = "datacheck",label = "Use Example Dataset")
    })

    ############################################
    output$samplesizenearest<-renderUI({
      textInput(inputId ="samplesizenearest",label = "Desired Sample Size",value = "")
    })

    output$skewnearest<-renderUI({
      textInput(inputId ="skewnearest",label = "Desired Skewness",value = "")
    })

    output$kurtnearest<-renderUI({
      textInput(inputId ="kurtnearest",label = "Desired Kurtosis",value = "")
    })

    output$checknearest<-renderUI({

     checkboxInput(inputId = "checknearest",label = "Use Skewness and Kurtosis Values of Population")
    })


    output$datachecknearest<-renderUI({

      checkboxInput(inputId = "datachecknearest",label = "Use Example Dataset")
    })

    output$location<-renderUI({
      textInput(inputId ="location",label = "Location(default value=0)",value = "")
    })
    output$delta<-renderUI({
      textInput(inputId ="delta",label = "Delta-var(default value=0)",value = "")
    })

    #######################################################


    drawsample<-reactive({

      n<-input$samplesize
      n<-as.numeric(n)

      if(input$datacheck==TRUE){

        dist<-example_data
      }
      else{dist<-data()}

      skew<-as.numeric(input$skew)
      kurt<-as.numeric(input$kurt)
      dist<-as.data.frame(dist)

      if(input$check==TRUE){
        skew<-skewness(dist[,2])
        kurt<-kurtosis(dist[,2])

      }

      result<-draw_sample(dist=dist,n=n,skew=skew,kurts=kurt,replacement =TRUE,output_name = c("sample","1"))
      result

    })
    ###########################

    drawsamplenearest<-reactive({



      ##################################################################

      n<-input$samplesizenearest
      n<-as.numeric(n)
      if(input$datachecknearest==TRUE){

        dist<-example_data
      }
      else{dist<-datanearest()}

      skew<-as.numeric(input$skewnearest)
      kurt<-as.numeric(input$kurtnearest)
      location<-as.numeric((input$location))
      delta<-as.numeric((input$delta))
      dist<-as.data.frame(dist)

      if(input$checknearest==TRUE){
        skew<-skewness(dist[,2])
        kurt<-kurtosis(dist[,2])}

      result<-draw_sample_n(dist=dist,n=n,skew=skew,kurts=kurt,location=location, delta_var=delta,output_name = c("sample","1"))


      result

    })

    #####################
    draw<-reactive({



      if(is.null(input$samplesize)==TRUE)
        return(NULL)
      result<-drawsample()
      draw<-round(result$sample,3)
      draw<-as.data.frame(draw)

    })

    ###############
    drawnearest<-reactive({



      if(is.null(input$samplesizenearest)==TRUE)
        return(NULL)
      result<-drawsamplenearest()
      draw<-round(result$sample,3)
      draw<-as.data.frame(draw)

    })

    ##################
    output$draw<-renderDataTable({

      draw()

    })
    #############
    output$drawnearest<-renderDataTable({

      drawnearest()

    })
    ####################
    stat<-reactive({

      if(is.null(input$samplesize)==TRUE)
        return(NULL)
      result<-drawsample()
      stats<-round(result$desc,3)
      as.data.frame(stats)

    })
    #####################
    statnearest<-reactive({

      if(is.null(input$samplesizenearest)==TRUE)
        return(NULL)
      result<-drawsamplenearest()
      stats<-round(result$desc,3)
      as.data.frame(stats)

    })
    ##########################
    output$stats<-renderDataTable({

      stat()

    })
    ########
    output$statsnearest<-renderDataTable({

      statnearest()

    })
    ################################
    output$graph<-renderPlot({


      if(is.null(input$samplesize)==TRUE)
        return("Please Select and Submit Your Conditions.")
      result<-drawsample()
      result$graph

    })

    output$info<-renderText({
      paste(p(strong('Package:'), "drawsample"),
 p(strong('Package Description:'), "draw_sample, function takes a sample of the specified
 sample size,skewness, and kurtosis form a data set (dist) with or without
 resampling. draw_sample_n function is the quicker version of function.
 The samples drawn with draw_sample_n will however be not as exact as possible.
 By modifying the location and delta_var arguments, the kurtosis and skewness of the
 distribution can be made even more acceptable.",p(strong('Package Author:'),"Kubra Atalay Kabasakal <katalay@hacettepe.edu.tr>"),p(strong('Application Developer:'),"Huseyin YILDIZ <huseyinyildiz35@gmail.com>"))



      )
    })


    output$downloadsample <- downloadHandler(
      filename = function() {
        paste("drawedsample", ".xlsx", sep="")
      },
      content = function(file) {
        xlsx::write.xlsx(draw(), file,row.names = FALSE)
      }
    )

    output$downloadstats <- downloadHandler(
      filename = function() {
        paste("statistics", ".xlsx", sep="")
      },
      content = function(file) {
        xlsx::write.xlsx(stat(), file,row.names = FALSE)
      }
    )

    #####################
    output$downloadsamplenearest <- downloadHandler(
      filename = function() {
        paste("drawedsample", ".xlsx", sep="")
      },
      content = function(file) {
        xlsx::write.xlsx(drawnearest(), file,row.names = FALSE)
      }
    )

    output$downloadstatsnearest <- downloadHandler(
      filename = function() {
        paste("statistics", ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(statnearest(), file,row.names = FALSE)
      }
    )



  }

  shiny::shinyApp(ui, server)

}





