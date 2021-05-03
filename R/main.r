#' @import bnutil
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @export


#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({

    sidebarLayout(

      sidebarPanel(
                     helpText("Spline regression"),
                     selectInput("whatref", "Use as reference:", c()),
                     selectInput("series", "Series in", c("Columns", "Rows")),
                     numericInput("spar", "Smoothing parameters", value = 0.7),
                     checkboxInput("allKnots", "Use max knots", value = TRUE),
                     numericInput("nknots", "Number of knots", value = 10),
                     actionButton("start", "Run"),
                     verbatimTextOutput("status")),
      mainPanel(
        sliderInput("xy", "XY set", min = 1, max = 10, step =1, value = 1),
        plotOutput("pfit"),
        plotOutput("pres")
      )
    )
  })

  getRunFolderReactive = context$getRunFolder()
  getStepFolderReactive = context$getFolder()
  getDataReactive = context$getData()

  observe({
    getData=getDataReactive$value
    if (is.null(getData)) return()

    getRunFolder = getRunFolderReactive$value
    if(is.null(getRunFolder)) return()

    getStepFolder = getStepFolderReactive$value
    if(is.null(getStepFolder)) return()

    bndata = getData()
    df = bndata$data

    if(bndata$hasXAxis){
      updateSelectInput(session, "whatref", choices = "X-Axis")
      if(length(levels(factor(df$QuantitationType))) > 1){
        stop("No more than 1 qunatitation type allowed with X-Axis as reference.")
      }

    } else {
      qt = levels(factor(df$QuantitationType))
      if (length(qt) != 2){
        stop("No X-axis, need exactly 2 quantitation types")
      }
      updateSelectInput(session, "whatref", choices = qt)
    }

    df_in = reactive({
      if(input$whatref == "") return()
      if(!bndata$hasXAxis){
        dfx = df %>% filter(QuantitationType == input$whatref) %>% select(rowSeq, colSeq, value)
        dfy = df %>% filter(QuantitationType != input$whatref) %>% select(rowSeq, colSeq, value)
        dfin = left_join(dfx, dfy, by = c("rowSeq", "colSeq"))
      } else {
        dfin = df%>%mutate(value.y = value, value.x = df[[bndata$xAxisColumnName]]) %>% select(rowSeq, colSeq, value.x, value.y)
      }
      if(input$series == "Columns"){
        dfin = dfin %>% mutate(xy = colSeq)
      } else if (input$series == "Rows"){
        dfin = dfin %>% mutate(xy = rowSeq)
      }
      updateSliderInput(session, "xy", min = 1, max = max(dfin$xy), step = 1)
      return(dfin)
    })

    df_plot = reactive({
      df_in() %>%
        filter(xy == input$xy) %>%
        do(splineOperator(., input$spar, allKnots = input$allKnots, input$nknots))
    })

    output$pfit = renderPlot({
      pfit = ggplot(df_plot(), aes(x = xFit, y = yFit)) + geom_line(colour = "blue")
      pfit = pfit + geom_point(aes(y = yData))
      print(pfit)
    })

    output$pres = renderPlot({
        pres = ggplot(df_plot(), aes(x = xFit, y = yRes)) + geom_point()
        pres = pres + geom_hline(yintercept = 0)
        print(pres)
    })

    output$status = renderText({
      if(input$start >0){
        showNotification(ui = "Running LOESS ...", type = "message", closeButton = FALSE, duration = NULL)
        result = df_in() %>% group_by(xy) %>% do(splineOperator(., spar = input$spar, allKnots = input$allKnots, input$nknots))
        result.out = result %>% ungroup() %>% select(rowSeq, colSeq, yFit, yRes)
        meta.result = data.frame(labelDescription = c("rowSeq", "colSeq", "yFit", "yRes"),
                              groupingType = c("rowSeq", "colSeq","QuantitationType", "QuantitationType"))
        aResult = AnnotatedData$new(data = result.out, metadata = meta.result)
        context$setResult(aResult)
        return("Done")
      } else {
        return(".")
      }
    })
  })
}

#' @export
shinyServerShowResults = function(input, output, session, context){

}
