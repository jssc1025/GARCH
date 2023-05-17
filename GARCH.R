library(shiny)
library(quantmod)
library(rugarch)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(e1071)
library(magrittr)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "GARCH Modeling Application"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("GARCH Model", tabName = "garch", icon = icon("line-chart")),
      menuItem("Model Evaluation", tabName = "evaluation", icon = icon("check")),
      menuItem("Forecast Table", tabName = "forecast", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "garch",
              h2("GARCH Modeling Application"),
              fluidRow(
                box(
                  title = "Inputs",
                  width = 4,
                  solidHeader = TRUE,
                  textInput("tickerInput", "Stock Ticker Symbol:", "AAPL"),
                  actionButton("runButton", "Run GARCH Model")
                ),
                box(
                  title = "Volatility Plot",
                  width = 8,
                  solidHeader = TRUE,
                  plotOutput("volatilityPlot")
                ),
                box(
                  title = "Model Fit",
                  width = 8,
                  solidHeader = TRUE,
                  plotOutput("modelFitPlot")
                )
              )
      ),
      
      tabItem(tabName = "evaluation",
              h2("Model Evaluation"),
              fluidRow(
                box(
                  title = "Model Evaluation",
                  width = 12,
                  solidHeader = TRUE,
                  dataTableOutput("modelEvaluation")
                )
              )
      ),
      
      tabItem(tabName = "forecast",
              h2("Forecast Table"),
              fluidRow(
                box(
                  title = "Forecast Table",
                  width = 12,
                  solidHeader = TRUE,
                  height = "350px",
                  dataTableOutput("forecastTable")
                )
              )
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 12,
            uiOutput("aboutContent")
          )
        )
      )
    )
  )
)
server <- function(input, output) {
  
  data <- reactiveValues(stockReturns = NULL, garchModels = NULL, garchForecasts = NULL)
  
  # Render about page content
  output$aboutContent <- renderUI({
    includeMarkdown("about.Rmd")
  })
  
  observeEvent(input$runButton, {
    stockData <- getSymbols(input$tickerInput, from = "2010-01-01", to = Sys.Date(), auto.assign = FALSE)
    data$stockReturns <- diff(log(Ad(stockData)))[-1]
    
    data$garchModels <- list(
      sGARCH_11 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0))),
      eGARCH = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0))),
      gjrGARCH = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0))),
      apARCH_11 = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0))),
      apARCH_21 = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(2, 1)), mean.model = list(armaOrder = c(1, 0))),
      eGARCH_21 = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2, 1)), mean.model = list(armaOrder = c(2, 0)), distribution.model = "sstd"),
      gjrGARCH_12 = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 2)), mean.model = list(armaOrder = c(1, 0)), distribution.model = "ged"),
      apARCH_22 = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(2, 2)), mean.model = list(armaOrder = c(2, 0)), distribution.model = "std"),
      apARCH_21 = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(2, 1)), mean.model = list(armaOrder = c(2, 0)), distribution.model = "sstd")
    )
    
    # Fit GARCH models and generate forecasts
    data$garchFits <- lapply(data$garchModels, function(spec) {
      ugarchfit(spec, data$stockReturns, solver.control = list(maxit = 500))
    })
    data$garchForecasts <- lapply(data$garchFits, function(fitModel) {
      ugarchforecast(fitModel, n.ahead = 250)
    })
    
    
    # Render model fit plot
    output$modelFitPlot <- renderPlot({
      actual <- as.numeric(data$stockReturns)
      predicted <- fitted(data$garchFits$sGARCH_11)
      data.frame(Time = 1:length(actual), Actual = actual, Predicted = predicted) %>%
        ggplot(aes(x = Time)) +
        geom_line(aes(y = Actual), colour = "blue") +
        geom_line(aes(y = Predicted), colour = "red") +
        labs(title = "Model Fit", y = "Returns", 
             colour = "Type") +
        scale_colour_manual(values = c("blue", "red"), labels = c("Actual", "Predicted"))
    })
    
    # Render model evaluation
    output$modelEvaluation <- renderDataTable({
      # Generate model evaluation metrics
      evaluationData <- data.frame(Model = character(), RMSE = numeric(), AIC = numeric(), BIC = numeric(),
                                   LogLikelihood = numeric(), HQIC = numeric(), SBC = numeric(),
                                   Mean = numeric(), SD = numeric(), Skewness = numeric(), Kurtosis = numeric(),
                                   stringsAsFactors = FALSE)
      
      for (i in 1:length(data$garchFits)) {
        fitModel <- data$garchFits[[i]]
        modelName <- names(data$garchFits)[i]
        
        # Calculate RMSE
        residuals <- residuals(fitModel, standardize = TRUE)
        rmse <- sqrt(mean(residuals^2))
        
        # Calculate log-likelihood
        logLikValue <- sum(log(density(residuals)$y))
        
        # Calculate AIC
        k <- sum(fitModel@fit$par != 0)
        n <- length(data$stockReturns)
        aic <- -2 * logLikValue + 2 * k
        
        # Calculate BIC
        bic <- -2 * logLikValue + k * log(n)
        
        # Calculate HQIC
        hqic <- -2 * logLikValue + 2 * k * log(log(n))
        
        # Calculate SBC
        sbc <- -2 * logLikValue + k * log(n) * (1 + log(log(n)) / n)
        
        # Calculate Mean, Standard Deviation, Skewness, and Kurtosis
        meanResidual <- mean(residuals)
        sdResidual <- sd(residuals)
        skewnessResidual <- skewness(residuals)
        kurtosisResidual <- kurtosis(residuals)
        
        # Append model evaluation metrics to the data frame (including new columns)
        evaluationData <- rbind(evaluationData, data.frame(Model = modelName, RMSE = rmse, AIC = aic, BIC = bic,
                                                           LogLikelihood = logLikValue, HQIC = hqic, SBC = sbc,
                                                           Mean = meanResidual, SD = sdResidual,
                                                           Skewness = skewnessResidual, Kurtosis = kurtosisResidual))
      }
      
      evaluationData
    })
    
    
    # Render volatility plot
    output$volatilityPlot <- renderPlot({
      plot(x = 1:250, y = sigma(data$garchForecasts$sGARCH_11), type = "l", ylim = c(0, max(sapply(data$garchForecasts, function(forecast) max(sigma(forecast))))), xlab = "Time Horizon", ylab = "Volatility", main = "Volatility Forecasts")
      colors <- rainbow(length(data$garchForecasts))
      for (i in 2:length(data$garchForecasts)) {
        lines(sigma(data$garchForecasts[[i]]), col = colors[i])
      }
      legend("topleft", legend = names(data$garchForecasts), fill = colors, cex = 0.8)
    })
    
    # Render forecast table
    output$forecastTable <- renderDataTable({
      forecastData <- do.call(cbind, lapply(data$garchForecasts, function(forecast) as.numeric(sigma(forecast))))
      colnames(forecastData) <- names(data$garchForecasts)
      forecastData <- data.frame(Time = 1:250, forecastData)
      
      # Format forecast data as percentages
      forecastData[, -1] <- lapply(forecastData[, -1], function(x) {
        paste0(format(round(x * 100, 2), nsmall = 2), "%")
      })
      
      forecastData
    })
  })
}

shinyApp(ui, server)