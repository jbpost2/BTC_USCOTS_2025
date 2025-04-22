# App to compare a user selected line to the least squares regression line
#Justin Post - USCOTS 2025
library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyjs)
library(tidyverse)
library(plotly)
library(gridExtra)

iris <- iris[1:50,]

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel(title="Exploring Least Squares"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h2("Create your own line!")
      ), 
      fluidRow(
        sliderInput("slr_int", 
                    "Intercept",
                    min = -2,
                    max = 2,
                    value = 0,
                    ticks = FALSE,
                    step = 0.01)
      ),
      fluidRow(
          sliderInput("slr_slope", 
                      "Slope",
                      min = -0.25,
                      max = 2.25,
                      value = 1,
                      ticks = FALSE)
      ),
      fluidRow(
          checkboxInput("SS_check_box", "Show Sum of Squares")
      ),
      fluidRow(
        checkboxInput("SLR_check_box", "Show Least Squares Line")
      )
    ),
    mainPanel(
      fluidRow(
        tabBox(
          id = "tabset1",
          width = 12,
          tabPanel("Scatter Plot with Line(s)", 
                   plotlyOutput("slr_scatter"),
          ),
          tabPanel("Residual Plot(s)", 
                   plotlyOutput("slr_residual"),
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          conditionalPanel("input.SS_check_box",
                           tableOutput("SS_info")),
          conditionalPanel("input.SLR_check_box",
                           tableOutput("slr_line")
          )
        )
      )
    )
  )
)

# Create the scatterplot and such
server <- function(input, output) {

  # #update the user slider for the intercept
  # observeEvent(input$slr_sample, {
  #   #grab data
  #   slr_data <- sample_slr$slr_data
  #   #grab the fit and summaries needed
  #   fit <- sample_slr$slr_ls
  #   coefs <- coef(fit)
  #   sigma <- summary(fit)$sigma
  #   #grab x-values to find max y-hat
  #   x_values <- slr_data |> 
  #     pull(input$slr_x)
  #   #find the y-intercept value to compare +/- 3 SE of line
  #   int_low <- coefs[1]-3*sigma
  #   int_high <- coefs[1]+3*sigma
  #   #find the max y-hat value and min y-hat value
  #   end_low <- min(coefs[1] + coefs[2]*x_values) -3*sigma
  #   end_high <- max(coefs[1] + coefs[2]*x_values) +3*sigma
  #   #set min and max for ease below
  #   min <- floor(min(int_low, end_low))
  #   max <- ceiling(max(int_high, end_high))
  #   updateSliderInput(session, 
  #                     "slr_int", 
  #                     min = min, 
  #                     max = max, 
  #                     value = round(slr_data |> 
  #                                     pull(input$slr_y) |>
  #                                     mean()),
  #                     step = (max-min)/1000)
  #   slope_min <- (ceiling(max(int_high, end_high))- floor(min(int_low, end_low)))/(min(x_values)-max(x_values))
  #   if(abs(slope_min) < 1) {
  #     slope_min <- signif(slope_min, 3)
  #   } else{
  #     slope_min <- ceiling(slope_min)
  #   }
  #   slope_max <- -slope_min
  #   updateSliderInput(session,
  #                     "slr_slope",
  #                     min = slope_min,
  #                     max = slope_max,
  #                     value = 0,
  #                     step = (slope_max-slope_min)/1000)
  # })
  # 

  #Create graph
  output$slr_scatter <- renderPlotly({
    #data and user values for line
    user_line <- function(x){
      input$slr_int + input$slr_slope * x
    }
    colors <- c("User Line" = "darkgreen", "Least Squares Line" = "blue")
    #values for plotting purposes
    x_values <- iris$Sepal.Length
    x_min <- min(x_values)
    x_max <- max(x_values)
    y_min <- min(c(user_line(x_min), user_line(x_max)))
    y_max <- max(c(user_line(x_min), user_line(x_max)))
    
    #user stuff
    user_y <- user_line(x_values)
    true_y <- iris$Sepal.Width
    user_resids <- true_y - user_y

    #slr stuff
    fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
    coefs <- coef(fit)
    ls_y <- coefs[1] + coefs[2]*x_values
    ls_resids <- true_y - ls_y
    
    #base plot
    g <- ggplot(iris) +
      geom_point(aes(x = Sepal.Length, 
                     y = Sepal.Width))
    #user plot
    if(input$SS_check_box){
      g1 <- g + 
        geom_rect(data = data.frame(xmin = x_values, ymin = true_y, ymax= user_y, xmax = x_values+abs(user_resids)), 
                  mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  color = "black",
                  fill = "yellow",
                  alpha = 0.1,
                  linetype = "dashed")
      g2 <- g +
        geom_rect(data = data.frame(xmin = x_values, ymin = true_y, ymax= ls_y, xmax = x_values+abs(ls_resids)), 
                  mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  color = "black",
                  fill = "yellow",
                  alpha = 0.1,
                  linetype = "dashed")
    } else {
      g1 <- g
      g2 <- g
    }
    #add lines
    g1 <- g1 + 
      geom_line(data = data.frame(x = seq(from = x_min, to = x_max, length = 500), 
                                  y = user_line(seq(from = x_min, to = x_max, length = 500))), 
                aes(x = x, 
                    y = y))
    g2 <- g2 + 
      geom_smooth(method = "lm",
                    se = FALSE,
                    aes(x = Sepal.Length,
                        y = Sepal.Width))
    
    tooltip <- c("x", "y")
    
    #############
    #create plots
    if(!input$SLR_check_box){
      ggplotly(g1, tooltip = tooltip)
    } else if(input$SLR_check_box){
      subplot(ggplotly(g1, tooltip = tooltip), 
              ggplotly(g2, tooltip = tooltip), 
              nrows = 1)
    } 
  })
  
  output$SS_info <- renderTable({
    user_line <- function(x){
      input$slr_int + input$slr_slope * x
    }
    colors <- c("User Line" = "darkgreen", "Least Squares Line" = "blue")
    #values for plotting purposes
    x_values <- iris$Sepal.Length
    x_min <- min(x_values)
    x_max <- max(x_values)
    y_min <- min(c(user_line(x_min), user_line(x_max)))
    y_max <- max(c(user_line(x_min), user_line(x_max)))
    
    #user stuff
    user_y <- user_line(x_values)
    true_y <- iris$Sepal.Width
    user_resids <- true_y - user_y
    
    #slr stuff
    fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
    coefs <- coef(fit)
    ls_y <- coefs[1] + coefs[2]*x_values
    ls_resids <- true_y - ls_y
    
    results <- data.frame("Line" = "User Line", "DF" = nrow(iris)-2, "SSE" = round(sum(user_resids^2), 2), "MSE" = round(sum(user_resids^2)/(nrow(iris)-2), 2), "RMSE" = round(sqrt(sum(user_resids^2)/(nrow(iris)-2)), 2))
    if(input$SLR_check_box){
      results[2, ] <- c("Least Squares Line", 
                        nrow(iris)-2,
                        round(sum(ls_resids^2), 2), 
                        round(sum(ls_resids^2)/(nrow(iris)-2), 2),
                        round(sqrt(sum(ls_resids^2)/(nrow(iris)-2)), 2))
    }
    results[, c(1,3)]
  }, digits = 2)
  
  
  #create ls output
  output$slr_line <- renderTable({
    if (input$SLR_check_box){
      #find the SSE for the user line
      fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
      temp_df <- as.data.frame(summary(fit)$coefficients)
      info <- tibble(Parameter = c("Intercept", "Slope"),
                         "User Value" = c(input$slr_int, input$slr_slope),
                         "SLR Value" = round(fit$coef, 4))
      info
    } else {
      NULL
    }
  }, digits = 4)
  
  
  
  #Create graph
  output$slr_residual <- renderPlotly({
    user_line <- function(x){
      input$slr_int + input$slr_slope * x
    }
    #values for plotting purposes
    x_values <- iris$Sepal.Length
    x_min <- min(x_values)
    x_max <- max(x_values)
    y_min <- min(c(user_line(x_min), user_line(x_max)))
    y_max <- max(c(user_line(x_min), user_line(x_max)))
    
    #user stuff
    user_y <- user_line(x_values)
    true_y <- iris$Sepal.Width
    user_resids <- true_y - user_y
    
    #slr stuff
    fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
    coefs <- coef(fit)
    ls_y <- coefs[1] + coefs[2]*x_values
    ls_resids <- true_y - ls_y
    
    #create one resid plot
    resid_df <- data.frame(x = x_values, y = user_resids)
    resid_df2 <- data.frame(x = x_values, y = ls_resids)
    
    #user plot
    g <- ggplot(resid_df, aes(x = x, y = y)) +
      geom_point() +
      geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0) +
      ylab("Residual")
    #if 0 not included, set the min or max
    if(sum(resid_df$y < 0) == length(resid_df$y)){
      g <- g +
        scale_y_continuous(limits = c(min(resid_df$y), 0.1))
    } else if(sum(resid_df$y > 0) == length(resid_df$y)){
      g <- g +
        scale_y_continuous(limits = c(-0.1, max(resid_df$y)))
    }
    
    if(!input$SLR_check_box){
    tooltip <- c("x", "y")
    ggplotly(g, tooltip = tooltip)
    } else {
      g2 <- ggplot(resid_df2, aes(x = x, y = y)) +
        geom_point() +
        geom_segment(x = min(x_values), xend = max(x_values), y = 0, yend = 0)
      tooltip <- c("x", "y")
      subplot(ggplotly(g, tooltip = tooltip), 
              ggplotly(g2, tooltip = tooltip), 
              nrows = 1)
    }
  })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
