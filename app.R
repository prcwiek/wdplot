## server.R
## Weibull distribution plot in Shiny
## Author: Pawel Cwiek <pablorcw@users.noreply.github.com> [aut,cre]
## License: GPL-3

library(ggplot2)
library(shiny)

weibull_dist <- function(c = 7.0, k = 2.0, ws = 5.0){
  out <- round((k/c)*((ws/c)^(k-1))*exp(-(ws/c)^k),6)
  out
}

df_wd <- data.frame(ws=seq(0,30,0.01),p=seq(0,30,0.01))
df_wd[2:3000,2] <- weibull_dist(ws=df_wd[2:3000,1])
k_value <- 2.0
c_value <- 7.0
vave <- round(c_value*gamma(1+1/k_value),2)

ui <- fluidPage(
  
  includeCSS("www/style.css"),
  
  fluidRow(
    column(4,
           h1("Weibull Distribution"),
           h3("Create Weibull distribution graph"),
           numericInput("k", label = h5("Shape factor k"), value = 2.0, step = 0.1, min = 0),
           numericInput("c", label = h5("Scale factor c"), value = 7.0, step = 0.1, min = 0),
           sliderInput("range", label = h5("Range of wind speeds"),
                       min = 0, max = 30, value = c(0,30))
           ),
    column(6,
           plotOutput("wbplot"),
           textOutput("text1")
           )
    )
  
  # fluidRow(
  #   column(8, offset = 2,
  #          plotOutput("wbplot"),
  #          textOutput("text1")
  #          )
  #   )
)


server <- function(input, output, session) {
  
  prepare_data <- reactive({
    vmin <- input$range[1]
    vmax <- input$range[2]
    s_range <- vmin/0.01
    e_range <- vmax/0.01
    k_value <- input$k
    c_value <- input$c
    vave <- round(c_value*gamma(1+1/k_value),2)
    df_wd[2:3000,2] <- weibull_dist(ws=df_wd[2:3000,1], k = k_value, c = c_value)
    df_wdplot <- as.data.frame(df_wd[s_range:e_range,])
    df_wdplot
  })
  
  mean_ws <- reactive({
    k_value <- input$k
    c_value <- input$c
    vave <- round(c_value*gamma(1+1/k_value),2)
    vave
  })
  
  output$wbplot <- renderPlot({
    df_wdplot <- prepare_data()
    ggplot(df_wdplot, aes(x = ws, y = p)) +
      xlab("Wind speed (m/s)") +
      ylab("Probability") +
      geom_line(color = "#A52A2A", size=1)
    
  })
  
  output$text1 <- renderText({
    paste("Mean wind speed:", mean_ws(), "m/s")
    
  }) 
  
  # End application after closing a window or tab
  session$onSessionEnded(stopApp)

}

# Run the application 
shinyApp(ui = ui, server = server)

