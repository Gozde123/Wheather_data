# weather data app
library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(knitr)
library(rsconnect)
library(psych)
dt1<-read.csv("GlobalWeatherRepository.csv")
set.seed(292)
rand_number_1 <- sample(x = 1:580, size = 20)
dt_plot<-dt1[rand_number_1,1:41]
dt_p<-dt1[,c(8,9,11,12,13,15,19,20,21,22,25,28,29,30,31,32,33,34,35)]
yaxis<-colnames(dt_p)
yaxis<-c("Temperature Celsius","Temperature Fahrenheit","Wind MPH","Wind KPH","Wind Degree","Pressure MB","Humidity","Cloud","Feels like Celsius","Feels Like Fahrenheit",
         "UV Index","Carbon Monoxide","Ozone","Nitrogen Dioxide","Sulphur Dioxide","PM2.5","PM10","EPA Index","GB Defra Index")
colChoices<-c("brown","pink","green","black","red","purple",
              "yellow","blue","orange","gray","darkred",
              "darkblue","magenta","lightsalmon")

ycorr<-dt1[,c(8,11,15,19,21,25,28,29,30,31,34)]
xcorr<-dt1[,c(8,11,15,19,21,25,28,29,30,31,34)]


ui<-fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("WEATHER DATA"),
  
  sidebarLayout(
    
    
    sidebarPanel(
      selectInput("ysel",label="Select y axis for plot:",choices = yaxis),
      selectInput("cl",label="Select color choices",choices=colChoices),
      actionButton("refreshPlot", label = "Refresh")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("General Information",verbatimTextOutput("general")),
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Summary", imageOutput("summary"))
        
      )
    )
  ),
  
  titlePanel("Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ycor",label="Select the y for correlation matrix",choices=colnames(ycorr)),
      selectInput("xcor",label="Select the x for correlation matrix",choices=colnames(xcorr)),
      actionButton("corbut",label="Calculate the Correlation")
      
    ),
    mainPanel (
      tabsetPanel(
        tabPanel("Correlation Matrix", tableOutput("table1")),
        tabPanel("Linear Regression",tableOutput("table2"))
      )
    )
  ),
  titlePanel("Power BI"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel (
      tabsetPanel(
        tabPanel("Made by Power BI",imageOutput("im1"))
      )
    )
  )
)
general1 <- function() {
  general_info <- c(
    "General Information",
    "Country: Country of the weather data",
    "Location Name: Name of the location (city)",
    "Latitude: Latitude coordinate of the location",
    "Longitude: Longitude coordinate of the location",
    "Timezone: Timezone of the location",
    "Last Updated Epoch: Unix timestamp of the last data update",
    "Last Updated: Local time of the last data update",
    "Temperature Celsius: Temperature in degrees Celsius",
    "Temperature Fahrenheit: Temperature in degrees Fahrenheit",
    "Condition Text: Weather condition description",
    "Wind MPH: Wind speed in miles per hour",
    "Wind KPH: Wind speed in kilometers per hour",
    "Wind Degree: Wind direction in degrees",
    "Wind Direction: Wind direction as a 16-point compass",
    "Pressure MB: Pressure in millibars",
    "Humidity: Humidity as a percentage",
    "Cloud: Cloud cover as a percentage",
    "Feels Like Celsius: Feels-like temperature in Celsius",
    "Feels Like Fahrenheit: Feels-like temperature in Fahrenheit",
    "UV Index: UV Index",
    "Air Quality Carbon Monoxide: Air quality measurement: Carbon Monoxide",
    "Air Quality Ozone: Air quality measurement: Ozone",
    "Air Quality Nitrogen Dioxide: Air quality measurement: Nitrogen Dioxide",
    "Air Quality Sulphur Dioxide: Air quality measurement: Sulphur Dioxide",
    "Air Quality PM2.5: Air quality measurement: PM2.5",
    "Air Quality PM10: Air quality measurement: PM10",
    "Air Quality US EPA Index: Air quality measurement: US EPA Index",
    "Air Quality GB Defra Index: Air quality measurement: GB DEFRA Index"
  )
  
  general_info <- paste(general_info, collapse = "\n")
  
  note <- "Note: This is general weather information for reference."
  
  text <- paste(general_info, note, sep = "\n\n")
  
  return(text)
  
}




server <- function(input, output) {
  
  plot1 <- eventReactive(input$refreshPlot, {
    if(input$ysel == "Temperature Celsius") {
      ggplot(data = dt_plot, aes(x = reorder(country, temperature_celsius), y = temperature_celsius)) +
        geom_col(fill = input$cl) +
        labs(title = "Temperature of Celsius by Country") +
        xlab("Country") + ylab("Temperature of Celsius") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Wind MPH" ) {
      ggplot(data = dt_plot, aes(x = reorder(country, wind_mph), y = wind_mph)) +
        geom_col(fill = input$cl) +
        labs(title = "Wind Speed in Miles per hour by Country") +
        xlab("Country") + ylab("Wind Speed in Miles per hour") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Wind KPH" ) {
      ggplot(data = dt_plot, aes(x = reorder(country, wind_kph), y = wind_kph)) +
        geom_col(fill = input$cl) +
        labs(title = "Wind Speed in Kilometers per hour by Country") +
        xlab("Country") + ylab("Wind Speed in Kilometers per hour") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Wind Degree") {
      ggplot(data = dt_plot, aes(x = reorder(country, wind_kph), y = wind_kph)) +
        geom_col(fill = input$cl) +
        labs(title = "Wind Direction in Degrees by Country") +
        xlab("Country") + ylab("Wind Direction in Degrees") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Temperature Fahrenheit") {
      ggplot(data = dt_plot, aes(x = reorder(country, temperature_fahrenheit), y = temperature_fahrenheit)) +
        geom_col(fill = input$cl) +
        labs(title = "Temperature Fahrenheit by Country") +
        xlab("Country") + ylab("Temperature Fahrenheit") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel ==  "Pressure MB" ) {
      ggplot(data = dt_plot, aes(x = reorder(country, pressure_mb), y = pressure_mb)) +
        geom_col(fill = input$cl) +
        labs(title = "Pressure in Millibars by Country") +
        xlab("Country") + ylab("Pressure in Millibars") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Humidity"  ) {
      ggplot(data = dt_plot, aes(x = reorder(country, humidity), y = humidity)) +
        geom_col(fill = input$cl) +
        labs(title = "Humidity by Country") +
        xlab("Country") + ylab("Humidity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Cloud" ) {
      ggplot(data = dt_plot, aes(x = reorder(country, cloud), y = wind_kph)) +
        geom_col(fill = input$cl) +
        labs(title = "Cloud Cover as a Percentage by Country") +
        xlab("Country") + ylab("Cloud Cover as a Percentage") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Feels like Celsius" ) {
      ggplot(data = dt_plot, aes(x = reorder(country,feels_like_celsius), y = feels_like_celsius)) +
        geom_col(fill = input$cl) +
        labs(title = "Feels-like temperature in Celsius by Country") +
        xlab("Country") + ylab("Feels-like temperature in Celsius") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Feels Like Fahrenheit") {
      ggplot(data = dt_plot, aes(x = reorder(country, feels_like_fahrenheit), y = feels_like_fahrenheit)) +
        geom_col(fill = input$cl) +
        labs(title = "Feels-like temperature in Fahrenheit by Country") +
        xlab("Country") + ylab("Feels-like temperature in Fahrenheit") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "UV Index" ) {
      ggplot(data = dt_plot, aes(x = reorder(country, uv_index), y =uv_index )) +
        geom_col(fill = input$cl) +
        labs(title = "UV Index by Country") +
        xlab("Country") + ylab("UV Index") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Carbon Monoxide") {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_Carbon_Monoxide), y = air_quality_Carbon_Monoxide)) +
        geom_col(fill = input$cl) +
        labs(title = " Carbon Monoxide by Country") +
        xlab("Country") + ylab("Carbon Monoxide") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Ozone") {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_Ozone), y = air_quality_Ozone)) +
        geom_col(fill = input$cl) +
        labs(title = "Ozone by Country") +
        xlab("Country") + ylab("Ozone") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Nitrogen Dioxide") {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_Nitrogen_dioxide), y = air_quality_Nitrogen_dioxide)) +
        geom_col(fill = input$cl) +
        labs(title = " Nitrogen Dioxide by Country") +
        xlab("Country") + ylab(" Nitrogen Dioxide") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$ysel == "Sulphur Dioxide" ) {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_Sulphur_dioxide), y = air_quality_Sulphur_dioxide)) +
        geom_col(fill = input$cl) +
        labs(title = "Sulphur Dioxide by Country") +
        xlab("Country") + ylab("Sulphur Dioxide") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }  else if(input$ysel == "PM2.5") {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_PM2.5), y = air_quality_PM2.5)) +
        geom_col(fill = input$cl) +
        labs(title = "Air quality measurement: PM2.5 by Country") +
        xlab("Country") + ylab("Air quality measurement: PM2.5") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }  else if(input$ysel == "PM10") {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_PM10), y = air_quality_PM10)) +
        geom_col(fill = input$cl) +
        labs(title = "Air quality measurement: PM10 by Country") +
        xlab("Country") + ylab(" Air quality measurement: PM10") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }  else if(input$ysel =="EPA Index") {
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_us.epa.index), y = air_quality_us.epa.index)) +
        geom_col(fill = input$cl) +
        labs(title = "Air quality measurement: US EPA Index by Country") +
        xlab("Country") + ylab("Air quality measurement: US EPA Index") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }  else{
      ggplot(data = dt_plot, aes(x = reorder(country, air_quality_gb.defra.index), y = air_quality_gb.defra.index)) +
        geom_col(fill = input$cl) +
        labs(title = "Air quality measurement: GB DEFRA Index by Country") +
        xlab("Country") + ylab("Air quality measurement: GB DEFRA Index") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
  })
  
  cor1 <- eventReactive(input$corbut, {
    cor_data <- dt_p
    y_col <- input$ycor
    x_col <- input$xcor
    
    cor_result <- cor(cor_data[, y_col], cor_data[, x_col])
    return(cor_result)
  })
  
  output$table1 <- renderTable({
    data.frame(
      Correlation = cor1()
    )
  })
  
  lin1 <- eventReactive(input$corbut, {
    lin_data <- dt_p
    y_col <- input$ycor
    x_col <- input$xcor
    
    # Check if both x_col and y_col are numeric
    if (is.numeric(lin_data[[y_col]]) && is.numeric(lin_data[[x_col]])) {
      lin_result <- lm(lin_data[[y_col]] ~ lin_data[[x_col]], data = lin_data)
      return(summary(lin_result))  # Return summary of linear regression
    } else {
      return(NULL)  # Return NULL if either x_col or y_col is not numeric
    }
  })
  
  output$table2 <- renderTable({
    lin_summary <- lin1()
    if (!is.null(lin_summary)) {
      coef_table <- as.data.frame(coef(lin_summary))
      names(coef_table) <- "Coefficients"
      return(coef_table)
    } else {
      return(data.frame(Summary = "Both x_col and y_col must be numeric."))
    }
  })
  
  output$summary <- renderImage({
    list(src = "s1.png", width = 550, height = 400)
  }, deleteFile = FALSE)
  
  output$plot <- renderPlot(print(plot1()))
  output$general <- renderText(general1())
  
  output$im1 <- renderImage({
    list(src = "general_information.png", width = 750, height = 600)
  }, deleteFile = FALSE)
}
shinyApp(ui,server)
