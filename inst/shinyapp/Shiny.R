#' Shiny app to query and visualize Wunderground data.
#'
#'@import shiny
#'@importFrom ggplot2 ggplot aes_string geom_point geom_boxplot
#'@importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addMarkers
#'

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  # Application title
  titlePanel("Regional Weather Data (rpwsdata)", windowTitle = "Regional Weather Data (rpwsdata)"),
  helpText("Click on button Get Personal Weather Stations to get PWS for location, select upto three PWS and click button Get conditions to get current and historical weather data"),
  fluidRow(
    column(4,
           textInput("APIkey", label = h3("Enter API Key from Wunderground"), value = ""),
           textInput("citystate", label = h3("Enter City,State"), value = "San Francisco,CA"),
           actionButton("action", label = "Get Personal Weather Stations")
    ),
    column(8, 
           h3("Map of Personal Weather Stations"),
           leaflet::leafletOutput("mymap")
    )      
  ),

  fluidRow(
    column(4,
           actionButton("action_getweather", label = "Get conditions")
    ),
    column(8,
           h3("Selected Personal Weather Stations"),
           tableOutput("table")
    )
  ),

  fluidRow(
    column(4,
           selectInput("weather_condition", label = h3("Select Condition to Plot"),
                       choices = list("temp_f", "relative_humidity", "wind_mph", "pressure_in", "feelslike_f", "precip_today_in"))
    ),
    column(8,
           h3("Current weather across PWS"),
           plotOutput("CurrentPlot")
    )
  ),
    
  fluidRow(
    column(4,
           textInput("StartDt", label = h3("Enter Start Date"), value = "2017/02/01"),
           textInput("EndDt", label = h3("Enter End Date"), value = "2017/02/01"),
           selectInput("condition", label = h3("Select Condition to Plot"), 
                       choices = list("tempm","tempi","dewptm","dewpti","hum","pressurem","pressurei"), 
                       selected = "tempi")
    ),
    column(8, 
           h3("Historical Pattern of MicroClimate"),
           plotOutput("HistoricalPlot")
    )  
  )
  
)

server <- function(input, output, session) {
  
  WebQuery <- reactiveValues(
   obj1 = NULL,
   Whistory = NULL,
   Current_conditions = NULL,
   clickedMarker = NULL,
   selectedpws = data.frame()
  )
  
  pwsdf <- eventReactive(input$action, {
    WebQuery$obj1 <- new("WGround", APIkey=input$APIkey, city=strsplit(input$citystate,",")[[1]][1],state = strsplit(input$citystate,",")[[1]][2]) %>%
      getPWSinfo()
    WebQuery$obj1@pws
  })
  
  select_pws <- eventReactive(input$mymap_marker_click,{
    WebQuery$clickedMarker <- input$mymap_marker_click
    WebQuery$selectedpws <- rbind(WebQuery$selectedpws,WebQuery$obj1@pws[which(WebQuery$obj1@pws$lon == WebQuery$clickedMarker$lng & WebQuery$obj1@pws$lat == WebQuery$clickedMarker$lat), c("id","neighborhood")])
    WebQuery$selectedpws <- tail(WebQuery$selectedpws, n = 3L)
  })
  
  observeEvent(input$mymap_click,{
    WebQuery$clickedMarker <- NULL
    })
  
  output$mymap <- leaflet::renderLeaflet({
    leaflet::leaflet(pwsdf()) %>%
      leaflet::addTiles() %>%
      leaflet::addMarkers(lng = ~lon, lat = ~lat, popup = ~id)
  })
  
  output$table <- renderTable(select_pws())
  
  currentconditions <- eventReactive(input$action_getweather, {
    WebQuery$obj1@selectedpws <- WebQuery$selectedpws
    WebQuery$obj1 <- CurrentWeatherbyPWS(WebQuery$obj1)
    WebQuery$obj1@conditions
  })
  
  output$CurrentPlot <- renderPlot({
    ggplot2::ggplot(currentconditions(),ggplot2::aes_string(x = "station_id",y = input$weather_condition)) + ggplot2::geom_point(color="red")
  })

  historicalconditions <- eventReactive(input$action_getweather, {
    WebQuery$obj1@selectedpws <- WebQuery$selectedpws
    WebQuery$Whistory <- getHistoricalWeather(WebQuery$obj1,startDt=input$StartDt,endDt=input$EndDt)
  })
  
  output$HistoricalPlot <- renderPlot({
    ggplot2::ggplot(historicalconditions(),ggplot2::aes_string(x="ID", y=input$condition)) + ggplot2::geom_boxplot(width=0.5)
  })
  
}

shinyApp(ui, server)
