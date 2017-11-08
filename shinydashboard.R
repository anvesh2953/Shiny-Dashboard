library(leaflet)
library(shiny)
library(shinydashboard)
<<<<<<< HEAD
# Reading Files 
df<-read.csv("DashboardData_SparkOutput.csv")
#df2<-read.csv("testdata_SparkOutput.csv")
df$date<-as.Date(as.character(df$Date),"%Y%m%d")
# --- Read Files

# Convert Latitude and Longitude into numeric in order to pass it into leaflet(Map)
df$Longitude<-as.numeric(as.character(df$Longitude))
df$Latitude<-as.numeric(as.character(df$Latitude))
# Latitude, Longitude converted.

# No.of Test conducted and Tests passed
num_tests_LTE_STA<-nrow(df)
pass_tests_LTE_STA<-sum(df$TestResult6!="FAIL")
percent_pass_LTS_STA<-(pass_tests_LTE_STA/num_tests_LTE_STA)*100
#----Tests passed??

# Average ul,dl,Motime caliculation
avg_ul<-mean(df$ultime,na.rm = TRUE)
avg_dl<-mean(df$dltime,na.rm = TRUE)
avg_ping<-mean(df$Pingtime,na.rm = TRUE)
avg_mo<-mean(df$motime,na.rm = TRUE)
#avg_mt<-mean(df$mttime,na.rm = TRUE)
#avg_tot<-mean(df$totaltime,na.rm = TRUE)
# caliculated


# UI CODE (Essential for Shiny)
ui <- dashboardPage(
  dashboardHeader(title = "TAPAS SITE",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("LTE Stationary", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Signals", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(leafletOutput("mymap"),
                        box(selectInput("signal","Signal",c("PDLColor","PINGColor","PULColor","MOColor","MTColor"),selected = "PDLColor"),
                            selectInput("status","Status",c("GREEN","RED","YELLOW"),selected = "GREEN"),
                            dateRangeInput("date", strong("Date range"), start = "2017-11-01", end = "2017-11-07", min = "2017-10-29", max = "2017-11-07")),
                        box(title=tags$b("LTE Stationary Sites Tested/Passed"),tableOutput("table")),
                        box(plotOutput("bar"))
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidPage(
                box(plotOutput("rsrp")) 
                        
              )
      )
    )
  )
)

#Server Code (essential for Shiny)
server <- function(input, output) {
  
  
 df1<- data.frame("No. of Tests"=num_tests_LTE_STA,"Passed tests"=pass_tests_LTE_STA,"Percent of passed tests"=percent_pass_LTS_STA)

 
 output$table<-renderTable(df1)
 
  output$mymap <- renderLeaflet({
    leaflet(data = df[df$date<=input$date[2] & df$date>=input$date[1] & df[,input$signal]==input$status,]) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng= ~Longitude, lat=~Latitude, popup = ~StationaryTestID)
  })
  output$text1=renderText(avg_ul)
  output$text2=renderText(avg_dl)
  output$avg_ping=renderText(avg_ping)
  output$avg_mo=renderText(avg_mo)
  output$avg_mt=renderText(avg_mt)
  output$avg_tot=renderText(avg_tot)
  
  output$bar=renderPlot(barplot(c(avg_ul,avg_dl,avg_ping,avg_mo), names = c("UL Time","DL_Time","Ping_Time",
                                                                     "MO_Time"),
          xlab = "Times", ylab = "Avg times", col = "lightblue",
          main = "Avg_Times"))
  

}

shinyApp(ui, server)
