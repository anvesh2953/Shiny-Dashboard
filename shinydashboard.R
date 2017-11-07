library(leaflet)
library(shiny)
library(shinydashboard)



#This is cool


df<-read.csv("test_setting.csv")
df2<-read.csv("testdata_SparkOutput.csv")

df$date<-as.Date(substr(df$timestamp,1,9),"%m/%d/%Y")
df$Logfile1 <- as.character(df$Logfile1)
df$Logfile2 <- as.character(df$Logfile2)

df$column1<-substr(df$Logfile1 ,nchar(df$Logfile1)-14, nchar(df$Logfile1))
df$column2<-substr(df$Logfile2 ,nchar(df$Logfile2)-14, nchar(df$Logfile2))

day<-paste(substr(df[,"column1"],1,4),substr(df[,"column1"],5,6),
           substr(df[,"column1"],7,8),sep='-')
times<-paste(substr(df[,"column1"],10,11),substr(df[,"column1"],12,13),substr(df[,"column1"],14,15),
             sep=':')
m<-paste(day,times)

day2<-paste(substr(df[,"column2"],1,4),substr(df[,"column2"],5,6),
            substr(df[,"column2"],7,8),sep='-')
times2<-paste(substr(df[,"column2"],10,11),substr(df[,"column2"],12,13),substr(df[,"column2"],14,15),
              sep=':')
m2<-paste(day2,times2)

log1<-strptime(m,"%Y-%m-%d %H:%M:%S")
log2<-strptime(m2,"%Y-%m-%d %H:%M:%S")


mindiff<-as.numeric(difftime(log2,log1,units="mins"))

df[,"minutes"]<-mindiff

df[,"good_condition"]<-df[,"minutes"]<=15
df$Longitude<-as.numeric(as.character(df$Longitude))
df$Latitude<-as.numeric(as.character(df$Latitude))

#Handling Outliers -------
temp<-df[df$StationaryTestID2=='5_POLIGONO108_20160109_141851','Longitude']
df[df$StationaryTestID2=='5_POLIGONO108_20160109_141851','Longitude']=df[df$StationaryTestID2=='5_POLIGONO108_20160109_141851','Latitude']
df[df$StationaryTestID2=='5_POLIGONO108_20160109_141851','Latitude']=temp

temp<-df[df$StationaryTestID2=='5_POLIGONO109_20160109_134351','Longitude']
df[df$StationaryTestID2=='5_POLIGONO109_20160109_134351','Longitude']=df[df$StationaryTestID2=='5_POLIGONO109_20160109_134351','Latitude']
df[df$StationaryTestID2=='5_POLIGONO109_20160109_134351','Latitude']=temp

temp<-df[df$StationaryTestID2=='5_POLIGONO109_20160109_134314','Longitude']
df[df$StationaryTestID2=='5_POLIGONO109_20160109_134314','Longitude']=df[df$StationaryTestID2=='5_POLIGONO109_20160109_134314','Latitude']
df[df$StationaryTestID2=='5_POLIGONO109_20160109_134314','Latitude']=temp

temp<-df[df$StationaryTestID2=='5_POLIGONO110_20160109_132756','Longitude']
df[df$StationaryTestID2=='5_POLIGONO110_20160109_132756','Longitude']=df[df$StationaryTestID2=='5_POLIGONO110_20160109_132756','Latitude']
df[df$StationaryTestID2=='5_POLIGONO110_20160109_132756','Latitude']=temp

temp<-df[df$StationaryTestID2=='5_POLIGONO110_20160109_132627','Longitude']
df[df$StationaryTestID2=='5_POLIGONO110_20160109_132627','Longitude']=df[df$StationaryTestID2=='5_POLIGONO110_20160109_132627','Latitude']
df[df$StationaryTestID2=='5_POLIGONO110_20160109_132627','Latitude']=temp

temp<-df[df$StationaryTestID2=='6_Lomas de Tonala_20151204_015901','Longitude']
df[df$StationaryTestID2=='6_Lomas de Tonala_20151204_015901','Longitude']=df[df$StationaryTestID2=='6_Lomas de Tonala_20151204_015901','Latitude']
df[df$StationaryTestID2=='6_Lomas de Tonala_20151204_015901','Latitude']=temp

#---------------- Outliers Handled

good_df<-df[df$good_condition,]
bad_df<-df[!df$good_condition,]
bad<-bad_df[order(bad_df$minutes,decreasing = T)[1:5],]
bad$Longitude<-as.numeric(as.character(bad$Longitude))
bad$Latitude<-as.numeric(as.character(bad$Latitude))


num_tests_LTE_STA<-nrow(df)
pass_tests_LTE_STA<-sum(df$TestResult6!="FAIL")
percent_pass_LTS_STA<-(pass_tests_LTE_STA/num_tests_LTE_STA)*100

net<-df$Network[df$TestResult6=="FAIL"]
#net<-net[!is.na(long)]

long<-as.numeric((as.character(df$Longitude[df$TestResult6=="FAIL"])))
net<-net[!is.na(long)]
long<-long[!is.na(long)]

lati<-as.numeric((as.character(df$Latitude[df$TestResult6=="FAIL"])))
lati<-lati[!is.na(lati)]


dframe<-data.frame("Latitude"=lati,"Longitude"=long, "Network"=net)


avg_ul<-mean(df2$ultime,na.rm = TRUE)
avg_dl<-mean(df2$dltime,na.rm = TRUE)
avg_ping<-mean(df2$Pingtime,na.rm = TRUE)
avg_mo<-mean(df2$motime,na.rm = TRUE)
avg_mt<-mean(df2$mttime,na.rm = TRUE)
avg_tot<-mean(df2$totaltime,na.rm = TRUE)

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
              fluidPage(leafletOutput("mymap"),box(title="Top 5 worst testing equipments",tableOutput("table2")),
                        box(selectInput("signal","Signal",c("PDLColor","PINGColor","PULColor","MOColor","MTColor"),selected = "PDLColor"),
                            selectInput("status","Status",c("GREEN","RED","YELLOW"),selected = "GREEN"),
                            dateRangeInput("date", strong("Date range"), start = "2016-02-25", end = "2016-03-03", min = "2015-01-01", max = "2016-03-03")),
                        box(title=tags$b("LTE Stationary Sites Tested/Passed"),tableOutput("table")),
                        box(plotOutput("bar2")),
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

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  points <- eventReactive(input$recalc, {
    cbind(as.numeric(df$Longitude),as.numeric(df$Latitude))
  }, ignoreNULL = FALSE)
  
 df1<- data.frame("No. of Tests"=num_tests_LTE_STA,"Passed tests"=pass_tests_LTE_STA,"Percent of passed tests"=percent_pass_LTS_STA)
 df2<- data.frame("LTE"=c(63.2,36.8),"UMTS"=c(54.9,45.1))
 #row.names(df)<-c("Stationary Tests","Mobility Tests","% Pass")
 row.names(df2)<-c("Green","Red")
 
 getColor <- function(dataf) {
   sapply(dataf$good_condition, function(good_condition) {
     
     if(!is.na(good_condition) && good_condition) {
       "green"
     }  else if(!is.na(good_condition) && !good_condition) {
       "red"
     }
     else{
       "Orange"
     }
     })
 }
 
 icons <- awesomeIcons(
   icon = 'ios-close',
   iconColor = 'black',
   library = 'ion',
   markerColor = getColor(bad_df)
 )
 
 #signal_df<-df[,input$signal]
 #new_df<-df[df[,input$signal]==input$status,]
 
 output$table<-renderTable(df1)
 output$table2<-renderTable(bad[,c('IMEI','minutes')])
  output$mymap <- renderLeaflet({
    leaflet(data = df[df$date<=input$date[2] & df$date>=input$date[1] & df[,input$signal]==input$status,]) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng= ~Longitude, lat=~Latitude, popup = ~StationaryTestID2)
  })
  output$text1=renderText(avg_ul)
  output$text2=renderText(avg_dl)
  output$avg_ping=renderText(avg_ping)
  output$avg_mo=renderText(avg_mo)
  output$avg_mt=renderText(avg_mt)
  output$avg_tot=renderText(avg_tot)
  
  output$bar=renderPlot(barplot(c(avg_ul,avg_dl,avg_ping,avg_mo,avg_mt,avg_tot), names = c("UL Time","DL_Time","Ping_Time",
                                                                     "MO_Time","MT_Time","Total_Time"),
          xlab = "Times", ylab = "Avg times", col = "lightblue",
          main = "Avg_Times"))
  
  mytable <- table(net)
  lbls <- paste(names(mytable), "\n", mytable, sep="")
  output$pie =renderPlot(pie(mytable, labels = lbls,
      main="Pie Chart of Networks\n (Failed)") )
  
  output$bar2=renderPlot(barplot(table(bad_df$Network),
                                xlab = "Network", ylab = "Frequency", col = "lightblue",
                                main = "Networks with Uplink time >15min"))
  
  output$plot2=renderPlot(plot(as.numeric(as.character(df$Latitude)),as.numeric(as.character(df$Longitude)),xlab="Latitude",ylab="Longitude",
       pch=ifelse(df$good_condition==TRUE, 19, 15),xlim=c(15,35),ylim=c(-105,-85),
       col=ifelse(df$good_condition==TRUE, "blue", "red")))
  output$rsrp=renderPlot(hist(as.numeric(bad_df$rsrp)))
  
}

shinyApp(ui, server)
