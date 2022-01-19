# sample R + Shiny example for CS 424 Spring 2022 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize

temp = list.files(pattern="*.tsv")
allData2 <- lapply(temp, read.delim)
allData3 <- do.call(rbind, allData2)

# some of the temperature readings are 32 (invalid value) so lets remove any row that has one of those
allData <- subset(allData3, S1 != "32" & S2 != "32" & S3 != "32" & S4 != "32" & S5 != "32" & S6 != "32" & S7 != "32" )


# convert the dates to the internal format
allData$newDate <- as.Date(allData$Date, "%m/%d/%Y")
allData$Date <- NULL

# convert the temperatures from strings to numbers
allData$S1 <- as.numeric(as.character(allData$S1))
allData$S2 <- as.numeric(as.character(allData$S2))
allData$S3 <- as.numeric(as.character(allData$S3))
allData$S4 <- as.numeric(as.character(allData$S4))
allData$S5 <- as.numeric(as.character(allData$S5))
allData$S6 <- as.numeric(as.character(allData$S6))
allData$S7 <- as.numeric(as.character(allData$S7))



# convert the room codes to more memorable room names
names(allData)[names(allData)=="S6"] <- "Meeting Room"
names(allData)[names(allData)=="S5"] <- "Main Lab"
names(allData)[names(allData)=="S7"] <- "Machine Room"
names(allData)[names(allData)=="S3"] <- "Thin Rooms"
names(allData)[names(allData)=="S4"] <- "Ph.D. Room"
names(allData)[names(allData)=="S2"] <- "Classroom"
names(allData)[names(allData)=="S1"] <- "Work Room"


# Create the menu items to select the different years and the different rooms
listNames <- c(colnames(allData))
listNamesGood <- listNames[listNames != "Hour" & listNames != "newDate"]
years<-c(2005:2021)

# Create the shiny dashboard
ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Spring 2022 Example Dashboard"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,

                     sidebarMenu(
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                     
                     selectInput("Year", "Select the year to visualize", years, selected = 2021),
                     selectInput("Room", "Select the room to visualize", listNamesGood, selected = "Machine Room")
                     ),
    dashboardBody(
  fluidRow(
    column(2,
        fluidRow(
          box(title = "Plotting on a jpeg", solidHeader = TRUE, status = "primary", width = 12,
              plotOutput("jpeg", height = 200)
          )
          ),


        fluidRow(
          box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
              leafletOutput("leaf", height = 530)
          )
        )
    ),
    
    column(8,
       fluidRow(
            box( title = "Room Temperature Range over a Year", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist1", height = 200)
            )
        ), 
       fluidRow(
            box( title = "Room Temperature at Noon over a Year", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist2", height = 200)
            )
        ),
       fluidRow(
         box( title = "Heatmap of Room Temperature over a Year", solidHeader = TRUE, status = "primary", width = 12,
              plotOutput("hist0", height = 250)
         )
       )
   ),
   column(2,
        fluidRow(
            box(title = "Noon Temps as Box Plot", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist4", height = 200)
                )
            ),
        fluidRow(
             box(title = "Noon Temps as Bar Chart", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist3", height = 200)
                )
            ),
        fluidRow(
             box(title = "Noon Temps as Table", solidHeader = TRUE, status = "primary", width = 12,
                dataTableOutput("tab1", height = 200)
                )
             )
        )
    )
))

server <- function(input, output) {

# increase the default font size
theme_set(theme_grey(base_size = 14)) 
  
  
# calculate the values one time and re-use them in multiple charts to speed things up
justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})

# in 2017 it was y=justOneYear["Hour"] - needed to make a change for 2018

# create heat map for the given year and room and play with the colors to make it more readable
output$hist0 <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=newDate, color=justOneYear[,input$Room], y=Hour)) + 
        labs(x=paste("Day in", input$Year), y = "Hour of the Day") +  geom_point(alpha = 1/2, size = 4, shape=15) + scale_y_continuous() + 
    
        theme_dark(18) + theme(plot.background = element_rect(fill = "gray50")) + theme(axis.title = element_text(colour = "white")) +
            theme(axis.text = element_text(colour = "white")) + theme(panel.grid.major = element_line(colour = "white")) + 
            theme(panel.grid.minor = element_line(colour = "white")) +
            theme(legend.background = element_rect(fill="gray50")) +
            theme(legend.text = element_text(colour = "white"))  +
            theme(legend.title = element_text(colour = "white"))  +

        scale_colour_gradient2(low = "green", high = "red", limits=c(60, 90), midpoint=70, mid="yellow") +
            theme(legend.position="top") + labs(colour = "Temperature (F) ") +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
})


# show all of the temperatures for a given room for a given year
output$hist1 <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Room])) +
        labs(x=paste("Day in", input$Year), y = "Temperature (F)") + geom_point(alpha = 1/12) + ylim(55,90) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    })


# show a line graph of the temperatures at noon for a given room for a given year
output$hist2 <- renderPlot({
    newNoons <- newNoonsReactive()
    ggplot(newNoons, aes(x=newDate, y=newNoons[,input$Room])) +
        labs(x=paste("Day in", input$Year), y = "Temperature (F)") + geom_point() + geom_line() + ylim(55,90) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
})


# show a bar chart of the temperatures at noon for a given room for a given year
output$hist3 <- renderPlot({
    newNoons <-  newNoonsReactive()
    temperatures <- as.data.frame(table(newNoons[,input$Room]))
    temperatures$Var1 <- as.numeric(as.character(temperatures$Var1))

    ggplot(temperatures, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
        labs(x="Temperature (F)", y = "Count") + xlim(60,90)
})


# show box plot of the temperatures at noon for a given room for a given year
output$hist4 <- renderPlot({
    newTemps2 <- justOneYearReactive()
    ggplot(newTemps2, aes(x = "", y = newTemps2[,input$Room])) + geom_boxplot() + labs(y="Temperature (F)", x="") + ylim(55,90)
})


# use DT to help out with the tables - https://datatables.net/reference/option/
output$tab1 <- DT::renderDataTable(
    DT::datatable({ 
    newNoons <-  newNoonsReactive()
    temperatures <- as.data.frame(table(newNoons[,input$Room], dnn = list("Temperature")), responseName = "Count")
  }, 
  options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE 
    )
)

# read in a jpeg map of the lab to show the room layout and plot some text on it
output$jpeg <- renderPlot({
    # read in a jpg image
    jp <- jpeg::readJPEG('evl_2nd_floor.jpg')

    df <- data.frame(x = 1:10, y = 1:10) # set the range to be 1 to 10 in x and y for the image
    
    markerX = 0
    markerY = 0
    
    if (input$Room == "Classroom")
    {
      markerX = 3
      markerY = 4.5
    }
    else if (input$Room == "Ph.D. Room")
    {
      markerX = 5.25
      markerY = 4.5
    }
    else if (input$Room == "Thin Rooms")
    {
      markerX = 6.7
      markerY = 4.5
    }
    else if (input$Room == "Machine Room")
    {
      markerX = 8.75
      markerY = 4.5
    }
    else if (input$Room == "Work Room")
    {
      markerX = 3
      markerY = 7.1
    }
    else if (input$Room == "Meeting Room")
    {
      markerX = 6
      markerY = 7.1
    }
    else if (input$Room == "Main Lab")
    {
      markerX = 8.75
      markerY = 7.1
    }
    else
    {
      markerX = 0
      markerY = 0
    }

    ggplot(df, aes(x,y)) + geom_blank() + labs(x="", y = "") +
        annotation_custom(rasterGrob(jp, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 

        annotate("text", x = 3, y = 4.5, label = "Class\nRoom", colour = "black", fontface =2) +
        annotate("text", x = 5.25, y = 4.5, label = "Ph.D\nRoom", colour = "black", fontface =2) +
        annotate("text", x = 6.7, y = 4.5, label = "Thin\nRooms", colour = "black", fontface =2) +
        annotate("text", x = 8.75, y = 4.5, label = "Machine\nRoom", colour = "black", fontface =2) +
        annotate("text", x = 3, y = 7.1, label = "Work\nRoom", colour = "black", fontface =2) +
        annotate("text", x = 6, y = 7.1, label = "Meeting\nRoom", colour = "black", fontface =2) +
        annotate("text", x = 8.75, y = 6.5, label = "Main\nLab", colour = "black", fontface =2) +
      
      # show the selected room with a colored overlay based on input$Room
      annotate("rect", xmin = markerX-0.9, xmax = markerX+0.9, ymin = markerY-0.5, ymax = markerY+0.5, fill="green", alpha=0.3) +

        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
})

# add a leaflet map and put a marker on it at the location of the lab
# while not overly useful this can certainly be expanded upon
output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.647998, lat = 41.870, zoom = 17)
    map <- addMarkers(map, lng = -87.6477, lat = 41.8698, popup = "evl")
    map
})


}

shinyApp(ui = ui, server = server)
