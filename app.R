########################################################
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Shiny app by Madisyn Rust
# Nov 1, 2026
#
# Shows factors contributing to fatal crashes in Texas in 2023
#
# Deployed at ___________
# Source code at ____________
#
############################################################

# libraries
library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)

##########################################################

# categories to remove entirely
unreported <- "Not Reported|Unknown|Not in State Inventory"

# import data and filter for Texas only

# 2023 accident data
acc <- read.csv("accident2023.csv", stringsAsFactors=F)
names(acc)<-toupper(names(acc))
acc<- acc[acc$STATENAME=="Texas", c("ST_CASE", "ROUTENAME", "LGT_CONDNAME")]
acc <- acc[!grepl(unreported, acc$ROUTENAME) &
             !grepl(unreported, acc$LGT_CONDNAME), ]


# 2023 impairment data
imp <- read.csv("drimpair2023.csv", stringsAsFactors=F)
names(imp)<-toupper(names(imp))
imp<- imp[imp$STATENAME=="Texas", ]
imp<- data.frame(ST_CASE=imp$ST_CASE, FACTOR=imp$DRIMPAIRNAME,
                 stringsAsFactors = F)
imp <- imp[!grepl(unreported, imp$FACTOR), ]


# 2023 distraction data
dis <- read.csv("distract2023.csv", stringsAsFactors=F)
names(dis)<- toupper(names(dis))
dis<- dis[dis$STATENAME=="Texas", ]
dis<- data.frame(ST_CASE=dis$ST_CASE, FACTOR=dis$DRDISTRACTNAME,
                 stringsAsFactors = F)
dis <- dis[!grepl(unreported, dis$FACTOR), ]


# heatmap of counts
make_heat<- function(df, xvar, yvar, xlab, ylab, ttl){
  tab<-as.data.frame(table(x=df[[xvar]], y=df[[yvar]]),
                     stringsAsFactors=F)
  ord<-names(sort(tapply(tab$Freq, tab$y, sum)))
  tab$y<-factor(tab$y, levels=ord)
  
  ggplot(tab, aes(x=x,y=y, fill=Freq))+
    geom_tile(color="white")+
    geom_text(aes(label=ifelse(Freq>0, Freq, "")), size=3.5)+
    scale_fill_gradient(low="white", high="red3", name="Count")+
    scale_x_discrete(labels=scales::label_wrap(12))+
    scale_y_discrete(labels=scales::label_wrap(35))+
    labs(
      title=ttl, 
      x=xlab,
      y=ylab)+
    theme_minimal(base_size=14)+
    theme(panel.grid = element_blank())
}

route_choices <- sort(unique(acc$ROUTENAME))

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("Texas 2023 Fatal Crashes"),
  
  tabsetPanel(
    
    # tab 1: route vs lighting
    tabPanel("Route vs. Lighting",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("routes1", "Route Types to Include:",
                                    choices = route_choices,
                                    selected = route_choices)
               ),
               mainPanel(
                 plotOutput("route_light_plot", height = "500px"),
                 textOutput("route_light_summary")
               )
             )
    ),
    
    # tab 2: driver factor
    tabPanel("Driver Factors",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("factor", "Driver Factor:",
                              choices = c("Driver Impairment", "Driver Distraction"),
                              selected = "Driver Impairment"),
                 
                 selectInput("by", "Compare Driver Factor With:",
                             choices = c("Route Type" = "ROUTENAME",
                                         "Lighting Condition" = "LGT_CONDNAME"),
                             selected = "ROUTENAME"),
                 
                 checkboxGroupInput("routes2", "Route Types to Include:",
                                    choices = route_choices,
                                    selected = route_choices)
               ),
               mainPanel(
                 plotOutput("driver_plot", height = "500px"),
                 textOutput("driver_summary")
         )
       )
    )
  )
)

# server

server<-function(input, output, session){
  
  # tab 1
  
  # crashes filtered by routes chosen
  acc_1<-reactive({
    validate(need(length(input$routes1)>0, "Select at least one route type."))
    d<-acc[acc$ROUTENAME %in% input$routes1, ]
    validate(need(nrow(d)>0, "No crashes match this selection."))
    d
  })
  
  # plot 1: route vs lighting
  output$route_light_plot<-renderPlot({
    make_heat(acc_1(), "ROUTENAME", "LGT_CONDNAME",
              "Route Type", "Lighting Condition",
              "Fatal Crashes by Route Type and Lighting")
  })
  
  
  # tab 2
  
  # drivers and crash
  driver_f<-reactive({
    drv<-if(input$factor=="Driver Impairment") imp else dis
    a<- acc[acc$ROUTENAME %in% input$routes2, ]
    d<- inner_join(a, drv, by="ST_CASE")
    validate(need(nrow(d)>0, "No drivers match this selection."))
    d
  })
  
  # plot 2: driver factor vs route
  output$driver_plot<-renderPlot({
    by_label<- if (input$by =="ROUTENAME") "Route Type" else "Lighting Condition"
    make_heat(driver_f(), input$by, "FACTOR",
              by_label, input$factor,
              paste(input$factor, "by", tolower(by_label)))
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
