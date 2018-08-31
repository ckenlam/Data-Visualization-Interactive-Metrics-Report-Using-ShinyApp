
library(rCharts)
library(data.table)
library(plyr)
library(dplyr)
library(dygraphs)
library(reshape2)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinyURL)

credentials <- list("admin" = "e3274be5c857fb42ab72d786e281b4b8"
                    
                )


############################################################################################################




source("dataprocessing.R", local = TRUE)
rawdata<- read.csv("data/raw.csv")
rawdata$date_id<- as.Date(as.character(rawdata$date_id), format="%Y%m%d")
df<- rawdata[,c(1,6:12)]
df$month <-as.Date(cut(df$date_id, breaks="month"))
df$month <-as.character(df$month, format = "%Y-%m")
df$week   <-floor_date(df$date_id, unit="week")
df$month_num <-as.Date(cut(df$date_id, breaks="month"))
df$month_num <-as.character(df$month_num, format = "%m")
df$year_num <-as.Date(cut(df$date_id, breaks="year"))
df$year_num <-as.character(df$year_num, format = "%Y")
teams<- sort(unique(df$Team_Function))


# Define server logic required to draw a histogram
shinyServer(function(input, output){
   
  # Define and initialize reactive values
  
  values <- reactiveValues()
  values$teams <- teams
  
  # Create teams checkbox
  output$teams <- renderUI({
    checkboxGroupInput('teams', 'Teams', c('Team no.1', 'Team no.2', 'Team no.3', 'Team no.4', 'Team no.5'), selected=values$teams)
  })

  # Create date unit radiobutton
  output$unitradio<- renderUI({
    if(input$metric < 3 ){
    radioButtons("unit", label = "Display data by:",choices = list("Day" = 1, "Week" = 2,"Month" = 3, "Year-to-Year Comparison" = 4),selected = 3)
    } else
    radioButtons("unit", label = "Display data by:",choices = list("Day" = 1, "Week" = 2,"Month" = 3),selected = 3)  
  })
  
  # Add observers on clear and select all buttons
  observeEvent(input$clear_all, {
    values$teams <- c()
  })
  
  observeEvent(input$select_all, {
    values$teams <-teams
  })
  
  #filtering the data with the chosen range of dates
  filtered_data<- reactive({
  filter_agg_by_inputs(df, input$range[1], input$range[2], input$teams)
  })
  
  #aggregate the data by the chosen unit
df_date<- reactive({
  if(input$unit == 1){
    #by day
    filtered_data()[,c(1,2,5:8)]
     } else if(input$unit == 2){
    #by week
    filtered_data()[,c(10,2,5:8)]
     } else if(input$unit == 3){
    #by month
    filtered_data()[,c(9,2,5:8)]   
    } else
    #by year-to-year comparison
    filtered_data()[,c(11,12,5:8)]
})


  #define the columns to be used based on the metric selected
col_sel<- reactive({
  if(input$metric ==1){
   #calls offered
   c(1,2,3)
  } else if(input$metric ==2){
   #calls answered
   c(1,2,4)
  } else 
    c(1,2,3)
}) 

#Rename the label based on the metric selected
lab<- reactive({
  if(input$metric ==1){
    #calls offered
    'Call Offered Volume'
  } else if(input$metric ==2){
    #calls answered
    'Call Answered Volume'
  } else
    'Service Level'
})


#prepare the data frame for plotting
df_new<- reactive({
  df_date()[,col_sel()]
  })

df_preplot<- reactive({
  if(input$unit < 4){
  setNames(aggregate(df_new()[,3], by= list(df_date()[,1], df_date()[,2]), FUN = sum, na.rm=TRUE), c('date','team','calls'))
  } else 
  setNames(aggregate(df_new()[,3], by= list(df_date()[,1], df_date()[,2]), FUN = sum, na.rm=TRUE), c('month','year','calls'))
  
})
############################################################################################################


#This section is dedicated to the calculation of Service Level


#1) The Service Level calculation will start on the daily application data 
df_svl_app<-reactive({
  filtered_data()[,c(1,10,9,2,6:8,11,12)]
})
#2) Sum all the Service Level components to a daily team level
df_svl_team<- reactive({
  setNames(aggregate(df_svl_app()[,5:7], by= list(df_svl_app()[,1], df_svl_app()[,2],df_svl_app()[,3],df_svl_app()[,4]), FUN = sum, na.rm=TRUE), c('date_id','week','month','team','ans', 'abd_aft', 'ans_aft'))
})
#3) Calculate the daily Service Level by team
SVL<- reactive({
  ((df_svl_team()[,5] - df_svl_team()[,7])/(df_svl_team()[,5] + df_svl_team()[,6]))
})  
df_svl_daily<- reactive({
  cbind(df_svl_team()[,c(1,2,3,4)],SVL())
})
#4) Aggregate the SVL data by the chosen unit

df_dateSVL<- reactive({
  if(input$unit == 1){
    #by day
    df_svl_daily()[,c(1,4,5)]
  } else if(input$unit == 2){
    #by week
    df_svl_daily()[,c(2,4,5)]
  } else 
    #by month
    df_svl_daily()[,c(3,4,5)]
  
})

df_preplotsvl<- reactive({
  setNames(aggregate(df_dateSVL()[,3], by= list(df_dateSVL()[,1], df_dateSVL()[,2]), FUN = mean, na.rm=TRUE), c('date','team','svl'))
})

############################################################################################################

# plot the call volumes or service level

output$plot_calls<- renderChart({
  if(input$metric == 1| input$metric == 2){
      if(input$unit == 1){
        #by day
        daplot_calls(df_preplot(), yAxisLabel = lab())
      } else if(input$unit == 2){
        #by week
        wplot_calls(df_preplot(), yAxisLabel = lab())
      } else if(input$unit == 3){
        #by month
        mplot_calls(df_preplot(), yAxisLabel = lab())
      } else
        yplot_calls(df_preplot(), yAxisLabel = lab())
        
  } else if(input$metric == 3){
    if(input$unit == 1){
      #by day
      daplot_calls_svl(df_preplotsvl(), yAxisLabel = 'Service Level')
    } else if(input$unit == 2){
      #by week
      wplot_calls_svl(df_preplotsvl(), yAxisLabel =  'Service Level')
    } else
      #by year
      mplot_calls_svl(df_preplotsvl(), yAxisLabel = 'Service Level')
  }
    
})

# prepare call volume data table to be displayed
output$table <- renderDataTable(
  if(input$metric == 1| input$metric == 2){
    df_preplot()
  }else if(input$metric == 3){
#prepare SVL data table to be displayed
    df_preplotsvl()
  },
 options = list(bFilter = FALSE, iDisplayLength = 20)
)

############################################################################################################

#This section will create a password protected environment for the application
shinyURL.server()

USER <- reactiveValues(Logged = FALSE)

observeEvent(input$.login, {
  if (isTRUE(credentials[[input$.username]]==input$.password)){
    USER$Logged <- TRUE
  } else {
    show("message")
    output$message = renderText("Invalid user name or password")
    delay(2000, hide("message", anim = TRUE, animType = "fade"))
  }
})


output$app <- renderUI(
  if (!isTRUE(USER$Logged)) {
    fluidRow(column(width=4, offset = 4,
                    wellPanel(id = "login",
                              textInput(".username", "Username:"),
                              passwordInput(".password", "Password:"),
                              div(actionButton(".login", "Log in"), style="text-align: center;")
                    ),
                    textOutput("message")
    ))
  } else {
  navbarPage("Desjardins Report",
             tabPanel("Plot",
                      sidebarPanel(
                        uiOutput("teams"),
                        dateRangeInput("range", "Range: ", min ="2014-02-01", max = Sys.Date(), start ="2017-01-01", end = Sys.Date(), format ="yyyy-mm-dd", startview = "year"),
                        actionButton(inputId = "clear_all", label = "Clear selection", icon = icon("check-square")),
                        actionButton(inputId = "select_all", label = "Select all", icon = icon("check-square-o")),
                        radioButtons("metric", label = "Metric:",choices = list("Call Offered" = 1, "Call Answered" = 2, "Service Level" = 3),selected = 1),
                        #radioButtons("unit", label = "Display data by:",choices = list("Day" = 1, "Week" = 2,"Month" = 3, "Year-to-Year Comparison" = 4),selected = 3),
                        uiOutput("unitradio"),
                        shinyURL.ui()
                      ),
                      
                      mainPanel(
                        h4('Report', align = "left"),
                        showOutput("plot_calls", "nvd3"),
                        dataTableOutput(outputId="table")
                      )
             ),
             
             tabPanel("About",
                      includeMarkdown("about.Rmd")
                
             )
  )
})




})
