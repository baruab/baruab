
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(DT)
library(tidyverse)
library(plotly)
library(miniUI)
library(leaflet)
library(mapdata)
library(mapview)
library(mapproj)

header <- dashboardHeader(
  title = "US Food Security"
)

sidebar <- dashboardSidebar(
  sliderInput("theYear", "Year",
              min = 2007, max = 2022, value = 2017, step = 1, sep = ""
  ),
  sidebarMenu(
    menuItem("Food Security By State", 
             tabName = "FoodSecurityByState", 
             icon = icon("bar-chart")),
    menuItem("Food Insecurity By Category", 
             tabName = "FoodSecurityByEmployment", 
             icon = icon("fa fa-table")),    
    menuItem("Children Food Security", 
             tabName = "ChildrenFoodSecurity", 
             icon = icon("fa fa-map-marker"))
  )
) 

body <- dashboardBody(
  
  tabItems(
    tabItem("FoodSecurityByState", h2("Food Security By State"),
            fluidRow(
              plotOutput('mapUSPlot'))    
    ), 
    tabItem("FoodSecurityByEmployment", h2("Food Insecurity By Category"),
            fluidRow(
              
              selectInput( inputId = "sel_Category",
                           label = "Choose Category",
                           "Names"),
                box(
                  title = "Food Insecurity Chart", solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("FoodInsecurityChart") 
              ),
              box(
                title = "Very Low Food Security Chart", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("VeryLowFoodSecurityChart") 
              )
            )
    ),
    tabItem("ChildrenFoodSecurity", h2("Children Food Security"),
            
        fluidRow(
          #box(
          #  title = "Percent Stacked Chart", solidHeader = TRUE, collapsible = TRUE,
            plotOutput("ChildrenStackedBarChart") 
         # )
        ),
        fluidRow(
            DTOutput("ChildrenFoodSecurityTable")
        )
    )
  ) # end of tabItems
) # end of body



############ SERVER SIDE ########################
server <- function(input, output, session) { 
 
  
  data_byState <- read_csv("https://raw.githubusercontent.com/baruab/baruab/main/foodsecurity_data_by_state.csv")
  head(data_byState)
  
  data_allHousehold <- read_csv("https://raw.githubusercontent.com/baruab/baruab/main/food_security_all_household.csv")
  head(data_allHousehold)
  
  data_byEmployment <- read_csv("https://raw.githubusercontent.com/baruab/baruab/main/food_security_by_employment.csv")
  head(data_byEmployment)
  
  data_hh_children <- read_csv("https://raw.githubusercontent.com/baruab/baruab/main/food_security_HH_children.csv")
  head(data_hh_children)
  
  
  # Remove Job description column
  #mydata <- dat[ -c(4) ]
  
  
  data <- reactive({
    data_byState$Year = substring(data_byState$Year,1,4)
    
    data_byState1 <- data_byState[-(which(data_byState$State %in% "U.S.")),]
    data_byState2 <- data_byState1[-(which(data_byState$State %in% "U.S. total")),]
    
    data_FSS <- data_byState2[, c("Year", "State", "Food insecurity prevalence")] 
    data_FSS <- data_FSS %>% filter(grepl(input$theYear, Year))
    
    return(data_FSS)
    
  })

  foodInsecurity <- reactive({
    data_FIS <- data_byEmployment[, c("Year", "Sub-subcategory", "Food insecure-percent","Very low food security-percent")] 
    data_FIS <- data_FIS %>% filter(grepl(input$sel_Category, `Sub-subcategory`))
    
    return(data_FIS)
    
  })
  
  #Update SelectInput dynamically
  observe({
    updateSelectInput(session, "sel_Category", choices =( sort(data_byEmployment$`Sub-subcategory`)))
  }) 



  
  #///////////////////////////////////////////////////////////////////////////
  # reactive function for comparison Map 1 and comparison table 1
  #///////////////////////////////////////////////////////////////////////////    
  updateHHChildren <- reactive({  
    
    # Data filtering from the original data
    dataHH <- data_hh_children %>% filter(grepl(input$theYear, Year))
    data_HHC <- dataHH[, c("Year", "Category", "Subcategory", "Total", "Food-secure households-percent", "Food-insecure households-percent", "Households with food-insecure children-percent","Households with very low food security among children-percent")]
    return(data_HHC)
  })
  
  updateHHChildrenChart <- reactive({  
    
    # Data filtering from the original data
    dataHH <- data_hh_children %>% filter(grepl("All households", Category))
    data_HH1 <- dataHH[, c( "Year", "Food-secure households-percent", "Food-insecure households-percent", "Households with very low food security among children-percent")]
    
    return(data_HH1)
  })
  
  updateHHChildrenChart2 <- reactive({  
    
    # Data filtering from the original data
    data_HH1 <- data_hh_children[, c( "Year", "Food-secure households-percent", "Food-insecure households-percent", "Households with food-insecure children-percent")]
    
    return(data_HH1)
  })
  
  
  
  #///////////////////////////////////////////////////////////////////////////
  #///////////////////////////////////////////////////////////////////////////
  output$ChildrenFoodSecurityTable <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable1 <- updateHHChildren() 
    
    # Change the call names
    colnames(dataForDTable1) <- c("Year", "Category", "Subcategory", "Total", "Food-secure-percent", "Food-insecure-percent", "insecure children-percent","low food children-percent")  
    
    dataForDTable1 # filtered data for the dataTable
    
  }, rownames = FALSE, 
     options = list(
     deferRender = TRUE,  
    searching = T
  )) ) 
 
  
  output$ChildrenStackedBarChart <- renderPlot({
    dataForChart <- updateHHChildrenChart2()
    
    dataForChart <- dataForChart %>% 
      group_by(Year) %>% summarise(`Food-secure households-percent` = mean(`Food-secure households-percent`),`Food-insecure households-percent` = mean(`Food-insecure households-percent`),`Households with food-insecure children-percent` = mean(`Households with food-insecure children-percent`))   
     
    print(head(dataForChart))
    
    p <- ggplot(dataForChart, aes(x=Year, y=`Food-secure households-percent`, group=1)) + 
      geom_line(aes(y = `Food-insecure households-percent`, group=2), color = "red") + 
      geom_line(aes(y = `Households with food-insecure children-percent`, group=3), color = "green") + 
      
      geom_line(aes(y = `Food-secure households-percent`, group=1), color="blue",
                linetype="twodash") +
      labs(title="Year vs Children Food security",x="Year", y = "Food security percent(mean)") 
    
    # Add a legend
    p <- p + theme(legend.position = "top")
    p <- p + labs(fill = "Children Food Security categories")
    
    p
    
  })
  
  
  output$mapUSPlot <- renderPlot({ 

    data_FSS <- data()
    Food_insecurity_prevalence <- data_FSS$`Food insecurity prevalence`
    x <- data_FSS$State
    
    x1 <- abbr2state(x)
    region <- tolower(x1)
    
    df <- data.frame(x, region, Food_insecurity_prevalence)
    states <- map_data("state")
    
    #merge
    map.geo <- merge(states, df, sort=FALSE, by="region")
  #  map.geo <- map.geo[order(map.geo$order), ]
    
    # plot
    p <- ggplot( map.geo, aes(long, lat)) + geom_polygon(aes(group=region, fill=Food_insecurity_prevalence)) + coord_map() 
    #+ coord_equal() + geom_point(aes(text=name, size=z), colour="red", alpha=1/2, name="region")
    p + labs(title = paste( "Food security By State, Year=" , as.character(input$theYear), sep=" "), y = "Longitude", x = "Latitude")
    
    
  })
  
  output$FoodInsecurityChart <-  renderPlotly({
    dataForChart <- foodInsecurity()
    p <- dataForChart %>%  ggplot(aes(y=`Food insecure-percent`, x=Year)) + 
      geom_col(stat = "identity",
               fill = "#1f5673",
               aes(text = paste0(
                 "<b>", " Summary", "</b>",
                 "<br>",
                 "<b>Year: </b>", Year,
                 "<br>",
                 "<b>Category: </b>", input$sel_Category,
                 "<br>",
                 "<b>Food Insecure Percent: </b>", `Food insecure-percent`
               ))) +
           labs(title = "Food Insecurity(%) thru the years",
           x = "Year",
           y = "Food Insecure Percent") +
           geom_text(aes(label = `Food insecure-percent`,
                    y = `Food insecure-percent` + 3,
                    text = paste0(`Food insecure-percent`, " %"))) +
    #     geom_bar(position="dodge", stat="identity") + 
           ylim(0, 50)
    
    
    # Wrap with ggplotly()
    ggp <-  ggplotly(p, tooltip = "text")%>% 
      layout(hoverlabel=list(bgcolor = "#ffffff"))
    ggp
  })
  
  output$VeryLowFoodSecurityChart <- renderPlotly({
    dataForChart <- foodInsecurity()
    p <-   dataForChart %>%  ggplot(aes(y=`Very low food security-percent`, x=Year)) + 
      geom_col(stat = "identity",
               fill = "#1f5673",
               aes(text = paste0(
                 "<b>", " Summary", "</b>",
                 "<br>",
                 "<b>Year: </b>", Year,
                 "<br>",
                 "<b>Category: </b>", input$sel_Category,
                 "<br>",
                 "<b>Food Insecure Percent: </b>", `Very low food security-percent`
               ))) +
      labs(title = "Very low food security(%) thru the years",
           x = "Year",
           y = "Very low food security Percent") +
      geom_text(aes(label = `Very low food security-percent`,
                    y = `Very low food security-percent` + 3,
                    text = paste0(`Very low food security-percent`, " %"))) +
      
      #  geom_bar(position="dodge", stat="identity")  
          ylim(0, 50)
    
    # Wrap with ggplotly()
    ggp <- ggplotly(p, tooltip = "text")%>% 
      layout(hoverlabel=list(bgcolor = "#ffffff"))
    ggp
    
  })
  
  # ...
  # ... Other functions are intentionally omitted for brevity ...
  # ...
  
}



shinyApp(
  ui = dashboardPage(header, sidebar, body, skin = "black"), 
  server
)