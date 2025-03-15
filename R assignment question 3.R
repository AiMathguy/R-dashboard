library(shinydashboard)
library(tidyverse)
library(shiny)
library(bslib)


ui <- dashboardPage(
  dashboardHeader(title = "Graduate Survey Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(
    # Used Fuild Row to keep a structure for elements
    fluidRow(
      # A static valueBox
      valueBox('1st', "Full Stack Engineer", icon = icon("laptop")),
      
      # Dynamic valueBoxes
      valueBoxOutput("progressBox"),
      
      valueBoxOutput("approvalBox")
    ),
    fluidRow(box(
      width = 4, actionButton("count", "Increment progress")
    )),
    fluidRow(
      box(plotOutput("Tools", height = 250)),
      box(plotOutput('Unemployment', height = 250))
      ,
      box(plotOutput("Industry", height = 250)),
      box(plotOutput('Roles', height = 250))
    )
  )
)



# Server
server <- function(input, output) {
  output$Tools <- renderPlot({
    long_data %>%
      group_by(ToolType, Tool) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(ToolType) %>%
      slice_max(order_by = n, n = 5) %>%
      ggplot(aes(
        x = reorder(Tool, n),
        y = n,
        fill = Tool
      )) +
      geom_col(show.legend = FALSE) + ggtitle('Top Tools') +
      coord_flip() +
      facet_wrap( ~ ToolType, nrow = 3, scales = 'free_y') + theme_minimal()
    
  })
  
  output$Industry <- renderPlot({
    graduates <- top_response %>% filter(str_detect(EduLevel, "^Bachelor|^Master|^Professional"))
    
    freq_table <- graduates %>% pull(Industry) %>% str_split(pattern = ';') %>%
      unlist(list) %>% table()
    
    
    #freq_table
    freq_table <- sort(freq_table, decreasing = TRUE)
    top_10 <- head(freq_table, 10)
    industries <- data.frame(freq_table)
    
    
    industries %>% ggplot(aes(x = ., y = Freq, fill = .)) + geom_bar(stat = 'identity') +
      labs(title = 'Top Industries') + theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()
      )
    
    
    
    
  })
  
  output$Unemployment <- renderPlot({
    unemployed <- data %>% filter(str_detect(data$Employment, 'Not em'))
    unemployed_col <- unemployed$Employment
    prop.table(table(data$Employment)) * 100
    cleaned_data <- data %>% mutate(
      Employment = case_when(
        str_detect(Employment, regex('^Not em', ignore_case = TRUE)) ~ 'Unemployed',
        str_detect(
          Employment,
          regex('^Employed | ^Developer', ignore_case = TRUE)
        ) ~ 'Employed',
        TRUE ~ 'Employed'
        
      )
    )
    
    unemployed_col <- unemployed$Employment
    categories <- c('Unemployed', 'Employed')
    
    table <- prop.table(table(cleaned_data$Employment)) * 100
    count_table <- table(cleaned_data$Employment)
    count_percentage <- round((count_table / sum(count_table)) * 100, 1)
    
    labels <- paste(names(count_table), "(", count_percentage, "%)", sep = "")
    pie_plot <- pie(
      count_table,
      labels = labels,
      col = rainbow(length(count_table)),
      main = 'Unemployment distribution'
    )
    
  })
  output$Roles <- renderPlot({
    graduates <- top_response %>% filter(str_detect(EduLevel, "^Bachelor|^Master|^Professional"))
    
    freq_table <- graduates %>% pull(Role) %>% str_split(pattern = ';') %>%
      unlist(list) %>% table()
    
    
    freq_table
    freq_table <- sort(freq_table, decreasing = TRUE)
    top_10 <- head(freq_table, 10)
    industries <- data.frame(top_10)
    
    
    industries %>% ggplot(aes(x = ., y = Freq, fill = .)) + geom_bar(stat = 'identity') +
      ggtitle('Top Roles') +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(1 + input$count, "st"),
      "Legal Services",
      icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "95.8%",
      "Employed",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
}


# Run the Shiny App
shinyApp(ui, server)
