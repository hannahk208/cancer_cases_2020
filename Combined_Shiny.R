library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)

# UI
ui <- fluidPage(
  titlePanel("Cancer Incidence Visualization"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Add inputs or controls for user interaction
      selectInput("gender_selection", "Select Gender:", choices = c("All", "Men", "Women"), selected = "All"),
      selectInput("cancer_type_selection", "Select Cancer Type:", choices = c("All", unique(total_cases_cancer$cancer)), selected = "All"),
      sliderInput("cases_threshold", "Filter by Cases Threshold:", min = 0, max = max(total_cases_cancer$new_cases_2020), value = 0, step = 100),
      checkboxInput("show_legend", "Show Legend", value = TRUE),
      br(),
      downloadButton("download_data", "Download Data")
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Gender Cases", plotOutput("gender_cases_plot")),
        tabPanel("Cancer Types", plotOutput("cancer_types_plot")),
        tabPanel("Unique Cancer Types in Men", plotOutput("unique_cancer_men_plot")),
        tabPanel("Unique Cancer Types in Women", plotOutput("unique_cancer_women_plot")),
        tabPanel("Most Prevalent Cancer Types", plotOutput("most_prevalent_cancer_plot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    gender_filter <- if (input$gender_selection == "All") TRUE else both_sex$gender == input$gender_selection
    cancer_filter <- if (input$cancer_type_selection == "All") TRUE else both_sex$cancer == input$cancer_type_selection
    threshold_filter <- both_sex$new_cases_2020 >= input$cases_threshold
    
    both_sex %>%
      filter(gender_filter, cancer_filter, threshold_filter)
  })
  ## Summary stats
  
  total_cases_gender <- both_sex%>%
    group_by(gender)%>%
    summarise(new_cases_2020 = sum(new_cases_2020))
  
  total_cases_cancer <- both_sex%>%
    group_by(cancer)%>%
    summarise(new_cases_2020 = sum(new_cases_2020))
  
  # Gender Cases Plot
  output$gender_cases_plot <- renderPlot({
    ggplot(filtered_data()) +
      geom_bar(aes(x = gender, y = new_cases_2020, fill = gender), stat = 'identity') +
      scale_fill_manual(values = c("men" = "blue", "women" = "pink")) +  # Specify colors
      
      ylab('Number of Cases in 2020') +
      ggtitle("Gender Cases") +
      theme(legend.position = ifelse(input$show_legend, "right", "none"))
  })
  
 
  # Cancer Types Plot
  output$cancer_types_plot <- renderPlot({
    ggplot(total_cases_cancer) +
      geom_bar(aes(reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer), stat = 'identity') +
      ylab('Number of Cases in 2020') +
      xlab('Cancer Types') +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggtitle("Cancer Types")
  })
  
 
  
  
  
  # Unique Cancer Types in Men Plot
  output$unique_cancer_men_plot <- renderPlot({
    ggplot(filter(both_sex, gender == 'men' & cancer %in% unique_cancer_men)) +
      geom_bar(aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer), stat = 'identity') +
      ylab('Number of Cases in 2020') +
      xlab('Unique Cancer Types in Men') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggtitle("Unique Cancer Types in Men")
  })
  
  # Unique Cancer Types in Women Plot
  output$unique_cancer_women_plot <- renderPlot({
    ggplot(filter(both_sex, gender == 'women' & cancer %in% unique_cancer_women)) +
      geom_bar(aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020, fill = cancer), stat = 'identity') +
      ylab('Number of Cases in 2020') +
      xlab('Unique Cancer Types in Women') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggtitle("Unique Cancer Types in Women")
  })
  
  # Most Prevalent Cancer Types Plot
  output$most_prevalent_cancer_plot <- renderPlot({
    plot_men <- ggplot() +
      geom_bar(
        data = filter(both_sex, gender == 'men' & cancer %in% most_prevalent_cancer_men$cancer),
        aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020),
        stat = 'identity', position = 'identity', fill = "blue"
      ) +
      ylab('Number of Cases in 2020') +
      xlab('Most Prevalent Cancer Types for Men') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    plot_women <- ggplot() +
      geom_bar(
        data = filter(both_sex, gender == 'women' & cancer %in% most_prevalent_cancer_women$cancer),
        aes(x = reorder(cancer, desc(new_cases_2020)), y = new_cases_2020),
        stat = 'identity', position = 'identity',fill="pink"
      ) +
      ylab('Number of Cases in 2020') +
      xlab('Most Prevalent Cancer Types for Women') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    # Arrange the two plots side by side
    grid.arrange(plot_men , plot_women,  ncol = 2)
  })
}

# Run the application
shinyApp(ui, server)
