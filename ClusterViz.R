install.packages(c("shiny", "ggplot2", "dplyr", "DT"))
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# --- UI Definition ---
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  
  # App Title
  titlePanel(
    div(
      h2("Cluster Sampling Simulator", style = "color: #2c3e50; font-weight: bold;"),
      p("Visualize design, estimate means, and calculate survey costs", style = "font-size: 14px; color: #7f8c8d;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Population Setup"),
      numericInput("num_clusters", "Total Clusters (N):", value = 20, min = 5, max = 100),
      numericInput("cluster_size", "Units per Cluster (M):", value = 10, min = 2, max = 50),
      sliderInput("pop_mean", "True Population Mean:", min = 0, max = 100, value = 50),
      sliderInput("pop_sd", "Population Std Dev:", min = 1, max = 20, value = 10),
      
      hr(),
      h4("2. Sampling Design"),
      sliderInput("sample_k", "Clusters to Sample (k):", min = 1, max = 20, value = 5, step = 1),
      
      hr(),
      h4("3. Cost & Time Params"),
      numericInput("fixed_cost", "Fixed Overhead ($):", value = 500),
      numericInput("cost_per_cluster", "Cost per Cluster ($):", value = 100),
      numericInput("cost_per_unit", "Cost per Unit Interview ($):", value = 10),
      numericInput("time_per_unit", "Time per Unit (mins):", value = 15),
      
      hr(),
      actionButton("run_sample", "Run Simulation", icon = icon("play"), class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      
      # Top Row: Key Metrics
      fluidRow(
        column(3, wellPanel(style = "background: #e8f6f3; border-left: 5px solid #1abc9c;",
                            h5("True Pop Mean", style = "font-weight: bold;"),
                            h3(textOutput("true_mean_disp")))),
        column(3, wellPanel(style = "background: #eaf2f8; border-left: 5px solid #3498db;",
                            h5("Estimated Mean", style = "font-weight: bold;"),
                            h3(textOutput("est_mean_disp")))),
        column(3, wellPanel(style = "background: #fef9e7; border-left: 5px solid #f1c40f;",
                            h5("Sampling Bias", style = "font-weight: bold;"),
                            h3(textOutput("bias_disp")))),
        column(3, wellPanel(style = "background: #fdedec; border-left: 5px solid #e74c3c;",
                            h5("Total Cost", style = "font-weight: bold;"),
                            h3(textOutput("cost_disp"))))
      ),
      
      tabsetPanel(
        type = "tabs",
        
        # Tab 1: Visualization
        tabPanel("Visualization", icon = icon("chart-area"),
                 br(),
                 plotOutput("cluster_plot", height = "500px"),
                 br(),
                 wellPanel(
                   h4("Interpretation"),
                   p("The plot above shows the entire population. Each dot is a unit (person/item)."),
                   tags$ul(
                     tags$li(tags$strong("Grey dots:"), "Unsampled units (clusters not selected)."),
                     tags$li(tags$strong("Colored dots:"), "Sampled units (clusters selected for survey)."),
                     tags$li(tags$strong("Boxes:"), "Represent the boundaries of the clusters.")
                   )
                 )
        ),
        
        # Tab 2: Statistical Details & Sample Size Calc
        tabPanel("Statistics & Planning", icon = icon("calculator"),
                 br(),
                 fluidRow(
                   column(6,
                          h4("Survey Efficiency"),
                          tableOutput("metrics_table"),
                          p(em("Note: 'Time Estimate' assumes one surveyor working continuously."))
                   ),
                   column(6,
                          h4("Sample Size Planner"),
                          wellPanel(
                            p("Calculate how many clusters are needed for a specific margin of error."),
                            numericInput("target_moe", "Target Margin of Error (+/-):", value = 2.0, step = 0.1),
                            h5("Required Clusters (k):"),
                            h2(textOutput("req_clusters"), style = "color: #2c3e50;")
                          )
                   )
                 )
        ),
        
        # Tab 3: Raw Data
        tabPanel("Data Explorer", icon = icon("table"),
                 br(),
                 DTOutput("data_table")
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive: Update max slider for 'k' based on 'N'
  observe({
    updateSliderInput(session, "sample_k", max = input$num_clusters)
  })
  
  # 1. Generate Population Data (Reactive)
  # We simulate clusters having slightly different means to make it realistic (Intraclass correlation)
  population_data <- reactive({
    input$run_sample # Dependency on button
    
    N <- isolate(input$num_clusters)
    M <- isolate(input$cluster_size)
    mu <- isolate(input$pop_mean)
    sigma <- isolate(input$pop_sd)
    
    # Generate Cluster effects (random variation between clusters)
    cluster_means <- rnorm(N, mean = mu, sd = sigma / 2)
    
    df <- data.frame()
    
    for(i in 1:N) {
      # Generate units within cluster
      # Variation within cluster is usually smaller than total variation
      vals <- rnorm(M, mean = cluster_means[i], sd = sigma / 2)
      temp <- data.frame(
        Cluster_ID = as.factor(paste("C", i, sep="")),
        Unit_ID = 1:M,
        Value = vals,
        Cluster_Mean_True = cluster_means[i]
      )
      df <- rbind(df, temp)
    }
    return(df)
  })
  
  # 2. Perform Sampling
  sample_result <- reactive({
    pop <- population_data()
    k <- isolate(input$sample_k)
    
    # Get unique clusters
    all_clusters <- unique(pop$Cluster_ID)
    
    # Randomly select k clusters
    selected_clusters <- sample(all_clusters, k)
    
    # Create a column in population data indicating if sampled
    pop$Is_Sampled <- ifelse(pop$Cluster_ID %in% selected_clusters, "Sampled", "Not Sampled")
    
    list(data = pop, selected = selected_clusters, k = k)
  })
  
  # 3. Calculations
  metrics <- reactive({
    res <- sample_result()
    pop <- res$data
    
    # True Population Params
    Y_bar <- mean(pop$Value)
    S2 <- var(pop$Value) # Population Variance
    
    # Sample Params
    sample_data <- pop %>% filter(Is_Sampled == "Sampled")
    y_bar_cl <- mean(sample_data$Value) # Estimate
    
    # Bias
    bias <- y_bar_cl - Y_bar
    
    # Cost & Time
    # Cost = Fixed + (k * Cost_per_cluster) + (k * M * Cost_per_unit)
    k <- res$k
    M <- isolate(input$cluster_size)
    
    total_cost <- input$fixed_cost + 
      (k * input$cost_per_cluster) + 
      (k * M * input$cost_per_unit)
    
    total_time_mins <- k * M * input$time_per_unit
    total_time_hrs <- round(total_time_mins / 60, 1)
    
    # Variance between cluster means (Sb^2) for Sample Size Calc
    # Aggregating by cluster to find variance between clusters
    cluster_summary <- pop %>% 
      group_by(Cluster_ID) %>% 
      summarise(c_mean = mean(Value))
    Sb2 <- var(cluster_summary$c_mean)
    
    list(
      true_mean = Y_bar,
      est_mean = y_bar_cl,
      bias = bias,
      cost = total_cost,
      time = total_time_hrs,
      Sb2 = Sb2,
      total_clusters = isolate(input$num_clusters)
    )
  })
  
  # --- Outputs ---
  
  output$true_mean_disp <- renderText({
    sprintf("%.2f", metrics()$true_mean)
  })
  
  output$est_mean_disp <- renderText({
    sprintf("%.2f", metrics()$est_mean)
  })
  
  output$bias_disp <- renderText({
    sprintf("%.2f", metrics()$bias)
  })
  
  output$cost_disp <- renderText({
    paste0("$", formatC(metrics()$cost, format="f", digits=0, big.mark=","))
  })
  
  output$cluster_plot <- renderPlot({
    res <- sample_result()
    df <- res$data
    
    # We arrange data so Sampled points are plotted last (on top)
    df <- df %>% arrange(Is_Sampled)
    
    ggplot(df, aes(x = Cluster_ID, y = Value, color = Is_Sampled, alpha = Is_Sampled)) +
      # Jitter points so they don't overlap
      geom_jitter(width = 0.2, height = 0, size = 3) +
      # Boxplots to show distribution
      geom_boxplot(aes(group = Cluster_ID), fill = NA, color = "black", alpha = 0.2, outlier.shape = NA) +
      scale_color_manual(values = c("Not Sampled" = "grey70", "Sampled" = "#E74C3C")) +
      scale_alpha_manual(values = c("Not Sampled" = 0.4, "Sampled" = 1.0)) +
      labs(title = "Population Visualization: Selected Clusters vs Others",
           subtitle = "Red dots represent data collected in the survey. Grey dots are unobserved.",
           y = "Variable Value", x = "Cluster ID") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$metrics_table <- renderTable({
    m <- metrics()
    data.frame(
      Metric = c("Total Cost", "Estimated Survey Time", "Sampling Bias", "Sampling Error %"),
      Value = c(
        paste0("$", formatC(m$cost, format="f", digits=2, big.mark=",")),
        paste(m$time, "Hours"),
        sprintf("%.4f", m$bias),
        sprintf("%.2f%%", (abs(m$bias)/m$true_mean)*100)
      )
    )
  })
  
  output$req_clusters <- renderText({
    m <- metrics()
    N <- m$total_clusters
    Sb2 <- m$Sb2
    E <- input$target_moe # Margin of Error
    
    # Formula for required clusters (k)
    # k = (N * Sb^2) / ( (N-1)*(E^2 / z^2) + Sb^2 )
    # Assuming 95% CI, z ~ 1.96, roughly z^2 = 4
    
    # V = (E/2)^2  (Target Variance of the mean) - simplified approximation
    # A robust approximation for k in cluster sampling:
    
    denom_part <- (E/1.96)^2 
    
    # N * Sb2 / ( (N-1)*TargetVar + Sb2 )
    k_req <- (N * Sb2) / ( (N-1)*denom_part + Sb2 )
    
    if(k_req > N) {
      return(paste("Entire Pop (", N, ")"))
    } else {
      return(ceiling(k_req))
    }
  })
  
  output$data_table <- renderDT({
    res <- sample_result()
    datatable(res$data, 
              options = list(pageLength = 10),
              filter = 'top') %>%
      formatRound(columns = c("Value", "Cluster_Mean_True"), digits = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

