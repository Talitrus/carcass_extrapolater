library(shiny)
library(bslib)
library(tidyverse)

# Define the custom theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "litera",
  primary = "#4582ec",
  base_font = font_google("Inter")
)

ui <- page_navbar(
  theme = app_theme,
  title = "PBT Sample Size Calculator",
  id = "nav",
  
  nav_panel(
    title = "Calculator",
    layout_sidebar(
      sidebar = sidebar(
        title = "Parameters",
        width = 350,
        numericInput("run_size", "Total Adult Run Size", value = 1000, min = 1),
        sliderInput("target_pj", "Target Identifiable Offspring (%)", min = 0, max = 100, value = 50, step = 1),
        sliderInput("geno_success", "Genotyping Success Rate (%)", min = 1, max = 100, value = 50, step = 1),
        hr(),
        helpText("Calculates the required collection fraction and number of carcasses to sample to achieve the target probability of identifying a juvenile (P_j), factoring in genotyping success.")
      ),
      
      uiOutput("warning_msg"),
      
      layout_columns(
        col_widths = c(6, 6),
        value_box(
          title = "Theoretical Minimum Collection Fraction",
          value = textOutput("samp_frac"),
          showcase = bsicons::bs_icon("pie-chart"),
          theme = "secondary"
        ),
        value_box(
          title = "Carcasses to Collect",
          value = textOutput("carcasses_needed"),
          showcase = bsicons::bs_icon("calculator"),
          theme = "primary"
        )
      ),
      
      card(
        card_header("Collection Fraction Curve"),
        plotOutput("curve_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  results <- reactive({
    # Convert percentages to proportions
    pj <- input$target_pj / 100
    success_rate <- input$geno_success / 100
    
    # Required successful sampling fraction (p)
    p_success <- 1 - sqrt(1 - pj)
    
    # Required collection fraction (accounting for failure rate)
    p_collection <- p_success / success_rate
    
    # Calculate total carcasses needed to collect
    carcasses <- ceiling(p_collection * input$run_size)
    
    is_possible <- p_collection <= 1
    
    list(
      p_success = p_success,
      p_collection = p_collection,
      carcasses = carcasses,
      is_possible = is_possible
    )
  })
  
  output$warning_msg <- renderUI({
    res <- results()
    if (!res$is_possible) {
      div(
        class = "alert alert-danger",
        style = "margin-bottom: 1rem; border-radius: 10px;",
        bsicons::bs_icon("exclamation-triangle-fill"),
        strong(" Warning:"),
        " The required collection fraction exceeds 100%. Identifying this proportion of offspring is impossible with the current genotyping success rate."
      )
    }
  })
  
  output$samp_frac <- renderText({
    res <- results()
    if (!res$is_possible) {
      return("Impossible")
    }
    paste0(round(res$p_collection * 100, 1), "%")
  })
  
  output$carcasses_needed <- renderText({
    res <- results()
    if (!res$is_possible) {
      return("N/A")
    }
    format(res$carcasses, big.mark = ",")
  })
  
  output$curve_plot <- renderPlot({
    success_rate <- input$geno_success / 100
    
    # Generate curve data
    pj_seq <- seq(0, 0.999, length.out = 100)
    p_seq_success <- 1 - sqrt(1 - pj_seq)
    p_seq_collection <- p_seq_success / success_rate
    
    df <- data.frame(Target_Pj = pj_seq * 100, Collection_Fraction = p_seq_collection * 100)
    
    current_pj <- input$target_pj
    current_p <- results()$p_collection * 100
    
    p <- ggplot(df, aes(x = Target_Pj, y = Collection_Fraction)) +
      geom_line(color = "#4582ec", linewidth = 1.5) +
      geom_hline(yintercept = 100, color = "#dc3545", linetype = "dashed", linewidth = 1) +
      theme_minimal() +
      labs(
        title = "Target Offspring vs. Required Collection Fraction",
        x = expression(paste("Target Identifiable Offspring ", (P[j]), " (%)")),
        y = "Required Collection Fraction (%)"
      ) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#212529", size = 14),
        axis.text = element_text(color = "#212529"),
        panel.grid.major = element_line(color = "#e9ecef"),
        panel.grid.minor = element_blank()
      )
      
    if (results()$is_possible) {
      p <- p + 
        geom_point(aes(x = current_pj, y = current_p), color = "#025ce2", size = 4) +
        geom_segment(aes(x = current_pj, xend = current_pj, y = 0, yend = current_p), linetype = "dashed", color = "#6c757d", alpha = 0.5) +
        geom_segment(aes(x = 0, xend = current_pj, y = current_p, yend = current_p), linetype = "dashed", color = "#6c757d", alpha = 0.5)
    }
    
    p
  }, bg = "transparent")
}

shinyApp(ui, server)
