library(shiny)
library(ggplot2)
library(tidyverse)
library(devtools)
library(extrafont)
library(truncnorm)

# Note: this code is adapted from Sophie Hill's shiny app:
# https://github.com/sophieehill/ci-sims/blob/master/app.R

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  title = "Sampling distributions",
  h1("Distributions of grades"),
  fluidRow(
    column(3,
           h4('Controls'),
           sliderInput("sample_size",
                       "Sample size",
                       min = 10,
                       max = 5000,
                       step = 5,
                       value = 10),
           sliderInput("sd1",
                       "Standard deviation (1st sample):",
                       min = 0,
                       max = 30,
                       step = 0.5,
                       value = 5),
           sliderInput("sd2",
                       "Standard deviation (2nd sample):",
                       min = 0,
                       max = 30,
                       step = 0.5,
                       value = 5),
           actionButton("refresh_button", "Refresh"),
           hr(),
           br()
    ),
    column(8,
           plotOutput('SamplingPlot', height = '500px')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$SamplingPlot <- renderPlot({
    # set.seed(123)
    input$refresh_button # This makes the renderPlot depend on the go_button
    
    
    sd1 = input$sd1
    sd2 = input$sd2
    sample_size = input$sample_size
    
    values <- data.frame(
      sample1 = truncnorm::rtruncnorm(n = sample_size,
                                     a = 0, b = 100,
                                     mean = 75, sd = sd1),
      sample2 = truncnorm::rtruncnorm(n = sample_size,
                                      a = 0, b = 100,
                                      mean = 75, sd = sd2)
    ) %>% 
      pivot_longer(cols = everything(),
                   names_to = "Sample",
                   values_to = "values")
    
    summary_stats <- values %>% 
      group_by(Sample) %>% 
      summarise(mean = mean(values),
                lwr = mean - sd(values)/sqrt(sample_size)*qt(0.975,sample_size),
                upr = mean + sd(values)/sqrt(sample_size)*qt(0.975,sample_size)) %>% 
      ungroup()
    
    # confidence_text <- paste0(input$alpha, "%")
    # title_text <- paste(confidence_text, "Confidence Intervals", sep = " ")
    # subtitle_text <- paste0('Observed coverage: ',
    #                         round(mean(contains_true_prop), 2) * 100, '%')
    
    # p <- tibble(
    #   sample_prop = sample_means
    #   ) %>%
    #   mutate(sample_number = as.factor(row_number())) %>%
    #   ggplot(aes(x = sample_number, y = sample_prop)) +
    #   scale_y_continuous(limits = c(0, 1)) +
    #   coord_flip() +
    #   ggtitle(title_text, subtitle = subtitle_text) +
    #   ylab('Sample Proportion') + 
    #   xlab("") + 
    #   theme_minimal() +
    #   theme(axis.text.y = element_blank(),
    #         axis.ticks.y = element_blank(),
    #         axis.line.x = element_line(color="black", size = 0.5),
    #         legend.position = "none") +
    #   scale_x_discrete(breaks = NULL) 
    # 
    # p <- p + geom_hline(aes(yintercept = true_proportion),
    #                     linetype = 'dashed', size = 1)
    # p <- p +
    #   geom_linerange(aes(ymin = lower_ci, ymax = upper_ci, color=contains_true_prop_f)) +
    #   geom_point(aes(color=contains_true_prop_f), size=2) +
    #   scale_color_manual(values=c("true" = "#009E73", "false" = "#D55E00"))
    
    p <- ggplot(data = summary_stats, aes(x = Sample, y = mean, ymin = lwr, ymax = upr)) +
      geom_bar(stat = 'identity', fill = "steel blue", col = "black", alpha = 0.3) +
      geom_errorbar(width = 0.2, size = 1.5) +
      geom_jitter(data = values, aes(x = Sample, y = values), 
                  width = 0.2, inherit.aes = FALSE, alpha = 0.2) +
      theme_bw(base_family = "Fira Sans") +
      labs(y = "Grades",
           x = "Distribution") +
      scale_y_continuous(limits = c(0, 100))
    
    p
  })
}


# Run the application
shinyApp(ui = ui, server = server)
