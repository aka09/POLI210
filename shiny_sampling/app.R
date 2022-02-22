library(shiny)
library(ggplot2)
library(tidyverse)
library(devtools)
library(extrafont)

# Note: this code is adapted from Sophie Hill's shiny app:
# https://github.com/sophieehill/ci-sims/blob/master/app.R

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  title = "Sampling distributions",
  h1("Sampling from the class survey's ideology"),
  
  p("This interactive web application allows you to draw repeated samples from our class survey and examine what the different samples have to say about the ideology of students in the class."),
    p("Remember what the central limit theorem states: under repeated sampling, the distribution of sample means (the sampling distribution) will approach a normal distribution centered on the true population parameter."),
  p("Our true population parameter in this case is the true mean of ideology, measured on a scale from 0-10, among all students: 2.95. This population parameter is what we are trying to estimate/infer using sample statistics."),
  p("What the sampling distribution will look like is influenced by two factors: the number of repeated samples, and the size of each sample. The larger the number of repeated samples, the closer the sampling distribution will approximate a normal distribution. The larger the sample size for each sample, the tighter the normal distribution will be around the true parameter (less sampling variance)."),
  p("You can play around with the sample size and the number of samples using the controls below."),
  
  fluidRow(
    column(3,
           h4('Controls'),
           sliderInput("sample_size",
                       "Sample size:",
                       min = 10,
                       max = 200,
                       step = 5,
                       value = 10),
           sliderInput("n_samples",
                       "Number of samples:",
                       min = 0,
                       max = 1000,
                       step = 10,
                       value = 100),
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
    
    
    n_samples = input$n_samples
    sample_size = input$sample_size
    
    # Import class survey
    survey <- read.csv("class_survey.csv")
    ideology <- survey$ideology
    true_mean <- mean(ideology, na.rm = TRUE)
    
    
    sample_means <- vector('numeric', n_samples)
    for (i in 1:n_samples) {
      # sample from the population 
      sample <- sample(ideology, sample_size, replace = FALSE)
      
      # sample proportion 
      sample_mean <- mean(sample, na.rm = T)
      
      # store values 
      sample_means[i] <- sample_mean
    }
    
    p <- tibble(sample_means = sample_means) %>% 
      ggplot(aes(x = sample_means)) +
      geom_histogram(binwidth = 0.1,
                     fill = "steel blue",
                     col = "black") +
      geom_vline(xintercept = true_mean, col = "red", linetype = "dashed",
                 size = 1.5) +
      theme_bw(base_family = "Fira Sans") +
      labs(y = "Number of samples",
           x = "Sample mean") +
      xlim(0, 6)
    
    p
  })
}


# Run the application
shinyApp(ui = ui, server = server)
