# ********************************************
# Packages
# ********************************************

library(shiny)
library(shinythemes)
library(tidyverse)
library(text2vec)
library(cowplot)




# ********************************************
# Functions
# ********************************************
source("../w2v_reddit.R")
source("modules/b2vecModule.R")


# ********************************************
# Data
# ********************************************

word_vectors <- readr::read_rds("consulting_matrix.rds")
subreddit <- readr::read_rds("consulting.rds")

brands <- c(Accenture = "accenture",
            Deloitte = "deloitte",
            EY = "ey",
            McKinsey = "mckinsey",
            KPMG = "kpmg",
            PWC = "pwc",
            Salesforce = "salesforce",
            Facebook = "facebook",
            Microsoft = "microsoft",
            Google = "google",
            Bain = "bain")

ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  includeCSS("stylesTBWA.css"),
  
  navbarPage(
    title = "/r/consulting", windowTitle = "subreddit tool",
    
    tabPanel(
      title = "B2Vec",
      
      B2VecUI("firstTab")
      
    ),
    tabPanel(
      title = "Subreddit Summary",
      
      fluidRow(
        column(width = 4, plotOutput("sub_description")),
        column(width = 4, offset = 2, 
               
               textInput(inputId = "query",
                         label = strong("Type any word:"),
                         value = "Consulting"),
               
               plotOutput("w2vec"))
        )
      ),
    tabPanel(
      title = "Individual Threads"
      ),
    tabPanel(
      title = "Audiences"
    ),
    tabPanel(
      title = "Methodology"
      )
    )
)

server <- function(input, output) {
  
  
  callModule(B2Vec, "firstTab", wv = word_vectors, b = brands)
  
  
  output$sub_description <- renderPlot({
    
    p1 <- filter(subreddit, num_comments > 0) %>% 
      ggplot(aes(x = num_comments)) +
      geom_histogram(color = "black") +
      scale_x_log10() 
      
      p2 <- ggplot(subreddit, aes(x = date)) +
        geom_histogram(color = "black") + 
        scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") 
        
        p3 <- ggplot(subreddit, aes(x = date, y = num_comments)) +
          geom_point(alpha = 0.5) +
          scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
        
        
    cowplot::plot_grid(p1, p2, p3, ncol = 1)
    
  })
  
  
  data <- reactive({
    
    string <- input$query %>% 
      str_squish() %>% 
      str_to_lower() %>% 
      str_split(pattern = " ") %>% 
      unlist()
    
    if (length(string) == 1) {
      
      validate(need(string %in% rownames(word_vectors), "Please select a different word"))
      new_word <- word_vectors[string, , drop = FALSE]
      
    } else if (length(string) == 2) {
      
      validate(need(string[[1]] %in% rownames(word_vectors) & string[[2]] %in% rownames(word_vectors), "Please select a different combination of words"))
      new_word <- word_vectors[string[[1]], , drop = FALSE] + word_vectors[string[[2]], , drop = FALSE]
      rownames(new_word) <- str_to_lower(input$query)
    }
    
    text2vec::sim2(word_vectors, new_word, method = "cosine", norm = "l2") %>% 
      as_tibble(rownames = "word") %>% 
      arrange(desc(!!sym(str_c(string, collapse = " "))))
    
  })
  
  output$w2vec <- renderPlot({
    
    string <- input$query %>% 
      str_squish() %>% 
      str_to_lower() %>% 
      str_split(pattern = " ") %>% 
      unlist()
    
    var <- sym(str_c(string, collapse = " "))
    
    data() %>% 
      top_n(15, wt = !!var) %>% 
      ggplot(aes(x = fct_reorder(word, !!var), y = !!var)) +
      geom_segment(aes(y = 0, xend = word, yend = !!var)) +
      geom_point(color = "steelblue1") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip(ylim = c(-1, 1)) +
      theme_minimal(base_line_size = 0) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(breaks = seq(-1, 1, 0.2), labels = seq(-1, 1, 0.2))
    
  })
  
  

}

shinyApp(ui, server)
