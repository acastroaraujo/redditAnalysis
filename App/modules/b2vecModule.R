B2VecUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        textInput(inputId = ns("query"),
                  label = strong("Type any word:"),
                  value = "Consulting"),
        
        plotOutput(ns("groupings"), height = "300px")
      ),
      mainPanel(plotOutput(ns("barchart")))
    )
  )
}

B2Vec<- function(input, output, session, wv, b) {
  
  output$groupings <- renderPlot({
    set.seed(911)
    fa <- factanal(covmat = w2v_matrix(b, wv), factors = 2)
    as_tibble(fa$loadings[, ], rownames = "org") %>% 
      full_join(enframe(kmeans(fa$loadings[, ], centers = 3)$cluster, "org", "cluster")) %>%
      full_join(enframe(b, "brand", "org")) %>% 
      ggplot(aes(Factor1, Factor2)) + 
      ggrepel::geom_label_repel(aes(label = brand, color = as.factor(cluster)), show.legend = FALSE) +
      labs(x = NULL, y = NULL) + 
      theme_void() +
      theme(plot.background = element_rect(fill = "#fafafa"))
    
  })
  
  
  data <- reactive({
    
    string <- input$query %>% 
      str_squish() %>% 
      str_to_lower() %>% 
      str_split(pattern = " ") %>% 
      unlist()
    
    if (length(string) == 1) {
      
      validate(need(string %in% rownames(wv), "Please select a different word"))
      new_word <- wv[string, , drop = FALSE]
      
    } else if (length(string) == 2) {
      
      validate(need(string[[1]] %in% rownames(wv) & string[[2]] %in% rownames(wv), "Please select a different combination of words"))
      new_word <- wv[string[[1]], , drop = FALSE] + wv[string[[2]], , drop = FALSE]
      rownames(new_word) <- str_to_lower(input$query)
    }
    
    text2vec::sim2(wv, new_word, method = "cosine", norm = "l2") %>% 
      as_tibble(rownames = "word") %>% 
      arrange(desc(!!sym(str_c(string, collapse = " "))))
    
  })
  
  output$barchart <- renderPlot({
    
    string <- input$query %>% 
      str_squish() %>% 
      str_to_lower() %>% 
      str_split(pattern = " ") %>% 
      unlist()
    
    var <- sym(str_c(string, collapse = " "))
    
    data() %>% 
      filter(word %in% b) %>% 
      full_join(enframe(b, "brand", "word")) %>% 
      ggplot(aes(x = fct_reorder(brand, !!var), y = !!var)) +
      geom_segment(aes(y = 0, xend = brand, yend = !!var)) +
      geom_point(color = "steelblue1") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip(ylim = c(-1, 1)) +
      theme_minimal(base_line_size = 0) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(breaks = seq(-1, 1, 0.2), labels = seq(-1, 1, 0.2))
    
  })
  
  
}