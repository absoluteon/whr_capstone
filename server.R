server <- function(input, output){
  
  # -------- TAB 1: Countries
  
  ## Plotly: Happiest Countries in the World
  
  output$plot1 <- renderPlotly({
    plot1 <- top10countries %>% 
      ggplot(aes(x = reorder(country_name, ladder_score) , y =  ladder_score, text = label)) +
      geom_col(aes(fill = ladder_score)) +
      coord_flip() +
      scale_fill_gradient(low = "#b6dce3", high = "#10a6c1") +
      labs(title = "The Happiest Countries in the World",
           x = NULL,
           y = "Happiness Score") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    ggplotly(plot1, tooltip = "text")
  })
  
  ## Plotly: UNHappiest Countries in the World
  
  output$plot2 <- renderPlotly({
    plot2 <- lowest10countries %>% 
      ggplot(aes(x = reorder(country_name, -ladder_score) , y =  ladder_score, text = label)) +
      geom_col(aes(fill = ladder_score)) +
      coord_flip() +
      scale_fill_gradient(low = "#f0593e", high = "#ffab9c") +
      labs(title = "The Unhappiest Countries in the World",
           x = NULL,
           y = "Happiness Score") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(# axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
    
    ggplotly(plot2, tooltip = "text")
  }) 
  
  # Leaflet
  
  output$plot_leaflet <- renderLeaflet({
    plot_leaflet <- leaflet(shape) %>%
      addTiles()  %>%
      setView(lat = 10, lng = 0 , zoom = 2) %>%
      addPolygons(
        fillColor = ~pal(ladder_score),
        stroke = FALSE,
        fillOpacity = 0.9,
        smoothFactor = 0.5,
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8 px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal,
                values = ~ladder_score,
                opacity = 0.9,
                title = "Happiness Score",
                position = "bottomleft")

    plot_leaflet
  })
  
  
  # -------- TAB 2: Through the Years    
  
  ## Plotly: Comparison for 2 countries through the years
  
  output$plot4 <- renderPlotly({
    
    # Choose country
    inputted_country <- c(input$country_1, input$country_2) # pass the query here for select
    
    # Render plot 
    plot4 <- whr_hist %>% 
      filter(country_name %in% inputted_country) %>% # filtered country only
      ggplot(aes(x = year, y = life_ladder, text = label)) +
      geom_line(aes(group = country_name, col = country_name)) +
      geom_point(aes(group = country_name, col = country_name)) +
      # scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
      labs(color = "Country",
           x = NULL,
           y = "Happiness Score") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "right", 
            legend.title.align = 0.5)
    
    
    ggplotly(plot4, tooltip = "text")
    
  })
  
  ## Plotly: Comparison for 2 countries through the years
  
  output$plot5 <- renderPlotly({
    
    # only take 15 highest countries in region, passing the below query here
    regions_top15 <- regions %>% 
      filter(year == 2020, # do not change
             region == input$region_1) %>% # passing the query from input$region_1 here
      dplyr::select(c("country_name", "region", "life_ladder")) %>% 
      group_by(country_name) %>% 
      arrange(desc(life_ladder)) %>% 
      head(15)
    
    plot5 <- regions %>% 
      filter(region %in% regions_top15$region,
             year >= input$year_1[1], year <= input$year_1[2], # passing the query from inputYear here
             country_name %in% regions_top15$country_name) %>% 
      ggplot(aes(x = factor(year), y = fct_reorder(country_name, life_ladder), fill = life_ladder, text = label)) +
      geom_tile() +
      geom_text(aes(label = life_ladder), size = 3, color = "white") +
      scale_fill_gradientn(colors = c("#f0593e", "gray", "gray", "#10a6c1")) +
      labs(x = NULL,
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot5, tooltip = "text")
    
  })
  
  # -------- TAB 3: Factors Affecting Happiness    
  
  ## Plotly: Measures Affecting Happiness Score for Top 15 Happiest & Unhappiest Countries
  
  output$plot6 <- renderPlotly({   
    
    # Plot graph for happiest
    plot6 <- whr_factors %>%
      filter(country_name %in% top10countries$country_name) %>%
      ggplot(mapping = aes(x = fct_reorder(country_name, ladder_score), y = values, fill = fct_reorder(factors, orders), order = factors, text = label)) +
      geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
      geom_text(aes(label = ladder_score, y = ladder_score), nudge_y = 0.3, size = 3, check_overlap = TRUE) +
      scale_fill_manual(values = c("#996699", muted("light pink"), "#f6c7d9", "#ffcc00", "#cccc33", "#009999", "#006699")) +
      coord_flip() +
      labs(title = "Factors Affecting Happiness Score",
           x = NULL,
           y = "Cumulative Score (from Factors)") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            
            legend.position = "none")
    
    ggplotly(plot6, tooltip = "text")
    
  })
  
  output$plot7 <- renderPlotly({
    
    # Plot graph for unhappiest
    plot7 <- whr_factors %>%
      filter(country_name %in% lowest10countries$country_name) %>%
      ggplot(mapping = aes(x = fct_reorder(country_name, -ladder_score), y = values, fill = fct_reorder(factors, orders), order = factors, text = label)) +
      geom_bar(stat = "identity", position = position_stack(reverse = FALSE)) +
      geom_text(aes(label = ladder_score, y = ladder_score), nudge_y = 0.3, size = 3, check_overlap = TRUE) +
      scale_fill_manual(values = c("#996699", muted("light pink"), "#f6c7d9", "#ffcc00", "#cccc33", "#009999", "#006699")) +
      coord_flip() +
      labs(title = "Factors Affecting Happiness Score",
           x = NULL,
           y = "Cumulative Score (from Factors)") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "none")
    
    ggplotly(plot7, tooltip = "text")
    
  })
  
  
  ## Plotly: Compare between Measures - Happiness Score - Log GDP
  
  output$plot8 <- renderPlotly({   
    
    # # Choose country
    # inputted_continent <- c(input$continent_1, input$continent_2) # pass the query here for select
    # 
    # plot
    plot8 <- whr_all %>% 
      filter(year == 2020, log_gdp != 0) %>% # do not change
      mutate(label = glue("Country: {country_name}
                      Log GDP/capita: {log_gdp}"
      )) %>% 
      ggplot(aes(x = healthy_life_expectancy, y = life_ladder, color = continent, text = label)) + # pass select to X, Y is Happiness Score
      geom_point(aes(size = log_gdp)) +
      scale_color_manual(name = "Continent",
                         breaks = c(input$continent_1, input$continent_2),
                         values = c("#10a6c1", "#f0593e", "grey", "grey", "grey")) +
      scale_x_continuous(limit = c(45, 75)) +
      labs(
           x = "Healthy Life Expectancy",
           y = "Happiness Score") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(col = guide_legend("Continent"),
             size = guide_legend(""))
    
    
    ggplotly(plot8, tooltip = "text")
    
  })
  
  
}