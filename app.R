library(shiny)
library(bslib)

# Read data
source("analysis.R")


# Create the Shiny app
ui <- page_sidebar(

  title = "Music Streaming Analytics",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    title = "Filters",
    dateRangeInput(
      "date_range", 
      "Select Date Range:",
      start = "2021-04-01",
      end = "2025-01-30",
      min = "2021-04-01",
      max = "2025-01-30"
    ),
    hr(),
    helpText("Use the date range picker above to filter the streaming data for analysis.")
  ),
  
  
  layout_columns(
    value_box(
      title = "Total Streams",
      value = textOutput("totalStreams"),
      showcase = bsicons::bs_icon("music-player"),
      theme = value_box_theme(bg = "#dc3545", fg = "white")
    ),
    value_box(
      title = "Favourite Artist",
      value = textOutput("favouriteArtist"),
      showcase = bsicons::bs_icon("mic"),
      theme = value_box_theme(bg = "#dc3545", fg = "white")
    ),
    value_box(
      title = "Favourite Song",
      value = textOutput("favouriteSong"),
      showcase = bsicons::bs_icon("music-note"),
      theme = value_box_theme(bg = "#dc3545", fg = "white")
    )
  ),
  
  card(
    card_header("Daily Streaming Trends"),
    card_body(
      plotOutput("daily_streams_plot", height = "400px")
    )
  ),
  
  
  
  layout_column_wrap(
    width = 1/2, fill = TRUE,
    card(
      card_header("Top Artists by Streams"),
      card_body(
        plotOutput("top_artists_plot" , height = "350px")
      )
    ),
    card(
      card_header("Artist Stream Trends"),
      card_body(
        plotOutput("artist_trends_plot", height = "350px")
      )
    )
  ),
  
  card(
    card_header("Listening Hours"),
    card_body(
      plotOutput("hour_streams_plot", height = "400px")
    )
  ),
  
  # card(
  #   card_header("Data Summary"),
  #   card_body(
  #     tableOutput("summary_table")
  #   )
  # )
)

server <- function(input, output, session) {
  # Load or generate data
  stream_data <- clean_data
  
  # Filtered data based on date range
  filtered_data <- reactive({
    stream_data %>%
      filter(stream_date >= input$date_range[1] & stream_date <= input$date_range[2])
  })
  
  # Total Streams
  output$totalStreams <- renderText({
    total <- nrow(filtered_data())
    format(total, big.mark = ",")
  })
  
  # Favourite Artist
  output$favouriteArtist <- renderText({
    fav_artist <- filtered_data() %>% 
      count(artist, sort = TRUE) %>%
      slice(1) %>%
      pull(artist)
    
    fav_artist
  })
  
  # Favourite Song
  output$favouriteSong <- renderText({
    fav_song <- filtered_data() %>% 
      count(song, sort = TRUE) %>%
      slice(1) %>%
      pull(song)
    
    fav_song
  })
  
  # Daily total streams plot
  output$daily_streams_plot <- renderPlot({
    daily_counts <- filtered_data() %>%
      group_by(stream_date) %>%
      summarise(stream_count = n(), .groups = 'drop')

    ggplot(daily_counts, aes(x = stream_date, y = stream_count)) +
      geom_line(color = "#2c3e50", size = 1) +
      geom_point(color = "#3498db", size = 2) +
      labs(
        x = "Date",
        y = "Number of Streams",
        title = "Daily Total Streams"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12)
      )
  })
  
  # # Top artists plot
  output$top_artists_plot <- renderPlot({
    top_artists <- filtered_data() %>%
      group_by(artist) %>%
      summarise(stream_count = n(), .groups = 'drop') %>%
      arrange(desc(stream_count)) %>%
      head(10)

    ggplot(top_artists, aes(x = reorder(artist, stream_count), y = stream_count, fill = stream_count)) +
      geom_col() +
      coord_flip() + 
      scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
      labs(
        x = "Artist",
        y = "Number of Streams",
        title = "Top 10 Artists by Stream Count"
      ) +
      theme_minimal() +
      theme(
        #axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "none"
      )
  })
  
  # Artist trends over time plot
  output$artist_trends_plot <- renderPlot({
    top_5_artists <- filtered_data() %>%
      group_by(artist) %>%
      summarise(total_streams = n(), .groups = 'drop') %>%
      arrange(desc(total_streams)) %>%
      head(5) %>%
      pull(artist)

    artist_daily <- filtered_data() %>%
      filter(artist %in% top_5_artists) %>%
      group_by(artist, stream_date) %>%
      summarise(stream_count = n(), .groups = 'drop')

    ggplot(artist_daily, aes(x = stream_date, y = stream_count, color = artist)) +
      geom_line(size = 1) +
      labs(
        x = "Date",
        y = "Number of Streams",
        title = "Stream Trends for Top 5 Artists",
        color = "Artist"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
  })
  
  # Listening Hour
  output$hour_streams_plot <- renderPlot({
    hourly_data <- filtered_data() %>%
        select(stream_date, stream_hour, day_night, weekday_or_not) %>% 
        mutate(
          stream_hour = as.numeric(stream_hour),
          stream_hour_factor = factor(stream_hour, levels = 0:23)
        )
    
    ggplot(hourly_data, aes(x = stream_hour_factor, fill = day_night)) + 
      geom_bar(width = 1) +
      scale_fill_manual(values = c("#F8E443", "#7F9648")) +
      coord_polar(start = 0) +
      facet_wrap(vars(weekday_or_not)) +
      theme_void() + 
      theme(
        #plot.background = element_rect(fill = "#ECF0F1", color = NA),  # light gray background
        panel.border = element_rect(color = "#2C3E50", fill = NA, size = 1),  # navy-ish border
        axis.text = element_text(color = "#2C3E50", family = "sans", size = 14),
        panel.grid.major.y = element_line(colour = "#BDC3C7", linetype = 1, size = 0.6),  # soft gray grid
        axis.title.y = element_text(color = "#2C3E50", family = "sans", size = 24, angle = 90),
        
        panel.spacing = unit(1, "lines"),
        strip.background = element_rect(fill = "#18BC9C", size = 1),  # teal facet labels
        strip.text = element_text(color = "white", family = "sans", size = 14),
        
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "#2C3E50"),
        
        plot.title = element_text(hjust = 0.5, size = 28, face = "bold", color = "#2C3E50"),
        plot.subtitle = element_text(hjust = 0.5, size = 20, color = "#2C3E50"),
        plot.caption = element_text(hjust = 0, size = 12, color = "#7F8C8D")
      ) +
      labs(x = "", y = "")
  })
  
  
  #  # Summary table
  # output$summary_table <- renderTable({
  #   date_range_text <- paste(format(input$date_range[1], "%b %d, %Y"), "to",
  #                            format(input$date_range[2], "%b %d, %Y"))
  # 
  #   most_streamed <- filtered_data() %>%
  #     group_by(artist) %>%
  #     summarise(stream_count = n(), .groups = 'drop') %>%
  #     arrange(desc(stream_count)) %>%
  #     head(1)
  # 
  #   total_streams <- nrow(filtered_data())
  #   unique_artists <- length(unique(filtered_data()$artist))
  #   unique_albums <- length(unique(filtered_data()$album))
  #   unique_songs <- length(unique(filtered_data()$song))
  # 
  #   data.frame(
  #     Metric = c("Date Range", "Total Streams", "Most Streamed Artist",
  #                "Artist Stream Count", "Unique Artists", "Unique Albums", "Unique Songs"),
  #     Value = c(date_range_text, total_streams, most_streamed$artist,
  #               most_streamed$stream_count, unique_artists, unique_albums, unique_songs)
  #   )
  # }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)