library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$DARKLY)
app$title("Netflix Dashboard")

df <- read_csv("data/processed/processed.csv")
genres_df <- read_csv("data/processed/df.csv")
selected_genres <- as.list(unique(df$genres))
selected_ratings <- as.list(df %>%
  filter(rating %in% c("PG-13", "TV-MA", "PG", "TV-14", "TV-PG", "TV-Y", "TV-Y7", "R", "TV-G", "G", "NC-17", "NR", "UR", "TV-Y7-FV")) %>%
    pull(rating) %>% unique())

transparent <- "#00000000" # for transparent backgrounds
color1 <- "#9E0600" # red
color2 <- "#993535" # border colors
plot_text_color <- "#ebe8e8" # plot axis and label color
title_color <- "#ebe8e8" # general title and text color
border_radius <- "5px" # rounded corner radius
border_width <- "3px" # border width

options_style <- list(
  "background" = color1, "color" = title_color,
  "textAlign" = "center", "border_radius" = border_radius,
  "margin-top" = "15px"
)
chart_style <- list("background"= color1, "color"= title_color, 
                   'textAlign'= 'center', 'border-radius'= border_radius)


app$layout(
  dbcContainer(list(
    dbcRow(
      htmlDiv(
        htmlH1("Netflix Explorer", style=list("font-weight"= "bold", "fontSize"=70)
               )
      )
    ),
    dbcRow(list(
      dbcCol(list(
        htmlP("Select Year",
              style = options_style),
        dccSlider(
          id = "year_slider",
          min = 1942,
          max = 2021,
          value = 2021,
          step = 1,
          marks = list(
            "1942" = "1942",
            "2021" = "2021"
          ),
          dots = TRUE,
          tooltip = list("placement"= "bottom", "always_visible"= FALSE)
        ),
        htmlP("Select Genres",
              style = options_style),
        dccDropdown(
          id = "genre_list",
          options = selected_genres,
          value = list("International", "Dramas", "Thrillers", "Comedies"),
          multi = T,
          style = list("background-color"= transparent, "border"= "0", "color"= "black")
        ),
        htmlP("Select Ratings",
              style = options_style),
        dccDropdown(
          id = "rating_list",
          options = selected_ratings,
          value = list('PG-13','TV-MA','PG','TV-14','TV-PG','TV-Y','R','TV-G','G','NC-17','NR'),
          multi = T,
          style = list("background-color"= transparent, "border"= "0", "color"= "black")
        )
        
      ), md = 4, style = list("border" =  paste(border_width,"solid", color2), "border-radius" = border_radius,
                              "width" = '17%')),
      dbcCol(list(
        dbcRow(list(
          htmlH3("Movies and TV shows produced worldwide",
                 style=chart_style),
          dccGraph(id = "world_map")
        ), style = list("border" = paste(border_width,"solid", color2), "width" = "100%", "border-radius" = border_radius)),
        htmlBr(),
        dbcRow(list(
          dbcCol(list(
            htmlH3("Top 10 directors",
                   style=chart_style),
            dccGraph(id = "directors")
          ), style = list("border" = paste(border_width,"solid", color2), "width" = "50%", "border-radius" = border_radius)),
          dbcCol(list(
            htmlH3("Durations",
                   style=chart_style),
            htmlDiv(
              list(
                dbcTabs(
                  id = "type_name",
                  active_tab = "Movies",
                  list(
                    dbcTab(
                      dccGraph(id = "movies"),
                      style = list("width"= "520px", "height"= "520px"),
                      label = "Movies", tab_id = "Movies"
                    ),
                    dbcTab(
                      dccGraph(id = "tv"),
                      style = list("width"= "520px", "height"= "520px"),
                      label = "TV shows", tab_id = "TV Shows"
                    )
                  )
                )
              ),
              style = list("color" = "#b20710")
            )
          ), style = list("border" = paste(border_width,"solid", color2), "width" = "50%", "border-radius" = border_radius))
        ))
      ))
    ))
  ))
)


plot_world_map <- function(year) {
  country_codes <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
  world_map <- read.csv("data/processed/world_map_data.csv")

  summation <- world_map %>%
    filter(release_year <= {{ year }}) %>%
    group_by(country) %>%
    summarize(total_count = sum(count))

  merged <- merge(summation, country_codes, by.x = "country", by.y = "COUNTRY")

  # map
  l <- list(color = toRGB("black"), width = 0.5)
  g <- list(
    scope = "world",
    visible = F,
    showcountries = T,
    countrycolor = toRGB("lightgrey"),
    resolution = 50,
    showland = TRUE,
    landcolor = toRGB("lightgrey")
  )

  p <- plot_ly(merged,
    type = "choropleth",
    locations = ~CODE,
    z = ~total_count,
    colors = c(
      "#ff8080",
      "#ff2e2e",
      "#ff0e0e",
      "#dc0000",
      "#c30000",
      "#ab0000",
      "#8a0000"
    ),
    marker = list(line = l)
  ) %>%
    colorbar(title = "Count") %>%
    layout(
      clickmode = "event+select",
      geo = g
    )
  p
}

app$callback(
  output("world_map", "figure"),
  list(input("year_slider", "value")),
  function(year) {
    plot_world_map(year)
  }
)

plot_hist_duration <- function(type_name, rate, cat, year) {
  plot_df <- genres_df %>%
    filter(type == {{ type_name }}) %>%
    filter(genres %in% cat) %>%
    filter(rating %in% rate) %>%
    filter(release_year <= {{ year }}) %>%
    select(duration, genres, show_id) %>%
    distinct()

  chart <- plot_df %>%
    ggplot() +
    geom_boxplot(aes(y = duration, x = genres)) +
    xlab("") +
    ylab("Number of Movies") +
    labs(col = "Genres") +
    theme_bw() +
    coord_flip()

  ggplotly(chart)
}

app$callback(
  output("movies", "figure"),
  list(
    input("rating_list", "value"),
    input("genre_list", "value"),
    input("year_slider", "value")
  ),
  function(rate, cat, year) {
    plot_hist_duration("Movie", rate, cat, year)
  }
)

app$callback(
  output("tv", "figure"),
  list(
    input("rating_list", "value"),
    input("genre_list", "value"),
    input("year_slider", "value")
  ),
  function(rate, cat, year) {
    plot_hist_duration("TV Show", rate, cat, year)
  }
)


plot_directors <- function(rate, cat, year) {
  plot_df <- df %>%
    filter(director != "Missing") %>%
    filter(genres %in% cat) %>%
    filter(rating %in% rate) %>%
    filter(release_year <= {{ year }}) %>%
    group_by(director) %>%
    summarise(Count = n_distinct(show_id))

  chart <- plot_df %>%
    arrange(desc(Count)) %>%
    slice(1:10) %>%
    ggplot() +
    geom_bar(aes(x = Count, y = reorder(director, Count)), stat = "identity", fill = "#b20710") +
    ylab("") +
    xlab("Number of Movies + TV shows") +
    theme_bw()

  ggplotly(chart, tooltip = "Count")
}

app$callback(
  output("directors", "figure"),
  list(
    input("rating_list", "value"),
    input("genre_list", "value"),
    input("year_slider", "value")
  ),
  function(rate, cat, year) {
    plot_directors(rate, cat, year)
  }
)


app$run_server(debug = FALSE, host = "0.0.0.0")
