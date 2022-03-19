library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$DARKLY)
app$title("Netflix Dashboard")

df <- read_csv("data/processed/processed.csv")
genres_df <- read_csv('data/processed/df.csv')
selected_genres <- as.list(unique(df$genres))

app$layout(
  dbcContainer(list(
    dbcRow(
      htmlDiv(
        htmlH1("Netflix Dashboard")
      )
    ),
    dbcRow(list(
      dbcCol(list(
        htmlP("Select genres"),
        dccDropdown(
          id = "genre_list",
          options = selected_genres,
          value = "International",
          multi = T,
          style = list("color" = "blue")
        ),
        htmlP("Select year range"),
        dccRangeSlider(
          id = "year_slider",
          min = 1942,
          max = 2021,
          marks = list(
            "1942" = "1942",
            "2021" = "2021"
          ),
          value = list(1960, 2015),
          allowCross = FALSE
        )
      ), md = 4, style = list("border" = "1px solid #d3d3d3", "width" = "20%", "border-radius" = "10px")),
      dbcCol(list(
        dbcRow(list(
          htmlH4("Number of Movies and TV shows Worldwide on Netflix"),
          dccGraph(id = "world_map")
        ), style = list("border" = "1px solid #d3d3d3", "width" = "100%", "border-radius" = "10px")),
        htmlBr(),
        dbcRow(list(
          dbcCol(list(
            htmlH5("Top 10 Directors in terms of content"),
            dccGraph(id = "directors")
          ), style = list("border" = "1px solid #d3d3d3", "width" = "50%", "border-radius" = "10px")
          ),
          dbcCol(list(
            htmlH5("Popular content over the years by genres"),
            htmlDiv(
              list(
                dbcTabs(
                  id = "type_name",
                  active_tab = "Movies",
                  list(
                    dbcTab(
                      dccGraph(id = "movies"),
                      label = "Movies", tab_id = "Movies"
                    ),
                    dbcTab(
                      dccGraph(id = "tv"),
                      label = "TV shows", tab_id = "TV Shows"
                    )
                  )
                )
              ),
              style = list("color" = "#b20710")
            )
          ), style = list("border" = "1px solid #d3d3d3", "width" = "50%", "border-radius" = "10px"))
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

plot_movies <- function(cat, year) {
  plot_df <- genres_df %>%
    filter(type == "Movie") %>%
    filter(genres %in% cat) %>%
    filter(release_year <= {{ year }}) %>%
    select(duration, genres, show_id) %>%
    distinct()

  chart <- plot_df %>%
    ggplot() +
    geom_boxplot(aes(y = duration, x = genres)) +
    xlab("") +
    ylab("Number of Movies") +
    labs(col="Genres") +
    theme_bw() + coord_flip()
  
  ggplotly(chart)
}

app$callback(
  output("movies", "figure"),
  list(input("genre_list", "value"),
       input("year_slider", "value")),
  function(cat, year) {
    plot_movies(cat, year)
  }
)

plot_tv <- function(cat, year) {
  plot_df <- genres_df %>%
    filter(type == "TV Show") %>%
    filter(genres %in% cat) %>%
    filter(release_year <= {{ year }}) %>%
    select(duration, genres, show_id) %>%
    distinct()
  
  chart <- plot_df %>%
    ggplot() +
    geom_boxplot(aes(y = duration, x = genres)) +
    xlab("") +
    ylab("Number of Seasons") +
    labs(col="Genres") +
    theme_bw() + coord_flip()
  
  ggplotly(chart)
}

app$callback(
  output("tv", "figure"),
  list(input("genre_list", "value"),
       input("year_slider", "value")),
  function(cat, year) {
    plot_tv(cat, year)
  }
)


plot_directors <- function(cat, year) {
  plot_df <- df %>%
    filter(director != "Missing") %>%
    filter(genres %in% cat) %>%
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
  list(input("genre_list", "value"),
       input("year_slider", "value")),
  function(cat, year) {
    plot_directors(cat, year)
  }
)


app$run_server(debug = FALSE, host = "0.0.0.0")
