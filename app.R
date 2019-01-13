library(shiny)
source("analysis.R")

ui <- fluidPage(
  titlePanel("Eviction Data"),
  sidebarLayout(
    sidebarPanel(
      # Sets up the inputs for the app
      selectInput("state",
        label = "State",
        # Only include continental states
        choices = setdiff(state.name, c("Alaska", "Hawaii")),
        selected = "Washington"
      ),
      sliderInput("year",
        label = "Year",
        min = min(range(eviction_counties$year)),
        max = max(range(eviction_counties$year)),
        value = mean(range(eviction_counties$year)),
        sep = "", step = 1
      ),
      selectInput("data",
        label = "Data", choices = data_dictionary,
        selected = "Eviction Rate"
      )
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Map",
          br(),
          textOutput("map_sum"),
          plotOutput("map", click = "map_click"),
          tableOutput("county_info")
        ),
        tabPanel(
          "Table",
          br(),
          textOutput("avgs"),
          dataTableOutput("obvs_table")
        )
      )
    )
  ),
  fluidRow(
    column(
      12,
      hr(),
      p(citation)
    )
  )
)

server <- function(input, output) {
  data <- reactiveValues()
  data$county <- NULL

  data_name <- reactive({
    names(data_dictionary[which(data_dictionary == input$data)])
  })

  # Sets up the data, filtered for the selected state and year,
  # for use throughout the app
  eviction_data <- reactive({
    eviction_counties %>%
      filter(parent.location == input$state) %>%
      filter(year == input$year) %>%
      mutate("Percent Non-White" = pct.af.am + pct.hispanic + pct.am.ind +
        pct.asian + pct.nh.pi + pct.other + pct.multiple)
  })

  # Calculates the state average of the selected data
  state_average <- reactive({
    round(mean(eviction_data()[[input$data]], na.rm = T), 3)
  })

  # Calculates the national average of the selected data
  national_average <- reactive({
    input_data_sym <- rlang::sym(input$data)
    national <- eviction_counties %>%
      filter(year == input$year)

    round(mean(national[[input$data]], na.rm = T), 3)
  })

  # Returns a string describing how the states average compares to the national
  # average
  national_string <- reactive({
    if (state_average() > national_average()) {
      return(paste("greater than the national average of", national_average()))
    } else if (state_average() < national_average()) {
      return(paste("less than the national average of", national_average()))
    } else {
      return(paste("equal to the national average of", national_average()))
    }
  })

  # Creates a table of data observations to use later
  obvs_table <- reactive({
    data_name_sym <- rlang::sym(data_name())
    input_data_sym <- rlang::sym(input$data)
    percent_non_white <- "Percent Non-White"

    table <- eviction_data() %>%
      mutate(
        "County" = name, !!data_name_sym := !!input_data_sym,
        "Population" = population
      ) %>%
      select(County, Population, !!data_name_sym, !!percent_non_white)
  })

  # Outputs the observation table
  output$obvs_table <- renderDataTable({
    obvs_table()
  })

  # Creates a table of average values for the given state
  avg_table <- reactive({
    avg_data_sym <- rlang::sym(paste("Average", data_name()))
    input_data_sym <- rlang::sym(input$data)
    percent_non_white <- rlang::sym("Percent Non-White")
    table <- eviction_data() %>%
      group_by("State" = parent.location) %>%
      summarize(
        "Average County Population" = round(mean(population, na.rm = T), 3),
        !!avg_data_sym := round(mean(!!input_data_sym, na.rm = T), 3),
        "Average Percent Non-White" = round(mean(!!percent_non_white, na.rm = T), 3)
      )
  })

  # Outputs a paragraph describing the observation table and the average
  # values across the state, and how it compares to the national average. 
  output$avgs <- renderText({
    avg_population <- floor(mean(eviction_data()$population, na.rm = T))
    avg_non_white <- mean(eviction_data()[["Percent Non-White"]], 3)
    input_data_sym <- rlang::sym(input$data)
    avgs <- paste(
      "The below table details observations of the",
      tolower(data_name()), "across", paste0(input$state, "."),
      "The average population per county in", input$state,
      paste0("is ", avg_population, ", of which"), paste0(avg_non_white, "%"),
      "is non-white.", "The average", tolower(data_name()),
      " per county is", paste0(state_average(), ","),
      "which is", national_string()
    )
  })

  # Outputs a summary of the map
  output$map_sum <- renderText({
    paste(
      "The map below shows", tolower(data_name()), "across",
      input$state, "during", paste0(input$year, "."), 
      "Average values for", input$state,
      "are shown below the map. Click on a county to get specific information."
    )
  })

  # Reset the selected county when the selected state changes to prevent 
  # a old value from being passed into get_county()
  observeEvent(input$state, {
    data$county <- NULL
  })

  # Outputs the choropleth map
  output$map <- renderPlot({
    state_map <- counties %>%
      filter(region == tolower(input$state))

    state_data <- left_join(state_map, eviction_data(),
      by = c("fips" = "GEOID")
    )

    ggplot(state_data) +
      geom_polygon(aes_string("long", "lat",
        group = "group",
        fill = input$data
      ),
      color = "darkGrey"
      ) +
      scale_fill_distiller(palette = "GnBu", type = "seq")+
      coord_quickmap() +
      labs(
        x = "Longitude",
        y = "Latitude",
        fill = data_name(),
        title = paste(
          data_name(), "across", input$state,
          "counties during", input$year
        )
      )
  })

  # Outputs specific info for the selected county
  output$county_info <- renderTable({
    if (!is.null(input$map_click)) {
      data$county <- get_county(input$map_click$y, input$map_click$x, input$state)
      if (!is.null(data$county)) {
        obvs_table() %>%
          filter(County == paste(tools::toTitleCase(data$county), "County"))
      }
    } else {
      avg_table()
    }
  })
}
shinyApp(ui, server)
