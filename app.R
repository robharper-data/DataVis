library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
library(shinyalert)
library(shinyBS)
library(readxl)
library(fitzRoy)
library(lubridate)
library(purrr)
library(stringr)
library(readr)

# Original Data Loaded
#attend <- fetch_player_stats_fryzigg(season=1999:2019)
# ladder_df <- fetch_ladder_afltables(
#   season = 1999:2019,
#   round_number = 1:24
# )
# # Original data saved
# #write_csv(attend, "attend.csv")
# write_csv(ladder_df, "ladder_df.csv")
# Original data loaded
attend <- read_csv("attend.csv")
ladder_df <- read_csv("ladder_df.csv")

# Function to standardise team names
standardise_team_names <- function(team_vector) {
  team_vector <- case_when(
    team_vector == "Brisbane Lions" ~ "Brisbane",
    team_vector == "Footscray" ~ "Western Bulldogs",
    team_vector == "Greater Western Sydney" ~ "GWS",
    TRUE ~ team_vector
  )
  return(team_vector)
}

##### Attendance data cleaning ######
# Extract year from ymd column
attend <- attend %>%
  mutate(
    Year = year(ymd(match_date))
  )

# Change data type to integer for match_round
attend$match_round <- as.integer(attend$match_round)

# Exclude data from finals and view attendance by match
attend_clean <- attend %>%
  filter(match_round <= 24) %>%
  group_by(Year, match_round, match_home_team, match_away_team, venue_name) %>%
  summarise(match_attendance = first(match_attendance), .groups = "drop")

# Apply standardised team names
attend_clean <- attend_clean %>%
  mutate(
    match_home_team = standardise_team_names(match_home_team),
    match_away_team = standardise_team_names(match_away_team)
  )

# Pivot data longer
attendance_long <- attend_clean %>%
  select(Year, match_round, match_home_team, match_away_team, match_attendance, venue_name) %>%
  pivot_longer(
    cols = c(match_home_team, match_away_team),
    names_to = "home_or_away",
    values_to = "Team"
  )

# Extract attendance by team
team_attendance <- attendance_long %>%
  group_by(Year, Team) %>%
  summarise(
    Avg_match_attendance = mean(match_attendance, na.rm = TRUE),
    Num_matches = n(),   # optional — just to check how many games counted
    .groups = "drop"
  )

# Extract attendance by venue
venue_attendance <- attendance_long %>%
  group_by(Year, venue_name) %>%
  summarise(
    Avg_match_attendance = mean(match_attendance, na.rm = TRUE),
    Num_matches = n()
  ) %>%
  ungroup()

# Add state column to venue
venue_attendance <- venue_attendance %>%
  mutate(
    venue_state = case_when(
      venue_name %in% c("MCG", "Marvel Stadium", "Ikon Park", "Mars Stadium", "GMHBA Stadium") ~ "VIC",
      venue_name %in% c("Adelaide Oval", "Football Park") ~ "SA",
      venue_name %in% c("SCG", "GIANTS Stadium","ANZ Stadium", "Blacktown") ~ "NSW",
      venue_name %in% c("Optus Stadium", "Subiaco") ~ "WA",
      venue_name %in% c("Gabba", "Metricon Stadium", "Riverway Stadium", "Cazaly's Stadium") ~ "QLD",
      venue_name %in% c("TIO Stadium", "TIO Traeger Park") ~ "NT",
      venue_name %in% c("Blundstone Arena", "University of Tasmania Stadium") ~ "TAS",
      venue_name %in% c("WACA", "Subiaco") ~ "WA",
      venue_name == "UNSW Canberra Oval" ~ "ACT",
      TRUE ~ "Intl."
    )
  )

# Summarise match attendance by state
state_attendance <- venue_attendance %>%
  group_by(Year, venue_state) %>%
  summarise(
    Avg_match_attendance = sum(Avg_match_attendance * Num_matches) / sum(Num_matches),
    Num_matches = sum(Num_matches)
  ) %>%
  ungroup()

###### Ladder Data Cleaning ######
# Standardise team names
ladder_df <- ladder_df %>%
  mutate(
    Team = standardise_team_names(Team)
  )

# Extract ladder position for the final round of each season
ladder_final_df <- ladder_df %>%
  group_by(Season) %>%
  filter(Round.Number == max(Round.Number)) %>%
  ungroup() %>%
  select(Season, Team, Ladder.Position)

#### JOINS ####
merged_df <- team_attendance %>%
  inner_join(ladder_final_df, by = c("Year" = "Season", "Team"))

# Create full grid of all teams and years
all_teams <- sort(unique(merged_df$Team))
all_years <- sort(unique(merged_df$Year))
full_grid <- expand.grid(Year = all_years, Team = all_teams)

# Merge full grid with existing data
# This needed to be done to account for no data for expansion teams before 2011/2012
merged_complete <- full_grid %>%
  left_join(merged_df, by = c("Year", "Team"))

# Edit data for those expansion teams
merged_complete <- merged_complete %>%
  mutate(
    Avg_match_attendance = ifelse(is.na(Avg_match_attendance), 0, Avg_match_attendance),
    Num_matches = ifelse(is.na(Num_matches), 0, Num_matches),
    Ladder.Position = ifelse(is.na(Ladder.Position), 18, Ladder.Position)
  )

# AFL entry years (needed for visualisation)
entry_years <- list(
  "Gold Coast" = 2011,
  "GWS" = 2012
)

# Create shortened names for visualisation. Retaining original/official names
unique(venue_attendance$venue_name)
venue_attendance <- venue_attendance %>%
  mutate(
    venue_short = case_when(
      venue_name == "University of Tasmania Stadium" ~ "UTAS",
      venue_name == "Marvel Stadium" ~ "Marvel",
      venue_name == "Adelaide Oval" ~ "Adelaide Oval",
      venue_name == "GMHBA Stadium" ~ "GMHBA",
      venue_name == "UNSW Canberra Oval" ~ "UNSWCO",
      venue_name == "Metricon Stadium" ~ "Metricon",
      venue_name == "Riverway Stadium" ~ "Riverway",
      venue_name == "Jiangwan Stadium" ~ "Jiangwan",
      venue_name == "Mars Stadium" ~ "Mars",
      venue_name == "Optus Stadium" ~ "Optus",
      venue_name == "GIANTS Stadium" ~ "GIANTS",
      venue_name == "TIO Stadium" ~ "TIO S",
      venue_name == "TIO Traeger Park" ~ "TIO TP",
      venue_name == "Ikon Park" ~ "Ikon",
      TRUE ~ venue_name  # keep default if no mapping
    )
  )


# venue_attendance_long <- attendance_long %>%
#   mutate(
#     venue_state = case_when(
#       venue_name %in% c("MCG", "Marvel Stadium", "Ikon Park", "Mars Stadium", "GMHBA Stadium") ~ "VIC",
#       venue_name %in% c("Adelaide Oval", "Football Park") ~ "SA",
#       venue_name %in% c("SCG", "GIANTS Stadium","ANZ Stadium", "Blacktown") ~ "NSW",
#       venue_name %in% c("Optus Stadium", "Subiaco") ~ "WA",
#       venue_name %in% c("Gabba", "Metricon Stadium", "Riverway Stadium", "Cazaly's Stadium") ~ "QLD",
#       venue_name %in% c("TIO Stadium", "TIO Traeger Park") ~ "NT",
#       venue_name %in% c("Blundstone Arena", "University of Tasmania Stadium") ~ "TAS",
#       venue_name %in% c("WACA", "Subiaco") ~ "WA",
#       venue_name == "UNSW Canberra Oval" ~ "ACT",
#       TRUE ~ "Intl."
#     )
#   )



team_colors <- c(
  "Adelaide"         = "#F8766D",  # Coral red
  "Brisbane"         = "#800000",  
  "Carlton"          = "#000075",  
  "Collingwood"      = "#000000",  
  "Essendon"         = "#F564E3",  # Pink-violet
  "Fremantle"        = "#911eb4",  
  "Geelong"          = "#4363D8",
  "Gold Coast"       = "#ffe119",  
  "GWS"              = "#F58231",  
  "Hawthorn"         = "#9A6324",  
  "Melbourne"        = "#fabed4",  
  "North Melbourne"  = "#ffd8b1",  
  "Port Adelaide"    = "#469990",  
  "Richmond"         = "#a9a9a9",
  "St Kilda"         = "#808000",  
  "Sydney"           = "#e6194B",  
  "West Coast"       = "#bfef45",  # Purple
  "Western Bulldogs" = "#42D4F4"   # Teal
)




# Messages/Help Text
welcome <- "<b>Welcome to the AFL Attendance Analysis App</b><br>
Go ahead and compare teams attendance at AFL matches from 2000 to 2020! 
You can also compare how your favourite stadium and state stack up against the others!"

view_mode <- "Compare match attendance by team, venue, or state (Select one)"

teamtip <- "Select up to five teams to compare"

# UI
ui <- fixedPage(
  titlePanel("AFL Analysis App"),
  
  sidebarPanel(
    
    radioButtons("view_mode", "View attendance By: ",
                 choices = c("Team", "Venue", "State"),
                 selected = "Team",
                 inline = TRUE),
    
    bsTooltip(id = "view_mode", title = view_mode, 
              placement = "center", trigger = "hover"),
    
    conditionalPanel(
      condition = "input.view_mode == 'Team'",
      selectizeInput("teams", "Select Teams (up to 5):",
                     choices = sort(unique(merged_complete$Team)),
                     selected = c("Geelong", "Western Bulldogs", "Essendon", "Collingwood", "Sydney"),
                     multiple = TRUE,
                     options = list(maxItems = 5)
                     ),
    
      bsTooltip(id = "teams", title = teamtip, 
                placement = "center", trigger = "hover"),
    )
    ),
  mainPanel(
    plotlyOutput("attendancePlot"),
    HTML("
  <div style='font-size:12px; color:grey; text-align:left; margin-top:20px;'>
    By Robert Harper<br>
    Data scraped from AFL Tables using the fitzRoy R package. Data from 2020 onward unable to be obtained.<br>
    <b>References:</b><br>
    Baker, J., & Forster, T. (2023). <i>fitzRoy: Easily scrape and process AFL data</i> (R package version 1.1.1). 
    <a href='https://cran.r-project.org/package=fitzRoy' target='_blank'>https://cran.r-project.org/package=fitzRoy</a><br>
    Ross, R. (n.d.). <i>AFL Tables: Australian Rules Football statistics</i>. Retrieved June 16, 2025, from 
    <a href='https://afltables.com/' target='_blank'>https://afltables.com/</a><br>
  </div>
  ")
  )
)

# Server
server <- function(input, output, session) {
  shinyalert("Welcome", welcome, 
             html = TRUE,
             imageUrl = "")
  
  filtered_data <- reactive({
    if (input$view_mode == "Team") {
      req(input$teams)
      merged_complete %>% filter(Team %in% input$teams)
      
    } else if (input$view_mode == "Venue") {
      venue_attendance %>%
        group_by(Year) %>%
        mutate(
          is_top_venue = Avg_match_attendance == max(Avg_match_attendance, na.rm = TRUE),
          bar_color = ifelse(is_top_venue, "orange", "steelblue")
        ) %>%
        ungroup()
      
    } else {  # State mode
      state_attendance %>%
        group_by(Year) %>%
        mutate(
          is_top_state = Avg_match_attendance == max(Avg_match_attendance, na.rm = TRUE) & !is.na(Avg_match_attendance),
          bar_color = ifelse(is_top_state, "orange", "purple")
        ) %>%
        ungroup()
    }
  })
  
  output$attendancePlot <- renderPlotly({
    df <- filtered_data()
    
    #### TEAM VIEW ####
    if (input$view_mode == "Team") {
      annotations_static <- list()
      
      if ("Gold Coast" %in% input$teams)
        annotations_static <- append(annotations_static, list(list(text="Gold Coast didn't join until 2011", x=60000, y=17, showarrow=FALSE, font=list(size=12,color="grey"))))
      
      if ("GWS" %in% input$teams)
        annotations_static <- append(annotations_static, list(list(text="GWS didn't join until 2012", x=60000, y=18, showarrow=FALSE, font=list(size=12,color="grey"))))
      
      #### Build plot ####
      p <- plot_ly(
        data = df,
        x = ~Avg_match_attendance,
        y = ~Ladder.Position,
        color = ~Team,
        colors = team_colors,
        type = 'scatter',
        mode = 'markers',
        frame = ~Year,
        marker = list(size = 15, opacity = 0.85),
        text = ~paste("Team:", Team, "<br>Year:", Year, "<br>Attendance:", round(Avg_match_attendance, 0),
                      ifelse(Year == 2020, "<br><span style='color:red;'>Unable to obtain match attendance data for 2020 onwards</span>", "")),
        hoverinfo = "text"
      ) 
      
      p <- p %>% layout(
        title = list(text = "Average Match Attendance Vs. Ladder Position", x = 0.5, xanchor = "center"),
        yaxis = list(title = "Ladder Position", autorange = FALSE, range = c(20, 0), tickmode = "array", tickvals = seq(1, 18, 2), zeroline = FALSE),
        xaxis = list(title = "Average Match Attendance", range = c(-5000, 70000), tickformat = "~s", zeroline = FALSE),
        legend = list(orientation = "v", x = 1.05, y = 0.95, xanchor = "left", yanchor = "top"),
        annotations = annotations_static
      ) %>%
        config(displayModeBar = FALSE) %>%
        animation_slider(currentvalue = list(prefix = "Year ", font = list(color = "black")))
      
      #### VENUE VIEW ####
    } else if (input$view_mode == "Venue") {
      
      p <- plot_ly(
        data = df,
        x = ~Avg_match_attendance,
        y = ~reorder(venue_short, Avg_match_attendance),
        type = 'bar',
        orientation = 'h',
        frame = ~Year,
        marker = list(color = ~bar_color, opacity = 0.85),
        text = ~paste("Venue:", venue_name, "<br>State:", venue_state, "<br>Avg Attendance:", round(Avg_match_attendance, 0)),
        hoverinfo = "text"
      )
      
      p <- p %>% layout(
        title = list(text = "Average Match Attendance by Venue", x = 0.5, xanchor = "center"),
        yaxis = list(title = "", categoryorder = "total ascending", fixedrange = TRUE),
        margin = list(l = 100, r = 20, t = 50, b = 50), 
        xaxis = list(title = "Average Match Attendance", range = c(0, 60000), tickformat = "~s", zeroline = FALSE),
        legend = list(orientation = "h", x = 0.5, y = 1.15, xanchor = "center", yanchor = "bottom")
      ) %>%
        config(displayModeBar = FALSE) %>%
        animation_slider(currentvalue = list(prefix = "Year ", font = list(color = "black")))
      
      #### STATE VIEW ####
    } else {
      p <- plot_ly(
        data = df,
        x = ~Avg_match_attendance,
        y = ~reorder(venue_state, Avg_match_attendance),
        type = 'bar',
        orientation = 'h',
        frame = ~Year,
        marker = list(color = ~bar_color, opacity = 0.9),
        text = ~paste("Avg Attendance:", round(Avg_match_attendance, 0)),
        hoverinfo = "text"
      )
      
      p <- p %>% layout(
        title = list(text = "Average Match Attendance by State", x = 0.5, xanchor = "center"),
        yaxis = list(title = "", categoryorder = "total ascending", fixedrange = TRUE),
        margin = list(l = 50, r = 20, t = 50, b = 50), 
        xaxis = list(title = "Average Match Attendance", range = c(0, 55000), tickformat = "~s", zeroline = FALSE),
        showlegend = FALSE
      ) %>%
        config(displayModeBar = FALSE) %>%
        animation_slider(currentvalue = list(prefix = "Year ", font = list(color = "black")))
    }
    
    return(p)
  })
  
}
# Run App
shinyApp(ui, server)