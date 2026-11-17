# AFL attendance analysis: an interactive Shiny app

How does a team's crowd size relate to where it finishes on the ladder? And which venues and states draw the biggest crowds? This interactive R Shiny app animates average match attendance across 21 AFL seasons, from 1999 to 2019.

**[Open the live app](https://01977c3c-503a-6409-7cf4-bf8d0edcd2bb.share.connect.posit.cloud/)**

Completed in 2025 during my Master of Data Science at RMIT University.

## What the app shows

The app has three views, each animated by season with a play button and year slider:

- **Team:** average attendance at each team's matches plotted against its final ladder position. Select up to five teams to compare how crowds move with on-field success.
- **Venue:** average attendance at each venue, ranked, with the best-attended venue each season highlighted.
- **State:** average attendance by state or territory, weighted by the number of matches played there, with the top state highlighted.

Hover over any point or bar for the underlying figures.

## Data

- **Match attendance:** 3,882 home-and-away matches from 1999 to 2019, scraped from [AFL Tables](https://afltables.com/) with the [fitzRoy](https://cran.r-project.org/package=fitzRoy) R package. Finals are excluded so that every team is compared on the same fixture.
- **Ladder positions:** each team's position after the final home-and-away round of each season, also from AFL Tables through fitzRoy.

Data from 2020 onwards could not be obtained, which also means the COVID-affected seasons are not included.

## Method

- The attendance data comes from a player-level source, with one row per player per match. It was reduced to one row per match.
- Historic team names were standardised so that every season lines up: Brisbane Lions to Brisbane, Footscray to Western Bulldogs, and Greater Western Sydney to GWS.
- A team's attendance figure for a season is the average crowd across all of its matches, home and away.
- Venue figures average every match played at that ground. State figures are weighted by the number of matches at each venue, so that a state's few small games do not count as much as its many large ones.
- Gold Coast and GWS joined the competition in 2011 and 2012. Before then they appear at zero attendance at the bottom of the ladder, and the app labels this on the chart.

## Files

- `app.R`: the complete app, including the data preparation. The scraping code is kept, commented out, for reference.
- `attend_clean.csv`: match attendance, one row per match
- `ladder_df.csv`: ladder standings by season and round
- `manifest.json`: deployment settings for Posit Connect Cloud

## Running it locally

Install the packages, then run the app from the repository folder:

```r
install.packages(c("shiny", "tidyr", "dplyr", "plotly", "shinyalert", "shinyBS",
                   "readxl", "fitzRoy", "lubridate", "purrr", "stringr", "readr"))
shiny::runApp()
```

## References

Baker, J., & Forster, T. (2023). *fitzRoy: Easily scrape and process AFL data* (R package version 1.1.1). https://cran.r-project.org/package=fitzRoy

Ross, R. (n.d.). *AFL Tables: Australian Rules Football statistics*. Retrieved June 16, 2025, from https://afltables.com/
