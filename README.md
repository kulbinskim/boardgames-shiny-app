# Board Games Explorer (Shiny App)

This is a small Shiny application for exploring BoardGameGeek (BGG) data from Kaggle.

## Features
- Filtering by genre, complexity, rating, number of players
- Top 3 games by rating, complexity, and popularity
- Rating distributions comparison
- Regression model between rating and selected variable

## How to run
```r
shiny::runApp("app.R")
