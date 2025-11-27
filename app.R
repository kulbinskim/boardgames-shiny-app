
source("data_prep.R")
library(shiny)
library(reactable)
library(ggplot2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  tabsetPanel(
    tabPanel("Board game browser",
      
      fluidRow(
        column(3, selectInput("genre", "Genre", domains)),
        column(3, numericInput("rating", "Minimal rating", value = 7, min = 1, max = 10)),
        column(3, sliderInput("complexity", "Complexity", value = c(2,3),step = 0.01, min = 1, max = 5)), 
        column(3, sliderInput("players", "players", value = c(2,4), step = 1, min = 0, max = 10))
        
      ),
      fluidRow(
        column(4,
               div(style = "
               background-color: #d4edda; 
               border-left: 5px solid #28a745; 
               padding: 15px; 
               margin-bottom: 20px;
               border-radius: 5px;",
               h4("Top 3 games by rating"),
               tableOutput("top3_rating")
               )
        ),
      column(4,
               div(style = "
               background-color: #F2DD2E; 
               border-left: 5px solid #F2C42E; 
               padding: 15px; 
               margin-bottom: 20px;
               border-radius: 5px;",
                   h4("Top 3 games by complexity"),
                   tableOutput("top3_complexity")
               )
        ),
      column(4,
             div(style = "
               background-color: #2EC1F2; 
               border-left: 5px solid #2E89F2; 
               padding: 15px; 
               margin-bottom: 20px;
               border-radius: 5px;",
                 h4("Top 3 most pupular games"),
                 tableOutput("top3_popular")
             )
      )
 
      ),
      
      fluidRow(
        reactableOutput("chosen")
    
    )
  ),
  tabPanel("Ratings comparisons",
           fluidRow(h2("About"),
                    textOutput("tab2_description")),
           fluidRow(
             column(4, h2("Group 1"), style = "background-color:#D8F252;",
                    selectInput("genre1", "Genre", domains),
                    sliderInput("complexity1", "Complexity", value = c(2,3),step = 0.01, min = 1, max = 5),
                    sliderInput("players1", "Players", value = c(2,4), step = 1, min = 0, max = 10)),
           column(4, h2("Group 2"), style = "background-color:#52CFF2;",
                  selectInput("genre2", "Genre", domains),
                  sliderInput("complexity2", "Complexity", value = c(2,3),step = 0.01, min = 1, max = 5),
                  sliderInput("players2", "Players", value = c(2,4), step = 1, min = 0, max = 10)),
           column(4, h2("Average ratings"),
                  textOutput("mean1"),
                  textOutput("mean2")),
                  
                  
           fluidRow(
              column(6, h2("Histograms"), plotOutput("plot1")),
              column(6, h2("Boxplots"), plotOutput("plot2"))
             
           )
  )
  ),
  tabPanel("Complexity vs. Ratings",
           fluidRow(h2("About"),
                    textOutput('tab3_description')),
           fluidRow(
             column(4,h2("Data selection"),
                    selectInput("genre1a", "Genre", domains),
                    selectInput("variable", "Independent variable", 
                                c("Complexity" = "Complexity.Average", 
                                  "Number of users" = "Owned.Users"
                                     )),
                    sliderInput("players1a", "Players", value = c(2,4), step = 1, min = 0, max = 10)),
             column(8, h2("Model"),
                    verbatimTextOutput("model_description")
           ),
           fluidRow(
             plotOutput("model_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    thematic::thematic_shiny()
    filter(games, grepl(input$genre, Domains), Rating.Average >= input$rating,
        Complexity.Average >= input$complexity[1], Complexity.Average <= input$complexity[2], 
        Min.Players >= input$players[1], Max.Players <= input$players[2])
    })
  output$chosen <- renderReactable(reactable(selected()))
  top3_rating <- reactive({
    selected() %>%
      arrange(desc(Rating.Average)) %>%
      head(3) %>%
      select(Name, Rating.Average)
  })
  output$top3_ratig <- renderTable(top3_rating())
  top3_complexity <- reactive({
   
    selected() %>%
      arrange(desc(Complexity.Average)) %>%
      head(3) %>%
      select(Name, Complexity.Average)
  })
  output$top3_complexity <- renderTable(top3_complexity())
  top3_popular <- reactive({
    selected() %>%
      arrange(desc(Owned.Users)) %>%
      head(3) %>%
      select(Name, Owned.Users)
  })
  output$top3_popular <- renderTable(top3_popular())
  
  output$top3_rating <- renderTable(top3_rating(), striped = TRUE)
  output$top3_complexity <- renderTable(top3_complexity(), striped = TRUE)
  
  output$tab2_description <- renderText("Here you can investigate diffrences 
                                        between average ratings of two selected groups of games. \n")
  sample1 <- reactive({
    filter(games, grepl(input$genre1, Domains),
           Complexity.Average >= input$complexity1[1], Complexity.Average <= input$complexity1[2], 
           Min.Players >= input$players1[1], Max.Players <= input$players1[2]) %>%
      select(Rating.Average) %>%
      mutate(name = paste(input$genre1, "(Group 1)"))
      
  })
  sample2 <- reactive({
      filter(games, grepl(input$genre2, Domains),
             Complexity.Average >= input$complexity2[1], Complexity.Average <= input$complexity2[2], 
             Min.Players >= input$players2[1], Max.Players <= input$players2[2]) %>%
    select(Rating.Average) %>%
    mutate(name = paste(input$genre2," (Group 2)"), .before = 1)
  })
  mean1 <- reactive({sample1() %>% pull(Rating.Average) %>% mean()})
  mean2 <- reactive({sample2() %>% pull(Rating.Average) %>% mean()})
  
  output$mean1 <- renderText(paste("Mean rating in the Group 1 (",input$genre1, "): ", round(mean1(), 2)))
  output$mean2 <- renderText(paste("Mean rating in the Group 2 (",input$genre2, "): ", round(mean2(), 2)))
  
  output$plot1 <- renderPlot({
    binded <- rbind(sample1(), sample2())
   
    ggplot(binded, aes(x=Rating.Average, fill=name)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#52CFF2", "#D8F252")) + 
      labs(fill="") + theme(legend.position = c(0.9,0.9))
  
  })
  
  output$plot2 <- renderPlot({
    binded <- rbind(sample1(), sample2())
    
    ggplot(binded, aes(x=Rating.Average, fill=name)) +
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE) +
      scale_fill_manual(values=c("#52CFF2", "#D8F252")) + 
      labs(fill="") + theme(legend.position = c(0.9,0.9))
  })
  
output$tab3_description <- renderText("Here you can build a linear regression model to 
                                      investigate the relationship between complexity score and rating. \n ")
  
data_to_model <- reactive({
    games %>% filter(grepl(input$genre1a, Domains),
                            Min.Players >= input$players1a[1], Max.Players <= input$players1a[2]) %>%
                            select(Rating.Average, all_of(input$variable))
  })
  
model <- reactive({
  formula <- as.formula(paste("Rating.Average ~", input$variable))
  lm(formula, data = data_to_model())
})

output$model_description <- renderPrint(summary(model()))
output$model_plot <- renderPlot({
  ggplot(data_to_model(),
         aes(x = .data[[input$variable]], y = Rating.Average)) +
    geom_point() +
    geom_smooth(method = "lm")
})
  

}

shinyApp(ui, server)
