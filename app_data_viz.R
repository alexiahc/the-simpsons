# Load packages ----
library(shiny)
library(quantmod)
library(readr)
library(seededlda)
library(quanteda)
library(dplyr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(circlize)
library(grid)
library(reshape2)


# User interface ----
ui <- fluidPage(
  titlePanel("Script analysis - the Simpsons"),
  
  sidebarLayout(
    sidebarPanel(

      sliderInput(inputId = "seasons",
                  label = "Select the seasons",
                  value = c(1,26),
                  min = 1, max = 26),
    ),
    mainPanel()
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText(h1("Sentiment analysis"))
    ),
    mainPanel(helpText(h5("Representation of positive and negative words")),
              plotOutput("plot_cloud_sent"),
              helpText("Negative words are represented in red, postive words in blue.
                       The words' size is related to their number of apparition in the scripts."), 
              plotOutput("plot_overall_mood"),
              helpText("The emotional words are grouped by theme, and this is a 
                       representation of the main lexical fields of feelings that are 
                       used in the serie."),
              plotOutput("plot_chord_simps"), 
              helpText("This is a representation of the negativeness and positiveness 
                       you can observe in the Simpson family, who are the main speakers 
                       of the serie."), )
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText(h1("Women's representation "))
    ),
    mainPanel(plotOutput("plot_talkative"),
              helpText("This represents the number of time each character speaks in
                       the serie, categorization by gender."),
              helpText(h5("Overall mood by gender")),
              plotOutput("plot_sentiment_gender"),
              helpText("With the same principle as before, you can compare 
                       women and men's mood in The Simpsons."), 
              plotOutput("plot_ratio"),
              helpText("This is a visualization of the percentages of average speaking time
                        for women, of feminine character, of lines said by a woman, of speaking time of women 
                        and words said by a woman in the scripts."))
  ),
)

# Server logic
server <- function(input, output) {
  
  characters <- read_csv("data/simpsons_characters.csv")
  episodes <- read_csv("data/simpsons_episodes.csv")
  locations <- read_csv("data/simpsons_locations.csv")
  script_lines <- read_csv("data/simpsons_script_lines.csv")
  bing <- read_csv("data/Bing.csv")
  nrc <- read_csv("data/NRC.csv")
  afinn <- read_csv("data/Afinn.csv")
  
  script_lines <- script_lines %>% rename(line_id = "id")
  characters <- characters %>% rename(character_id = "id",
                                      character_name = "name",
                                      character_norm_name = "normalized_name")
  data <- merge(script_lines, characters, by= "character_id")
  locations <- locations %>% rename(location_id = "id",
                                    location_name = "name",
                                    location_norm_name = "normalized_name")
  data <- merge(data, locations, by= "location_id")
  episodes <- episodes %>% rename(episode_id = "id",
                                  episode_title = "title")
  data <- merge(data, episodes, by= "episode_id")
  data <- subset(data, speaking_line == TRUE)
  data <- subset(data, select = -c(number, raw_text, raw_character_text,
                                   raw_location_text, spoken_words, character_name,
                                   location_name, speaking_line))
  data$word_count <- as.numeric(data$word_count)
  data$word_count[is.na(data$word_count)] <- 0 
  
  dataInput <- reactive({
    return(subset(data, season >= input$seasons[1] & season <= input$seasons[2]))
  })
  
  output$plot_cloud_sent <- renderPlot({
    tokens <- dataInput() %>%
      mutate(dialogue = as.character(dataInput()$normalized_text)) %>%
      unnest_tokens(word, dialogue)
    
    tokens %>% head(5) %>% select(character_norm_name, word)
    
    tokens %>%
      # append the bing sentiment and prepare the data
      inner_join(bing, "word") %>%
      count(word, sentiment, sort=T) %>%
      acast(word ~ sentiment, value.var = "n", fill=0) %>%
      
      # wordcloud
      comparison.cloud(colors=c("#991D1D", "#327CDE"), max.words = 100)
  })
  
  output$plot_overall_mood <- renderPlot({
    tokens <- dataInput() %>% 
      mutate(dialogue = as.character(dataInput()$normalized_text)) %>% 
      unnest_tokens(word, dialogue)
    
    sentiments <- tokens %>% 
      inner_join(nrc, "word") %>%
      count(sentiment, sort=T)
    
    # The plot:
    sentiments %>% 
      ggplot(aes(x=reorder(sentiment, n), y=n)) +
      geom_bar(stat="identity", aes(fill=sentiment), show.legend=F) +
      geom_label(label=sentiments$n) +
      labs(x="Sentiment", y="Frequency", title="How is the overall mood") +
      coord_flip() + 
      theme_bw() 
    
  })
  
  output$plot_chord_simps <- renderPlot({
    tokens <- dataInput() %>% 
      mutate(dialogue = as.character(dataInput()$normalized_text)) %>% 
      unnest_tokens(word, dialogue)
    
    to_plot <- tokens %>% 
      # get 'bing' and filter the data
      inner_join(bing, "word") %>% 
      filter(character_norm_name %in% c("homer simpson", "marge simpson", "lisa simpson", "bart simpson")) %>% 
      
      # sum number of words per sentiment and character
      count(sentiment, character_norm_name) %>% 
      group_by(character_norm_name, sentiment) %>% 
      summarise(sentiment_sum = sum(n)) %>% 
      ungroup()
    
    # The Chord Diagram  
    circos.clear()
    circos.par(gap.after = c(rep(2, length(unique(to_plot[[1]])) - 1), 15,
                             rep(2, length(unique(to_plot[[2]])) - 1), 15), gap.degree=2)
    
    myColors = c("homer simpson" = "#FA8072", "marge simpson" = "#04700A", "lisa simpson" = "#75A9F3", "bart simpson" = "#FFCC46", "positive" = "#D7DBDD", "negative" = "#D7DBDD")
    
    chordDiagram(to_plot, grid.col = myColors, transparency = 0.4, annotationTrack = c("name", "grid"),
                 annotationTrackHeight = c(0.01, 0.02))
    title("Relationship between mood and the Simpson family")
  })
  
  output$plot_talkative <- renderPlot({
    subset(dataInput(), gender=="f" | gender=="m") %>% 
      # prepare the table
      count(character_norm_name) %>%
      arrange(desc(n)) %>% 
      slice(1:15) %>%
      merge(characters, by= "character_norm_name") %>%
      arrange(desc(n)) %>% 
      # the plot
      ggplot(aes(x=reorder(character_norm_name, n), y=n, color=gender)) +
      geom_bar(stat="identity", aes(fill=n), show.legend=F) +
      geom_label(aes(label=n)) +
      scale_fill_gradient(low="#33D1FF", high="#3383FF") +
      scale_color_manual(values=c("#E69F53", "#40B742")) +
      labs(x="Character", y="Number of dialogues", title="Most talkative in The Simpsons by gender") +
      coord_flip() +
      theme_bw()
  })
  
  output$plot_sentiment_gender <- renderPlot({
    tokens_women <- subset(dataInput(), gender=='f') %>% 
      mutate(dialogue = as.character(subset(dataInput(), gender=='f')$normalized_text)) %>% 
      unnest_tokens(word, dialogue)
    
    sentiments_women <- tokens_women %>% 
      inner_join(nrc, "word") %>%
      count(sentiment, sort=T)
    sentiments_women$gender <- "f"
    
    tokens_men <- subset(dataInput(), gender=='m') %>% 
      mutate(dialogue = as.character(subset(dataInput(), gender=='m')$normalized_text)) %>% 
      unnest_tokens(word, dialogue)
    
    sentiments_men <- tokens_men %>% 
      inner_join(nrc, "word") %>%
      count(sentiment, sort=T)
    sentiments_men$gender <- "m"
    
    sentiments <- rbind(sentiments_women, sentiments_men)
    
    p1 <- ggplot(sentiments[sentiments$gender == 'f',], aes(x=sentiment, y= n)) + geom_col(fill='#E69F53') + theme_minimal() +
      coord_flip() + scale_y_reverse(name= "Women",expand = expand_scale(mult= c(c(0.05,0)))) +
      theme(panel.spacing.x = unit(0, "mm")) +
      theme(plot.margin = unit(c(5.5, 0, 5.5, 5.5), "pt"))
    
    p2 <- ggplot(sentiments[sentiments$gender == 'm',], aes(x=sentiment, y= n)) + geom_col(fill='#40B742') + theme_minimal() +
      coord_flip() + scale_y_continuous(name = "Men", expand = expand_scale(mult= c(c(0,0.05)))) +
      theme(panel.spacing.x = unit(0, "mm")) + 
      theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
            axis.line.y = element_blank(), axis.ticks.y=element_blank(),
            plot.margin = unit(c(5.5, 5.5, 5.5, -3.5), "pt"))
    
    grid.newpage()
    grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  })
  
  output$plot_ratio <- renderPlot({
    # number of women 
    ratio_nb_women <- length(unique(subset(dataInput(), gender == "f")$character_id)) / 
                      (length(unique(subset(dataInput(), gender == "f")$character_id)) + 
                         length(unique(subset(dataInput(), gender == "m")$character_id)))
    
    # time of speaking
    ratio_women_time <- sum(subset(dataInput(), gender=='f')$timestamp_in_ms) / (sum(subset(dataInput(), gender=='f')$timestamp_in_ms) 
                                                           + sum(subset(dataInput(), gender=='m')$timestamp_in_ms))
    
    # mean time of speaking per lines
    ratio_women_time_mean <- mean(subset(dataInput(), gender=='f')$timestamp_in_ms) / (mean(subset(dataInput(), gender=='f')$timestamp_in_ms) + 
                                                                   mean(subset(dataInput(), gender=='m')$timestamp_in_ms))
    
    # number of lines 
    ratio_women_nb_lines <- length(subset(dataInput(), gender=='f')$line_id) / (length(subset(dataInput(), gender=='f')$line_id) + 
                                                            length(subset(dataInput(), gender=='m')$line_id))
    # word count
    ratios_nb_word <- sum(subset(dataInput(), gender=='f')$word_count) / (sum(subset(dataInput(), gender=='f')$word_count) 
                                                    + sum(subset(dataInput(), gender=='m')$word_count))
    
    df_ratios <- data.frame(criteria=c("number of characters", "number of lines in scripts", 
                                       "speaking time", "mean of speaking time", 
                                       "word count"),
                            ratio=c(ratio_nb_women, ratio_women_nb_lines,
                                    ratio_women_time, ratio_women_time_mean, 
                                    ratios_nb_word))
    ggplot(data=df_ratios, aes(x=criteria, y=ratio)) +
      geom_bar(stat="identity", fill ="#E69F53") +
      labs(title="Frequency of women's apparition for some criteria",
           y="Frequency")
  })
  
}

# Run the app
shinyApp(ui, server)