library(shiny)
library(cleanNLP)
library(tidyverse)
library(Frankenstein)
library(plotly)

anno <- anno_Frankenstein

token_with_chapters <- anno$token |>
  mutate(section_number = 
           cumsum((str_detect(token, regex("^Chapter", ignore_case = FALSE)) 
                   | str_detect(token, regex("^Letter", ignore_case = FALSE)) |
                     doc_id == 5945) # switches over to Walton POV in chapter 24
                  & tid == 1),
         narrator = case_when(
           section_number %in% c(1, 2, 3, 4, 29) ~ "Captain Walton",
           section_number %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 21, 22, 23, 
                                 24, 25, 26, 27, 28) ~ "Victor Frankenstein",
           section_number %in% c(15, 16, 17, 18, 19, 20) ~ "The Creature"
         ))

# create a function to pull out the parts of speech by sentence
extract_sentence_info <- function(df) {
  # empty df
  sentence_data <- tibble(
    narrator = character(),
    sentence = character(),
    num_nouns = integer(),
    num_verbs = integer(),
    num_proper_nouns = integer(),
    num_adjectives = integer(),
    num_adverbs = integer(),
    num_words = integer(),
    section_number = integer()
  )
  
  # part of speech variables
  num_nouns <- 0
  num_verbs <- 0
  num_proper_nouns <- 0
  num_adjectives <- 0
  num_adverbs <- 0
  temp_sentence <- ""
  current_narrator <- df$narrator[1]  # set the first narrator
  
  for (i in 1:nrow(df)) {
    current_token <- df$token[i]
    current_upos <- df$upos[i]
    current_narrator <- df$narrator[i]  # Update current narrator
    current_chapter <- df$section_number[i]  # Update current chapter
    
    # Count parts of speech
    if (current_upos == "NOUN") {
      num_nouns <- num_nouns + 1
    } else if (current_upos == "VERB") {
      num_verbs <- num_verbs + 1
    } else if (current_upos == "PROPN") {
      num_proper_nouns <- num_proper_nouns + 1
    } else if (current_upos == "ADJ") {
      num_adjectives <- num_adjectives + 1
    } else if (current_upos == "ADV") {
      num_adverbs <- num_adverbs + 1
    }
    
    # combine tokens to sentence, handling punctuation
    if (current_upos != "PUNCT" && !str_detect(
      df$token[max(i - 1, 1)], "[.?!]")) {
      temp_sentence <- paste(temp_sentence, current_token, sep = " ")
    } else {
      temp_sentence <- paste(temp_sentence, current_token, sep = "")
    }
    
    # if sentence ends, add to df
    if (current_upos == "PUNCT" && str_detect(current_token, "[.?!]")) {
      temp_sentence <- str_squish(temp_sentence)  # Clean up sentence
      num_words <- str_count(temp_sentence, "\\S+")  # Count words
      
      # Add variables to df
      sentence_data <- rbind(sentence_data, tibble(
        narrator = current_narrator,
        sentence = temp_sentence,
        num_nouns = num_nouns,
        num_verbs = num_verbs,
        num_proper_nouns = num_proper_nouns,
        num_adjectives = num_adjectives,
        num_adverbs = num_adverbs,
        num_words = num_words,
        section_number = current_chapter
      ))
      
      # reset variables
      temp_sentence <- ""
      num_nouns <- num_verbs <- num_proper_nouns <- num_adjectives <- num_adverbs <- 0
    }
  }
  
  sentence_data <- sentence_data |>
    mutate(dominant_pos = case_when(
      num_nouns >= num_verbs & num_nouns >= num_adjectives & num_nouns >= num_adverbs ~ "Nouns",
      num_verbs >= num_nouns & num_verbs >= num_adjectives & num_verbs >= num_adverbs ~ "Verbs",
      num_adjectives >= num_nouns & num_adjectives >= num_verbs & num_adjectives >= num_adverbs ~ "Adjectives",
      num_adverbs >= num_nouns & num_adverbs >= num_verbs & num_adverbs >= num_adjectives ~ "Adverbs",
      TRUE ~ "Tie"
    ))
  
  return(sentence_data)
}

# extract the sentence info
sentence_info <- extract_sentence_info(token_with_chapters)

# summarize all of the information
pos_by_chap <- sentence_info |>
  group_by(section_number, narrator) |>
  summarise(
    NOUN = sum(num_nouns),
    VERB = sum(num_verbs),
    PROPN = sum(num_proper_nouns),
    ADJ = sum(num_adjectives),
    ADV = sum(num_adverbs),
    total_words = sum(num_words),
    .groups = 'drop') |> 
  pivot_longer(cols = 3:7, names_to = "upos", values_to = "count") |>
  mutate(prop = count/total_words)

# ui
ui <- navbarPage("Style", 
  # TAB 1: ABOUT
  tabPanel("About", 
           fixedPage(
             titlePanel("About"),
             fluidRow(
               column(12,
                      h3("Frankenstein Style Shiny App"),
                      p("This Shiny App provides an exploration into the 
                        literary style and structure of Mary Shelley's novel, 
                        'Frankenstein'. It allows users to explore the details 
                        of the text, including its unique sentence structures 
                        and the frequency of different parts of speech. 
                        This app offers a new perspective on this classic 
                        work."),
                      br(),
                      
                      h4("Parts of Speech Tab"),
                      p("Here is an in-depth analysis of the frequency and 
                        distribution of various parts of speech: nouns, proper 
                        nouns, adjectives, verbs, and adverbs relative to the 
                        total words in each section. Interactive charts and 
                        visualizations allow you to explore how these elements 
                        contribute to the novel's unique style."),
                      br(),
                      
                      h4("Sentence Structure Tab"),
                      p("Explore Mary Shelley's narrative style. This tab 
                        provides a detailed look at the sentence structure 
                        within 'Frankenstein', highlighting the variation in 
                        length and composition. Look at the frequency of 
                        different parts of speech in each sentence, the 
                        narrative voice, and the total word count to better 
                        understand the sentence structure. You can also explore
                        the the part of speech that appears the most in each 
                        sentence."),
                      br(),
                      
                      h4("Resources"),
                      p("For this Shiny app, cleanNLP was an essential package
                        that was used to create the annotated version of the 
                        text. To see the Gutenberg version of Frankenstein,
                        use the link below:"),
                      a("Gutenberg version of Frankenstein", 
                        href="https://www.gutenberg.org/ebooks/84",
                        target="_blank"),
                      br(),
                      
                      h4("About Me"),
                      p("My name is Justin Papagelis. I am an Amherst College 
                        statistics student enrolled in STAT325: Text Analysis 
                        where I am using the skills I have learned to conduct 
                        an important analysis on a novel of my choosing!")
                      
               )
             )
           )
  )
  ,
  
  
  # TAB 2: PARTS OF SPEECH
  tabPanel("Parts of Speech",
           fixedPage(
             titlePanel("Parts of Speech"),
             fixedRow(
               column(4,
                      wellPanel(
                        selectInput(inputId = "pos_type", 
                                    label = "Choose Part of Speech:",
                                    choices = c("Verb" = "VERB", 
                                                "Noun" = "NOUN", 
                                                "Proper Noun" = "PROPN", 
                                                "Adjective" = "ADJ", 
                                                "Adverb" = "ADV"))
                        )
                      ),
               fixedRow(column(12, mainPanel(plotOutput(
                 outputId = "part_of_speech"))))
              )
            )
          ),

  # TAB 3: SENTENCE STRUCTURE
  tabPanel("Sentence Structure",
         fixedPage(
           titlePanel("Interactive Sentence Structure"),
           fluidRow(
             column(4,
                    wellPanel(
                      selectInput(inputId = "selected_section", 
                                  label = "Choose a section (or all):",
                                  choices = c("All", as.character(1:28))) 
                    )
             ),
             column(12, plotlyOutput("sentencePlot")),
             column(12, verbatimTextOutput("selectedSentenceDetails"))
            )
          )
  )
)



# server
server <- function(input, output) {
  
  
  # TAB 1: INTERACTIVE Part of Speech Plot
  
  output$part_of_speech <- renderPlot({
    pos_by_chap |>
      filter(upos == input$pos_type) |>
      ggplot(aes(x = section_number, y = prop)) +
      geom_col(aes(fill = narrator)) +
      geom_smooth(se = FALSE) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Frequency of Parts of Speech in Frankenstein",
           fill = "Narrator", 
           x = "Section", y = "Frequency")
  })
  
  
  
  
  filtered_data <- reactive({
    # filter based on selected section
    if (input$selected_section == "All") {
      sentence_info |>
        mutate(row_num = row_number())
    } else {
      sentence_info |>
        filter(section_number == as.numeric(input$selected_section)) |>
        mutate(row_num = row_number())
    }
    
  })
  
  
  # Plotly graph
  output$sentencePlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~row_num, y = ~num_words, type = 'scatter', 
            mode = 'markers', hoverinfo = 'x+y', text = ~sentence,
            color = ~dominant_pos,
            source = 'sentencePlot') |>
      layout(xaxis = list(title = 'Sentence Number'), 
             yaxis = list(title = 'Number of Words'),
             legend = list(title = list(text = 'Dominant Part of Speech')))
  })
  
  # Display sentence details when a point is clicked
  output$selectedSentenceDetails <- renderText({
    eventdata <- event_data("plotly_click", source = 'sentencePlot')
    if (!is.null(eventdata)) {
      selected_index <- eventdata$pointNumber + 1  # Adjust index
      selected_sentence <- filtered_data()[selected_index, ] 
      sentence <- paste("Selected Sentence:", selected_sentence$sentence)
      narrator <- paste("Narrator", selected_sentence$narrator)
      nouns <- paste("Nouns:", selected_sentence$num_nouns)
      verbs <- paste("Verbs:", selected_sentence$num_verbs)
      adjs <- paste("Adjectives:", selected_sentence$num_adjectives)
      advs <- paste("Adverbs:", selected_sentence$num_adverbs)
      num_words <- paste("Number of words:", selected_sentence$num_words)
      
      sentence <- str_wrap(sentence, width = 115) # make adjustable
      
      
      
      paste(sentence, narrator, nouns, verbs, adjs, advs, num_words,
            sep = "\n")
      
      
    } else {
      "Choose on a point in the scatter plot to see more information about the sentence."
    }
  })
}


# call 
shinyApp(ui = ui, server = server)

