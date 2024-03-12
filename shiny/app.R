library(shiny)
library(cleanNLP)
library(tidyverse)
library(Frankenstein)
library(plotly)

anno <- anno_Frankenstein


anno$token


token_with_chapters <- anno$token |>
  mutate(section_number = 
           cumsum((str_detect(token, regex("^Chapter", ignore_case = FALSE)) 
                   | str_detect(token, regex("^Letter", ignore_case = FALSE)) |
                     doc_id == 5945) # switches over to Walton POV in chapter 24
                  & tid == 1),
         narrator = case_when(
           section_number %in% c(1, 2, 3, 4, 29) ~ "Captain Walton",
           section_number %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 21, 22, 23, 24, 25, 26, 27, 28) ~ "Victor Frankenstein",
           section_number %in% c(15, 16, 17, 18, 19, 20) ~ "The Creature"
         ))

extract_sentence_info <- function(df) {
  # Initialize an empty data frame to store results
  sentence_data <- tibble(
    narrator = character(),
    sentence = character(),
    num_nouns = integer(),
    num_verbs = integer(),
    num_proper_nouns = integer(),
    num_adjectives = integer(),
    num_adverbs = integer(),
    num_words = integer(),
    chapter_number = integer()
  )
  
  # Variables for counting parts of speech within a sentence
  num_nouns <- num_verbs <- num_proper_nouns <- num_adjectives <- num_adverbs <- 0
  temp_sentence <- ""
  current_narrator <- df$narrator[1]  # Assuming the first entry gives the first narrator
  
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
    
    # Concatenate token to sentence, handling punctuation
    if (current_upos != "PUNCT" && !str_detect(df$token[max(i - 1, 1)], "[.?!]")) {
      temp_sentence <- paste(temp_sentence, current_token, sep = " ")
    } else {
      temp_sentence <- paste(temp_sentence, current_token, sep = "")
    }
    
    # Check if the sentence ends, then save to data frame
    if (current_upos == "PUNCT" && str_detect(current_token, "[.?!]")) {
      temp_sentence <- str_squish(temp_sentence)  # Clean up sentence
      num_words <- str_count(temp_sentence, "\\S+")  # Count words
      
      # Add sentence information to data frame
      sentence_data <- rbind(sentence_data, tibble(
        narrator = current_narrator,
        sentence = temp_sentence,
        num_nouns = num_nouns,
        num_verbs = num_verbs,
        num_proper_nouns = num_proper_nouns,
        num_adjectives = num_adjectives,
        num_adverbs = num_adverbs,
        num_words = num_words,
        chapter_number = current_chapter
      ))
      
      # Reset variables for next sentence
      temp_sentence <- ""
      num_nouns <- num_verbs <- num_proper_nouns <- num_adjectives <- num_adverbs <- 0
    }
  }
  
  return(sentence_data)
}

sentence_info <- extract_sentence_info(token_with_chapters)




average_sentence_length <- token_with_chapters |>
  group_by(narrator, section_number, doc_id, sid) |>
  summarise(tokens_per_sentence = n(), .groups = 'drop') |>
  group_by(narrator, section_number, doc_id) |>
  summarise(
    average_length = mean(tokens_per_sentence),
    num_sentences = n(),
    .groups = 'drop'
  ) |>
  ungroup() 


chapter_info <- token_with_chapters |>
  filter(upos != "PUNCT") |>
  group_by(narrator, section_number, doc_id, sid) |>
  summarise(tokens_per_sentence = n(), .groups = 'drop') |>
  group_by(narrator, section_number) |>
  summarise(num_words = sum(tokens_per_sentence), num_sentences = n(), 
            .groups = 'drop') |>
  ungroup() |>
  mutate(words_per_sentence = num_words/num_sentences)



# done
part_of_speech_df <- token_with_chapters |>
  group_by(upos, section_number, narrator) |>
  summarize(count = n(), .groups = "drop") |>
  arrange(section_number) |>
  pivot_wider(names_from = upos, values_from = count) |>
  select(section_number, narrator, VERB, NOUN, PROPN, ADJ, ADV) |>
  inner_join(chapter_info, by = c("section_number", "narrator")) |>
  pivot_longer(cols = 3:7, names_to = "upos", values_to = "n" ) |>
  mutate(prop = n/num_words)



  


#  group_by(narrator, section_number) |>
 # summarise(
  #  chapter_average = mean(average_length),
   # total_sentences = sum(num_sentences),
    #.groups = 'drop'
#  )

#section_average_sentence <- token_with_chapters |>
#  group_by(section_number, doc_id, sid) |>
#  summarise(tokens_per_sentence = n(), .groups = 'drop') |>
#  group_by(section_number, doc_id) |>
#  summarise(average_length = mean(tokens_per_sentence)) |>
#  ungroup() |>
#  filter(section_number == 1)


#verbs <- token_with_chapters |> 
 # filter(upos == "VERB") |>
#  group_by(section_number, narrator) |>
#  summarize(count = n(), .groups = "drop") |>
#  arrange(section_number)



# need to remove names ? and __
bigrams_with_sections <- token_with_chapters |>
  filter(upos %in% c("NOUN", "ADJ", "VERB", "ADV", "PROPN")) |>
  group_by(doc_id, section_number, sid) |>
  mutate(next_word = lead(token, order_by = tid)) |> 
  ungroup() |>
  filter(!is.na(next_word)) |>
  unite("bigram", c(token, next_word), sep = " ") |> 
  count(bigram, section_number, sort = TRUE) 



# ui
ui <- navbarPage("Style", 
  # TAB 1: ABOUT
  tabPanel("About", 
           fixedPage(
             titlePanel("About"),
             # ADD ABOUT SECTION HERE
             )
           ),
  
  # TAB 2: PARTS OF SPEECH
  tabPanel("Parts of Speech",
           fixedPage(
             titlePanel("Parts of Speech"),
             fixedRow(
               column(4,
                      wellPanel(
                        selectInput(inputId = "pos_type", label = "Choose POS:",
                                    choices = c("Verb" = "VERB", 
                                                "Noun" = "NOUN", 
                                                "Proper Noun" = "PROPN", 
                                                "Adjective" = "ADJ", 
                                                "Adverb" = "ADV"))
                        )
                      ),
               fixedRow(column(12, mainPanel(plotOutput(outputId = "part_of_speech"))))
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
    part_of_speech_df |>
      filter(upos == input$pos_type) |>
      ggplot(aes(x = section_number, y = prop)) +
      geom_col(aes(fill = narrator)) +
      geom_smooth(se = FALSE) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Frequency",
           fill = "Narrator", 
           x = "Section", y = "Frequency")
  })
  
  
  
  
  output$sentence_length <- renderPlot({
    input$plot_choice |> 
      switch(
        avg_length_whole_novel = {
          average_sentence_length |>
          group_by(narrator, section_number) |>
          summarise(
          chapter_average = mean(average_length),
          total_sentences = sum(num_sentences),
          .groups = 'drop') |>
          ggplot(aes(x = section_number, y = chapter_average)) +
          geom_point(aes(size = total_sentences, color = narrator)) + geom_smooth() +
          theme_minimal() 
        },
        num_sentences_by_chapter = {
          average_sentence_length |>
            filter(section_number %in% input$section_num) |> 
            mutate(sentence_num = row_number()) |>
            ggplot(aes(x = doc_id, y = average_length)) +
            geom_col(aes(fill = narrator)) +
            geom_smooth(se = FALSE) +
            theme_minimal() 
        }
      )
  })
  
  
  filtered_data <- reactive({
    # Filter based on selected section
    if (input$selected_section == "All") {
      sentence_info |>
        mutate(row_num = row_number())
    } else {
      sentence_info %>%
        filter(chapter_number == as.numeric(input$selected_section)) |>
        mutate(row_num = row_number())
    }
  })
  
  
  # Plotly graph
  output$sentencePlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~row_num, y = ~num_words, type = 'scatter', 
            mode = 'markers', hoverinfo = 'x+y', text = ~sentence, 
            source = 'sentencePlot') |>
      layout(xaxis = list(title = 'Sentence Number'), 
             yaxis = list(title = 'Number of Words'))
  })
  
  # Display sentence details when a point is clicked
  output$selectedSentenceDetails <- renderText({
    eventdata <- event_data("plotly_click", source = 'sentencePlot')
    if (!is.null(eventdata)) {
      selected_index <- eventdata$pointNumber + 1  # Adjust index
      selected_sentence <- filtered_data()[selected_index, ] # str_wrap(string, width = 80, indent = 0, exdent = 0)
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
      "Click on a point in the plot to see information about the sentence."
    }
  })
  
  
  
}


# call 
shinyApp(ui = ui, server = server)

