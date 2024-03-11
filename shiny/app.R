library(shiny)
library(cleanNLP)
library(tidyverse)
library(Frankenstein)

anno <- anno_Frankenstein



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
  tabPanel("Parts of Speech",
  fixedPage(
  
  titlePanel("Parts of Speech"),
  fixedRow(
    column(4,
           wellPanel(
  
             selectInput(inputId = "pos_type", 
                         label = "Choose POS:",
                         choices = c("Verb" = "VERB", "Noun" = "NOUN", 
                                      "Proper Noun" = "PROPN",
                                     "Adjective" = "ADJ", "Adverb" = "ADV"))
             
           )
    ),
    fixedRow(
      column(12,
             mainPanel(plotOutput(outputId = "part_of_speech"))
      )
    )
  )
)
),
tabPanel("Sentence Structure", 
         fixedPage(
           titlePanel("Sentence Length"),
          
           fixedRow(
             column(4,
                    wellPanel(
                      radioButtons(inputId = "plot_choice", 
                                   label = "Choose plot type:",
                                   choices = c("Average Sentence Length through Novel" = "avg_length_whole_novel", 
                                               "Number of Sentences by Chapter" = "num_sentences_by_chapter")),
                      selectInput(inputId = "section_num", 
                                  label = "Choose section:",
                                  choices = c(1:28))

                      
                      
                      
                    )
             ),
           fixedRow(
             column(12,
                    mainPanel(plotOutput(outputId = "sentence_length"))
             )
           )
         )
)
)
)



# server
server <- function(input, output) {
  
  
  # TAB 1: INTERACTIVE Part of Speech Plot
  
  output$part_of_speech <- renderPlot({
    token_with_chapters |>
      filter(upos == input$pos_type) |>
      group_by(section_number, narrator) |>
      summarize(count = n(), .groups = "drop") |>
      arrange(section_number) |>
      ggplot(aes(x = section_number, y = count)) +
      geom_col(aes(fill = narrator)) +
      geom_smooth(se = FALSE) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_blank()) +
      labs(title = "Frequency",
           fill = "Narrator", 
           x = "Section", y = "Frequency")
  })
  
 # output$sentence_length <- renderPlot({
#    average_sentence_length |>
      
#      ggplot(aes(x = section_number, y = chapter_average)) +
#      geom_point(aes(size = total_sentences, color = narrator)) + geom_smooth() +
#      theme_minimal() 
#  })
  
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
  
  
}


# call 
shinyApp(ui = ui, server = server)

