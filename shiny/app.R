library(shiny)
library(cleanNLP)
library(dplyr)
library(tidyverse)


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
  group_by(section_number, doc_id, sid) |>
  summarise(tokens_per_sentence = n(), .groups = 'drop') |>
  group_by(section_number, doc_id) |>
  summarise(average_length = mean(tokens_per_sentence)) #|>
  #ungroup() |>
  #group_by(section_number) |>
  #summarise(chapter_average = mean(average_length))

section_average_sentence <- token_with_chapters |>
  group_by(section_number, doc_id, sid) |>
  summarise(tokens_per_sentence = n(), .groups = 'drop') |>
  group_by(section_number, doc_id) |>
  summarise(average_length = mean(tokens_per_sentence)) |>
  ungroup() |>
  filter(section_number == 1)

#verbs <- token_with_chapters |> 
 # filter(upos == "VERB") |>
#  group_by(section_number, narrator) |>
#  summarize(count = n(), .groups = "drop") |>
#  arrange(section_number)




# Load your dataset
novel_data <- readRDS("~/Documents/GitHub/Frankenstein/data/anno_frankenstein.Rds")
tokens <- novel_data$token
entities <- novel_data$entity


# For TAB 1 widgets:

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
                                     "Noun" = "NOUN", "Proper Noun" = "PROPN",
                                     "Adjective" = "ADJ", "Adverb" = "ADV"))
             
           )
    ),
    fixedRow(
      column(12,
             mainPanel(plotOutput(outputId = "part_of_speech")),
      )
    )
  )
)
),
tabPanel("New Tab", # This is your new tab
         fixedPage(
           titlePanel("Sentence Length"),
          
           fixedRow(
             column(4,
                    wellPanel(
                      
                      selectInput(inputId = "pos_type", 
                                  label = "Choose section:",
                                  choices = c(1:28))
                      
                    )
             ),
           fixedRow(
             column(12,
                    mainPanel(
                      textOutput(outputId = "sentence_length")
                    )
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
  
  output$sentence_length <- renderPlot({
    average_sentence_length |>
      ggplot(aes(x = as.factor(section_number), y = average_length)) +
      geom_boxplot()
  })
  
  
}


# call 
shinyApp(ui = ui, server = server)

