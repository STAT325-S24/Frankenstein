# Frankenstein
Justin Papagelis
2024-01-01

This file describes the Frankenstein package.

The package is interesting because it reads in Frankenstein by Mary
Shelley to a dataset.

The Frankenstein package can be installed by running:

    devtools::install_github("STAT325-S24/Frankenstein")
    library(Frankenstein)

``` r
glimpse(Frankenstein)
```

Here are some sample analyses:

``` r
frankenstein_afinn <- Frankenstein |>
   drop_na() |>
   unnest_tokens(word, text) |>
   anti_join(get_stopwords(), by = "word") |>
   inner_join(get_sentiments("afinn")) |>
   filter(section_type != "letter")
glimpse(frankenstein_afinn)
```

``` r
frankenstein_plot <- frankenstein_afinn |>
     group_by(section) |>
     summarize(sentiment = sum(value))
ggplot(frankenstein_plot, aes(x = section, y = sentiment)) + geom_point() + geom_smooth() +
  labs(title = "AFINN Sentiment by Chapter of Frankenstein", x = "Chapter", y = "Sentiment")
```
