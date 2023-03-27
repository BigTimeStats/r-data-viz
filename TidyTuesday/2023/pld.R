library(ggrepel)
library(ggplot2)
library(

tuesdata <- tidytuesdayR::tt_load('2023-03-21')
languages <- tuesdata$languages

languages %>% 
    filter(language_rank < 1000) %>% 
    filter(log(number_of_jobs) > 0) %>% 
    mutate(color = ifelse(title == 'HTTP', '1', '0')) %>% 
    ggplot(aes(x = number_of_users, y = number_of_jobs, color = color)) + 
    geom_point(alpha = .9) + 
    ggrepel::geom_text_repel(aes(label = title),
                    nudge_x = 100, 
                    nudge_y = 1000,
                    max.overlaps = 10,
                    max.iter = 200,
                    max.time = 10,
                    alpha = .9,
                    na.rm = TRUE) +
    geom_smooth(aes(group = 1), method = 'lm', span = .5, alpha = 0.3, size = 0) + 
    stat_smooth(aes(group = 1), method = 'lm', color = 'steelblue3', geom = "line", size = 1, alpha = .7) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) + 
    scale_color_manual(values = c('grey45', '#c86558')) +
    labs(title = 'According to the Programming Language Database,\nHTTP has some of the fewest users with the highest job openings',
         subtitle = 'Top 1,000 programming languages; numbers estimated & could be anomolous; trend line for reference', 
         y = '# of Jobs',
         x = '# of Users', 
         caption = 'Source: TidyTuesday & PLD - Graphic: Adam Vagner') + 
    ggthemes::theme_fivethirtyeight() + 
    theme(axis.title.x = element_text(),
          axis.title.y = element_text()) + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    theme(legend.position = 'none')
