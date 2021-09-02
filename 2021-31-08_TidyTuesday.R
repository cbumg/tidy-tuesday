# 2021 TidyTuesday Week 36, data from Cleary et al, 2016 

# load required libraries
library(tidyverse)
library(hrbrthemes)

# load processed data and do some EDA
bird_baths <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

bird_baths %>% summarise_all(n_distinct)
unique(bird_baths$survey_year)

# drop incomplete observations (169 only contain type and count resulting from initial cleaning) 
# there are observations for each year for each bird type even if none were spotted (n=0)
# which type had the largest loss year-over-year?
dat <- bird_baths |> 
  drop_na(survey_year) |>
  group_by(bird_type, survey_year) |>
  tally(bird_count) |>
  mutate(diff = lead(n) - n) |>
  arrange(desc(abs(diff)))

# lorikeets!
lorikeets <- bird_baths |>
  drop_na(survey_year) |>
  filter(str_detect(str_to_lower(bird_type), 'lorikeet')) |>
  group_by(survey_year, urban_rural, bird_type) |>
  tally(bird_count)

rl_counts <- lorikeets |> 
  filter(urban_rural == 'Urban' & bird_type == 'Rainbow Lorikeet') %>%
  pivot_wider(names_from=survey_year, values_from=n, names_prefix = 'yr')

annotation <- tibble(survey_year='2015', urban_rural='Urban', n=100, 
                     bird_type='Purple-crowned Lorikeet', 
                     label=paste0('Urban Rainbow Lorikeet\nsightings decreased from\n',
                                  rl_counts$yr2014, ' in 2014 to ', rl_counts$yr2015, ' in 2015.'))

# create plot
ggplot(lorikeets, aes(x=n, y=fct_rev(bird_type), fill = fct_rev(as.factor(survey_year)))) +
  geom_col(position='dodge') +
  geom_label(data=annotation, mapping=aes(label=label), show.legend = FALSE, 
             fill='lightyellow', label.r = unit(0, 'lines'), family=font_rc) + 
  scale_fill_ipsum(guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~urban_rural) +
  theme_ipsum_rc() +
  labs(title='Whither the Lorikeet?',
       subtitle=paste0('Sightings of lorikeets at Australian bird baths decreased ',
                       'significantly between Winter 2014 and Summer 2015.'),
       x='Lorikeets Spotted', y='',
       fill='Survey Year',
       caption = 'Source: Cleary et al, 2016 | 2021-08-31 #TidyTuesday | @cbumg')

ggsave('plots/2021-08-31_Bird-Baths.png', bg = 'white',
       width=10, height=7, units='in', device='png', type='cairo')
