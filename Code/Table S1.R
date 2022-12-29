
library(tidyverse)

recoder <- c(hookworm = 'Hookworm (misc.)',
             LF = 'Lymphatic filariasis (misc.)',
             Hookworm = 'Hookworm (misc.)',
             Schistosomiases = 'Schistosoma spp.',
             'Intestinal helminths' = 'Misc.',
             'Soil-transmitted helminthiases' = 'STH (misc.)',
             'soil-transmitted helminthiases' = 'STH (misc.)',
             'soil-transmitted helminths' = 'STH (misc.)',
             'Dirofilariosis' = 'Dirofilaria spp.',
             schistosomiases = 'Schistosoma spp.',
             schistosomiasis = 'Schistosoma spp.',
             Trematodes = 'Misc.',
             trematodes = 'Misc.',
             'Trichuris Trichiura' = 'Trichuris trichiura')

helm <- read_csv('WormMapsDB-Grid view.csv')
helm %>% separate_rows(Binomial, sep=', ') %>%
  separate_rows(Binomial, sep=' and ') %>% 
  mutate(Latin = str_replace(Binomial, 'and ', '')) %>%
  mutate(Latin = recode(Latin, !!!recoder)) -> helm


helm %>% select(Binomial) %>% separate_rows(Binomial, sep = ",", ) %>% 
  group_by(Binomial) %>% tally() %>% arrange(-n) %>% View()

