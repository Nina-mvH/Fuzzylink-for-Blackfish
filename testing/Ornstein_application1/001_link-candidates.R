#' ---
#' title: Link CEDA and L2 Datasets
#' author: Joe Ornstein
#' date: 2025-06-20
#' version: 0.4
#' ---

rm(list = ls())

library(tidyverse)
library(glue)
# library(fuzzylink)
library(fastLink)

## Load and clean raw datasets -----------------

load('testing/Ornstein_application1/ceda.RData') # CEDA
load('testing/Ornstein_application1/L2.RData') # selected L2 data from Alameda, Kern, and Ventura counties

# for this application, just keep Alameda, Kern, and Ventura county candidates since 2016
ceda <- ceda |>
  filter(cntyname %in% c('ALAMEDA', 'KERN', 'VENTURA'),
         date > '2016-01-01')

# create fuzzy string and blocking variables
ceda <- ceda |>
  # convert UTF-8 encoding to ASCII for blocking.variables
  # (L2 data doesn't include characters with accents or tildes)
  mutate(
    first = iconv(first, to = 'ASCII//TRANSLIT'),
    last = iconv(last, to = 'ASCII//TRANSLIT'),
    place = iconv(place, to = 'ASCII//TRANSLIT')
  ) |>
  mutate(fullname = str_squish(glue('{first} {last}', .na = ''))) |>
  # remove suffixes from last names for blocking
  mutate(last = str_remove_all(
    last,
    ' Jr.|, Jr.| Jr|, Jr| Sr.|, Sr.| Sr|, Sr|, III| III|, II| II|, IV| IV'
  )) |>
  # harmonize place names
  mutate(
    place = case_when(
      place == 'Union' ~ 'Union City',
      place == 'California' ~ 'California City',
      place == 'McFarland' ~ 'Mc Farland',
      place == 'San Buenaventura' ~ 'Ventura',
      TRUE ~ place
    )
  )

l2 <- l2 |>
  mutate(
    last = Voters_LastName,
    place = case_when(
      Residence_Addresses_City == 'Calif City' ~ 'California City',
      TRUE ~ Residence_Addresses_City
    ),
    fullname = str_squish(
      glue(
        '{Voters_FirstName} {Voters_MiddleName} {Voters_LastName}',
        .na = ''
      )
    )
  ) |>
  rename(cntyname = County)


## fuzzylink ---------------

# this script performs the record linkage reported in the main text;
# to reproduce variants reported in the appendix, modify
# model, fmla, or learner

# model <- 'local'
fmla <- match ~ sim + jw
learner <- 'glm'

df <- BLfuzzylink::fuzzylink(
  dfA = ceda,
  dfB = l2,
  by = 'fullname',
  blocking.variables = c('last', 'place', 'cntyname'),
  record_type = 'person',
  instructions = 'The first name comes from a list of candidates for public office in California. The second name comes from a voter registration file. Bear in mind that some candidates may run for office under a nickname or middle name, and some records may contain spelling errors.',
  fmla = fmla,
  learner = learner,
  openai_api_key = "sealsaretasty",
  model = "EMPTY", 
  embedding_model = "EMPTY",
  embedding_port_num = 8081,
  text_gen_port_num = 8080,
  embedding_dimensions = 2048
  # debug = TRUE
)

save(df,
     file = paste0('testing/Ornstein_application1/l2_fuzzylink_Qwen3-32B.RData'))









## fastLink ----------------------

# # clean so names are lowercase and the
# # 'firstname' variable includes both first and middle (as in CEDA)
# ceda <- ceda |>
#   mutate(firstname = str_to_lower(first),
#          lastname = str_to_lower(last),
#          city = str_to_lower(place))

# l2 <- l2 |>
#   mutate(firstname = glue('{Voters_FirstName} {Voters_MiddleName}', .na = '') |>
#            str_to_lower(),
#          lastname = str_to_lower(Voters_LastName),
#          city = str_to_lower(Residence_Addresses_City))

# # partial string match on firstname and lastname, exact match on city
# # keep everything else default (Jaro-Winkler cutoffs at 0.88 and 0.94)
# system.time(
#   fl.out <- fastLink(
#     dfA = ceda,
#     dfB = l2,
#     varnames = c('firstname', 'lastname', 'city'),
#     stringdist.match = c('firstname', 'lastname'),
#     partial.match = c('firstname', 'lastname'),
#     dedupe.matches = FALSE
#   )
# )
# # approximately 55 minutes without blocking

# fastLink_matches <- fastLink::getMatches(dfA = ceda,
#                                          dfB = l2,
#                                          fl.out)

# save(fl.out, fastLink_matches, file = 'testing/l2_fastLink.RData')
  