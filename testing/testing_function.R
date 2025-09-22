library(tidyverse)


dfA <- tribble(~name, ~state, ~age,
               'Joe Biden', 'Delaware', 81,
               'Donald Trump', 'New York', 77,
               'Barack Obama', 'Illinois', 62,
               'George W. Bush', 'Texas', 77,
               'Bill Clinton', 'Arkansas', 77)

dfB <- tribble(~name, ~state, ~hobby,
               'Joseph Robinette Biden', 'Delaware', 'Football',
               'Donald John Trump ', 'Florida', 'Golf',
               'Barack Hussein Obama', 'Illinois', 'Basketball',
               'George Walker Bush', 'Texas', 'Reading',
               'William Jefferson Clinton', 'Arkansas', 'Saxophone',
               'George Herbert Walker Bush', 'Texas', 'Skydiving',
               'Biff Tannen', 'California', 'Bullying',
               'Joe Riley', 'South Carolina', 'Jogging')

df <- BLfuzzylink::fuzzylink(dfA, dfB, by = 'name', record_type = 'person',blocking.variables = 'state', openai_api_key="none")
