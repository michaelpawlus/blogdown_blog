---
title: Predicting with FIFA scores
author: Michael Pawlus
date: '2018-07-10'
slug: predicting-with-fifa-scores
categories:
  - rstats
tags:
  - Soccer
---

# World Cup 2018 Predicted Results

This is post five in a series using data for all World Cup teams collected in the [first post](https://michaelpawlus.netlify.com/2018/06/05/all-world-cup-squads/)

In the [second post](https://michaelpawlus.netlify.com/2018/06/08/age-of-world-cup-players/) we looked explored the ages of players over time, by team and by position as well as checked to see if there was a skewed distribution among birth months.

In [post three](https://michaelpawlus.netlify.com/2018/06/12/world-cup-player-caps-and-clubs/) we looked at the number of caps for players over time and by team as well as the clubs sending the largest delegations.

[Post four](http://michaelpawlus.netlify.com/2018/06/14/world-cup-2018-predicted-results/) was just a list of predicted results

This post will show how we arrived at these predicted results and how they compared with reality as well as some even simpler baseline prediction methods.

## Libraries Required



## Function to get all historical results

The first step will be to get all historical results and then later rankings to build a training set.  The function below is the function for all match results. We create a vector of world cup editions to then paste into the url via a call to map.


```r
# get the section of the url that we need for as many cups as we want -- here I use the last five
wc <- c("brazil2014","southafrica2010","germany2006","koreajapan2002","france1998")

# function to get all columns
get_results <- function(wc) {
url <- paste0("https://www.fifa.com/worldcup/archive/",wc,"/matches/index.html")

# datetimes
all_datetimes <- url %>%
  read_html() %>%
  html_nodes(".mu-i-datetime") %>%
  html_text()

# dates
all_dates <- url %>%
  read_html() %>%
  html_nodes(".mu-i-date") %>%
  html_text()

# match number
all_match_numbers <- url %>%
  read_html() %>%
  html_nodes(".mu-i-matchnum") %>%
  html_text()

# round
all_rounds <- url %>%
  read_html() %>%
  html_nodes(".mu-i-group") %>%
  html_text()

# long and short notes for matches that end aet or with pens
all_tiebreakers <- url %>%
  read_html() %>%
  html_nodes(".text-reasonwin") %>%
  html_text()

# scores as text
all_scores <- url %>%
  read_html() %>%
  html_nodes(".s-scoreText") %>%
  html_text()

# teams
all_teams <- url %>%
  read_html() %>%
  html_nodes(".t-nText ") %>%
  html_text()

# team codes
all_codes <- url %>%
  read_html() %>%
  html_nodes(".t-nTri") %>%
  html_text()

# indices to split vectors with odd/even pattern
home_index <- seq(1,length(all_teams)-1,2)
away_index <- seq(2,length(all_teams),2)

# split all_teams to home/away
home_teams <- all_teams[home_index]
away_teams <- all_teams[away_index]

# split all_codes to home/away
home_codes <- all_codes[home_index]
away_codes <- all_codes[away_index]

# split notes for games that end after 90' into long and short
tiebreak_long <- all_tiebreakers[home_index]
tiebreak_short <- all_tiebreakers[away_index]

# create the tibble
tibble(
  edition = wc,
  datetime = all_datetimes,
  date = all_dates,
  match_no = all_match_numbers,
  round = all_rounds,
  home_team = home_teams,
  home_code = home_codes,
  # get number before the hyphen - start of character string - as integer: home_score
  home_score = as.integer(str_extract(all_scores,"^[:digit:]")),
  score = all_scores,
  # get number after the hyphen - end of character string - as integer: away_score
  away_score = as.integer(str_extract(all_scores,"[:digit:]$")),
  away_team = away_teams,
  away_code = away_codes,
  tiebreak_long = tiebreak_long,
  tiebreak_short = tiebreak_short
)
}
```


After we create the function, we then map over it as mentioned above to get all match results and then create a table to use later.


```r
# map over get_results function to get results for all cups included in wc vector
results <- map_df(wc, get_results)

# a few duplicate rows so remove those here
results <- distinct(results)

# use score to get win/lose/draw columns
results <- results %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    home_score == away_score ~ str_extract(tiebreak_long, "[:print:]+(?= win?)")
  ),
  home_result = case_when(
    home_score > away_score ~ 'W',
    away_score > home_score ~ 'L',
    home_score == away_score & tiebreak_long == " " ~ 'D',
    home_score == away_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") == home_team ~ 'W',
    home_score == away_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") != home_team ~ 'L'
  ),
  away_result = case_when(
    away_score > home_score ~ 'W',
    home_score > away_score ~ 'L',
    away_score == home_score & tiebreak_long == " " ~ 'D',
    away_score == home_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") == away_team ~ 'W',
    away_score == home_score & str_extract(tiebreak_long, "[:print:]+(?= win?)") != away_team ~ 'L'
  )
  ## add in code to add a tag for expected result or upset (ask Joe about when a draw is an upset)
  )
```

```
## Warning: The `printer` argument is deprecated as of rlang 0.3.0.
## This warning is displayed once per session.
```

We can look at a few of these rows.


```r
kable(head(results))
```



|edition    |datetime                       |date        |match_no |round   |home_team |home_code | home_score|score | away_score|away_team   |away_code |tiebreak_long |tiebreak_short |winner      |home_result |away_result |
|:----------|:------------------------------|:-----------|:--------|:-------|:---------|:---------|----------:|:-----|----------:|:-----------|:---------|:-------------|:--------------|:-----------|:-----------|:-----------|
|brazil2014 |12 Jun 2014 - 17:00 Local time |12 Jun 2014 |Match 1  |Group A |Brazil    |BRA       |          3|3-1   |          1|Croatia     |CRO       |              |               |Brazil      |W           |L           |
|brazil2014 |13 Jun 2014 - 13:00 Local time |13 Jun 2014 |Match 2  |Group A |Mexico    |MEX       |          1|1-0   |          0|Cameroon    |CMR       |              |               |Mexico      |W           |L           |
|brazil2014 |13 Jun 2014 - 16:00 Local time |13 Jun 2014 |Match 3  |Group B |Spain     |ESP       |          1|1-5   |          5|Netherlands |NED       |              |               |Netherlands |L           |W           |
|brazil2014 |13 Jun 2014 - 18:00 Local time |13 Jun 2014 |Match 4  |Group B |Chile     |CHI       |          3|3-1   |          1|Australia   |AUS       |              |               |Chile       |W           |L           |
|brazil2014 |14 Jun 2014 - 13:00 Local time |14 Jun 2014 |Match 5  |Group C |Colombia  |COL       |          3|3-0   |          0|Greece      |GRE       |              |               |Colombia    |W           |L           |
|brazil2014 |14 Jun 2014 - 16:00 Local time |14 Jun 2014 |Match 7  |Group D |Uruguay   |URU       |          1|1-3   |          3|Costa Rica  |CRC       |              |               |Costa Rica  |L           |W           |

## Function to get all historical rankings

After getting all the results we will then need all rankings in order to do some simple predicting just based on the FIFA ranking. The FIFA ranking is problematic and there may be a future post that dives into why in more detail soon (or just links to others that have already covered this).  For now, we get ratings using a similar method as used to get results. The one issue that I noticed was that country names are not always consistent and the code below adjusts for this.


```r
## ratings which have to be gathered one by one because they are not stored uniformily in wikitables

# Brazil 2014:

url <- "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_seeding"

ratings <- url %>%
read_html() %>%
html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
html_table(fill = TRUE) %>%
as.tibble(
)
```

```
## Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
## This warning is displayed once per session.
```

```r
ratings <- ratings %>%
  mutate(Team = str_extract(Team, '[^\\(]+'), Team = str_trim(Team, side = "right"), edition = "brazil2014") %>%
  rename(team = Team, ranking = `FIFA RankingOctober 2013`) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "Iran" ~ "IR Iran",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings14 <- ratings

# South Africa 2010:

url <- "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_seeding"

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE) %>%
  as.tibble(
  )

ratings <- ratings %>%
  mutate(Association = str_extract(Association, '[^\\(]+'), Association = str_trim(Association, side = "right"), edition = "southafrica2010") %>%
  rename(team = Association, ranking = `FIFA RankingOctober 2009`) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "South Korea" ~ "Korea Republic",
    team == "North Korea" ~ "Korea DPR",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings10 <- ratings

# Germany 2006:

url <- "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_seeding"

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

ratings <- ratings[,c(2,12)]

colnames(ratings) <- c('team','ranking')

ratings <- ratings %>%
  slice(3:34) %>%
  mutate(edition = "germany2006", ranking = as.integer(ranking)) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "Iran" ~ "IR Iran",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

## add extra row for Iran name mismatch
rating_extra_iran_row <- tribble(
  ~team, ~ranking, ~edition,
  "Iran",   19,     "germany2006" 
)

ratings06 <- ratings

# Korea/Japan 2002:

url <- 'https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_seeding'

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

ratings <- ratings[,c(2,12)]

colnames(ratings) <- c('team','ranking')

## results for 2002 have Iran listed as Iran and IR Iran
ratings <- ratings %>%
  slice(3:34) %>%
  mutate(edition = "koreajapan2002", ranking = as.integer(ranking)) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "Iran" ~ "IR Iran",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings02 <- ratings

# France 1998:

url <- 'https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_seeding'

ratings <- url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = TRUE)

ratings <- ratings[,c(2,12)]

colnames(ratings) <- c('team','ranking')

ratings <- ratings %>%
  slice(3:34) %>%
  mutate(edition = "france1998", ranking = as.integer(ranking)) %>%
  mutate(team = case_when(
    team == "United States" ~ "USA",
    team == "South Korea" ~ "Korea Republic",
    team == "Ivory Coast" ~ "Côte d'Ivoire",
    TRUE ~ as.character(team)
  ))

ratings98 <- ratings
```
