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

We can look at a few of these rows.


