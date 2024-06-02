start_time <- proc.time()

## getting all the matches
## packages
"data.table" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

"rvest" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

"elo" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

DT <- `[`

## A function to get all the rugby matches from ESPN.
## ESPN does not allow the usage of it's API any more.
## This means one needs to scrape all the data from the website.
## This allows it to be done, and save the data.
get_espn_rugby <- function(date) {
    date |>
        format("%Y%m%d") |>
        sprintf(fmt = "https://www.espn.co.uk/rugby/scoreboard?date=%s") |>
        read_html() ->
        page

    n_x <- 1L

    sprintf("//*[@id='events']/div[%d]/a", n_x) |>
        html_element(page, xpath = _) |>
        html_text2() ->
        competition

    home_team <- "/div/div/div/header/a/div[1]/div/div[1]/div[1]/div/span/span[1]" # nolint: line_length_linter.

    home_score <- "/div/div/div/header/a/div[1]/div/div[2]/div"

    away_team <- "/div/div/div/header/a/div[3]/div/div[2]/div[2]/div/span/span[1]" # nolint: line_length_linter.

    away_score <- "/div/div/div/header/a/div[3]/div/div[1]/div"

    while (!is.na(competition)) {

        n_y <- 1L

        sprintf("//*[@id='events']/div[%d]/article[%s]",
                n_x, n_y) |>
            html_element(page, xpath = _) |>
            html_text2() ->
            match

        while (!is.na(match)) {

            sprintf("//*[@id='events']/div[%d]/article[%s]/div/div/div/header/a", # nolint: line_length_linter.
                    n_x, n_y) |>
                html_element(page, xpath = _) |>
                html_attr("href") |>
                sub("/rugby/match\\?", "", x = _) |>
                strsplit("&") |>
                as.data.table() |>
                DT(, V1 := gsub("gameId|league|\\=", "", V1)) |>
                DT() ->
                tries

            if (nrow(tries) > 1) {
                Sys.sleep(5L)

                sprintf("https://www.espn.co.uk/rugby/matchstats?gameId=%s&league=%s", # nolint: line_length_linter.
                        tries[1, ], tries[2, ]) |>
                    read_html() ->
                    page2

                "//*[@id='main-container']/div/div[2]/div[1]/div[2]/div[1]/article/div/table" |> # nolint: line_length_linter.
                    html_element(page2, xpath = _) |>
                    html_table() |>
                    as.data.table() |>
                    DT(, .(X2, X3, X5)) |>
                    DT(, X2 := fifelse(grepl("Kick", X3), sub("\\%", "", X2) |>
                                           as.integer() |>
                                           {\(x) x / 100}(), as.double(X2))) |> # nolint: brace_linter, line_length_linter.
                    DT(, X5 := fifelse(grepl("Kick", X3), sub("\\%", "", X5) |>
                                           as.integer() |>
                                           {\(x) x / 100}(), # nolint: brace_linter, line_length_linter.
                                       as.double(X5))) |>
                    DT() ->
                    match_stats
            } else {
                match_stats <- data.table(X2 = c("", "", "", "") |>
                                              as.integer(),
                                          X5 = c("", "", "", "") |>
                                              as.integer())
            }

            data.table(match_date = as.character(date),
                       competition_name = competition,
                       home_team = sprintf("//*[@id='events']/div[%d]/article[%d]%s", # nolint: line_length_linter.
                                           n_x, n_y, home_team) |>
                           html_element(page, xpath = _) |>
                           html_text2() |>
                           trimws("both"),
                       home_score = sprintf("//*[@id='events']/div[%d]/article[%d]%s", # nolint: line_length_linter.
                                            n_x, n_y, home_score) |>
                           html_element(page, xpath = _) |>
                           html_text2() |>
                           as.integer(),
                       home_elo = NA_integer_,
                       home_tries = match_stats[1, X2][[1]],
                       home_conversion = match_stats[2, X2][[1]],
                       home_penalty = match_stats[3, X2][[1]],
                       home_kick = match_stats[4, X2][[1]],
                       away_team = sprintf("//*[@id='events']/div[%d]/article[%d]%s", # nolint: line_length_linter.
                                           n_x, n_y, away_team) |>
                           html_element(page, xpath = _) |>
                           html_text2() |>
                           trimws("both"),
                       away_score = sprintf("//*[@id='events']/div[%d]/article[%d]%s", # nolint: line_length_linter.
                                            n_x, n_y, away_score) |>
                           html_element(page, xpath = _) |>
                           html_text2() |>
                           as.integer(),
                       away_elo = NA_integer_,
                       away_tries = match_stats[1, X5][[1]],
                       away_conversion = match_stats[2, X5][[1]],
                       away_penalty = match_stats[3, X5][[1]],
                       away_kick = match_stats[4, X5][[1]]) |>
                DT(!(home_score == 0 & away_score == 0)) |>
                DT() |>
                fwrite("rugby.csv", append = TRUE)

            n_y <- n_y + 1L

            sprintf("//*[@id='events']/div[%d]/article[%s]", n_x, n_y) |>
                html_element(page, xpath = _) |>
                html_text2() ->
                match
        }

        n_x <- n_x + 1L

        sprintf("//*[@id='events']/div[%d]/a", n_x) |>
            html_element(page, xpath = _) |>
            html_text2() ->
            competition
    }
}

## decompose the rugby score into tries, conversions, and penalties
## This functions estimate the number of tries, conversions, and penalties from
## a rugby score.
decompose_score <- function(score, what) {
    result <- lpSolve::lp("max", c(3, 1, 1), matrix(c(7, 5, 3), nrow = 1,
                                                    byrow = TRUE), c("="),
                          c(score), all.int = TRUE)

    result <- switch(what, penalty = result[["solution"]][3],
                     tries = result[["solution"]][2] + result[["solution"]][1],
                     conversions = result[["solution"]][1])

    return(result)
}

decompose_score <- Vectorize(decompose_score)


## rows before addition
start_rows <- NROW(fread("rugby.csv"))

get_espn_rugby(Sys.Date() - 1)

fread("rugby.csv", colClasses = list(Date = "match_date")) |>
    DT(, venue := competition_name == "Rugby World Cup") |>
    DT(, venue := fifelse((venue), 0, 10)) |>
    (\(x) elo.run(score(home_score, away_score) ~ adjust(home_team, venue) +
                      away_team + k(fcase(venue == 0 &
                                              abs(home_score - away_score) > 15,
                                          60,
                                          venue == 0 &
                                              abs(home_score - away_score) == 0,
                                          20,
                                          venue == 0 &
                                              abs(home_score - away_score) < 16,
                                          40,
                                          abs(home_score - away_score) > 15, 30,
                                          abs(home_score - away_score) == 0, 10,
                                          abs(home_score - away_score) < 16,
                                          20)), x, initial.elos = 2500) |>
         as.data.table() |>
         cbind(x))() |>
    DT(, .(match_date, home_team, home_elo = as.integer(elo.A), away_team,
           away_elo = as.integer(elo.B))) |>
    DT(fread("rugby.csv") |>
           DT(, match_date := as.Date(match_date)),
       on = .(match_date, home_team, away_team)) |>
    DT(, .(match_date, competition_name, home_team, home_score, home_elo,
           home_tries, home_conversion, home_penalty, home_kick, away_team,
           away_score, away_elo, away_tries, away_conversion, away_penalty,
           away_kick)) |>
    DT(order(match_date)) |>
    DT(, all_count := rowid(match_date, competition_name, home_team,
                            away_team)) |>
    DT(all_count == 1, -"all_count") |>
    fwrite("rugby.csv")

if (fread("rugby.csv", select = c("home_tries", "home_conversion",
                                  "home_penalty", "away_tries",
                                  "away_conversion", "away_penalty")) |>
        DT(, all_count := seq_len(.N)) |>
        melt(id.vars = "all_count", variable.factor = FALSE) |>
        DT(, value) |>
        is.na() |>
        sum() > 0) {
    fread("rugby.csv") |>
        DT(, `:=`(home_tries = fifelse(is.na(home_tries),
                                       decompose_score(home_score, "tries"),
                                       home_tries),
                  away_tries = fifelse(is.na(away_tries),
                                       decompose_score(away_score, "tries"),
                                       away_tries)),
           .(match_date, competition_name, home_team, home_score, away_team,
             away_score)) |>
        DT(, `:=`(home_conversion = fifelse(is.na(home_conversion),
                                            decompose_score(home_score,
                                                            "conversions"),
                                            home_conversion),
                  away_conversion = fifelse(is.na(away_conversion),
                                            decompose_score(away_score,
                                                            "conversions"),
                                            away_conversion)),
           .(match_date, competition_name, home_team, home_score, away_team,
             away_score)) |>
        DT(, `:=`(home_penalty = fifelse(is.na(home_penalty),
                                         decompose_score(home_score, "penalty"),
                                         home_penalty),
                  away_penalty = fifelse(is.na(away_penalty),
                                         decompose_score(away_score, "penalty"),
                                         away_penalty)),
           .(match_date, competition_name, home_team, home_score, away_team,
             away_score)) |>
        DT(, `:=`(home_kick = fcoalesce(home_kick, 0L),
                  away_kick = fcoalesce(away_kick, 0L))) |>
        fwrite("rugby.csv")
}

## rows before addition
end_rows <- NROW(fread("rugby.csv"))

sprintf("The rugby data have updated for %s, adding %d rows, and %s",
        Sys.Date() - 1, end_rows - start_rows, timetaken(start_time)) |>
    shQuote() |>
    httr::POST(url = paste0("https://ntfy.sh/", "rugby_koortzjg"), body = _) |>
    invisible()
