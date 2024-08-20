"data.table" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

DT <- `[`

fread("rugby.csv") |>
    DT(, match_date := as.Date(match_date)) |>
    DT(is.element(home_team,
                  fread("irb_countries.csv")[["country_name"]])) |> # nolint: line_length_linter.
    DT(is.element(away_team,
                  fread("irb_countries.csv")[["country_name"]])) |> # nolint: line_length_linter.
    {\(x) x |> # nolint: brace_linter.
         DT(, .(match_date, team_name = home_team,
                team_ranking = home_elo)) |>
         rbind(x |>
                   DT(, .(match_date, team_name = away_team,
                          team_ranking = away_elo)))}() |> # nolint: brace_linter, line_length_linter.
    DT(, period_monday := as.Date(cut(match_date, "weeks"))) |>
    DT(order(period_monday, -team_ranking), .(period_monday, team_name,
                                              team_ranking)) |>
    DT(order(period_monday, -team_ranking)) |>
    DT(, all_count := seq_len(.N), by = .(period_monday)) |>
    DT(all_count == 1) |>
    DT(, all_count := NULL) |>
    DT() |>
    {\(x) x |> # nolint: brace_linter.
         DT(data.table(period_monday = seq(min(x[["period_monday"]]),
                                           max(x[["period_monday"]]),
                                           by = "week")), on = .(period_monday),
            roll = TRUE)}() |> # nolint: brace_linter.
    DT(order(period_monday, -team_ranking)) |>
    DT(, highest_team := character()) |>
    DT(, highest_ranking := integer()) |>
    DT() ->
    rugby

for (i in seq_len(nrow(rugby))) {
    if (i == 1) {
        rugby[i, "highest_team"] <- rugby[i, "team_name"]

        rugby[i, "highest_ranking"] <- rugby[i, "team_ranking"]
    } else {

        if (rugby[i - 1, "highest_ranking"] > rugby[i, "team_ranking"]) {
            rugby[i, "highest_team"] <- rugby[i - 1, "highest_team"]

            rugby[i, "highest_ranking"] <- rugby[i - 1, "highest_ranking"]
        } else {
            rugby[i, "highest_team"] <- rugby[i, "team_name"]

            rugby[i, "highest_ranking"] <- rugby[i, "team_ranking"]
        }

        if (rugby[i, "team_name"] == rugby[i, "highest_team"]) {
            rugby[i, "highest_ranking"] <- rugby[i, "team_ranking"]
        }
    }

}

rugby |>
    DT(, .(count = .N), .(period_monday, team_name = highest_team,
                          team_ranking = highest_ranking)) |>
    DT(, .(period_monday, team_name)) |>
    DT(, streak := integer()) |>
    DT() ->
    rugby

for (i in seq_len(nrow(rugby))) {
    if (i == 1) {
        rugby[i, "streak"] <- 1

        n_row <- 2
    } else {
        if (rugby[i, "team_name"] == rugby[i - 1, "team_name"]) {
            rugby[i, "streak"] <- n_row

            n_row <- n_row + 1
        } else {
            rugby[i, "streak"] <- 1

            n_row <- 2
        }
    }
}

fwrite(rugby, "ranking_over_time.csv")

fread("rugby.csv") |>
    DT(, match_date := as.Date(match_date)) |>
    DT(is.element(home_team,
                  fread("irb_countries.csv")[["country_name"]])) |>
    DT(is.element(away_team,
                  fread("irb_countries.csv")[["country_name"]])) |>
    (\(x) x |>
         DT(, .(match_date, team_name = home_team,
                team_ranking = home_elo)) |>
         rbind(x |>
                   DT(, .(match_date, team_name = away_team,
                          team_ranking = away_elo))))() |>
    DT(order(match_date, team_ranking, decreasing = TRUE)) |>
    DT(, match_date := NULL) |>
    DT(, all_count := seq_len(.N), by = .(team_name)) |>
    DT(all_count == 1) |>
    DT(, all_count := NULL) |>
    DT(order(team_ranking, decreasing = TRUE)) |>
    DT(, team_position := seq_len(.N)) |>
    DT(, .(team_position, team_name, team_ranking)) |>
    DT() |>
    fwrite("latest_rankings.csv")
