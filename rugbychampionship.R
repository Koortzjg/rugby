"igraph" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

"data.table" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

DT <- `[`

# rugby championship
match_data <- fread("rugby.csv") |>
    DT(
        (competition_name == "Rugby Championship" |
            competition_name == "The Rugby Championship") &
            match_date >= as.Date("2012-01-01")
    ) |>
    DT(order(match_date)) |>
    DT(, `:=`(
        winner = ifelse(home_score > away_score, home_team, away_team),
        loser = ifelse(home_score > away_score, away_team, home_team)
    )) |>
    DT()

# calculate statistics
stats <- match_data |>
    DT(, .(games = .N), by = .(team = home_team)) |>
    rbind(
        match_data |>
            DT(, .(games = .N), by = .(team = away_team))
    ) |>
    DT(, .(games = sum(games)), by = team) |>
    merge(
        match_data |>
            DT(, .(wins = .N), by = .(team = winner)),
        by = "team",
        all.x = TRUE
    ) |>
    DT(is.na(wins), wins := 0) |>
    DT(, win_rate := wins / games) |>
    DT(order(-wins, -games, -win_rate, -team)) |>
    DT()

# create undirected unique edge list
edges <- match_data |>
    DT(, .(
        team1 = pmin(home_team, away_team),
        team2 = pmax(home_team, away_team),
        winner
    )) |>
    DT(,
        .(
            total_matches = .N,
            wins_team1 = sum(winner == team1),
            wins_team2 = sum(winner == team2)
        ),
        by = .(team1, team2)
    ) |>
    DT(, `:=`(
        dominant_team = ifelse(wins_team1 >= wins_team2, team1, team2),
        weaker_team = ifelse(wins_team1 < wins_team2, team1, team2),
        weight = pmax(wins_team1, wins_team2) / total_matches
    )) |>
    DT(, .(from = weaker_team, to = dominant_team, weight)) |>
    DT()

# create directed graph
g <- graph_from_data_frame(edges, directed = TRUE)

# assign node attributes
palette <- colorRampPalette(c("red", "orange", "green"))(100)
V(g)$wins <- stats[match(V(g)$name, stats[["team"]]), wins]
V(g)$wins[is.na(V(g)$wins)] <- 0
V(g)$win_rate <- stats[match(V(g)$name, stats[["team"]]), win_rate]
V(g)$win_rate[is.na(V(g)$win_rate)] <- 0
V(g)$size <- NROW(stats) + V(g)$wins
V(g)$games <- stats[match(V(g)$name, stats[["team"]]), games]
V(g)$colour <- palette[as.numeric(cut(
    V(g)$win_rate,
    breaks = seq(0, max(V(g)$win_rate), length.out = 100),
    include.lowest = TRUE
))]

# assign edge attributes
palette <- colorRampPalette(c("red", "orange", "green"))(15)
E(g)$color <- palette[as.numeric(cut(
    E(g)$weight,
    breaks = seq(0, max(E(g)$weight), length.out = 16),
    include.lowest = TRUE
))]

# order nodes by number of wins
ordered_nodes <- V(g)$name[order(
    -V(g)$wins,
    -V(g)$games,
    -V(g)$win_rate
)]
layout_circle <- layout_in_circle(g, order = match(ordered_nodes, V(g)$name))

# plot with uniform edge width and gradient color
plot(
    g,
    layout = layout_circle,
    edge.width = 3,
    edge.color = E(g)$color,
    edge.arrow.mode = 1,
    edge.arrow.size = 0.5,
    vertex.color = V(g)$colour,
    vertex.size = V(g)$size,
    main = "Rugby Championship Matches"
)
