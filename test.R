"igraph" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

"data.table" |>
    library() |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(
    name = c(
        "Alice",
        "Bob",
        "Cecil",
        "David",
        "Esmeralda"
    ),
    age = c(48, 33, 45, 34, 21),
    gender = c("F", "M", "F", "M", "F")
)
relations <- data.frame(
    from = c(
        "Bob",
        "Cecil",
        "Cecil",
        "David",
        "David",
        "Esmeralda"
    ),
    to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
    same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
    friendship = c(4, 5, 5, 2, 1, 1),
    advice = c(4, 5, 5, 4, 2, 3)
)
g <- graph_from_data_frame(relations, directed = TRUE, vertices = actors)
plot(g)
