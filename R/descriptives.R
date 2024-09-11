# descriptives


# number of events --------------------------------------------------------
number_events <- function(data) {
    table(data$nafld) %>% print()
}
