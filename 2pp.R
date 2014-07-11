# Load required functions
source(file="functions.R")

# Create a pretty ggplot2 graph with nice defaults
create.2pp.graph <- function(data) {
  # Create a ggplot2 graph of the
  graph <- ggplot(data, aes(y=value, x=poll.end.date, colour=variable))
  graph + geom_point(alpha=.4) + 
    stat_smooth() + 
    # This manually sets the legend values, see:
    # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/#modifying-the-text-of-legend-titles-and-labels
    scale_colour_manual(values=c("#0971B2", "#FF0000"),
                        name="Parties",
                        labels=c("Coalition", "Labor")) + 
    labs(list(title="Two-party Preferred Polling, 1996-2014", y="Per cent", x="Year")) +
    theme_bw()
}



####################################################
# Script to create a chart of polling since 1996.  #
####################################################

data.raw <- read.csv(file="Opinion_Poll_Master.csv")
data.clean <- clean.data(data.raw)
data.long <- create.long(data.clean)

# Create a table of only two party preferred results after 1996, then graph them
two.pp <- filter(data.long, variable=='Labor (2pp)' | variable=='Coalition (2pp)', poll.end.date >= '1996-01-01')
create.2pp.graph(two.pp)