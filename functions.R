require(dplyr)
require(lubridate)
require(reshape2)
require(ggplot2)

#######################################################
# Functions to clean up and normalise the poll data.  #
#######################################################

clean.data <- function(data) {
  # clean up data names and remove unused columns from Toby's poll spreadsheet
  # leave in wide format
  names(data) <- tolower(names(data))
  data <- select(data, 
                 poll.end.date,
                 poll.source = source,
                 liberal,
                 national,
                 coalition,
                 alp,
                 greens,
                 palmer.united,
                 family.first,
                 democrats,
                 pauline.hanson.one.nation = pauline.hanson...one.nation,
                 others,
                 coalition.2pp = coalition..2pp.,
                 labor.2pp = labor..2pp.,
                 satisfied.pm=satisfied..pm.,
                 dissatisfied.pm=dissatisfied..pm.,
                 satisfied.loto = satisfied..loto.,
                 dissatisfied.loto = dissatisfied..loto.,
                 prime.minister.ppm = prime.minister..ppm.,
                 opposition.leader.ppm = opposition.leader..ppm.,
                 prime.minister,
                 opposition.leader
                 )
  data$poll.end.date <- as.Date(dmy(data$poll.end.date))
  return(data)
}

create.long <- function(data.clean) {
  # Make a long version of the cleaned up poll table for easier analysis
  data.long <- melt(data.clean, id.vars=c("poll.end.date", "poll.source", 
                                          "prime.minister", "opposition.leader"), na.rm=TRUE)
  data.long <- arrange(data.long, desc(poll.end.date), poll.source)
  
  # Tidy up variable names
  data.long$variable <- as.character(data.long$variable) # Can't perform the character operations on factors
  data.long$variable[data.long$variable=='alp'] <- "ALP"
  data.long$variable[data.long$variable=='coalition'] <- "Coalition"
  data.long$variable[data.long$variable=='liberal'] <- "Liberal"
  data.long$variable[data.long$variable=='national'] <- "National"
  data.long$variable[data.long$variable=='greens'] <- "Greens"
  data.long$variable[data.long$variable=='palmer.united'] <- "Palmer United"
  data.long$variable[data.long$variable=='others'] <- "Others"
  data.long$variable[data.long$variable=='coalition.2pp'] <- "Coalition (2pp)"
  data.long$variable[data.long$variable=='labor.2pp'] <- "Labor (2pp)"
  data.long$variable[data.long$variable=='satisfied.pm'] <- "Satisfied (PM)"
  data.long$variable[data.long$variable=='dissatisfied.pm'] <- "Dissatisfied (PM)"
  data.long$variable[data.long$variable=='satisfied.loto'] <- "Satisfied (LOTO)"
  data.long$variable[data.long$variable=='dissatisfied.loto'] <- "Dissatisfied (LOTO)"
  data.long$variable[data.long$variable=='prime.minister.ppm'] <- "Preferred Prime Minister (PM)"
  data.long$variable[data.long$variable=='opposition.leader.ppm'] <- "Preferred Prime Minister (LOTO)"
  data.long$variable <- as.factor(data.long$variable) # Convert back to factor

  return(data.long)
}

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