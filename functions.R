require(dplyr)
require(lubridate)
require(reshape2)
require(ggplot2)

#######################################################
# Functions to clean up and normalise the poll data.  #
#######################################################

clean.data <- function(data) {
  # clean up data names and remove unused columns from Toby's poll spreadsheet
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
  # Make a long version of the table
  data.long <- melt(data.clean, id.vars=c("poll.end.date", "poll.source", 
                                          "prime.minister", "opposition.leader"), na.rm=TRUE)
  data.long <- arrange(data.long, desc(poll.end.date), poll.source)
  return(data.long)
}

create.parties <- function(data) {
  # Create a table of primary votes and 2pps from cleaned poll data
  data.subset <- select(data, poll.end.date, poll.source, liberal, national, coalition,
                        alp, greens, palmer.united, family.first, democrats, pauline.hanson.one.nation,
                        others, coalition.2pp, labor.2pp)
  data.subset.long <- melt(data.subset, id.vars= c("poll.end.date", "poll.source"), na.rm = TRUE)
  return(data.subset.long)
}

create.2pp <- function(data, start_date) {
  # Create a subsetted table with only two party preferred results
  data.2pp <- select(data, Party=variable, Percent=value, poll.end.date)
  data.2pp$Party <- as.character(data.2pp$Party)
  data.2pp <- filter(data.2pp, Party=="coalition.2pp" | Party=="labor.2pp", poll.end.date>=start_date)
  # Make pretty variable names
  data.2pp$Party[data.2pp$Party=='labor.2pp'] <- "Labor"
  data.2pp$Party[data.2pp$Party=='coalition.2pp'] <- "Coalition"
  data.2pp <- mutate(data.2pp, type="Two-party preferred")
  return(data.2pp)
}

create.preferred.pm <- function(data) {
  data <- select(data, 
                 poll.end.date,
                 poll.source,
                 satisfied.pm,
                 dissatisfied.pm,
                 satisfied.loto,
                 dissatisfied.loto,
                 prime.minister.ppm,
                 opposition.leader.ppm,
                 prime.minister,
                 opposition.leader)
  data <- melt(data, id.vars = c("poll.end.date", "poll.source", "prime.minister", "opposition.leader"), na.rm = TRUE)
  data <- mutate(data, type="PM satisfaction")
  return(data)
}

create.primaries <- function(data) {
  data <- select(data, 
                 poll.end.date,
                 poll.source,
                 liberal,
                 national,
                 coalition,
                 alp,
                 greens,
                 palmer.united,
                 family.first,
                 democrats,
                 pauline.hanson.one.nation,
                 others
    )
  data <- melt(data, id.vars = c("poll.end.date", "poll.source"), na.rm = TRUE)  
  data <- mutate(data, type="Primary voting intention")
}

create.2pp.graph <- function(data) {
  # Create a ggplot2 graph of the
  graph <- ggplot(data, aes(y=Percent, x=poll.end.date, colour=Party))
  graph + geom_point(alpha=.4) + 
    stat_smooth() + 
    scale_colour_manual(values=c("#0971B2", "#FF0000")) + 
    labs(list(title="Two-party Preferred Polling, 1996-2014", y="Per cent", x="Year")) +
    theme_bw()
}

####################################################
# Script to create a chart of polling since 1996.  #
####################################################

data.raw <- read.csv(file="Opinion_Poll_Master.csv")
data.clean <- clean.data(data.raw)
parties <- create.parties(data.clean)
two.pp <- create.2pp(parties, '1996-01-01')
preferred.pm <- create.preferred.pm(data.clean)
primaries <- create.primaries(data.clean)
create.2pp.graph(two.pp)