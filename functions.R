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
                 x,
                 poll.end.date,
                 poll.source = source,
                 voting.intention,
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
                 two.party.preferred,
                 coalition.2pp = coalition..2pp.,
                 labor.2pp = labor..2pp.,
                 pm.satisfaction,
                 satisfied.pm=satisfied..pm.,
                 dissatisfied.pm=dissatisfied..pm.,
                 loto.satisfaction,
                 satisfied.loto = satisfied..loto.,
                 dissatisfied.loto = dissatisfied..loto.,
                 preferred.pm,
                 prime.minister.ppm = prime.minister..ppm.,
                 opposition.leader.ppm = opposition.leader..ppm.,
                 x.1,
                 prime.minister,
                 opposition.leader
                 )
  data$poll.end.date <- as.Date(dmy(data$poll.end.date))
  return(data)
}

create.parties <- function(data) {
  # Create a table of primary votes and 2pps from cleaned poll data
  data.subset <- select(data, poll.end.date, poll.source, liberal, national, coalition,
                        alp, greens, palmer.united, family.first, democrats, pauline.hanson.one.nation,
                        others, coalition.2pp, labor.2pp)
  data.subset.long <- melt(data.subset, id.vars= c("poll.end.date", "poll.source"))
  return(data.subset.long)
}

create.2pp <- function(data, start_date) {
  # Create a subsetted table with only two party preferred results
  data.2pp <- select(data, Party=variable, Percent=value, poll.end.date)
  data.2pp$Party <- as.character(data.2pp$Party)
  data.2pp <- filter(data.2pp, Party=="coalition.2pp" | Party=="labor.2pp", poll.end.date>=start_date)
  data.2pp$Party[data.2pp$Party=='labor.2pp'] <- "Labor"
  data.2pp$Party[data.2pp$Party=='coalition.2pp'] <- "Coalition"
  return(data.2pp)
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

raw.data <- read.csv(file="Opinion_Poll_Master.csv")
cleaned.data <- clean.data(raw.data)
parties <- create.parties(cleaned.data)
two.pp <- create.2pp(parties, '1996-01-01')
create.2pp.graph(two.pp)