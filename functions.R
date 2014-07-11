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
  unrequired.columns <- c("x", "voting.intention", "two.party.preferred", 
                          "pm.satisfaction", "loto.satisfaction", 
                          "preferred.pm", "x.1")
  data <- data[,!colnames(data) %in% unrequired.columns]
  # Rename columns
  names(data)[names(data)=="source"] <- "poll.source"
  names(data)[names(data)=="pauline.hanson...one.nation"] <- "pauline.hanson.one.nation"
  names(data)[names(data)=="coalition..2pp."] <- "coalition.2pp"
  names(data)[names(data)=="labor..2pp."] <- "labor.2pp"
  names(data)[names(data)=="satisfied..pm."] <- "satisfied.pm"
  names(data)[names(data)=="dissatisfied..pm."] <- "dissatisfied.pm"
  names(data)[names(data)=="satisfied..loto."] <- "satisfied.loto"
  names(data)[names(data)=="dissatisfied..loto."] <- "dissatisfied.loto"
  names(data)[names(data)=="prime.minister..ppm."] <- "prime.minister.ppm"
  names(data)[names(data)=="opposition.leader..ppm"] <- "opposition.leader.ppm"
  data$poll.end.date <- as.Date(dmy(data$poll.end.date))
  return(data)
}

create.long <- function(data.clean) {
  # Make a long version of the cleaned up poll table for easier analysis
  data.long <- melt(data.clean, id.vars=c("poll.end.date", "poll.source", 
                                          "prime.minister", "opposition.leader"), na.rm=TRUE)
  data.long <- arrange(data.long, desc(poll.end.date), poll.source)
  # Tidy up variable names
  pretty.variable.names <- c("Liberal", "National", "Coalition", "ALP", "Greens",
                             "Palmer United", "Family First", "Democrats", 
                             "Pauline Hanson One Nation", "Others",
                             "Coalition (2pp)", "Labor (2pp)", "Satisfied (PM)", 
                             "Dissatisfied (PM)", "Satisfied (LOTO)",
                             "Dissatisfied (LOTO)", "Preferred Prime Minister (PM)", 
                             "Preferred Prime Minister (LOTO)")
  levels(data.long$variable) <- pretty.variable.names
  return(data.long)
}

clean.breakdowns <- function(breakdowns.raw) {
  # Clean up the breakdowns file
  names(breakdowns.raw) <- tolower(names((breakdowns.raw)))
  names(breakdowns.raw)[names(breakdowns.raw)=="source"] <- "poll.source"
  unrequired.columns <- c("x", "x.1", "x.2", "by.state", 
                        "by.geographic", "by.gender", 
                        "by.age.group..1.", "by.age.group..2.", "x.3")
  # Don't select the columns that are in the unrequired_columns list
  breakdowns.clean <- breakdowns.raw[,!colnames(breakdowns.raw) %in% unrequired.columns]
  return(breakdowns.clean)
}

create.breakdowns.long <- function(breakdowns.clean) {
  # Create a long version of the breakdowns table
  breakdowns.long <- melt(breakdowns.clean, id.vars=c("poll.end.date", 
                                                      "poll.source", "party"), na.rm=TRUE)
  data.long <- arrange(data.long, desc(poll.end.date), poll.source)
  pretty.variable.names <- c("National", "New South Wales", "Victoria", 
                             "Queensland", "South Australia/Northern Territory", 
                             "Western Australia", "Capital cities", 
                             "Other (non-capitals)", "Male", "Female", 
                             "18-24", "25-39", "40-54", "55+", "18-34", 
                             "35-49", "50+")
  levels(breakdowns.long$variable) <- pretty.variable.names
  return(breakdowns.long)
}

