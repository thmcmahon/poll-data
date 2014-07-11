# Load required functions
source(file="functions.R")

# Read the raw data
data.raw <- read.csv(file="Opinion_Poll_Master.csv")
breakdowns.raw <- read.csv(file="Poll_Breakdowns.csv")

# Read in main poll data and spit out some CSVs
data.clean <- clean.data(data.raw)
write.csv(file="tables/aph_poll_data_wide.csv", data.clean)

data.long <- create.long(data.clean)
write.csv(file="tables/aph_poll_data_long.csv", data.long)

# Read in break down data and spit out some CSVs
breakdowns.clean <- clean.breakdowns(breakdowns.raw)
write.csv(file="tables/aph_poll_breakdowns_wide.csv", breakdowns.clean)

breakdowns.long <- create.breakdowns.long(breakdowns.clean)
write.csv(file="tables/aph_poll_breakdowns_long.csv", breakdowns.long)