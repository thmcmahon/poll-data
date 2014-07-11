Poll data 1985 - present
------------------------

This is a repository of opinion polling data going back to 1985.
The dataset was originally provided by the parliamentary library.

This repository also contains some useful R functions for interacting
with the polling data.

This data is up to date as at 11/07/14.

Data
----
If you just want to analyse the polling data, you can simply download the files in
the tables folder.

There are four files:

1. aph_poll_data_wide.csv - each row represents a poll. This data is not normalised.
2. aph_poll_data_long.csv - each row represents a datapoint, each poll has multiple
rows, this data is normalised.
3. aph_poll_breakdowns_wide.csv - each row represents a breakdown by party of certain
demographics. This data is not normalised.
4. aph_poll_breakdowns_long.csv - each row represents a datapoint, each poll has
multiple rows. This data is normalised.

The variables should be self explanatory. If not ask Tom McMahon @thmcmahon 
or thmcmahon@gmail.com

Code
----
* functions.R - contains the various functions to clean up the data
* 2pp.R - a script to create a graph of two party preferred polling
1996 to 2014
* create.tables.R - a script to read raw data and output tables to the
tables folder