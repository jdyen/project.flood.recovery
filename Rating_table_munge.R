library(stringr)
library(tidyverse)
source("local_paths.R")

#https://gist.github.com/TonyLadson/ac0a68a3ec27ae23211bfedd2ac56657

my.path = str_c(get_project_path(), 'ratings_tables', sep = '/')
my.file <- 'rt_406201.txt'

fname <- str_c(my.path, my.file, sep = '/')

#  Read the file in as a character vector 
rating_table <- read_lines(fname)

# Text processing to extract the parts we need

# 1. Remove leading spaces 

rating_table <- str_replace(rating_table,  "^[ ]+", "")

# 2. Remove lines that don't start with a digit

rating_table <- rating_table[str_detect(rating_table, "^[0-9]")]

# 3. Remove characters we don't need

rating_table <- str_replace_all(rating_table, 
                                c("[ ]+" = " ", # find one or more spaces, replace with a space.
                                  "[ ]$" = "", # replace a trailing space with nothing
                                  "[^(0-9|.| )]+" = "")) # remove anything that isn't a number, decimal or space)  


# 4. Split at spaces
rating_table <- str_split_fixed(rating_table, "[ ]", n = 11 )


# 5. Separate gauge heights and flows

GH <- as.numeric(rating_table[ ,1])
flow <- t(rating_table[ ,-1])

# 6. Combine into data frame

rating_table <- data_frame(stage = seq(from = first(GH), to = last(GH) + 0.09, by = 0.01),
                           flow = as.numeric(flow))


# 7. Remove trailing NAs and leading zeros

ind_last_non_missing <- max(which(!is.na(rating_table$flow))) # Index of the last non-missing value

# Index of the last leading zero (1 or greater)
ind_last_zero <- max(min(which(rating_table$flow != 0)) - 1, 1) 

rating_table <- rating_table[ind_last_zero:ind_last_non_missing, ]


rating_table %>% View

# 7. Plot

# Function to create nice axes breaks for log scale plots

log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}

log10_major_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    return(10^(major_breaks))
  }
}

# Plot

rating_table %>% 
  ggplot(aes(x =  flow, y = stage)) + 
  geom_line(colour = 'blue') +
  geom_point(shape = 21, fill = 'grey', colour = 'blue') +
  scale_x_continuous(name = 'Discharge (ML/d)',
                     trans = 'log10', 
                     limits = c(1, NA), 
                     breaks = log10_major_break(),
                     minor_breaks = log10_minor_break(),
                     labels = scales::comma) +
  scale_y_continuous(name = 'Stage (m)',
                     trans = 'log10', 
                     limits = c(0.1, 10),
                     breaks = log10_major_break(),
                     minor_breaks = log10_minor_break()
  )


# 8. Save as a csv

my.file <- str_c('reformatted', my.file, sep = '')

write.csv(rating_table, file = fname <- str_c(my.path, my.file, sep = '/'), row.names = TRUE)

# Best to use write.csv rather than write_csv to avoid weird formatting of output
# write.csv(rating_table, file = /your file/, row.names = FALSE )