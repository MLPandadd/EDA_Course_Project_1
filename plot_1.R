# ggplot2 is used to generate plots
# hms is used to handle time series
# cowplot is used to arrange plots

library(tidyverse)
library(hms)
library(cowplot)

#### Download the file
# URL of the zip file
file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

# Destination file path
destination_path <- "./data"

# Create the destination folder if it does not exist
if (!file.exists(destination_path)) {
  dir.create(destination_path, recursive = TRUE)
}

# Download and unzip the file if it does not exist
if (!file.exists(paste0(destination_path, "/household_power_consumption.zip"))) {
  download.file(file_url, destfile = file.path(destination_path, "household_power_consumption.zip"), mode = "wb")
  
  # Unzip the file
  unzip(file.path(destination_path, "household_power_consumption.zip"), exdir = destination_path)
  
}

# load the data
household_power_consumption <- read_csv2(file = "./data/household_power_consumption.txt", 
                                         col_names = TRUE, 
                                         col_types = cols(
                                           # parse everything as character
                                           .default = col_character()
                                         ),
                                         ) %>% 
  #combine date and time into a new column
  unite(col = DateTime, Date, Time, sep = " ", remove = F) %>% 
  # convert date, time columns into time, and other columns to numbers
  mutate(DateTime = dmy_hms(DateTime), 
         Date = dmy(Date), 
         Time = as_hms(Time), 
         across(Global_active_power : Sub_metering_3, as.numeric)
         ) %>%
  # filter the date
  filter(Date == ymd("2007-02-01") | Date == ymd("2007-02-02"))
 

  
#plot 1
plot_1 <- household_power_consumption %>% 
  ggplot(aes(x = Global_active_power)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "red", boundary = 0)+
  xlab("Global Active Power (kilowatts)") +
  ylab("Frequency") +
  ggtitle("Global Active Power") +
  #removing grids, set background, borders
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        )


print(plot_1)

# save plot_1
ggsave(filename = "plot_1.png",
       plot = plot_1, 
       path = "./plots/", 
       #save to A4 size landscape
       width = 11.69, 
       height = 8.27, 
       units = "in", 
       dpi = 600, 
       bg = "white"
       )
