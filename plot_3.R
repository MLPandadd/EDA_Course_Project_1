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
 

#plot 3
plot_3 <- household_power_consumption %>% 
  ggplot(aes(x = DateTime)) +
  geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1")) +
  geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, color = "Sub_metering_3")) +
  scale_color_manual(name = NULL, 
                     values = c("Sub_metering_1" = "black", "Sub_metering_2" = "red", "Sub_metering_3" = "blue")) +
  scale_x_datetime(date_breaks = "day", date_labels = "%a") +
  xlab(NULL) +
  ylab("Energy sub metering") +
  #removing grids, set background, borders and legend box
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = c(0.975, 0.975), 
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 12))

print(plot_3)

# save plot_3
ggsave(filename = "plot_3.png",
       plot = plot_3, 
       path = "./plots/", 
       #save to A4 size landscape
       width = 11.69, 
       height = 8.27, 
       units = "in", 
       dpi = 600, 
       bg = "white"
)
