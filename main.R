# Definitions
# rec_i = recession begin
# rec_f = recession end
# rec_d = recession duration
# exp_d = expansion duration

# Set working directory to Dow Jones data
setwd("D:/Github/Recession-Prediction")

# Load libraries
library(openxlsx)
library(ggplot2)
library(lubridate)

# Initialize filename
filename <- "data.xlsx" 

# Read Dow Jones price history into data frame 1
df1 <- read.xlsx(filename, sheet = 1, skipEmptyRows = TRUE, detectDates = TRUE)

# Read recession dates into data frame 2
df2 <- read.xlsx(filename, sheet = 2, skipEmptyRows = TRUE, detectDates = TRUE)

# Parse column data into vectors
value = as.double(df1[,1])
date_input = as.Date(ymd_hms(df1[,3]))
rec_i = as.Date(ymd(df2[,1]))
rec_f = as.Date(ymd(df2[,2]))
rec_name = as.character(df2[,3])

# Calculate duration of each recession
rec_d = rec_f - rec_i   # returns time difference in days

# Calculate duration of each expansion
exp_d = rep(0,length(rec_d) - 1)
for (i in 1:nrow(df2) - 1) {
  exp_d[i] = rec_i[i+1] - rec_f[i]
}

# Calculate average duration of recession and between end of WWII and 9/11
relevant_dates = which(rec_f > as.Date(ymd(19450902)) & rec_i < as.Date(ymd(20010911)))

avg_rec_d = mean(rec_d[relevant_dates])
avg_exp_d = mean(exp_d[relevant_dates])


# Separate df2 data into two parts 

df2_lo <- df2[1:13,]
df2_hi <- df2[14:nrow(df2),]
rec_i_lo <- rec_i[1:13]
rec_i_hi <- rec_i[14:nrow(df2)]
rec_name_lo <- rec_name[1:13]
rec_name_hi <- rec_name[14:nrow(df2)]

# Create plot with base dataset and aesthetic mappings
ggplot() + xlab("Year") + ylab("Value of Dow Jones Industrial") + 
  
  # Add price history line graph layer
geom_line(data = df1,
          mapping = aes(x = date_input, 
                        y = value, 
                        group = 1)) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          scale_x_date(date_labels = "%Y", date_breaks = "10 year") +
          scale_y_log10() + 
  
# Add shaded recession duration layer
geom_rect(data = df2, 
          mapping = aes(xmin = rec_i,
                        xmax = rec_f,
                        ymin = 0,
                        ymax = 20000),
          fill = 'red',
          alpha = 0.5) + 
  
# Add recession names layer
geom_text(data = df2[c(1:10,12),],
          mapping = aes(x = rec_i[c(1:10,12)],
                        y = 400,
                        label = rec_name[c(1:10,12)]),
          size = 3.5,
          angle = 90,
          hjust = 0,
          vjust = 0,
          colour = "red") +
  
# Add recession names layer
geom_text(data = df2[c(11,13:nrow(df2)),],
          mapping = aes(x = rec_i[c(11,13:nrow(df2))],
                        y = 20,
                        label = rec_name[c(11,13:nrow(df2))]),
          size = 3.5,
          angle = 90,
          hjust = 0,
          vjust = 0,
          colour = "red") +

geom_segment(data = df2,
             mapping = aes(x = as.Date(ymd(19450902)),
                           y = 4000,
                           xend = as.Date(ymd(20010911)),
                           yend = 4000),
             arrow = arrow(length=unit(0.2,"cm"),ends = "both"),
             color = "black") +
  
geom_text(data = df2,
          mapping = aes(x = as.Date(ymd(19450902)),
                        y = 4000,
                        label = "End of WWII"),
          size = 5,
          angle = 90,
          vjust = 0,
          color = "black") + 
  
  geom_text(data = df2,
            mapping = aes(x = as.Date(ymd(20010911)),
                          y = 4000,
                          label = "9/11"),
            size = 5,
            angle = 90,
            vjust = 1,
            color = "black") + 

  # Add average recession duration
  geom_text(data = df2,
            mapping = aes(x = as.Date(ymd(19730101)),
                          y = 4000,
                          label = sprintf("Mean recession duration: %4.2f months",avg_rec_d/30)),
            size = 5,
            vjust = -0.5,
            color = "black") + 
  
  # Add average expansion duration
  geom_text(data = df2,
            mapping = aes(x = as.Date(ymd(19730101)),
                          y = 4000,
                          label = sprintf("Mean expansion duration: %4.2f months",avg_exp_d/30)),
            size = 5,
            vjust = 1,
            color = "black")