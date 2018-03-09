###########################################################################
# DataJenius: School Shootings Project
#
###########################################################################
# Script 001
#--------------------------------------------------------------------------
# Data collected from Wikipedia list of school shootings in the USA
# https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States
#
# This script will import the data, clean it up for use in R, and create 
# some visualizations of the data.
#
###########################################################################
# Author(s)
#--------------------------------------------------------------------------
# Josh Pause
###########################################################################

###########################################################################
# Load all of our required dependencies - install as needed
require(dplyr)
require(ggplot2)
require(PerformanceAnalytics)
require(tidyr)
require(xts)
require(zoo)

###########################################################################
# Load our data into a data frame and clean it up
wiki.df <- read.csv("data/Wikipedia_List_of_school_shootings_in_the_United_States.csv", na.strings = "NULL")
wiki.df <- subset(wiki.df, select=-c(description, location, date_string))

# The incident of March 2, 2018 had 2 deaths, 0 injuries
wiki.df[474, c("injuries")] <- 0

# Some records prior to 1900 have unknown counts of injuries (?, 1+, NULL)
# Replace these counts with the mean of available data
wiki.df[is.na(wiki.df$injuries), c("injuries")] <- mean(wiki.df$injuries, na.rm=TRUE)

# Combined deaths and injuries 
wiki.df$combined <- wiki.df$deaths + wiki.df$injuries

# Reorder non-alphabetical factors
wiki.df$day_of_week = factor(wiki.df$day_of_week,levels(wiki.df$day_of_week)[c(2,6,7,5,1,3,4)])
wiki.df$month = factor(wiki.df$month,levels(wiki.df$month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])

# Create decade feature using modulus 
wiki.df$decade <- wiki.df$year - wiki.df$year %% 10

# just checking
#levels(wiki.df$month)
#head(wiki.df)
#tail(wiki.df)
#str(wiki.df)


###########################################################################
# This function will summarize the wiki data by the selected feature
summarize_data_by_feature <- function(df,selected_feature) {
  output <- df %>%
    group_by_(selected_feature) %>%
    summarise(
      incidents = n(),
      deaths = sum(deaths),
      injuries = sum(injuries),
      combined = sum(combined),
      deaths.per.incident = sum(deaths) / n(),
      injuries.per.incident = sum(injuries) / n(),
      combined.per.incident = sum(combined) / n()
    )
  return(output)
}


###########################################################################
# This function will plot summarized wiki data by the selected feature
plot_summarize_data_by_feature <- function(df,selected_feature, label, title, incidents_only = FALSE) {
  ggplot.data <- gather(df, status, count, incidents:injuries, factor_key=TRUE)
  ggplot.data <- subset(ggplot.data, select=-c(combined))
  if(incidents_only) {
    ggplot.data <- ggplot.data[ggplot.data$status=="incidents",]
  }
  ggplot(ggplot.data, aes_string(x=selected_feature, y="count", fill="status")) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c("#094dba", "#ba0823", "#ba7807")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    guides(fill=guide_legend(title="Total")) +
    xlab(label) +
    ylab("Total") +
    labs( title = title,
          subtitle = "All school shootings: July 26, 1764 through March 2, 2018" 
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

#####################################################################################
# This function will plot summarized wiki data by the selected feature per incident
plot_summarize_data_by_feature_per_incident <- function(df,selected_feature, label, title) {
  ggplot.data <- gather(df, status, count, deaths.per.incident:combined.per.incident, factor_key=TRUE)
  ggplot.data <- subset(ggplot.data, select=-c(combined, incidents, deaths, injuries))
  ggplot(ggplot.data, aes_string(x=selected_feature, y="count", group="status", color="status")) +
    geom_line(size=1.5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab(label) +
    ylab("Deaths & Injuries per Incident") +
    labs( title = title,
          subtitle = "All school shootings: July 26, 1764 through March 2, 2018" 
    ) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    scale_color_manual(
      name="Per Incident",
      values=c("#ba0823", "#ba7807","#ae08ba"),
      breaks=c("combined.per.incident", "deaths.per.incident", "injuries.per.incident"),
      labels=c("combined", "deaths", "injuries"))
}


###########################################################################
# Look at this data on a day-of-week basis
by.dow <- summarize_data_by_feature(wiki.df,"day_of_week")
plot_summarize_data_by_feature(by.dow,"day_of_week", "Day of Week", "School Shootings by Day of Week")
plot_summarize_data_by_feature_per_incident(by.dow, "day_of_week", "Day of Week", "Per Incident by Day of Week")


###########################################################################
# Look at this data on a month-by-month basis
by.month <- summarize_data_by_feature(wiki.df,"month")
plot_summarize_data_by_feature(by.month,"month", "Month", "School Shootings by Month")
plot_summarize_data_by_feature_per_incident(by.month, "month", "Month", "Per Incident by Month")


###########################################################################
# Look at this data on a state-by-state basis
by.state <- summarize_data_by_feature(wiki.df,"state")
plot_summarize_data_by_feature(by.state,"state", "State", "School Shootings by State")
plot_summarize_data_by_feature_per_incident(by.state, "state", "State", "Per Incident by State")

# Same graph as above but with only the incidents in blue
plot_summarize_data_by_feature(by.state,"state", "State", "School Shootings by State", TRUE)

###########################################################################
# Look at this data by decade
by.decade <- summarize_data_by_feature(wiki.df,"decade")
plot_summarize_data_by_feature(by.decade,"decade", "10 Year Periods", "School Shootings by Decade")
plot_summarize_data_by_feature_per_incident(by.decade, "decade", "10 Year Periods", "Per Incident by Decade")


###########################################################################
# Look at this data by decade - since 1950
by.decade.1950 <- summarize_data_by_feature(wiki.df[wiki.df$year >= 1950,],"decade")
plot_summarize_data_by_feature(by.decade.1950,"decade", "10 Year Periods", "School Shootings by Decade Since 1950")
plot_summarize_data_by_feature_per_incident(by.decade.1950, "decade", "10 Year Periods", "Per Incident by Decade Since 1950")

###########################################################################
# Look at this data by year - since 1950
by.year.1950 <- summarize_data_by_feature(wiki.df[wiki.df$year >= 1950,],"year")
plot_summarize_data_by_feature(by.year.1950, "year", "Individual Years", "School Shootings by Year Since 1950")
plot_summarize_data_by_feature_per_incident(by.year.1950, "year", "Individual Years", "Per Incident by Year Since 1950")


###########################################################################
# To better visualize changes over time, convert to a proper time series
wiki.df$date <- as.character(wiki.df$date)

# We have more than one incident on some days
wiki.series <- wiki.df %>%
  group_by(date) %>%
  summarise(
    incidents = n(),
    deaths = sum(deaths),
    injuries = sum(injuries),
    combined = sum(combined)
  )

# Create a full series for every day in this period
full.series <- data.frame(date=as.character(seq(as.POSIXct("1764-07-26"),as.POSIXct("2018-03-03"),by="days")))
full.series$date <- as.character(substr(full.series$date, 1, 10))

# merge wiki data into the full series
wiki.series.full <- merge(full.series, wiki.series, by = c("date"), all.x = TRUE)
wiki.series.full[is.na(wiki.series.full$incidents), c("incidents")] <- 0
wiki.series.full[is.na(wiki.series.full$deaths), c("deaths")] <- 0
wiki.series.full[is.na(wiki.series.full$injuries), c("injuries")] <- 0
wiki.series.full[is.na(wiki.series.full$combined), c("combined")] <- 0

# Convert to XTS object
wiki.series.full$date <- as.POSIXct(wiki.series.full$date, format="%Y-%m-%d", tz="America/New_York")
wiki.xts <- xts(x=wiki.series.full[,c("incidents","deaths","injuries","combined")], order.by=wiki.series.full$date)


###########################################################################
# Are incidents of school shooting becoming more frequent?
incidents.xts <- wiki.xts$incidents

# get the average number of incidents per day
incidents.xts$MA25yr <- rollapply(wiki.xts$incidents, width=9125, FUN=mean)
incidents.xts$MA10yr <- rollapply(wiki.xts$incidents, width=3650, FUN=mean)
incidents.xts$MA5yr <- rollapply(wiki.xts$incidents, width=1825, FUN=mean)

# put mean incidents on an annualized scale
incidents.xts$MA25yr <- incidents.xts$MA25yr*365
incidents.xts$MA10yr <- incidents.xts$MA10yr*365
incidents.xts$MA5yr <- incidents.xts$MA5yr*365

# plot the xts of MA of incidents since 1950
plot.incidents.xts <- incidents.xts["1950/2019"]
plot.incidents.xts <- subset(plot.incidents.xts, select = -c(incidents))
chart.TimeSeries(plot.incidents.xts,
                 colorset = c("darkorange","darkred","darkblue"),
                 main = "School Shootings per Year, Rolling Average",
                 lwd = 2,
                 legend.loc = "topleft")


###########################################################################
###########################################################################
###########################################################################
# Great idea from Mo - can we add unemployment and/or S&P and/or other 
# crime rates to this graph?
###########################################################################
###########################################################################
###########################################################################


###########################################################################
# Are deaths and injuries becoming more extreme?
combined.xts <- incidents.xts
combined.xts$combined <- wiki.xts[,c("combined")]

# get the average number of combined deaths and injuries per day
combined.xts$cMA25yr <- rollapply(combined.xts$combined, width=9125, FUN=mean)
combined.xts$cMA10yr <- rollapply(combined.xts$combined, width=3650, FUN=mean)
combined.xts$cMA5yr <- rollapply(combined.xts$combined, width=1825, FUN=mean)

# put mean combined on an annualized scale
combined.xts$cMA25yr <- combined.xts$cMA25yr*365
combined.xts$cMA10yr <- combined.xts$cMA10yr*365
combined.xts$cMA5yr <- combined.xts$cMA5yr*365

# calculate rolling average of combined mean / incident mean
combined.xts$diMA25yr <- combined.xts$cMA25yr/combined.xts$MA25yr
combined.xts$diMA10yr <- combined.xts$cMA10yr/combined.xts$MA10yr
combined.xts$diMA5yr <- combined.xts$cMA5yr/combined.xts$MA5yr

# plot the xts of MA of incidents since 1950
plot.combined.xts <- combined.xts["1950/2019", c("diMA25yr","diMA10yr","diMA5yr")]
chart.TimeSeries(plot.combined.xts,
                 colorset = c("darkorange","darkred","darkblue"),
                 main = "Deaths & Injuries per Incident, Rolling Average",
                 lwd = 2,
                 legend.loc = "topleft")

# the large number of incidents makes the above graph potentially deceptive

# let's unpack it further and graph the most horrible events - quick and dirty here
jojo <- wiki.df
jojo$combined <- jojo$deaths+jojo$injuries
jojo <- jojo[jojo$year >= 1950,]
jojo <- head(jojo[order(jojo$combined,decreasing = TRUE),],n=25)
jojo <- jojo[order(jojo$year),]
ggplot(jojo, aes(x=date,y=combined, fill=decade)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Date of Incident") +
  ylab("Combined Deaths & Injuries") +
  guides(fill=guide_legend(title="Year")) +
  labs( title = "The 25 Worst School Shootings since 1950",
        subtitle = "School shootings with the most combined deaths and injuries" 
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# % of incidents with at least 10 combined?
jojo <- wiki.df
jojo <- jojo[jojo$year >= 1950,]
jojo$combined <- jojo$deaths+jojo$injuries
jojo$extreme <- ifelse(jojo$combined >= 10,1,0)
bobo <- jojo %>%
  group_by(decade) %>%
  summarize(
    count = sum(extreme),
    all = n(),
    per=mean(extreme))
jojo <- bobo[,c("per","decade")]
ggplot(jojo, aes(x=decade,y=per, fill=decade)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Decade") +
  scale_y_continuous("Incidents with 10+ Deaths & Injuries", labels = scales::percent) +
  guides(fill=guide_legend(title="Year")) +
  labs( title = "Relative Frequency of Extreme Incidents",
        subtitle = "Extreme incidents as a percentage of all school shootings by decade" 
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# quick and dirt summary stats
jojo <- wiki.df
jojo <- jojo[jojo$year >= 1950,]
jojo$combined <- jojo$deaths+jojo$injuries
jojo$extreme.incidents <- ifelse(jojo$combined >= 10,1,0)
bobo <- jojo %>%
  group_by(decade) %>%
  summarize(
    incidents = n(),
    extreme.incidents = sum(extreme.incidents)
  )
bobo



###########################################################################
# Would be great to add timeline of gun control laws and notorious 
# school schootings, e.g. Comlumbine & Sandy Hook
# https://www.thoughtco.com/us-gun-control-timeline-3963620

# 1968 looks like a strong corr to the Gun Control Act of 1968
# but- the incidents spike up through the creation of the ATF


###########################################################################
# Do these scores correlate with school shootings?
# http://lawcenter.giffords.org/scorecard/
scorecard.df <- read.csv("data/Giffords_lawcenter_scorecard_grades_2018_03_04.csv")

# Only look at incidents on after 2000
by.state <- summarize_data_by_feature(wiki.df[wiki.df$year >= 2000,], "state")

# Merge grades with states
state.grades <- merge(by.state, scorecard.df, by = c("state"), all.y = TRUE)

# Set states with no incidents to 0 for accuracy
state.grades[is.na(state.grades$incidents),c("incidents")] <- 0
state.grades[is.na(state.grades$deaths),c("deaths")] <- 0
state.grades[is.na(state.grades$injuries),c("injuries")] <- 0
state.grades[is.na(state.grades$combined),c("combined")] <- 0

# Add population data for each state
# https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population
pop.df <- read.csv("data/Wikipedia_states_by_population_2018-03-05.csv")
state.grades <- merge(state.grades, pop.df, by = c("state"))

# Add homicide rate data for each state
# https://en.wikipedia.org/wiki/List_of_U.S._states_by_homicide_rate
homicide.df <- read.csv("data/Wikipedia_states_by_homicide_rate_2018-03-05.csv")
state.grades <- merge(state.grades, homicide.df, by = c("state"))

# Assign each letter grade a numeric value for the sake of calculating correlation
state.grades[state.grades$grade=="A",c("grade.score")] <- 9
state.grades[state.grades$grade=="A-",c("grade.score")] <- 8
state.grades[state.grades$grade=="B+",c("grade.score")] <- 7
state.grades[state.grades$grade=="B",c("grade.score")] <- 6
state.grades[state.grades$grade=="C+",c("grade.score")] <- 5
state.grades[state.grades$grade=="C",c("grade.score")] <- 4
state.grades[state.grades$grade=="C-",c("grade.score")] <- 3
state.grades[state.grades$grade=="D",c("grade.score")] <- 2
state.grades[state.grades$grade=="D-",c("grade.score")] <- 1
state.grades[state.grades$grade=="F",c("grade.score")] <- 0

# Group grades into three categories
state.grades[state.grades$grade=="A",c("grade.group")] <- "A-B"
state.grades[state.grades$grade=="A-",c("grade.group")] <- "A-B"
state.grades[state.grades$grade=="B+",c("grade.group")] <- "A-B"
state.grades[state.grades$grade=="B",c("grade.group")] <- "A-B"
state.grades[state.grades$grade=="C+",c("grade.group")] <- "C-D"
state.grades[state.grades$grade=="C",c("grade.group")] <- "C-D"
state.grades[state.grades$grade=="C-",c("grade.group")] <- "C-D"
state.grades[state.grades$grade=="D",c("grade.group")] <- "C-D"
state.grades[state.grades$grade=="D-",c("grade.group")] <- "C-D"
state.grades[state.grades$grade=="F",c("grade.group")] <- "F"
state.grades$grade.group <- as.factor(state.grades$grade.group)

head(state.grades)

###########################################################################
# Now that we've got all our stats let's look at correlation
# The date selected above (1980 or 2000) has no effect on this
cor(state.grades$grade.score,state.grades$homicide.rate, method="spearman")
cor(state.grades$grade.score,state.grades$homicide.rate, method="kendall")
cor(state.grades$grade.score,state.grades$homicide.rate, method="pearson")

# Homicides v Giffords Gun Score: -0.2285 as grade improves
ggplot(state.grades, aes(x=grade.score, y=homicide.rate))+
  geom_point() + 
  ylab("Homicides per 100k population (2016)") +
  scale_x_continuous("Giffords Gun Score (2017)", breaks=c(0,2,4,6,9), labels=c("F","D","C","B","A")) + 
  labs( title = "Homicides v Giffords Gun Score",
        subtitle = paste("All 50 States by Score, Pearson Correlation Coeffecient: ",round(cor(state.grades$grade.score,state.grades$homicide.rate, method="pearson"),4), sep="")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_smooth(method='lm')

# Homicides v Giffords Gun Score: Box & Whiskers
ggplot(state.grades, aes(x=grade.group, y=homicide.rate))+
  geom_boxplot() + 
  ylab("Homicides per 100k population (2016)") +
  xlab("Giffords Gun Score (2017)") +
  labs( title = "Homicides v Giffords Gun Score",
        subtitle = "All 50 States by Score, Grouped by Grades") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



##############################################################################
# Calculate school shooting incidents and d&i per 100,000 general population
# The date selected above (1980 or 2000) has an effect on this

# Some states had no incidents
state.grades[state.grades$incidents==0,c("incidents.per.100k")] <- 0
state.grades[is.na(state.grades$incidents.per.100k),c("incidents.per.100k")] <- state.grades[is.na(state.grades$incidents.per.100k),c("incidents")]/(state.grades[is.na(state.grades$incidents.per.100k),c("population")]/100000)

# Total combined injuries and deaths
state.grades[state.grades$incidents==0,c("combined.per.100k")] <- 0
state.grades[is.na(state.grades$combined.per.100k),c("combined.per.100k")] <- state.grades[is.na(state.grades$combined.per.100k),c("combined")]/(state.grades[is.na(state.grades$combined.per.100k),c("population")]/100000)
state.grades


# Incidents v Giffords Gun Score: -0.3028 as grade improves  (since 1980)
ggplot(state.grades, aes(x=grade.score, y=incidents.per.100k))+
  geom_point() + 
  ylab("Incidents per 100k population (2017)") +
  scale_x_continuous("Giffords Gun Score (2017)", breaks=c(0,2,4,6,9), labels=c("F","D","C","B","A")) + 
  labs( title = "School Shootings (Since 2000) v Giffords Gun Score",
        subtitle = paste("All 50 States by Score, Pearson Correlation Coeffecient: ",round(cor(state.grades$grade.score,state.grades$incidents.per.100k, method="pearson"),4), sep="")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_smooth(method='lm')

# Incidents v Giffords Gun Score: Box & Whiskers 
ggplot(state.grades, aes(x=grade.group, y=incidents.per.100k))+
  geom_boxplot() + 
  ylab("Incidents per 100k population (2017)") +
  xlab("Giffords Gun Score (2017)") +
  labs( title = "School Shootings (Since 2000) v Giffords Gun Score",
        subtitle = "All 50 States by Score, Grouped by Grades") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# D&I v Giffords Gun Score: -0.1113 as grade improves (since 1980)
ggplot(state.grades, aes(x=grade.score, y=combined.per.100k))+
  geom_point() + 
  ylab("Deaths & injuries per 100k population (2017)") +
  scale_x_continuous("Giffords Gun Score (2017)", breaks=c(0,2,4,6,9), labels=c("F","D","C","B","A")) + 
  labs( title = "School Shootings (Since 2000) v Giffords Gun Score",
        subtitle = paste("All 50 States by Score, Pearson Correlation Coeffecient: ",round(cor(state.grades$grade.score,state.grades$combined.per.100k, method="pearson"),4), sep="")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_smooth(method='lm')

# D&I v Giffords Gun Score: -0.1113 as grade improves
# We have an outlier - Wyoming - 1986
# https://en.wikipedia.org/wiki/Cokeville_Elementary_School_hostage_crisis

# D&I v Giffords Gun Score: Box & Whiskers 
ggplot(state.grades, aes(x=grade.group, y=combined.per.100k))+
  geom_boxplot() + 
  ylab("Deaths & injuries per 100k population (2017)") +
  xlab("Giffords Gun Score (2017)") +
  labs( title = "School Shootings (Since 2000) v Giffords Gun Score",
        subtitle = "All 50 States by Score, Grouped by Grades") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

