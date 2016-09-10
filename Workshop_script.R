install.packages(c("caret","ggplot2","dplyr","shiny",
                   "leaflet","plotly","networkD3",
                   "knitr"))

library(caret)
library(dplyr)
library(ggplot2)
library(plotly)
data("GermanCredit")
 
str(GermanCredit)


names(GermanCredit)[1:20]


## Data Preparation cd...

GermanCredit$Sex <- 
  GermanCredit$Personal.Male.Divorced.Seperated +
  GermanCredit$Personal.Male.Single + 
  GermanCredit$Personal.Male.Married.Widowed

GermanCredit$Sex <- ifelse(GermanCredit$Sex, "Male", "Female")

## Data Preparation cd...

GermanCredit$Property <- 
  sapply(1:dim(GermanCredit)[1], function(i)
    if(GermanCredit$Property.RealEstate[i]){
      "RealEstate"
    } else if(GermanCredit$Property.Insurance[i]) {
      "Insurance"
    } else if(GermanCredit$Property.CarOther[i]) {
      "CarOther"
    } else {
      "Unknown"
    })

## Data Preparation cd...

GermanCredit$Purpose <- 
  names(GermanCredit[20:30])[max.col(GermanCredit[20:30])]

## Data Preparation cd... 

GermanCredit <- GermanCredit[, c(1:7, 10, 63:65)]
str(GermanCredit, width = 80, strict.width = "wrap")
ggplot(data = GermanCredit, aes(x = Age, y = Amount)) +

## Altering features 

ggplot(data = GermanCredit, aes(x = Age, y = Amount)) +
  geom_point(alpha = 0.5, color = "DarkGreen", 
GermanCredit_tmp <- GermanCredit[1:200, ]
ggplot(data = GermanCredit_tmp, aes(x = Amount, y = Duration,
                                    color = Sex, size = Duration*Amount)) +
  geom_point(alpha = 0.3)

# Test yourself

GermanCredit_tmp <- GermanCredit[1:200, ]
ggplot(data = GermanCredit_tmp, aes(x = Amount, y = Duration,
                                    color = Sex, size = Duration*Amount)) +
  geom_point(alpha = 0.3)


## More alterations {.smaller}

ggplot(data = GermanCredit, aes(x = Age, y = Amount, color = Duration)) +
  geom_point(alpha = 0.45) +
  facet_grid(. ~ Sex)

ggplot(data = GermanCredit, aes(x = Age, y = Amount, color = Duration,
                                size = factor(NumberExistingCredits))) +
  geom_point(alpha = 0.45) +
  facet_grid(. ~ Sex)


## Anatomy of ggplots

#ggplot(data = [dataframe], aes(x = [var_x], y = [var_y], 
#                               color = [var_for_color], fill = [var_for_fill], 
#                               shape = [var_for_shape])) +
#  geom_[some_geom] +
#  ... # other options


## Histograms

ggplot(data = GermanCredit, aes(x = Amount)) +
  geom_histogram(binwidth = 500)


## Boxplots

ggplot(data = GermanCredit, aes(y = Age, x = Sex, fill = Sex)) +
  geom_boxplot()

## Barplots

ggplot(data = GermanCredit, aes(x = Property)) +
  geom_bar()

## Segmented barplots

ggplot(data = GermanCredit, aes(x = Property, fill = Purpose)) +
  geom_bar()

## Segmented barplots - proportions

ggplot(data = GermanCredit, aes(x = Property, fill = Purpose)) +
  geom_bar(position="fill")

## Try yourself 

k <- ggplot(GermanCredit, aes(Purpose, fill=Class))
k1 <- k + geom_bar(position="dodge") 
k2 <- k1 + scale_fill_brewer() 
k3 <- k2 + coord_flip()
k4 <- k3 + facet_grid(. ~ Sex)
k1
k2 
k3
k4

## `plotly` trailer {.smaller}

plot_ly(GermanCredit, x = Duration, y = Amount, 
text = paste("Duration: ", Duration),
mode = "markers", color = Age, size = Duration)

## What else can be done with R 

library(networkD3)

# Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
Source = "source", Target = "target",
Value = "value", NodeID = "name",
Group = "group", opacity = 0.8,
zoom = TRUE)

## What else can be done with R {.smaller}

suppressMessages(library(leaflet))

m <- leaflet() %>%
addTiles() %>%  # Add default OpenStreetMap map tiles
addMarkers(lng=44.7693005, lat=41.6881125, popup="Techpark Tbilisi")
m

# Data wrangling

GermanCredit %>%
filter(Purpose == "Purpose.Retraining") %>% 
select(Age, Amount, Sex, Class, Property)


GermanCredit %>%
filter(Purpose == "Purpose.Retraining", Sex == "Male") %>% 
select(Age, Amount, Sex, Class, Property)

GermanCredit %>%
group_by(Purpose) %>%
summarise(Purpose_count = n())

install.packages("stringr")
library(stringr)

GermanCredit <- GermanCredit %>%
mutate(Purpose = str_replace(Purpose, "Purpose.", "")) 

GermanCredit %>%
  group_by(Purpose) %>%
  summarise(Purpose_count = n()) 

GermanCredit %>%
  slice(1:5)

last_row <- nrow(GermanCredit)
GermanCredit %>%
  slice((last_row-4):last_row)

GermanCredit %>%
  select(Class, Sex) %>%

GermanCredit %>%
  select(-Property) %>%
  slice(1:5)

names(GermanCredit)

GermanCredit <- GermanCredit %>%
  rename(Gender = Sex)

names(GermanCredit)

GermanCredit %>%
  group_by(Purpose) %>%
  summarise(count = n()) %>%
  arrange(count)

GermanCredit %>%
  group_by(Purpose) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
