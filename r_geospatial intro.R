install.packages("dplyr")
install.packages("ggplot2")
getwd("C:/Users/TeedL/Documents/r-geospatial")
getwd()
nordic <- read.csv("data/nordic-data.csv")
nordic$lifeExp
nordic$lifeExp + nordic$country
class(nordic$lifeExp)
nordic_2 <- read.csv("data/nordic-data-2.csv")
class(nordic_2$lifeExp)
str(nordic$country)
str(nordic_2$lifeExp)
nordic <- read.csv(file = "data/nordic-data.csv", stringsAsFactors = FALSE)
?read.csv
str(nordic)
nordic[1, ]
gapminder <- read.csv("data/gapminder_data.csv") ##can also read in directly from web, but no local copy stored unless set as dest file
str(gapminder)
length(gapminder)
class(gapminder)
nrow(gapminder)
dim(gapminder) ##gives number of rows and columns
colnames(gapminder)
head(gapminder) ##gives first couple rows of data...tail will give last few rows
gapminder[sample(nrow(gapminder), 5), ] ##gives pseudorandom sample
below_average <- gapminder$lifeExp < 70.5 ##adds new column to data stating whether life expectency is below world average of 70.5
head(gapminder)
cbind(gapminder, below_average) ##shows results of adding new column
head(cbind(gapminder, below_average))
below_average <- c(TRUE, TRUE, FALSE)
head(cbind(gapminder, below_average))
below_average <-  as.logical(gapminder$lifeExp<70.5)
gapminder <- cbind(gapminder, below_average) ##going back to original df now with extra column added in
new_row <- list('Norway', 2016, 5000000, 'Nordic', 80.3, 49400.0, FALSE) ##adding a new row of data
gapminder_norway <- rbind(gapminder, new_row) ##warning message given because new row of data has no continent listed from those already in data (its trying to add Nordic, but it isn't a continent factor already in data), so this column given an NA
tail(gapminder_norway)
levels(gapminder$continent)
levels(gapminder$continent) <- c(levels(gapminder$continent), "Nordic")
gapminder_norway  <- rbind(gapminder,
                           list('Norway', 2016, 5000000, 'Nordic', 80.3,49400.0, FALSE)) ##as Nordic in as continent option
tail(gapminder_norway)
gapminder$continent <- as.character(gapminder$continent)
str(gapminder)
gapminder <- rbind(gapminder, gapminder) ##binds two data frames together, but now row names are complicated...not consecutive numbers
tail(gapminder, n=3)
rownames(gapminder) <- NULL ##remove rownames and allows R to re-name them sequentially
head(gapminder)
df <- data.frame(first = c("laura"), last = c("teed"), lucky_number = c(7), stringsAsFactors = FALSE) ##test code for generating data
df_2 <- rbind(df, list('simonne', "paine", "3", FALSE)) ##test bind two df
df_2 ##show results
x <- c(5.4, 6.2, 7.1, 4.8, 7.5) ##introducing data
names(x) <- c('a', 'b', 'c', 'd', 'e') ##fiving column names to that data
x #view data
x[c(1, 3)] ##square brackets act as funtion. here it means, get me the nth element. You can ask for same element multiple times, or a range (using :)
x[-2] ##negative number used as index of a vector, R will return every element except the one specified (in this case, the second one)
x[-(1:3)] ##removes all elements between 1 and 3
x[c(2, 3, 4)] ##only include elements 2, 3, and 4
x[-c(1,5)] ##different way of doing above, can also extract by using column name (a, e)
x[c(FALSE, TRUE, TRUE, TRUE, FALSE)] ##different way of doing above...this is the logical vector way of subsetting
x[x>7] ##comparison operation returning values greater than 7. Other comparisons are <, ==
##can combine logical criteria, & (logical and) operator returns TRUE if both left and right are true. | (logical or) returns true if either left, right, or both are true
## ! is logical not which converts true to false and false to true
x[x>4&x<7]

head(gapminder[3]) ##gives you column 3 (can also just name is with $)
gapminder[1:3, ] ##subsets first 3 rows and all columns
gapminder[, -(1:4)] #extract all columns except 1 through 4
gapminder[gapminder$year == 1957, ] #observations collected for only year 1957
gapminder[gapminder$lifeExp > 80, ] #observations with life expectancy >80 years
gapminder[1, 4:5] #extract first row and the fourth and fifth columns
gapminder[gapminder$year == 2002 | gapminder$year == 2007, ] ##extract rows that contain data for the years 2002 and 2007
gapminder_small <- gapminder[c(1:9, 19:23), ] #creates new data frame that only contains row 1-9 and 19-23
mean(gapminder[gapminder$continent == "Africa", "gdpPercap"]) #mean GDP for continent Africa

library("dplyr") ##useful package for manipulating data frames to reduce repetition 
year_country_gdp <- select(gapminder, year, country, gdpPercap) #this will keep only variables selected
year_country_gdp <- gapminder %>% select(year,country,gdpPercap) #same as above just using 'pipes' instead of normal grammar
year_country_gdp_euro <- gapminder %>%
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap) #only european countries (filter) with above only selected variables 
year_country_gdp_afr <- gapminder %>% filter(continent =="Africa") %>% select(lifeExp, country, year)
gapminder %>% group_by(continent) %>% str() #group_by function producing lists where each list contains only rows that correspond to grouped variable (continent in this case)
gdp_bycontinents <- gapminder %>% group_by(continent) %>% summarize(mean_gdpPercap = mean(gdpPercap))##separates out continents and then gives mean gdp for each continent
gdp_bycontinents
gdp_bycontinents <- gapminder %>% group_by(country) %>% summarize(mean_lifeExp = mean(lifeExp)) ##life expectancy average by country (forgot to give it a new name)
gdp_bycontinents %>% filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp)) #gives the countries with the lowest and highest life expectancies
gdp_bycontinents %>% arrange(mean_lifeExp) %>% head(1) #another way to get min value using arrange function to arrange data in ascending order
gdp_bycontinents %>% arrange(desc(mean_lifeExp)) %>% head(1) #same as above but sorts in descending order to give highest value
gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap)) #data grouped by year and continent to calculate mean gdp and can do this for multiple variables
gdp_pop_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop)) #for each continent, in each year, gives mean and sd gdp and population
gdp_pop_bycontinents_byyear #display results
gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE) #count function gives the number of observations for each continent (sort true sorts the results in descending order)

gapminder %>%
  group_by(continent) %>%
  summarize(se_le = sd(lifeExp)/sqrt(n())) #use n function to calculate standard error of life exp per continent
gapminder %>%
  group_by(continent) %>%
  summarize(
    mean_le = mean(lifeExp),
    min_le = min(lifeExp),
    max_le = max(lifeExp),
    se_le = sd(lifeExp)/sqrt(n())) #chain together several summary operations
gdp_pop_bycontinents_byyear <- gapminder %>%
  mutate(gdp_billion = gdpPercap*pop/10^9) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion)) #mutate function changes pop to billions 

##lesson 7 (intro to visualization)
library("ggplot2")
ggplot(data = gapminder, aes(x = lifeExp)) +   
  geom_histogram() ##aes function tells ggplot how variables in the data map to aesthetic properties of figure. this one plots histogram lifeExp
gapminder_small <- filter(gapminder, year == 2007, continent == "Americas") #data from 2007 in americas
ggplot(data = gapminder_small, aes(x = country, y = gdpPercap)) + 
  geom_col() #plots this subset with country x-axis and gdp y-axis (all countries overlap and can't read them)
ggplot(data = gapminder_small, aes(x = country, y = gdpPercap)) + 
  geom_col() +
  coord_flip() ##flip axes so we can read country names
gapminder_small_2 <- gapminder %>%
  filter(continent == "Americas",
         year %in% c(1952, 2007)) #filtered dataset from americas in year 1952 and 2007
ggplot(gapminder_small_2, 
       aes(x = country, y = gdpPercap, 
           fill = as.factor(year))) +
  geom_col(position = "dodge") + 
  coord_flip() #plot of above with assigned colours for each year. Multiple bars for each country require position parameter of dodge so they appear side-by-side (otherwise deault is stack)
pdf("Distribution-of-gdpPercap.pdf", width=12, height=4)
ggplot(data = gapminder, aes(x = gdpPercap)) +   
  geom_histogram() ##saves plot as pdf. can also do the same to save as jpeg or png
dev.off()
ggplot(data = gapminder_small_2, aes(x = country, y = gdpPercap, fill = as.factor(year))) +
  geom_col(position = "dodge") + coord_flip()
aust_subset <- filter(gapminder, country == "Australia") #creates subset of Australia data
write.csv(aust_subset,
          file="data/gapminder-aus.csv") #saves this subset as own csv (i.e., writing data)
write.csv(
  aust_subset,
  file="cleaned-data/gapminder-aus.csv",
  row.names=FALSE) #same as above, but will take out row numbers from entire dataset and just give ones from subset