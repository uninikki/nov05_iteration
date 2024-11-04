##***************************
##  Software Tools - Iteration
##
## Karl Cottenie
##
## 2024-11-03
##
##***************************

## _ Packages used -------
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(viridis)
# + scale_color/fill_viridis_c/d()
theme_set(theme_light())


# Startup ends here

## _ One solution to coding challenge from Functions ----

dfMice

dfWhales

fn_uniqueSpecies <- function(df) {
  df.unique <- df %>%
    group_by(species_name) %>%
    summarize(num_species = length(unique(species_name))) %>%
    arrange(desc(num_species))
  return(df.unique)
}

dfMice %>% fn_uniqueSpecies()
dfWhales %>% fn_uniqueSpecies()

# However, this function made a couple of mistakes!
# Use this page https://cosimameyer.com/post/mastering-debugging-in-r/
# and explore how debug(), broswer(), undebug() work to identify these mistakes

debug(fn_uniqueSpecies)
fn_uniqueSpecies(dfWhales)
undebug(fn_uniqueSpecies)

# Fix the function so it performs the task of counting the number of species for each bin_uri

# Finally, consult "Content" > "Oct 17 - Sequence Clustering" > "Is ecology ready for big code"
# Create a smart check that your code actually works as intended

# Also, check out the package flow to visualize your function structure
# e.g.

flow::flow_run(fn_uniqueSpecies(dfWhales))

## _ Repeat a function multiple times -----

## __ Option 1: write it multiple times ----

dfMice %>% fn_uniqueSpecies()
dfWhales %>% fn_uniqueSpecies()

# Easy to do for 2 groups, but what if you have 10, 1000, 1000 groups?
# Be smart, and automate :-)

## __ Option 2: write a for loop ----

# First create a list with your data sets
ls_mammals = list(Mice = dfMice, Whales = dfWhales)
ls_mammals
ls_mammals$Mice %>% class()
ls_mammals[1] %>% class()
ls_mammals[[1]] %>% class()

for( i in 1:2) {
  fn_uniqueSpecies(ls_mammals[i])
}

# TODO for you in class: Fix the code!!!!!!!

# If you want to save the output, you could write something like this

ls_uniqueSp = vector(mode = "list", length = length(ls_mammals))
for( i in 1:2) {
  ls_uniqueSp[[i]] = fn_uniqueSpecies(ls_mammals[[i]])
}

ls_uniqueSp

## ===> this is a lot of overhead!!!!!
# initiate a object to store results
# create a for loop
# initiate the iterator
# write the function and save it correctly

## __ Option 3: map functions -----

# tidyverse has a very elegant solution to this problem
# the code from line 76-79 becomes

map(ls_mammals, fn_uniqueSpecies)
# the function map should be read as:
# for each element in the list ls_mammals
# apply the function fn_uniqueSpecies
# and save the output as a list with a similar structure as ls_mammals

# Consult https://rstudio.github.io/cheatsheets/html/purrr.html
# download the pdf of this cheatsheet and put it on your desktop!!!!

# or via piping

ls_mammals %>% 
  map(fn_uniqueSpecies)

# and we continue in our exploratory data analysis with piping

ls_mammals %>% 
  map(fn_uniqueSpecies) %>% 
  map(dim)

# and if we want to save the output

ls_uniqueSp2 = map(ls_mammals, fn_uniqueSpecies)

# if you want the output in a different format

ls_mammals %>% 
  map(fn_uniqueSpecies) %>% 
  list_rbind()

ls_mammals %>% 
  map(fn_uniqueSpecies) %>% 
  list_rbind(names_to = "group")

## __ Option 4: complex and anonymous functions in map ----

# the function that use in map does not have to be a function available
# you can make anonymous functions only for this one step in the analysis

ls_mammals %>% 
  map(function(x) dim(x)[1] * dim(x)[2])
# this computes the number of data points in each data set

# the function map in this case should be read as:
# for each element x in the list ls_mammals
# compute the dimensions of x and take the first element (i.e., the number of rows)
# compute the dimensions of x and take the second element (i.e., the number of columns)
# multiply those two
# and save the output as a list with a similar structure as ls_mammals

# you can make the function construction similar to last week

ls_mammals %>% 
  map(function(x){
    nr_rows = dim(x)[1] 
    nr_columns = dim(x)[2]
    return(nr_rows * nr_columns)
  })

# Simplifying the output, this time to a vector

ls_mammals %>% 
  map_dbl(function(x) dim(x)[1] * dim(x)[2])

# shorthand for anonymous functions

ls_mammals %>% 
 map_dbl(\(x) dim(x)[1] * dim(x)[2])

## _ group_nest and map -----

# in data analysis, you will want to perform complex manipulations of groups within a dataset
# group_by creates groups, and summarize computes something for each group

dfMice %>% 
  group_by(bin_uri) %>% 
  summarize(num_species = length(unique(species_name)))

# if we want to do more complex manipulations for each group

dfMice %>% 
  group_nest(bin_uri)  # a tibble with lists in the column data!!!!

dfMice %>% 
  group_nest(bin_uri) %>% 
  pull(data, name = bin_uri) # and now we have a list of tibbles, similar to Option 2, 3, and 4

# e.g., remove Mus sp. before calculating the number of unique species

dfMice %>% 
  group_nest(bin_uri) %>% 
  pull(data, name = bin_uri) %>% 
  map(\(x){
    x %>% 
      filter(species_name != "Mus sp.") %>% 
      summarize(unique_sp = length(unique(species_name)))
  }) %>% 
  list_rbind(names_to = "bin_uri") %>% 
  arrange(unique_sp)

# compare to 
fn_uniqueSpecies(dfMice) %>% 
  arrange(num_species)

# ===> BOLD:ADW6309 !

## __ Example w/ Pantheria database -----

# this code is adapted from code created by Sally Adamowicz

#The PanTHERIA dataset was a major collaborative effort by Jones et al. (2009) to compile species-level biological information for mammal species.
#https://ecologicaldata.org/wiki/pantheria
#http://esapubs.org/archive/ecol/E090/184/#data
#I chose the following file for us to use: "PanTHERIA_1-0_WR05_Aug2008.txt -- 5416 records, not including the header row, ASCII text, tab-delimited, no compression scheme was used."

#Here was the code used to acquire the data (Oct 28, 2019)
#df <- read_tsv("http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt")

#Writing to hard disk so that we have a copy of the original data saved. I have also uploaded this file to CourseLink.
#write_tsv(df, "PanTHERIA.tsv")

## __ Data input and cleaning ----
#We can read this file in from hard disk as follows and view a summary of the data.
df_traits <- read_tsv("../data/PanTHERIA.tsv")
summary(df_traits)

#Let's look at the data in the viewer. What do we notice? Lots of -999. That's commonly used to indicate missing data! As the last time we worked with this dataset, let's clean this up.

#Making a copy, so that we have the copy handy in case we need to go back.
df_traits.copy <- df_traits

#Let' replace -999 values with NA
df_traits[df_traits == -999] <- NA

#Let's compare original and edited data side by side, by looking at column 6. We will bind column 6 from the original dataframe with column 6 from the new dataframe. Then, we will look at the first 100 rows.
check <- cbind(df_traits.copy[, 6], df_traits[, 6])
check[1:100, ]

#Let's look at a summary. We can see that there are a lot of NAs. So, thoughtful decisions about the treatment of NAs would be needed for statistical analysis.
summary(df_traits)

#In base R, we can use single or double quotation marks to work with variable names starting with numbers, e.g. df1$`5-1_AdultBodyMass_g`. However, this gets annoying to work with (in my opinion!). As we have seen, we can readily clean up the variable names instead before working with the data. This next part is familiar...

#Example of using base R functions with variable names starting with a number.
hist(df_traits$`5-1_AdultBodyMass_g`)
mean(df_traits$'5-1_AdultBodyMass_g', na.rm = TRUE)

#Let's look at the names. We can see these are structured names.
names(df_traits)

#Let's work with the names separately for now, in their own character vector.
names.original <- as.vector(names(df_traits))
names.original
class(names.original)

#Let's clean up the names, as we have seen before using regex, but now we can do this in just one line by using OR |.
names.edited <- str_replace(names.original, "^MSW05_|^[0-9]+-[0-9]+_", "")

names.edited

#assignment new names of columns to df_traits
names(df_traits) <- names.edited
names(df_traits)

#Looking good! OK, now these names are easier to work with.

#You can see our previous ggplot class to see various types of plots made with this dataset.

#One example plot as a reminder: body mass vs. longevity as a scatterplot, coloured by taxonomic order.
ggplot(data = df_traits) +
  geom_point(mapping = aes(x = log10(AdultBodyMass_g), y = log(MaxLongevity_m), colour = Order)) +
  labs(title = "Body Size vs. Longevity in Mammals", x = "Log10 Adult Body Size (g)", y = "Ln Maximum Longevity (m)")

#Interestingly, we can see that bats have high longevity in relation to their body mass. (And remember we saw an example previously of reducing the dataset to species-rich orders to remove clutter and reduce the total number of colours.) An effect of order upon the body mass/longevity relationship could be tested through multiple linear regression.

massLongevityModel <- lm(log(df_traits$MaxLongevity_m) ~ log10(df_traits$AdultBodyMass_g) +  df_traits$Order)

#Here, we can see that both body mass and order are significant predictors of maximum longevity.
summary(massLongevityModel)

#removing objects not needed downstream
rm(massLongevityModel, names.original, names.edited)

## __ Nest-by and map combo ----

#Let's split the data by order. Each order will be an element in a list. Each element will contain a subset of the original data.
ls_traits_order <- df_traits %>% 
  nest_by(Order) %>% 
  pull(data, name = Order)

#Let's explore
class(ls_traits_order)

#This is how many elements are in the list, hence how many orders are in the full dataset.
length(ls_traits_order)

#using single square brackets returns a smaller list.
ls_traits_order[1]
class(ls_traits_order[1])

#Using double square brackets returns the contents of the element. Each element is a tibble / tidyverse data frame.
ls_traits_order[[1]]
class(ls_traits_order[[1]])

#Now, we can repeat an analysis on every element.

#We can start with summary statistics. For example, we can get the dataframe dimensions for every order (element) in the list. 
map(ls_traits_order, dim) 

#Next, we can fit a linear model for every order, using map and a tiny anonymous function to specify exactly what we want to do.

#Let's investigate evidence for Bergmann's rule. This "rule" states that larger-bodied species and populations are found in colder environments. This is normally studied among closely related species. Therefore, we only want to examine the relationship between latitude and body mass in closely related organisms (i.e. not an overall model for all mammal species together... i.e. we don't want to lump mice and whales, for examples). We want to see which genera display this pattern. Also, Bergmann's rule is not expected to apply in primarily tropical groups, but rather in taxonomic groups living nearer the poles.

#We'll create a new tibble with filtered data. We'll omit rows with missing data for our variables of interest (body mass and mid-range latitude) from the original. We'll also put in a filter only to keep rows (our rows are species) that have an absolute value for latitude greater than 30 (thus eliminating species mainly living in tropical and subtropical environments). We are using absolute values because, for example, -80 and +80 are both cold! Then, we'll group by genera and keep only genera with 10 or more species remaining, with data for both variables and meeting our latitude condition. The ordering of these steps is important to consider!
df_traits_berg <- df_traits %>%
  filter(!is.na(GR_MidRangeLat_dd)) %>%
  filter(abs(GR_MidRangeLat_dd) > 30) %>%
  filter(!is.na(AdultBodyMass_g)) %>%
  group_by(Genus) %>%
  filter(n() >= 10) %>%
  ungroup()

#Some checks that we did what we intended to do!
length(df_traits_berg$Genus)
length(unique(df_traits_berg$Genus))
table(df_traits_berg$Genus)
hist(df_traits_berg$GR_MidRangeLat_dd)
summary(abs(df_traits_berg$GR_MidRangeLat_dd))
sum(is.na(df_traits_berg$AdultBodyMass_g))
sum(is.na(df_traits_berg$GR_MidRangeLat_dd))

#Next, we are splitting by genus, creating a list of data frames. Each element is a tibble df with the data for a genus.
ls_traits_berg_genus <- df_traits_berg %>% 
  nest_by(Genus) %>% 
  pull(data, name = Genus)

ls_traits_berg_genus

class(ls_traits_berg_genus)

length(ls_traits_berg_genus)

#class of one element
class(ls_traits_berg_genus[[1]])
head(ls_traits_berg_genus[[1]])

#Fitting a linear model to test if mid-range latitude is a predictor of body mass among species within every genus remaining in the filtered dataset. I've done a log (base 10) transformation in body mass, because body mass is so highly right skewed, as we've seen before. Note I've used the absolute value of mid-range latitude so that, for example, values near Antarctica are considered similar to Arctic values (both cold, polar).
ls_models_berg <- map(ls_traits_berg_genus, 
                    function(x) lm(log10(x$AdultBodyMass_g) ~ abs(x$GR_MidRangeLat_dd)))

#Above, we are creating a tiny anonymous function for fitting the linear model that we want. We are applying our function across the elements in our list. The elements are data frames. So, we are fitting a separate model for every genus.

class(ls_models_berg)

#Let's see the model summaries. Again, we can use map to apply the function summary() across every element (i.e. a linear model) in our list.

ls_models_berg %>% 
  map(summary)

#By scrolling through, we can see that some genera display a statistically significant Bergmann's rule and others do not. (If we are doing an overall test, we would also need to consider multiple statistical testing.)

#What if we want the model coefficients combined into a nice dataframe that we can work with. The function coef will retrieve the linear model coefficients for us. We apply that function over every model that we created using map. The other steps are used to create a useful tibble.

df_models = ls_models_berg %>% 
  map(coef) %>% 
  map(as_tibble_row) %>%  # necessary for the next step
  list_rbind(names_to = "Genus")

df_models

#plotting the slopes of the linear models
boxplot(df_models[, 3], xlab = "Mammals (each point is an genus)", ylab = "Slope of linear model for latitude vs. log body mass (g)", main = "Test of Bergmann's Rule among non-Tropical Mammal Genera")

#Adding a reference line at zero. (Zero would mean a slope of zero in a model for an individual genus. So, we can see if the trend is towards positive or negative slopes.)
abline(h = 0, col = "Red", lty = 3)

#We see a trend towards positive slopes. Hit "Zoom" to have a look at the figure.

#Conducting a t-test test to see if the mean slope in the linear models across genera is significantly different from zero. (Note I would have used a Wilcoxon (non-parametric) test if there had been outliers.) There is a positive mean slope, with a significant difference from zero (p = 0.009). This leads us to conclude that body size increases in mammals living closer to the poles (high absolute latitudes), as a general pattern among the genera investigated here.
t.test(df_models[, 3], mu = 0)

#Interesting!

#Please note that to expand upon this line of enquiry regarding Bergmann's rule, we may also wish to consider PGLS (phylogenetic generalized least squares), which takes phylogeny into account in estimating model coefficients. Additionally, careful thought should be put into use of minimum, maximum, and/or mid-range of latitude, and research on this topic may also consider populations within broadly distributed species (not only aggregate species-level body size values). So, this is an interesting topic for our biological example for today (but not a definitive treatment of the topic of Bergmann's rule).

# For Quiz 8, upload a screenshot of a ggplot that illustrates ls_models_berg.

# Start from
df_traits_berg %>% 
  ggplot(aes()) # continue adding code here :-)
