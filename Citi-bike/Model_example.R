library(tidyverse)
library(RSQLite)
library(tidypredict)
library(ranger)
source("Plots/ggplot-theme.R")
options(scipen = 999)


# connect to database  ----------------------------------------------------

# establish the connection to the database
conn <- dbConnect(RSQLite::SQLite(), "NYC.db")

# disconnect from the database
# dbDisconnect(conn)


# model building example -----------------------------------------------------------------

# determine which rows to sample from main dataset
samp.size <- 10000
n.pop.rows <- tbl(conn, "citibike.2019") %>% tally() %>% collect() %>% as.numeric()
samp.rows <- sample(n.pop.rows, size = samp.size, replace = FALSE)

# pull in a local dataframe containing those sample rows
#   and the columns to be used for modeling
samp.df <- tbl(conn, "citibike.2019") %>%
  select(Tripduration, Starttime, Birth.year, Gender, Usertype) %>%
  filter(row_number() %in% samp.rows) %>%
  collect()

# look for outliers
FUNS <- c(max, min, mean, median, sd)
summary.stats <- sapply(FUNS, function(FUN){
  TD <- FUN(samp.df$Tripduration)
  BY <- FUN(samp.df$Birth.year)
  return(c(TD, BY))
}) %>% t()
rownames(summary.stats) <- c("max", "min", "mean", "median", "sd")
colnames(summary.stats) <- c("Tripduration", "Birth.year")
summary.stats
rm(FUNS, summary.stats)

# remove outliers
samp.df <- samp.df %>%
  filter(Tripduration < (60 * 60), #limit to trips under an hour
         Birth.year > 1900) #don't want those 100+ year olds in the dataset

# build three models in memory (note: these are poor models, it's just an exmaple)
model.lm <- lm(Tripduration ~ Starttime * Birth.year * Gender * Usertype, data = samp.df)
model.glm <- glm(Tripduration ~ Starttime * Birth.year * Gender * Usertype, data = samp.df, family = poisson)
model.rf <- ranger(Tripduration ~ Starttime + Birth.year + Gender + Usertype, data = samp.df)

# summary(model.lm)
# summary(model.glm)
# # treeInfo(model.rf)

# visual comparison of the models on the same data they were trained on
samp.df %>%
  mutate(LM = predict(model.lm),
         GLM = predict(model.lm),
         RandomForest = model.rf$predictions) %>%
  gather(key = model.type, value = "Predicted.trip.duration",
         -c("Tripduration", "Starttime", "Birth.year", "Gender", "Usertype")) %>%
  ggplot(aes(x = Tripduration, y = Predicted.trip.duration, color = Birth.year)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(xlim = c(0, (60 * 60)),
                  ylim = c(0, (60 * 60))) +
  facet_grid(~ model.type) +
  light.theme
  
# tidypredict_test(model.lm)
# tidypredict_test(model.glm)
# # tidypredict_test(model.rf) # not supported
# 
# tidypredict_sql(con = conn, model.rf)
# tidypredict_fit(model.rf)
# parse_model(model.rf)

# test the models on database
# determine which rows from main table to validate against
samp.size <- 10000
samp.rows <- sample(n.pop.rows, size = samp.size, replace = FALSE)

# visual comparison of the models on new data
tbl(conn, "citibike.2019") %>%
  filter(row_number() %in% samp.rows) %>%
  tidypredict_to_column(model.lm, vars = "LM") %>% # this runs the model on-disk
  tidypredict_to_column(model.glm, vars = "GLM") %>% # this runs the model on-disk
  select(LM, GLM, Tripduration, Starttime, Birth.year, Gender, Usertype) %>%
  filter(Tripduration < (60 * 60), #limit to trips under an hour
         Birth.year > 1900) %>%
  collect() %>%
  gather(key = model.type, value = "Predicted.trip.duration",
         -c("Tripduration", "Starttime", "Birth.year", "Gender", "Usertype")) %>%
  ggplot(aes(x = Tripduration, y = Predicted.trip.duration, color = Birth.year)) +
  geom_point() +
  coord_cartesian(xlim = c(0, (60 * 60)),
                  ylim = c(0, (60 * 60))) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid( ~ model.type) +
  light.theme

