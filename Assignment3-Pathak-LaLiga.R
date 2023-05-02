library(h2o)
library(dplyr)
library(DALEXtra)
library(ggplot2)

# Load the data
laliga <- read.csv("data/laliga.csv")

# Start an H2O cluster
h2o.init()

laliga <- slice(laliga, 1:1500)



laliga <- laliga[, !(names(laliga) %in% c("player.url", "name", "slug", "nickname", "firstname", "lastname", "place_of_birth", "gender", "international", "twitter", "instagram", "team.shortname", "team.foundation", "team.shield", "shirt_number", "photo", "stadium", "stadium.image"))]


selected_cols <- c( 
                   "forward_passes", 
                   "foul_won_penalty", "goal_assists", "goals", 
                   "ground_duels_won", "hit_woodwork", 
                   "open_play_passes", 
                   "recoveries", 
                   "successful_dribbles", 
                   "total_passes", "total_shots", 
                   "total_touches_in_opposition_box")

df_selected <- select(laliga, selected_cols)

df_selected[is.na(df_selected)] <- 0


laliga_h2o <- as.h2o(df_selected)


h2o.describe(laliga_h2o)



# Split the data into training and validation sets
#split <- h2o.splitFrame(laliga_h2o, ratios = 0.7)
splits <- h2o.splitFrame(data = laliga_h2o, ratios = c(0.6, 0.2), seed = 1234, destination_frames = c("train", "valid", "test"))

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Define the response variable and predictor variables
response <- "goals"
predictors <- setdiff(names(train), response)

# Train the h2o model
model <- h2o.glm(x = predictors,
                 y = response,
                 model_id = "Assignment3-Pathak-LaLiga.h2o",
                 training_frame = train,
                 validation_frame = valid,
                 family = "gaussian",
                 alpha = 0.5,
                 lambda = 0,
                 seed = 1234)

# Evaluate the performance of the model
h2o.performance(model, valid = TRUE)

predictions <- h2o.predict(model, test)


performance <- h2o.performance(model, newdata = test)

accuracy <- h2o.r2(performance)

performance
accuracy

summary(model)

h2o.saveModel(object = model, # the model you want to save
              path = getwd(), # the folder to save
              force = TRUE) # whether to overwrite an existing file


h2o_exp <- explain_h2o(
  model,
  data = train,
  y = train$goals,
  label = "H2O",
  type = "regression"
)

test_df <- as.data.frame(test)
train_df <- as.data.frame(train)

play= test_df[5, ]

h2o_exp_bd <- predict_parts(
  explainer = h2o_exp, new_observation = play,
  type = "break_down")
plot(h2o_exp_bd) + ggtitle("Break-down plot for the new player")



h2o_exp_cp <- predict_profile(
  explainer = h2o_exp, new_observation = play)
plot(h2o_exp_cp, variables = c("total_shots", "hit_woodwork")) +
  ggtitle("Ceteris-paribus profile")

h2o_exp_vip <- model_parts(
  explainer = h2o_exp,
  B = 50, type = "difference")
plot(h2o_exp_vip) +
  ggtitle("Mean variable-importance over 50 permutations")

h2o.shutdown(prompt = F)