library(tidyverse)
library(tidymodels)

# Okay let's work through the (cleaned) titanic data from last week:
titanic <- read_csv('https://www.dropbox.com/s/92funarubgk5rzh/titanic_clean.csv?dl=1')


# First let's make sure factors are factors
leo <- titanic %>% 
  mutate(across(c(survived, had_cabin, sex), ~as.factor(.x)))



# Now let's do a train/test split
leo_split <- initial_split(leo, strata = survived)

leo_training <- training(leo_split)
leo_testing <- leo_split %>% testing()

# Plan the model setup, including the engine and mode
leo_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

leo_spec_dt <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")


show_engines("logistic_reg")
show_engines("decision_tree")

# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg')




# Now fit a model, look at output with tidy()
leo_fit <- leo_spec %>% 
  fit(survived ~ pclass + had_cabin + sex + age + sib_sp + parch + fare,
      data = leo_training)

leo_fit <- leo_spec %>% 
  fit(survived ~ .,
      data = leo_training)

leo_fit_dt <- leo_spec_dt %>% 
  fit(survived ~ .,
      data = leo_training)




# Calculate predictions, 
# including class predictions _and_ probabilities

leo_preds <- leo_fit %>% 
  predict(new_data = leo_testing)

leo_preds_prob <- leo_fit %>% 
  predict(new_data = leo_testing, type = "prob")



leo_test_for_evaluation <- leo_testing %>% 
  bind_cols(leo_preds, leo_preds_prob)


  

# Now let's build a confusion matrix and explore a few of the related metrics.
# conf_mat(), sens()


leo_test_for_evaluation %>% 
  sens(truth = survived,
       estimate = .pred_class)


leo_test_for_evaluation %>% 
  conf_mat(truth = survived,
           estimate = .pred_class) %>% 
  summary()




# Let's get fancy:
# roc_curve(), roc_auc(), autoplot()
roc_data <- leo_results %>% 
  roc_curve(truth = survived, .pred_0)

roc_data %>% 
  autoplot()

leo_results %>% 
  roc_auc(truth = survived, .pred_0)

# Finalize the model with last_fit()




# finalized object, extract predictions, metrics 
# with dedicated collect_* functions:





