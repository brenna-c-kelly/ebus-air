

library(sf) 
library(vip)
library(pdp)
library(tree)
library(mlr3)
library(dismo)
library(dplyr)
library(raster)
library(mlr3learners)
library(mlr3tuning)
library(RColorBrewer)

# raw data
dat <- read.csv("/Users/brenna/Downloads/adj_o3_2023.csv")

# creating time variables
dat$day_time <- ymd_hms(dat$day_time)
dat$date_time <- as_datetime(dat$day_time)
dat$date <- as_date(dat$day_time)

dat$hour_of_day <- as.factor(hour(dat$day_time))
dat$month <- as.factor(month(dat$day_time))

dat$day_of_week <- as.factor(wday(dat$day_time))

head(dat)


#
library(gt)
library(DALEXtra)
library(tidyverse)
library(tidymodels)

set.seed(4595)

# cleaning


aq <- dat |>
  dplyr::select(yhat, yhat_lo, yhat_hi, 
                adj, hour_of_day, month,
                day_of_week)

df <- aq %>%
  filter(row_number() %% 1000 == 1)


aq_split <- initial_split(df)
aq_train <- training(aq_split)
aq_holdout  <- testing(aq_split) 


rf_recipe <- 
  recipe(
    adj ~ yhat + hour_of_day + month + day_of_week, 
    data = aq_train
  ) # %>%
  # step_log(Sale_Price, base = 10) %>%
  # step_other(Neighborhood, Overall_Qual, threshold = 50) %>% 
  # step_novel(Neighborhood, Overall_Qual) %>% 
  # step_dummy(Neighborhood, Overall_Qual) 


rf_mod <- rand_forest() %>%
  set_engine("ranger", importance = "impurity", seed = 63233, quantreg = TRUE) %>%
  set_mode("regression")

set.seed(63233)

rf_wf <- workflows::workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe) %>% 
  fit(aq_train)




## performance
rf_preds_train <- preds_bind(aq_train)
bind_rows(
  yardstick::mape(rf_preds_train, Sale_Price, .pred),
  yardstick::mape(rf_preds_test, Sale_Price, .pred)
) %>% 
  mutate(dataset = c("training", "holdout")) %>% 
  gt::gt() %>% 
  gt::fmt_number(".estimate", decimals = 1)


## coverage

coverage <- function(df, ...){
  df %>%
    mutate(covered = ifelse(adj >= .pred_lower & adj <= .pred_upper, 1, 0)) %>% 
    group_by(...) %>% 
    summarise(n = n(),
              n_covered = sum(
                covered
              ),
              stderror = sd(covered) / sqrt(n),
              coverage_prop = n_covered / n)
}
rf_preds_test %>% 
  coverage() %>% 
  mutate(across(c(coverage_prop, stderror), ~.x * 100)) %>% 
  gt::gt() %>% 
  gt::fmt_number("stderror", decimals = 2) %>% 
  gt::fmt_number("coverage_prop", decimals = 1) 


## explainer
explainer_rf <- explain_tidymodels(
  rf_wf, 
  data = dplyr::select(aq_train, -adj), 
  y = aq_train$adj,
  label = "random forest"
)
#> Preparation of a new explainer is initiated
#>   -> model label       :  random forest 
#>   -> data              :  2930  rows  3  cols 
#>   -> data              :  tibble converted into a data.frame 
#>   -> target variable   :  2930  values 
#>   -> predict function  :  yhat.workflow  will be used ( [33m default [39m )
#>   -> predicted values  :  No value for predict function target column. ( [33m default [39m )
#>   -> model_info        :  package tidymodels , ver. 0.1.3 , task regression ( [33m default [39m ) 
#>   -> predicted values  :  numerical, min =  4.896018 , mean =  5.220595 , max =  5.518857  
#>   -> residual function :  difference between y and yhat ( [33m default [39m )
#>   -> residuals         :  numerical, min =  -0.8083636 , mean =  4.509735e-05 , max =  0.3590898  
#>  [32m A new explainer has been created! [39m

pdp_rf <- model_profile(explainer_rf, N = NULL,
                        variables = c("hour_of_day"))

plot(pdp_rf, geom = "profiles")


pdp_time <- model_profile(
  explainer_rf,
  variables = "hour_of_day",
  N = NULL,
  groups = "month"
)

as_tibble(pdp_time$agr_profiles) %>%
  # mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_groups_`, group = `_groups_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(color = 'Month', x = "Hour of Day", y = "Predicted Adjustment (ppm)")


?model_profile

library(vip)

workflows::workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe) %>% 
  fit(aq_train) |>
  extract_fit_parsnip() %>% 
  vip(num_features = 10)






pe.df <- data.frame(pe.crds, pa = pe.pa, pe.env)


# task
task_pe <- TaskClassif$new(id = "pe", backend = pe.df, 
                           target = "pa")
task_pe$col_roles

## random forest
lrn_rf = lrn("classif.ranger", 
             predict_type = "prob", 
             importance = "permutation")
rr = resample(task_pe, lrn_rf, resamp_hout, store_models = TRUE)
rr$score(measure)


