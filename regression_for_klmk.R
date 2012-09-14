regression_for_klmk <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
  initial_months <- 6;
  statement <- paste("select age_category, american_indian, asian, black, ",
                     "case when child_disability = 't' then 1 ",
                           "when child_disability = 'f' then 0 ",
                           "else null ",
                     "end as child_disability,",
                     "count_previous_removal_episodes, family_structure_married_couple, ",
                     "family_structure_single_female, ",
                     "case when parent_alcohol_abuse = 't' then 1 ",
                           "when parent_alcohol_abuse = 'f' then 0 ",
                           "else null ",
                     "end as parent_alcohol_abuse,",
                     "case when parent_drug_abuse = 't' then 1 ",
                           "when parent_drug_abuse = 'f' then 0 ",
                           "else null ",
                     "end as parent_drug_abuse,", 
                     "white, (re.end_date - re.start_date) as length_of_stay, ",
                     "(select count(*) from removal_locations rl ",
                     " where re.child_id = rl.person_id ",
                     " and rl.type = 'RemovalLocation::Placement' ",
                     " and date(rl.started_at) >= re.start_date ",
                     "and (rl.ended_at is null or date(rl.ended_at) <= re.end_date)) as n_placements, ",
                     "(select count(*) from removal_locations rl ",
                     " where re.child_id = rl.person_id ",
                     " and rl.type = 'RemovalLocation::Placement' ",
                     " and date(rl.started_at) between re.start_date ",
                     " and (re.start_date + ", initial_months, "*30)) as n_plcmnts_in_initial_months ", 
                     "from klmk_metrics km, removal_episodes re ",
                     " where km.child_id = re.child_id",
                     " and km.episode_number = re.episode_number ",
                     " and re.end_date is not null", 
                     " and age_category is not null ",
                     " and american_indian is not null ",
                     " and asian is not null ",
                     " and black is not null ",
                     " and child_disability is not null ",
                     " and family_structure_married_couple is not null ",
                     " and family_structure_single_female is not null ",
                     " and parent_alcohol_abuse is not null ",
                     " and parent_drug_abuse is not null ",
                     "and white is not null ",
                      sep = "");

  #cat(statement);
  res <- dbSendQuery(con, statement);
  kids_with_los <- fetch(res, n = -1);
  kids_with_los$los_in_month <- kids_with_los$length_of_stay/30;
  kids_with_los$placements_per_month <- 
     kids_with_los$n_placements/kids_with_los$los_in_month;

  #Standardize the data and store the centers and scales
  kids_with_los <- as.data.frame(scale(kids_with_los));
  center_for_los <- attr(kids_with_los$length_of_stay, "scaled:center");
  scale_for_los <- attr(kids_with_los$length_of_stay, "scaled:scale");
  cat(paste("center_for_los = ", center_for_los, ", scale_for_los = ",
            scale_for_los, "\n", sep = ""));

  #correlation_matrix <- cor(kids_with_los);
  #print(correlation_matrix);
  cat(paste("base_error_rate = ", compute_base_error_rate(kids_with_los),
             "\n", sep = ""));
  linear_model_with_k_fold_cross_validation(kids_with_los, 10);

  #linearModel <- create_linear_model(kids_with_los);
  #ridge_regression(kids_with_los);
  dbDisconnect(con);
  drops <- c(
             #"length_of_stay", 
             "los_in_month", "n_placements");
  kids_with_los <- kids_with_los[,!(names(kids_with_los) %in% drops)];
  #print(colnames(kids_with_los));
  #stepwise_selection(kids_with_los);
  #subset_selection <- find_best_covariates(kids_with_los);
}

#Compute the mean LoS, and see if it is used as the predicted value for
#all kids and removal episodes, then how much the average prediction error 
#would be
compute_base_error_rate <- function(kids_with_los)
{
  cat(paste("With scaling, min LoS = ", min(kids_with_los$length_of_stay),
            ", max LoS = ", max(kids_with_los$length_of_stay), 
            ", median LoS = ", median(kids_with_los$length_of_stay),
            "\n", sep = ""));
  mean_LoS <- mean(kids_with_los$length_of_stay);
  v <- rep(mean_LoS, nrow(kids_with_los));
  base_error_rate <- mean((kids_with_los$length_of_stay - v)^2);
  return(base_error_rate);
}

create_linear_model <- function(kids_with_los)
{
  if (FALSE)
  {
    #The top 6 covariates that minimized the AIC most
    linearModel <- lm(length_of_stay ~  
                      n_plcmnts_in_initial_months + factor(age_category) + 
                          count_previous_removal_episodes + factor(black) 
                          + factor(child_disability) + 
                          factor(parent_alcohol_abuse),
                          kids_with_los);
  }
  #Full model
  linearModel <- lm(length_of_stay ~ factor(age_category) 
                          + factor(american_indian)
                          + factor(asian)
                          + factor(black)
                          + factor(child_disability)
                          + count_previous_removal_episodes
                          + factor(family_structure_married_couple) 
                          + factor(family_structure_single_female) 
                          + factor(parent_alcohol_abuse)
                          + factor(parent_drug_abuse) 
                          + n_plcmnts_in_initial_months
                          , kids_with_los);


  print(summary(linearModel));
  cat(paste("average prediction error with this model = ", 
               round(mean((linearModel$residuals)^2), 3), "\n", sep = ""));
  return(linearModel);
}


#Find the best variables for each cardinality size, from a given set of cardinality sizes,
#using the Leaps and Bounds algorithm of Furnival and Wilson
find_best_covariates <- function(kids_with_los)
{
  library(subselect);
  correlation_matrix <- cor(kids_with_los);
  #print(eigen(correlation_matrix));
  eleaps(correlation_matrix,nsol=3, criterion="RM");
}

#Stepwise feature selection, using the Akaike Information Criterion.
stepwise_selection <- function(kids_with_los)
{
  step(lm(length_of_stay ~1, kids_with_los), 
       scope=list(lower=~1,
                  upper = ~factor(age_category) 
                          + factor(american_indian)
                          + factor(asian)
                          + factor(black)
                          + factor(child_disability)
                          + count_previous_removal_episodes
                          + factor(family_structure_married_couple)
                          + factor(parent_alcohol_abuse)
                          + n_plcmnts_in_initial_months
                          ));
}

ridge_regression <- function(kids_with_los)
{
  library(MASS);
  ridgeModel <- lm.ridge(length_of_stay ~ factor(age_category) 
                          + factor(american_indian)
                          + factor(asian)
                          + factor(black)
                          + factor(child_disability)
                          + count_previous_removal_episodes
                          + factor(family_structure_married_couple) 
                          + factor(family_structure_single_female) 
                          + factor(parent_alcohol_abuse)
                          + factor(parent_drug_abuse) 
                          + n_plcmnts_in_initial_months
                          + placements_per_month, kids_with_los);
  print(summary(ridgeModel));
}

linear_model_with_k_fold_cross_validation <- function(kids_with_los, k)
{
   kids_with_los$fold_id <- round(runif(nrow(kids_with_los), 1, k));
   #We get one residual for each observation, because each observation is 
   #used in the validation dataset exactly once
   residual_matrix_start_point <- 1;
   residuals <- data.frame(mat.or.vec(nrow(kids_with_los), 2));
   colnames(residuals) <- c("scaled_los", "predicted_los");
   

   for (i in 1:k)
   {
     training_set <- subset(kids_with_los, (fold_id != i));
     validation_set <- subset(kids_with_los, (fold_id == i));
     cat(paste("k = ", k, ", i = ", i, 
               ", size of training_set = ", nrow(training_set), 
               ", size of validation_set = ", nrow(validation_set), "\n", sep = ""));
     #The top 6 covariates that minimized the AIC most
     linearModel <- lm(length_of_stay ~  
                      n_plcmnts_in_initial_months + factor(age_category) + 
                          count_previous_removal_episodes + factor(black) 
                          + factor(child_disability) + 
                          factor(parent_alcohol_abuse),
                          training_set);
     cat(paste("R-squared = ", (summary(linearModel))$r.squared, "\n", sep = ""));
     predictions <- predict.lm(linearModel, validation_set);
     cat(paste("filling in rows ",
               residual_matrix_start_point,
               " to ",
               residual_matrix_start_point + nrow(validation_set)-1,
               "\n", sep = ""));

     l <- 1;
     for (j in residual_matrix_start_point:
          (residual_matrix_start_point + nrow(validation_set)-1))
     {
      residuals[j, "scaled_los"] <- validation_set[l, "length_of_stay"];
      residuals[j, "predicted_los"] <- predictions[l];
      l <- l + 1;
     }
     residual_matrix_start_point <- residual_matrix_start_point + 
                                    nrow(validation_set);
    }
    cat(paste("cross-validated average prediction error with this model = ", 
               round(mean((residuals$scaled_los - residuals$predicted_los)^2), 3), "\n", sep = ""));
}
