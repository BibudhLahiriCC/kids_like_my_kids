regression_for_klmk <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
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
                     "and (rl.ended_at is null or date(rl.ended_at) <= re.end_date)) as n_placements ",
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
  res <- dbSendQuery(con, statement);
  kids_with_los <- fetch(res, n = -1);
  kids_with_los$los_in_month <- kids_with_los$length_of_stay/30;
  kids_with_los$placements_per_month <- 
     kids_with_los$n_placements/kids_with_los$los_in_month;

  if (FALSE)
  {
   linearModel <- lm(length_of_stay ~ factor(age_category) + factor(american_indian) + 
                                     factor(asian) + factor(black) + factor(child_disability) +
                     count_previous_removal_episodes + placements_per_month + factor(family_structure_married_couple) +
                     factor(family_structure_single_female) + factor(parent_alcohol_abuse) +
                     factor(parent_drug_abuse) + factor(white), kids_with_los);
  }
  linearModel <- lm(length_of_stay ~ factor(age_category) + 
                     count_previous_removal_episodes + 
                     factor(parent_alcohol_abuse) +
                     factor(white), kids_with_los);

  print(summary(linearModel));

  #Measure the relative error on each estimate and find median relative error
  linearModel$observed_los <- linearModel$residuals + linearModel$fitted.values;
  linearModel$relative_error <-  
       abs(linearModel$residuals/linearModel$observed_los);
  cat(paste("median relative error = ", 
               round(median(linearModel$relative_error), 3), "\n", sep = ""));
  dbDisconnect(con);
  drops <- c("length_of_stay", "los_in_month");
  kids_with_los <- kids_with_los[,!(names(kids_with_los) %in% drops)];

  #subset_selection <- find_best_covariates(kids_with_los);
  #return(subset_selection);
}


#Find the best variables for each cardinality size, from a given set of cardinality sizes
find_best_covariates <- function(kids_with_los)
{
  library(subselect);

  if (FALSE)
  {
    kids_with_los$age_category <- factor(kids_with_los$age_category);
    kids_with_los$american_indian <- factor(kids_with_los$american_indian);
    kids_with_los$asian <- factor(kids_with_los$asian);
    kids_with_los$black <- factor(kids_with_los$black);
    kids_with_los$child_disability <- factor(kids_with_los$child_disability);
    kids_with_los$family_structure_married_couple <- 
        factor(kids_with_los$family_structure_married_couple);
    kids_with_los$family_structure_single_female <- 
        factor(kids_with_los$family_structure_single_female);
    kids_with_los$parent_alcohol_abuse <- factor(kids_with_los$parent_alcohol_abuse);
    kids_with_los$parent_drug_abuse <- factor(kids_with_los$parent_drug_abuse);
    kids_with_los$white <- factor(kids_with_los$white);
  }
  correlation_matrix <- cor(kids_with_los);
  #print(eigen(correlation_matrix));
  eleaps(correlation_matrix,nsol=3, criterion="RM");
}
