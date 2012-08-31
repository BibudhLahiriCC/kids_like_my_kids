kids_like_my_kids <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");

  statement <- paste("select klmk_metrics.*,",
                      " (select case_plan_focus_children.permanency_goal",
                      " from case_plan_focus_children, case_plans",
                      " where klmk_metrics.child_id = case_plan_focus_children.person_id",
                      " and case_plan_focus_children.case_plan_id = case_plans.id",
                      " order by start_on ",
                      " limit 1) as first_perm_goal,",
                      " case when people.gender = 'Male' then 1",
                      " when people.gender = 'Female' then 0 ",
                      " else null ",
                      " end as gender,",
                      " extract(year from age(re.start_date, people.date_of_birth)) as age_in_years,",
                      " case when people.multi_racial = 't' then 1 else 0 ",
                      " end as multi_racial,",
                      " case when people.american_indian = 't' then 1 else 0 ",
                      " end as american_indian,",
                      " case when people.white = 't' then 1 else 0 ",
                      " end as white,",
                      " case when people.black = 't' then 1 else 0", 
                      " end as black,",
                      " case when people.pacific_islander = 't' then 1 else 0", 
                      " end as pacific_islander,",
                      " case when people.asian = 't' then 1 else 0", 
                      " end as asian",
                      " from klmk_metrics, people, removal_episodes re",
                      " where klmk_metrics.child_id = people.id", 
                      " and re.child_id = klmk_metrics.child_id ",
                      " and re.episode_number = klmk_metrics.episode_number ",
                      " and re.end_date is not null"
                      sep = "");

  res <- dbSendQuery(con, statement);
  kid_metrics <- fetch(res, n = -1);

  kid_metrics$age_category <- apply(kid_metrics, 1, 
        function(row) categorize_age(as.numeric(row["age_in_years"])));

  dbDisconnect(con);
  compute_similarity(kid_metrics);
  cat(paste(compute_distance(kid_metrics[1,], kid_metrics[2,]), "\n", sep = ""));
}

  categorize_age <- function(age_in_years)
  {
    #cat(paste("age_in_years = ", age_in_years, "\n", sep = ""));
    if (is.na(age_in_years))
    {
      return(-1);
    }
    if (age_in_years <= 1)
    {
      return(1);
    }
    else if ((age_in_years > 1) & (age_in_years <= 5))
    {
      return(2);
    }
    else if ((age_in_years > 5) & (age_in_years <= 10))
    {  
      return(3);
    }
    else if ((age_in_years > 10) & (age_in_years <=15))
    {
      return(4);
    }
    else
      return(5);
  }

  



#Compute the number of metrics on which two vectors have identical values, and 
#divide by the total number of metrics. A vector represents the combination of a child
#and a removal episode.
compute_distance <- function(child_1, child_2)
{
   covariates <- c("family_structure_single_female", "family_structure_single_male",
                   "family_structure_married_couple",
                   "family_structure_unmarried_couple",
                   "parent_alcohol_abuse",
                   "parent_drug_abuse",
                   "child_behavioral_problem",
                   "child_disability",
                   "case_county_id",
                   "assessment_county_id",
                   "primary_caregiver_has_mental_health_problem",
                   "domestic_violence_reported", 
                   "count_previous_removal_episodes",
                   "initial_placement_setting",
                   "gender", "age_category",
                   "count_previous_removal_episodes", "multi_racial", "american_indian", 
                   "white", "black", "pacific_islander",
                   "asian");
   n_covariates <- length(covariates);
   non_matching_covariates <- 0;
   both_have_values <- 0;

   for (i in 1:n_covariates)
   {
     #cat(paste(kid_metrics[1, covariates[i]], "\n", sep = ""));
     if ((!is.na(child_1[, covariates[i]])) & (!is.na(child_2[, covariates[i]]))) 
     {
       both_have_values <- both_have_values + 1;
       if  (child_1[, covariates[i]] != child_2[, covariates[i]])
       {
         cat(paste("covariate = ", covariates[i], "\n", sep = ""));
         non_matching_covariates <- non_matching_covariates + 1;
       }
     }
   }
   if (both_have_values > 0)
   {
     cat(paste("non_matching_covariates = ", non_matching_covariates, ", both_have_values = ",
               both_have_values, "\n", sep = ""));
     return(non_matching_covariates/both_have_values);
   }
   return(-1);
}






 
