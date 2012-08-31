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
                      " (re.end_date - re.start_date) as length_of_stay,",
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
                      " and re.end_date is not null",
                      sep = "");

  res <- dbSendQuery(con, statement);
  kid_metrics <- fetch(res, n = -1);

  kid_metrics$age_category <- apply(kid_metrics, 1, 
        function(row) categorize_age(as.numeric(row["age_in_years"])));

  dbDisconnect(con);
  #cat(paste(compute_distance(kid_metrics[1,], kid_metrics[2,]), "\n", sep = ""));
  #N_values <- c(1000, 5000, 20000);
  #k_values <- c(100, 2000, 5000);
  #for (i in 1:3)
  #{
  #  fn <- find_kNN(kid_metrics[1, ], kid_metrics[2:(N_values[i] + 1), ], N_values[i], k_values[i]);
  #}
  #return(fn);
  histograms_by_dimensions(kid_metrics);
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
         #cat(paste("covariate = ", covariates[i], "\n", sep = ""));
         non_matching_covariates <- non_matching_covariates + 1;
       }
     }
   }
   if (both_have_values > 0)
   {
     #cat(paste("non_matching_covariates = ", non_matching_covariates, ", both_have_values = ",
     #          both_have_values, "\n", sep = ""));
     return(non_matching_covariates/both_have_values);
   }
   return(-1);
}

 find_kNN <- function(my_kid, other_kids, N, k)
 {
   n_other_kids <- nrow(other_kids);
   for (i in 1:n_other_kids)
   {
     other_kids[i, "distance_with_my_kid"] <- compute_distance(my_kid, other_kids[i,]);
   }
   other_kids <- other_kids[order(other_kids[,"distance_with_my_kid"]),];
   k_most_similar_kids <- other_kids[1:k, ];
   cat(paste("LoS for my kid = ", my_kid[, "length_of_stay"], "\n", sep = ""));
   cat(paste("LoS of most similar kids", "\n", sep = ""));
   print(cbind(other_kids[, "distance_with_my_kid"], other_kids[, "length_of_stay"]));
   cat("summary\n");
   descriptive_nums <- histogram_for_similar_kids(k_most_similar_kids, N, k); 
   return(descriptive_nums);

}

  histogram_for_similar_kids <- function(k_most_similar_kids, N, k)
  {
   descriptive_nums <- fivenum(k_most_similar_kids$length_of_stay);
   filename <- paste("length_of_stay_histogram_", N, "_", k, ".png", sep = "");
   png(filename,  width = 920, height = 960, units = "px");

   histogram <- hist(k_most_similar_kids$length_of_stay, 
                        #breaks = edges, 
                        plot = FALSE);
   customHistogram(histogram = histogram, 
         mainTitle = "Distribution of LoS among kids like my kid",
         xLabel = "length in days", yLabel = "Fraction of children",
         descriptive_nums);
   dev.off();
   return(descriptive_nums);
  }

customHistogram <- function(histogram, mainTitle, xLabel,
                            yLabel, fiveNumberSummary, 
                            queryPoint = as.character(Sys.Date()))
{
  #If there are n bars in the histogram, then 
  #histogram$breaks is an array of (n+1) points, including
  #the start-point of first bucket and last point of last bucket.
  #histogram$counts is an array of n numbers.
  nBars <- length(histogram$counts);
  totalFreq <- sum(histogram$counts);
  heights <- histogram$counts/totalFreq;
  widths <- c();
  barLabels <- c();
  xAxisRightEnd <- max(histogram$breaks);
  width <- ceiling(xAxisRightEnd/nBars);
  for (i in 1:nBars)
  {
    widths[i] <- width;
    #barLabels[i] <- as.character(histogram$breaks[i+1]);
    leftPoint <- 0;
    if (i > 1)
    {
      leftPoint <- histogram$breaks[i] + 1; 
    }
    barLabels[i] <- paste(as.character(leftPoint),
                          "-",
                          as.character(histogram$breaks[i+1]));
  }
 
 subTitle <- paste("Q1 = ", round(fiveNumberSummary[2]),
                    ", Median = ", round(fiveNumberSummary[3]),
                    ", Q3 = ", round(fiveNumberSummary[4]),
                    sep = "");
  barplot(height = heights, width = widths, xlim = c(0, xAxisRightEnd),
          beside = TRUE, horiz = FALSE, main = mainTitle, xlab = xLabel,
          ylab = yLabel, cex.lab = 1.5, space = 0, axisnames = TRUE, cex.names = 1.5,
          cex.axis = 1.5, cex.main = 1.5, col.main= "blue",
          names.arg = barLabels, 
          sub = subTitle, cex.sub = 1.5, col.sub = "red"
          );
}

  #Histograms to check what dimensions are important
  histograms_by_dimensions <- function(kid_metrics)
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
      for (i in 1:n_covariates)
      {
          filename <- paste("./histograms_by_dimensions/length_of_stay_by_", covariates[i],".png", sep = "");
          png(filename,  width = 920, height = 960, units = "px");
          distinct_values <- unique(kid_metrics[, covariates[i]]);
          distinct_values <- distinct_values[!is.na(distinct_values)];
          n_distinct_values <- length(distinct_values);
          par(mfrow=c(n_distinct_values, 1));
          for (j in 1:n_distinct_values)
          {
            cat(paste("covariate = ", covariates[i], ", value = ", distinct_values[j], "\n", sep = ""));
            kid_metrics_this_value <- subset(kid_metrics, (kid_metrics[, covariates[i]] == distinct_values[j]));
            cat(paste("nrow = ", nrow(kid_metrics_this_value), ", ncol = ",
                      ncol(kid_metrics_this_value), "\n", sep = ""));
            histogram <- hist(kid_metrics_this_value$length_of_stay, 
                                  plot = FALSE);
            customHistogram(histogram = histogram, 
                             mainTitle = paste("LoS for ", nrow(kid_metrics_this_value), 
                                               " children with ", covariates[i], " = ", distinct_values[j], sep = ""),
                             xLabel = "length in days", yLabel = "Fraction of children",
                             fivenum(kid_metrics_this_value$length_of_stay));
          }
          dev.off();
      }
  }

 

  #For a number K, take a random sample of K (child, removal episode) combinations as the test data.
  #All the remaining data are training data. For each (child, removal episode) combination in the test data,
  #take the k nearest neighbors from the test (child, removal episode) combinations, check the median LoS
  #of the k NNs, and then find the difference of the median LoS with the LoS of the test kid. 


 
