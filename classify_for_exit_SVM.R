poss_perm_outcomes <<- NA;

classify_for_exit_SVM <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
  initial_months <- 6;
  statement <- paste("select re.child_id || '_' || re.episode_number as child_id_episode_number, ",
                     "age_category, american_indian, asian, black, ",
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
                     " and (re.start_date + ", initial_months, "*30)) as n_plcmnts_in_initial_months, ",
                     "case when fn_remove_legacy(permanency_outcome) = 'Child is entering the Collaborative Care Program' then 'entering_cc'",
                                "when fn_remove_legacy(permanency_outcome) = 'Runaway with Wardship Dismissed' then 'runaway'",
                                "when fn_remove_legacy(permanency_outcome) = 'Transfer of Placement and Care to Another Indiana State Agency' then 'tpa'",
                                "when fn_remove_legacy(permanency_outcome) = 'Permanent Placement with a Relative' then 'relative'",
                                "when fn_remove_legacy(permanency_outcome) = 'Death of Child' then 'death'",
                                "when fn_remove_legacy(permanency_outcome) = 'Reunification' then 'reunification'",
                                "when fn_remove_legacy(permanency_outcome) = 'Legally Removed from Parent(s)' then 'legally_removed'",
                                "when fn_remove_legacy(permanency_outcome) = 'Rights Terminated for Parent(s)' then 'tpr'",
                                "when fn_remove_legacy(permanency_outcome) = 'Adoption' then 'adoption'",
                                "when fn_remove_legacy(permanency_outcome) = 'Emancipation' then 'emancipation'",
                                "when fn_remove_legacy(permanency_outcome) = 'Guardianship' then 'guardianship'",
                                "else permanency_outcome",
                     " end as processed_permanency_outcome",
                     " from klmk_metrics km, removal_episodes re, ", 
                     "court_hearings ch, court_hearing_outcomes cho ",
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
                     " and white is not null ",
                     " and re.child_id = ch.person_id",
                     " and ch.id = cho.court_hearing_id",
                     " and re.end_date = ch.date ",
                     " and permanency_outcome is not null",
                      sep = "");

  res <- dbSendQuery(con, statement);
  kids_with_permanency <- fetch(res, n = -1);
  cat(paste("nrow(kids_with_permanency) = ", nrow(kids_with_permanency), "\n", sep = ""));

  kids_with_permanency$age_category_1 <- as.numeric(kids_with_permanency$age_category == 2);
  kids_with_permanency$age_category_2 <- as.numeric(kids_with_permanency$age_category == 3);
  kids_with_permanency$age_category_3 <- as.numeric(kids_with_permanency$age_category == 4);
  kids_with_permanency$age_category_4 <- as.numeric(kids_with_permanency$age_category == 5);
 
  kids_with_permanency <- oversample_undersample(kids_with_permanency);
  best_model <- cross_validate(kids_with_permanency, 5);
  #classify(kids_with_permanency, poss_perm_outcomes);
  #pies_by_dimensions(kids_with_permanency);

  #model <- classify_simple(kids_with_permanency);
  #grid_search_for_gaussian(kids_with_permanency, 5);
  #grid_search_for_linear(kids_with_permanency, 5);
  dbDisconnect(con);
  return(best_model);
}


classify_simple <- function(kids_with_permanency)
{
  library(e1071);
  y <- kids_with_permanency$processed_permanency_outcome;
  columns_to_remove <- c("age_category", "child_id_episode_number", 
                         "processed_permanency_outcome"
                        ,"random_number", "sampled"
                         );

  kids_with_permanency <- 
  kids_with_permanency[, !(colnames(kids_with_permanency) %in% columns_to_remove)];
  model <- svm(kids_with_permanency, y, type = "C-classification");
  print(model);

  # test with train data
  kids_with_permanency$predicted_perm_outcome <- predict(model, kids_with_permanency);
  kids_with_permanency$processed_permanency_outcome <- y;

  poss_perm_outcomes <- unique(kids_with_permanency$processed_permanency_outcome);
  n_poss_perm_outcomes <- length(poss_perm_outcomes);
  compute_precision_recall(kids_with_permanency, poss_perm_outcomes, n_poss_perm_outcomes);
  return(model);
}

#Inputs: Takes a dataset, builds the model
#based on the dataset, returns the model. This is to be called from 
#within cross-validation. 

classify <- function(kids_with_permanency, poss_perm_outcomes, original_permanency_outcomes,
                     validation_set_start_index, 
                     validation_set_end_index,
                     test_set_start_index, 
                     test_set_end_index)
{
  library(e1071);
  n_poss_perm_outcomes <- length(poss_perm_outcomes);
  y <- kids_with_permanency$processed_permanency_outcome;
  columns_to_remove <- c("age_category", "child_id_episode_number", 
                       "processed_permanency_outcome"
                      ,"random_number", "sampled"
                       );

  kids_with_permanency <- 
   kids_with_permanency[, !(colnames(kids_with_permanency) %in% columns_to_remove)];
  model <- svm(kids_with_permanency, y, type = "C-classification",
               #cost = 16, gamma = 0.0078125);
               #cost = 512, gamma = 0.0009765625);
               kernel = "linear", cost = 8);
  
  #Bring back the original permanency outcomes.
  kids_with_permanency$predicted_perm_outcome <- predict(model, kids_with_permanency);
  kids_with_permanency$processed_permanency_outcome <- y;

  rows_to_remove <- c(validation_set_start_index:validation_set_end_index);
  rows_to_remove <- append(rows_to_remove, c(test_set_start_index:test_set_end_index));

  original_permanency_outcomes_training_this_fold <- 
      original_permanency_outcomes[-rows_to_remove];
  kids_with_permanency$processed_permanency_outcome <- 
       original_permanency_outcomes_training_this_fold;

  overall_classification_accuracy <- (sum(kids_with_permanency$processed_permanency_outcome 
                                         == kids_with_permanency$predicted_perm_outcome))/
                                          nrow(kids_with_permanency);
  cat(paste("overall_classification_accuracy on training set = ", 
            round(overall_classification_accuracy, 2), 
            "\n", sep = ""));
  #plot_precision_recall(precision_recall_matrix, poss_perm_outcomes);
  return(model);
}



compute_precision_recall <- function(kids_with_permanency, poss_perm_outcomes, n_poss_perm_outcomes)
{
   #Create the matrix of micro-average precision and recall
  precision_recall_matrix <- mat.or.vec(n_poss_perm_outcomes + 2, n_poss_perm_outcomes + 2);
  rownames(precision_recall_matrix) <- append(poss_perm_outcomes, 
                                        c("Micro-avg-precision", "Total_predicted"));
  colnames(precision_recall_matrix) <- append(poss_perm_outcomes, 
                                        c("Micro-avg-recall", "Total_actual"));
  for (i in 1:n_poss_perm_outcomes)
  {
    actual_outcome <- poss_perm_outcomes[i];
    for (j in 1:n_poss_perm_outcomes)
    {
      predicted_outcome <- poss_perm_outcomes[j];
      precision_recall_matrix[actual_outcome, predicted_outcome] <- 
        sum(((kids_with_permanency[, "processed_permanency_outcome"] == actual_outcome)
            & (kids_with_permanency[, "predicted_perm_outcome"] == predicted_outcome)));
    }
  }
  for (j in 1:n_poss_perm_outcomes)
  {
    colsum <- sum(precision_recall_matrix[, j]);
    if (colsum > 0) 
    {
      precision_recall_matrix["Micro-avg-precision", j] <- 
        round(precision_recall_matrix[j, j]/colsum, 2);
    }
    else
    {
      precision_recall_matrix["Micro-avg-precision", j] <- 0;
    }
    precision_recall_matrix["Total_predicted", j] <- colsum;
  }
  for (i in 1:n_poss_perm_outcomes)
  {
    rowsum <- sum(precision_recall_matrix[i, ]);
    if (rowsum > 0) 
    {
      precision_recall_matrix[i, "Micro-avg-recall"] <- 
        round(precision_recall_matrix[i, i]/rowsum, 2);  
    }
    else
    {
      precision_recall_matrix[i, "Micro-avg-recall"] <- 0;
    }
    precision_recall_matrix[i, "Total_actual"] <- rowsum;
  }
  print(precision_recall_matrix);
  return(precision_recall_matrix);
}

plot_precision_recall <- function(precision_recall_matrix, poss_perm_outcomes)
{
  filename <- paste("./outcome_classification/precision_recall.png", sep = "");
  png(filename,  width = 920, height = 960, units = "px");

  plot(x = precision_recall_matrix["Micro-avg precision",], 
       y = precision_recall_matrix[, "Micro-avg recall"], 
       xlab = "Micro-avg precision", ylab = "Micro-avg recall",
       xlim = c(0, .40), ylim = c(0, .70));
  library(calibrate);
  textxy(precision_recall_matrix["Micro-avg precision",], 
         precision_recall_matrix[, "Micro-avg recall"], 
         poss_perm_outcomes, cx = 1.0);
  dev.off();
}


oversample_undersample <- function(kids_with_permanency)
{
  count_by_perm_outcome <- aggregate(x = kids_with_permanency$child_id_episode_number, 
                        by = list(kids_with_permanency$processed_permanency_outcome), FUN = "length");
  colnames(count_by_perm_outcome) <- c("processed_permanency_outcome", "frequency");
  print(count_by_perm_outcome);

  frequency_threshold <- 100;
  insignificant_categories <- 
    (subset(count_by_perm_outcome, (count_by_perm_outcome$frequency < frequency_threshold)))[,1];

  cat("insignificant_categories\n");
  print(insignificant_categories);

  #Drop the coded columns corresponding to the insignificant categories.
  kids_with_permanency <- kids_with_permanency[, 
                          !(colnames(kids_with_permanency) %in% insignificant_categories)];

  count_by_perm_outcome <- subset(count_by_perm_outcome, (count_by_perm_outcome$frequency >= frequency_threshold));
  
  #expected_frequency_each_group <- round(0.8*min(count_by_perm_outcome$frequency));
  expected_frequency_each_group <- round(min(count_by_perm_outcome$frequency));
  
  cat(paste("expected_frequency_each_group = ", expected_frequency_each_group, "\n", sep = ""));
  significant_outcomes <- unique(count_by_perm_outcome$processed_permanency_outcome);

  #Changing poss_perm_outcomes globally so that the insignificant categories are not 
  #considered in subsequent computations.

  poss_perm_outcomes <<- significant_outcomes;
  count_by_perm_outcome$sampling_probability <- expected_frequency_each_group/(count_by_perm_outcome$frequency);
  
  #Converting the frequency table to a hashtable so that we can look up the sampling probabilities 
  #by categories.

  row.names(count_by_perm_outcome) <- count_by_perm_outcome$processed_permanency_outcome;
  kids_with_permanency$random_number <- runif(nrow(kids_with_permanency), 0, 1);
  kids_with_permanency$sampled <- 
    (kids_with_permanency$random_number <= 
       count_by_perm_outcome[kids_with_permanency$processed_permanency_outcome, 3]);
  kids_with_permanency <- subset(kids_with_permanency, (kids_with_permanency$sampled == TRUE));
  
  count_by_perm_outcome_sampled_data <- aggregate(x = kids_with_permanency$child_id_episode_number, 
                        by = list(kids_with_permanency$processed_permanency_outcome), FUN = "length");

  print(count_by_perm_outcome_sampled_data);
  return(kids_with_permanency);
}


  #Select the model that creates the lowest error on the validation set,
  #by k-fold cross-validation.

  cross_validate <- function(kids_with_permanency, k)
  {
   original_permanency_outcomes <- kids_with_permanency$processed_permanency_outcome;
   fraction_of_training_data <- 0.8;
   
   #Training dataset is 1..training_data_size in the original dataset

   training_data_size <- round(fraction_of_training_data*nrow(kids_with_permanency));

   #c[i] is the end-index of the i-th chunk for k-fold cross-validation

   size_of_each_chunk <- ceiling(training_data_size/k);
   markers_in_training_data <- mat.or.vec(k, 2);
   models <- list();
   accuracies <- list();

   for (i in 1:k)
   {
     markers_in_training_data[i, 1] <- 
       (i-1)*size_of_each_chunk + 1;
     markers_in_training_data[i, 2] <- 
       min(i*size_of_each_chunk, training_data_size);
   }

   training_data <- kids_with_permanency[1:training_data_size,];
   test_data <- kids_with_permanency[(training_data_size+1):nrow(kids_with_permanency),];

   cat(paste("nrow(training_data) = ", nrow(training_data),
             ", nrow(test_data) = ", nrow(test_data), "\n", sep = ""));

   for (i in 1:k)
   {
     #The i-th chunk is the validation set, rest all training set.

     validation_set_this_fold <- training_data[markers_in_training_data[i,1]:
                                               markers_in_training_data[i,2], ];
     training_set_this_fold <- training_data[-(markers_in_training_data[i,1]:
                                               markers_in_training_data[i,2]), ];
     cat(paste("k = ", k, ", i = ", i, 
               ", size of training_set_this_fold = ", nrow(training_set_this_fold), 
               ", size of validation_set_this_fold = ", nrow(validation_set_this_fold), "\n", sep = ""));

     model <- classify(training_set_this_fold, poss_perm_outcomes, original_permanency_outcomes,
                            markers_in_training_data[i,1], markers_in_training_data[i,2],
                            (training_data_size+1), 
                            nrow(kids_with_permanency));

     columns_to_remove <- c("age_category", "child_id_episode_number", 
                       "processed_permanency_outcome"
                      ,"random_number", "sampled"
                       );

     validation_set_this_fold <- 
         validation_set_this_fold[, !(colnames(validation_set_this_fold) %in% columns_to_remove)];
     validation_set_this_fold$predicted_perm_outcome <- predict(model, validation_set_this_fold);

      #Bring back the original permanency outcomes on validation dataset, by picking up
      #the appropriate range.

      validation_set_this_fold$processed_permanency_outcome <- 
        original_permanency_outcomes[markers_in_training_data[i,1]:
                                     markers_in_training_data[i,2]];
      classification_accuracy_this_fold <- (sum(validation_set_this_fold$processed_permanency_outcome 
                                         == validation_set_this_fold$predicted_perm_outcome))/
                                          nrow(validation_set_this_fold);
      models[[i]] <- model;
      accuracies[i] <- classification_accuracy_this_fold;

      n_poss_perm_outcomes <- length(poss_perm_outcomes);
      cat(paste("Over validation set, classification_accuracy_this_fold = ", 
                round(classification_accuracy_this_fold, 2), 
            "\n", sep = ""));
      } #end for (i in 1:k)

      #Pick the model that gave the best result on validation set, and apply that on the test set
      best_model_index <- which.max(accuracies);
      cat(paste("best_model_index = ", best_model_index, "\n", sep = ""));
      best_model <- models[[best_model_index]];
      test_data <- 
         test_data[, !(colnames(test_data) %in% columns_to_remove)];
      test_data$predicted_perm_outcome <- predict(best_model, test_data);
      test_data$processed_permanency_outcome <- original_permanency_outcomes[(training_data_size + 1):nrow(kids_with_permanency)];
      classification_accuracy_test_data <- (sum(test_data$processed_permanency_outcome 
                                                == test_data$predicted_perm_outcome))/
                                            nrow(test_data);
      compute_precision_recall(test_data, poss_perm_outcomes, n_poss_perm_outcomes);
      cat(paste("Over test set, classification_accuracy = ", 
                round(classification_accuracy_test_data, 2), 
            "\n", sep = ""));
      return(best_model);
  }

#We store the actual and predicted classes for each data point in a separate matrix.
#The results get added there during cross-validaiton, based on the validation subset 
#in each fold.

grid_search_for_gaussian <- function(kids_with_permanency, k)
{
  library(e1071);
  fraction_of_training_data <- 0.8;
  #Training dataset is 1..training_data_size in the original dataset
  training_data_size <- round(fraction_of_training_data*nrow(kids_with_permanency));
  training_data <- kids_with_permanency[1:training_data_size,];
  test_data <- kids_with_permanency[(training_data_size+1):nrow(kids_with_permanency),];
  cat(paste("nrow(training_data) = ", nrow(training_data),
             ", nrow(test_data) = ", nrow(test_data), "\n", sep = ""));
  
  #C is the regularization parameter in the optimization problem.
  exponents <- seq(-5, 15, by = 1);
  C_values <- 2^exponents;
  exponents <- seq(-15, 3, by = 1);

  #gamma is a parameter in the RBF kernel.
  gamma_values <- 2^exponents;
  n_C_values <- length(C_values);
  n_gamma_values <- length(gamma_values);

  best_cross_validation_accuracy <- 0;
  best_C <- 0;
  best_gamma <- 0;

  columns_to_remove <- c("age_category", "child_id_episode_number", 
                                   "processed_permanency_outcome"
                                  ,"random_number", "sampled"
                                   );
  accuracy_summary <-  data.frame(mat.or.vec(k, 2));
  colnames(accuracy_summary) <- c("validation_set_this_fold_size", "correct_classifications");

  #We will keep the combination of C and gamma that gives the best cross-validation accuracy
  #on the training dataset.
  for (i in 1:n_C_values)
  {
    for (j in 1:n_gamma_values)
    {
      training_data$fold_id <- round(runif(training_data_size, 1, k));
       for (m in 1:k)
       {
           training_set_this_fold <- subset(training_data, (fold_id != m));
           validation_set_this_fold <- subset(training_data, (fold_id == m));
           y <- training_set_this_fold$processed_permanency_outcome;
           training_set_this_fold <- 
              training_set_this_fold[, !(colnames(training_set_this_fold) %in% columns_to_remove)];
           model <- svm(training_set_this_fold, y, type = "C-classification",
                        cost = C_values[i], gamma = gamma_values[j]);

           #Apply the model based on training_set_this_fold on validation_set_this_fold.
           #Before doing that, store the actual classes of validation_set_this_fold.
           
           validation_set_actual_outcomes <- validation_set_this_fold$processed_permanency_outcome;
           validation_set_this_fold <- 
              validation_set_this_fold[, !(colnames(validation_set_this_fold) %in% columns_to_remove)];
           validation_set_this_fold$predicted_perm_outcome <- predict(model, validation_set_this_fold);
           validation_set_this_fold$processed_permanency_outcome <- validation_set_actual_outcomes;

           accuracy_summary[m, "validation_set_this_fold_size"] <- nrow(validation_set_this_fold);
           accuracy_summary[m, "correct_classifications"] <- (sum(validation_set_this_fold$processed_permanency_outcome 
                                                == validation_set_this_fold$predicted_perm_outcome))
      } #end for (m in 1:k)
        cross_validated_classification_accuracy <- sum(accuracy_summary$correct_classifications)/ 
                                              sum(accuracy_summary$validation_set_this_fold_size);
        cat(paste("average cross-validated accuracy with ", 
                "C = ", C_values[i], ", gamma = ", gamma_values[j], " is ",  
                round(cross_validated_classification_accuracy, 2), "\n", sep = ""));
        if (cross_validated_classification_accuracy > best_cross_validation_accuracy)
        {
           best_cross_validation_accuracy <- cross_validated_classification_accuracy;
           best_C <- C_values[i];
           best_gamma <- gamma_values[j];
        }
    }  #end for (j in 1:n_gamma_values)
  } #end for (i in 1:n_C_values)
  cat(paste("best_C = ", best_C, ", best_gamma = ", best_gamma, "\n", sep = ""));
}




#We store the actual and predicted classes for each data point in a separate matrix.
#The results get added there during cross-validaiton, based on the validation subset 
#in each fold.

grid_search_for_linear <- function(kids_with_permanency, k)
{
  library(e1071);
  fraction_of_training_data <- 0.8;
  #Training dataset is 1..training_data_size in the original dataset
  training_data_size <- round(fraction_of_training_data*nrow(kids_with_permanency));
  training_data <- kids_with_permanency[1:training_data_size,];
  test_data <- kids_with_permanency[(training_data_size+1):nrow(kids_with_permanency),];
  cat(paste("nrow(training_data) = ", nrow(training_data),
             ", nrow(test_data) = ", nrow(test_data), "\n", sep = ""));
  
  #C is the regularization parameter in the optimization problem.
  exponents <- seq(-5, 15, by = 1);
  C_values <- 2^exponents;
  n_C_values <- length(C_values);

  best_cross_validation_accuracy <- 0;
  best_C <- 0;

  columns_to_remove <- c("age_category", "child_id_episode_number", 
                                   "processed_permanency_outcome"
                                  ,"random_number", "sampled"
                                   );
  accuracy_summary <-  data.frame(mat.or.vec(k, 2));
  colnames(accuracy_summary) <- c("validation_set_this_fold_size", "correct_classifications");

  #We will keep the combination of C and gamma that gives the best cross-validation accuracy
  #on the training dataset.
  for (i in 1:n_C_values)
  {
      training_data$fold_id <- round(runif(training_data_size, 1, k));
       for (m in 1:k)
       {
           training_set_this_fold <- subset(training_data, (fold_id != m));
           validation_set_this_fold <- subset(training_data, (fold_id == m));
           y <- training_set_this_fold$processed_permanency_outcome;
           training_set_this_fold <- 
              training_set_this_fold[, !(colnames(training_set_this_fold) %in% columns_to_remove)];
           model <- svm(training_set_this_fold, y, type = "C-classification",
                        kernel = "linear", cost = C_values[i]);

           #Apply the model based on training_set_this_fold on validation_set_this_fold.
           #Before doing that, store the actual classes of validation_set_this_fold.
           
           validation_set_actual_outcomes <- validation_set_this_fold$processed_permanency_outcome;
           validation_set_this_fold <- 
              validation_set_this_fold[, !(colnames(validation_set_this_fold) %in% columns_to_remove)];
           validation_set_this_fold$predicted_perm_outcome <- predict(model, validation_set_this_fold);
           validation_set_this_fold$processed_permanency_outcome <- validation_set_actual_outcomes;

           accuracy_summary[m, "validation_set_this_fold_size"] <- nrow(validation_set_this_fold);
           accuracy_summary[m, "correct_classifications"] <- (sum(validation_set_this_fold$processed_permanency_outcome 
                                                == validation_set_this_fold$predicted_perm_outcome))
      } #end for (m in 1:k)
        cross_validated_classification_accuracy <- sum(accuracy_summary$correct_classifications)/ 
                                              sum(accuracy_summary$validation_set_this_fold_size);
        cat(paste("average cross-validated accuracy with ", 
                "C = ", C_values[i], " is ",  
                round(cross_validated_classification_accuracy, 2), "\n", sep = ""));
        if (cross_validated_classification_accuracy > best_cross_validation_accuracy)
        {
           best_cross_validation_accuracy <- cross_validated_classification_accuracy;
           best_C <- C_values[i];
        }
  } #end for (i in 1:n_C_values)
  cat(paste("best_C = ", best_C, "\n", sep = ""));
}

