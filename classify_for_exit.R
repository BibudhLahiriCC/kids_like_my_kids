classify_for_exit <- function()
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
                     " and (re.start_date + ", initial_months, "*30)) as n_plcmnts_in_initial_months, ",
                     "case when fn_remove_legacy(permanency_outcome) = 'Child is entering the Collaborative Care Program' then 'Entering_CC'",
                                "when fn_remove_legacy(permanency_outcome) = 'Runaway with Wardship Dismissed' then 'Runaway'",
                                "when fn_remove_legacy(permanency_outcome) = 'Transfer of Placement and Care to Another Indiana State Agency' then 'TPA'",
                                "when fn_remove_legacy(permanency_outcome) = 'Permanent Placement with a Relative' then 'Relative'",
                                "when fn_remove_legacy(permanency_outcome) = 'Death of Child' then 'Death'",
                                "when fn_remove_legacy(permanency_outcome) = 'Reunification' then 'Reunification'",
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

  #cat(statement);
  res <- dbSendQuery(con, statement);
  kids_with_los <- fetch(res, n = -1);
  cat(paste("nrow(kids_with_los) = ", nrow(kids_with_los), "\n", sep = ""));

  kids_with_los$age_category_1 <- as.numeric(kids_with_los$age_category == 2);
  kids_with_los$age_category_2 <- as.numeric(kids_with_los$age_category == 3);
  kids_with_los$age_category_3 <- as.numeric(kids_with_los$age_category == 4);
  kids_with_los$age_category_4 <- as.numeric(kids_with_los$age_category == 5);

  poss_perm_outcomes <- c("tpa", "relative", "reunification", "emancipation",
                                    "guardianship", "entering_cc", "death", "adoption",
                                    "runaway");
  poss_perm_outcomes_as_in_DB <- c("TPA", "Relative", "Reunification", 
                                   "Emancipation", "Guardianship", "Entering_CC",
                                   "Death", "Adoption", "Runaway");

  kids_with_los$tpa <- as.numeric(kids_with_los$processed_permanency_outcome == 'TPA');
  kids_with_los$relative <- as.numeric(kids_with_los$processed_permanency_outcome == 'Relative');
  kids_with_los$reunification <- as.numeric(kids_with_los$processed_permanency_outcome == 'Reunification');
  kids_with_los$emancipation <- as.numeric(kids_with_los$processed_permanency_outcome == 'Emancipation');
  kids_with_los$guardianship <- as.numeric(kids_with_los$processed_permanency_outcome == 'Guardianship');
  kids_with_los$entering_cc <- as.numeric(kids_with_los$processed_permanency_outcome == 'Entering_CC');
  kids_with_los$death <- as.numeric(kids_with_los$processed_permanency_outcome == 'Death');
  kids_with_los$adoption <- as.numeric(kids_with_los$processed_permanency_outcome == 'Adoption');
  kids_with_los$runaway <- as.numeric(kids_with_los$processed_permanency_outcome == 'Runaway');

  n_poss_perm_outcomes <- length(poss_perm_outcomes);
  for (i in 1:n_poss_perm_outcomes)
  {
      cat(paste("i = ", i, ", poss_perm_outcomes[i] = ", poss_perm_outcomes[i], "\n", sep = ""));
      formula_string <- paste("factor(", poss_perm_outcomes[i],
                                        ") ~ n_plcmnts_in_initial_months + ",
                                        "factor(age_category_1) + ",
                                      "factor(age_category_2) + factor(age_category_3) + ",
                                      "factor(age_category_4) + ",
                                      "count_previous_removal_episodes + factor(black) ",
                                      "+ factor(child_disability) + ",
                                      "factor(parent_alcohol_abuse)", sep = "");
      this_formula <- as.formula(formula_string);
            kids.logr <- glm(this_formula, 
                       family = binomial("logit"), data = kids_with_los);
      print(summary(kids.logr));
      probability_column_name <- paste(poss_perm_outcomes[i], "_probs", sep = "");
      kids_with_los[[probability_column_name]] <- predict(kids.logr, type = "response");
  }
  dbDisconnect(con);

  #For each row, identify the probability column that has highest value, 
  #and store its name in a different column
  if (FALSE)
  {
      n_kids_with_los <- nrow(kids_with_los);
      for (i in 1:n_kids_with_los)
      {
         max_prob <- 0;
         for (j in 1:n_poss_perm_outcomes)
         {
           probability_column_name <- paste(poss_perm_outcomes[j], "_probs", sep = "");
           if (kids_with_los[i, probability_column_name] > max_prob)
           {
             max_prob <- kids_with_los[i, probability_column_name];
             kids_with_los[i, "predicted_perm_outcome"] <- poss_perm_outcomes_as_in_DB[j];
           }
         }
         if (i %% 100 == 0)
         {
           cat(paste(i, " records processed", "\n", sep = ""));
         }
      }
  }
  probability_column_names <- paste(poss_perm_outcomes, "_probs", sep = "");
  kids_with_los$max_likely_outcome_col_index <- max.col(kids_with_los[, probability_column_names]);
  kids_with_los$predicted_perm_outcome <- poss_perm_outcomes_as_in_DB[kids_with_los$max_likely_outcome_col_index];
  #print(kids_with_los[1:20, ]);

  #Create the matrix of micro-average precision and recall
  precision_recall_matrix <- mat.or.vec(n_poss_perm_outcomes + 1, n_poss_perm_outcomes + 1);
  rownames(precision_recall_matrix) <- append(poss_perm_outcomes_as_in_DB, "Micro-avg precision");
  colnames(precision_recall_matrix) <- append(poss_perm_outcomes_as_in_DB, "Micro-avg recall");
  for (i in 1:n_poss_perm_outcomes)
  {
    actual_outcome <- poss_perm_outcomes_as_in_DB[i];
    for (j in 1:n_poss_perm_outcomes)
    {
      predicted_outcome <- poss_perm_outcomes_as_in_DB[j];
      precision_recall_matrix[actual_outcome, predicted_outcome] <- 
        sum(((kids_with_los[, "processed_permanency_outcome"] == actual_outcome)
            & (kids_with_los[, "predicted_perm_outcome"] == predicted_outcome)));
    }
  }
  for (j in 1:n_poss_perm_outcomes)
  {
    colsum <- sum(precision_recall_matrix[, j]);
    if (colsum > 0) 
    {
      precision_recall_matrix["Micro-avg precision", j] <- 
        round(precision_recall_matrix[j, j]/colsum, 2);
    }
    else
    {
      precision_recall_matrix["Micro-avg precision", j] <- 0;
    }
  }
  for (i in 1:n_poss_perm_outcomes)
  {
    rowsum <- sum(precision_recall_matrix[i, ]);
    if (rowsum > 0) 
    {
      precision_recall_matrix[i, "Micro-avg recall"] <- 
        round(precision_recall_matrix[i, i]/rowsum, 2);  
    }
    else
    {
      precision_recall_matrix[i, "Micro-avg recall"] <- 0;
    }
  }
  print(precision_recall_matrix);
}


