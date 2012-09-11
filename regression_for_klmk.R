regression_for_klmk <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
  statement <- paste("select age_category, american_indian, asian, black, child_disability,",
                     "count_previous_removal_episodes, family_structure_married_couple, ",
                     "family_structure_single_female, parent_alcohol_abuse,",
                     "parent_drug_abuse, white, (re.end_date - re.start_date) as length_of_stay ",
                     "from klmk_metrics km, removal_episodes re ",
                     " where km.child_id = re.child_id",
                     " and km.episode_number = re.episode_number ",
                     " and re.end_date is not null", sep = "");
  res <- dbSendQuery(con, statement);
  kids_with_los <- fetch(res, n = -1);
  kids_with_rem_locs$los_in_month <- kids_with_rem_locs$length_of_stay/30;
  #kids_with_rem_locs$placements_per_month <- 
  #   kids_with_rem_locs$n_placements/kids_with_rem_locs$los_in_month;

  linearModel <- lm(length_of_stay ~ factor(age_category) + factor(american_indian) + 
                                     factor(asian) + factor(black) + factor(child_disability) +
                     count_previous_removal_episodes + factor(family_structure_married_couple) +
                     factor(family_structure_single_female) + factor(parent_alcohol_abuse) +
                     factor(parent_drug_abuse) + factor(white), kids_with_los); 
  print(summary(linearModel));
  dbDisconnect(con);
  return(linearModel);
}
