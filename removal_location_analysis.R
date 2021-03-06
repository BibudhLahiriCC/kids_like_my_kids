#To see how number of removal locations per unit time influences length of stay

removal_location_analysis <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
  statement <- paste("select re.child_id, re.start_date, re.end_date, ",
                     "(re.end_date - re.start_date) as length_of_stay,",
                     "(select count(*) from removal_locations rl ",
                     " where re.child_id = rl.person_id ",
                     " and rl.type = 'RemovalLocation::Placement'",
                     " and date(rl.started_at) >= re.start_date ",
                     " and (rl.ended_at is null or date(rl.ended_at) <= re.end_date)) as n_placements ",
                     " from removal_episodes re ",
                     " where re.end_date is not null", sep = "");
  res <- dbSendQuery(con, statement);
  kids_with_rem_locs <- fetch(res, n = -1);
  kids_with_rem_locs$los_in_month <- kids_with_rem_locs$length_of_stay/30;
  LoS_by_n_placements(kids_with_rem_locs);
  dbDisconnect(con); 
}

plot_per_month <- function(kids_with_rem_locs)
{
  filename <- "./removal_location_analysis/removal_location_analysis.png";
  png(filename,  width = 920, height = 960, units = "px");
  par(mfrow=c(2, 1));

  kids_with_rem_locs$placements_per_month <- 
     kids_with_rem_locs$n_placements/kids_with_rem_locs$los_in_month;
  #print(kids_with_rem_locs[1:10, ]);
  kids_with_rem_locs <- subset(kids_with_rem_locs,
                               (kids_with_rem_locs$placements_per_month <= 1 & 
                                kids_with_rem_locs$los_in_month <= 20));
  plot(kids_with_rem_locs$n_placements, kids_with_rem_locs$los_in_month);
  plot(kids_with_rem_locs$placements_per_month, kids_with_rem_locs$los_in_month);
  dev.off(); 
}


LoS_by_n_placements <- function(kids_with_rem_locs)
{
  max_n_placements <- 8;
  filename <- "./removal_location_analysis/LoS_by_n_placements.png";
  png(filename, width = 920, height = 960, units = "px");
  par(mfrow=c(max_n_placements/2, 2));

  for (number_of_placements in 1:max_n_placements)
  {
    filtered <- subset(kids_with_rem_locs, (kids_with_rem_locs$n_placements == number_of_placements));
    histogram <- hist(filtered$los_in_month, 
                                    plot = FALSE);
    customHistogram(histogram = histogram, 
                     mainTitle = paste("LoS for ", nrow(filtered), 
                                       " children with ", number_of_placements, " locations", sep = ""),
                     xLabel = "LoS in months", yLabel = "Fraction of children",
                     fivenum(filtered$los_in_month));
  }
  dev.off();
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


n_placements_initial_months <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
  initial_month_values <- c(1, 2, 3, 6, 9, 12);
  n_initial_month_values <- length(initial_month_values);

  for (i in 1:n_initial_month_values)
  {
    filename <- paste("./removal_location_analysis/initial_month_analysis_", initial_month_values[i], 
                ".png", sep = "");
    png(filename,  width = 920, height = 960, units = "px");

    statement <- paste("select re.child_id, re.start_date, re.end_date, ",
                       "cast((re.end_date - re.start_date) as numeric)/30 as los_in_months,",
                       "(select count(*) from removal_locations rl ",
                       " where re.child_id = rl.person_id ",
                       " and rl.type = 'RemovalLocation::Placement'",
                       " and date(rl.started_at) between re.start_date ",
                       " and (re.start_date + ", initial_month_values[i], "*30)) as n_plcmnts_in_initial_months ", 
                       " from removal_episodes re ",
                       " where re.end_date is not null", sep = "");
    #cat(statement);
    res <- dbSendQuery(con, statement);
    kids_with_rem_locs <- fetch(res, n = -1);
    kids_with_rem_locs <- subset(kids_with_rem_locs, (kids_with_rem_locs$n_plcmnts_in_initial_months > 0
                                                      & kids_with_rem_locs$los_in_months <= 60));
    plot(kids_with_rem_locs$n_plcmnts_in_initial_months, 
         kids_with_rem_locs$los_in_months);
    dev.off();
  }
  dbDisconnect(con); 
}


n_placements_first_six_months <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="199.91.168.116", 
                   port="5438", dbname="casebook2_production");
  filename <- paste("./removal_location_analysis/six_month_analysis",  
                ".png", sep = "");
  png(filename,  width = 920, height = 960, units = "px");
  par(mfrow=c(7, 1));

  statement <- paste("select re.child_id, re.start_date, re.end_date, ",
                       "(re.end_date - re.start_date) as los,",
                       "(select count(*) from removal_locations rl ",
                       " where re.child_id = rl.person_id ",
                       " and rl.type = 'RemovalLocation::Placement'",
                       " and date(rl.started_at) between re.start_date ",
                       " and (re.start_date + 180)) as n_plcmnts_in_initial_months ", 
                       " from removal_episodes re ",
                       " where re.end_date is not null", sep = "");
    #cat(statement);
  res <- dbSendQuery(con, statement);
  kids_with_rem_locs <- fetch(res, n = -1);

  for (n_moves in 1:7)
  {
    kids_with_n_moves <- subset(kids_with_rem_locs, 
           (kids_with_rem_locs$n_plcmnts_in_initial_months == n_moves));
    histogram <- hist(kids_with_n_moves$los, 
                                    plot = FALSE);
    customHistogram(histogram = histogram, 
                     mainTitle = paste("LoS for ", nrow(kids_with_n_moves), 
                                       " children with ", n_moves, " moves in first 6 months", sep = ""),
                     xLabel = "LoS in days", yLabel = "Fraction of children",
                     fivenum(kids_with_n_moves$los));
  }
  dev.off();
  dbDisconnect(con); 
}


