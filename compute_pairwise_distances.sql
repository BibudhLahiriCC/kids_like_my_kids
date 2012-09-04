drop function if exists categorize_age(integer);
create function categorize_age(integer) returns integer as $$
  declare
    age_in_years alias for $1;
  begin
    if (age_in_years is null) then
      return(null);
    end if;
    if (age_in_years <= 1) then
      return(1);
    end if;
    if ((age_in_years > 1) AND (age_in_years <= 5)) then
      return(2);
    end if;
    if ((age_in_years > 5) AND (age_in_years <= 10)) then
      return(3);
    end if;
    if ((age_in_years > 10) AND (age_in_years <=15)) then
      return(4);
    end if;
    if (age_in_years > 15) then
      return(5);
    end if;
  end;
$$ LANGUAGE plpgsql;



drop function if exists compute_pairwise_distances();
create function compute_pairwise_distances() returns void as $$
  declare

   non_matching_covariates integer;
   both_have_values integer;
   distance numeric(6, 4);
   n_recs_processed integer := 0;
   current_t varchar(100) := '';

   curKidsAndRemEps cursor for
    select klmk_metrics.child_id, klmk_metrics.episode_number, 
           categorize_age(
            cast(extract(year from age(re.start_date, people.date_of_birth)) as integer)) 
                as age_category, 
           case when people.white = 't' then 1 
                when people.white = 'f' then 0
                else null 
           end as white,
           case when people.black = 't' then 1 
                 when people.black = 'f' then 0
                else null 
           end as black,
           child_disability, count_previous_removal_episodes,
           family_structure_married_couple, family_structure_single_female,
           parent_alcohol_abuse, parent_drug_abuse
    from klmk_metrics, people, removal_episodes re
    where klmk_metrics.child_id = people.id
    and re.child_id = klmk_metrics.child_id 
    and re.episode_number = klmk_metrics.episode_number
    and re.end_date is not null
    order by klmk_metrics.child_id, klmk_metrics.episode_number
    limit 100;

  curToCompareDistanceWith cursor(v_child_id bigint, v_episode_number integer) is
    select klmk_metrics.child_id, klmk_metrics.episode_number, 
           categorize_age(
              cast(extract(year from age(re.start_date, people.date_of_birth)) as integer)) 
                 as age_category, 
           case when people.white = 't' then 1 
                when people.white = 'f' then 0
                else null 
           end as white,
           case when people.black = 't' then 1 
                 when people.black = 'f' then 0
                else null 
           end as black,
           child_disability, count_previous_removal_episodes,
           family_structure_married_couple, family_structure_single_female,
           parent_alcohol_abuse, parent_drug_abuse
    from klmk_metrics, people, removal_episodes re
    where ((klmk_metrics.child_id > v_child_id) 
           or (klmk_metrics.child_id = v_child_id and klmk_metrics.episode_number > v_episode_number))
    and re.child_id = klmk_metrics.child_id 
    and klmk_metrics.child_id = people.id
    and re.episode_number = klmk_metrics.episode_number
    and re.end_date is not null;
     
  begin
    for kids_and_rem_eps in curKidsAndRemEps loop
      /*Compute the distance with children with a higher child_id,
        or, if the child_id's are same, then the other one should have 
        a higher removal episode number.*/
        raise notice 'my kid id = %, episode number = %',
            kids_and_rem_eps.child_id, kids_and_rem_eps.episode_number;

        for compare_distance_with in curToCompareDistanceWith
            (kids_and_rem_eps.child_id, kids_and_rem_eps.episode_number) loop

          non_matching_covariates := 0;
          both_have_values := 0;

          if (kids_and_rem_eps.age_category is not null 
              AND compare_distance_with.age_category is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.age_category != compare_distance_with.age_category) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.black is not  null 
              AND compare_distance_with.black is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.black != compare_distance_with.black) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.child_disability is not null 
              AND compare_distance_with.child_disability is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.child_disability != compare_distance_with.child_disability) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.count_previous_removal_episodes is not null 
              AND compare_distance_with.count_previous_removal_episodes is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.count_previous_removal_episodes != 
                compare_distance_with.count_previous_removal_episodes) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.family_structure_married_couple is not null 
              AND compare_distance_with.family_structure_married_couple is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.family_structure_married_couple != 
                compare_distance_with.family_structure_married_couple) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.family_structure_single_female is not null 
              AND compare_distance_with.family_structure_single_female is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.family_structure_single_female != 
                compare_distance_with.family_structure_single_female) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.parent_alcohol_abuse is not null 
              AND compare_distance_with.parent_alcohol_abuse is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.parent_alcohol_abuse != 
                compare_distance_with.parent_alcohol_abuse) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.parent_drug_abuse is not null 
              AND compare_distance_with.parent_drug_abuse is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.parent_drug_abuse != 
                compare_distance_with.parent_drug_abuse) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

          if (kids_and_rem_eps.white is not null 
              AND compare_distance_with.white is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.white != 
                compare_distance_with.white) then
               non_matching_covariates := non_matching_covariates + 1;
            end if;
          end if;

         if (both_have_values > 0) then
           distance := trunc(cast(non_matching_covariates as numeric)/both_have_values, 4);
           /*raise notice 'non_matching_covariates = %, both_have_values = %, 
                         distance = %',
              non_matching_covariates, both_have_values, distance;*/
         else
          distance := -1;
        end if;
 
        insert into pairwise_distances
        (child_id_1, episode_number_1, child_id_2, episode_number_2, distance)
        values (kids_and_rem_eps.child_id, kids_and_rem_eps.episode_number,
                compare_distance_with.child_id, compare_distance_with.episode_number,
                distance);
                
        end loop;
        n_recs_processed := n_recs_processed + 1;
        --if (n_recs_processed % 100 = 0) then
         select substring(timeofday() from 1 for 19) into current_t;
         raise notice 'n_recs_processed = %, current time = %', n_recs_processed, 
               current_t;
        --end if;
    end loop;
  end;
$$ LANGUAGE plpgsql;
