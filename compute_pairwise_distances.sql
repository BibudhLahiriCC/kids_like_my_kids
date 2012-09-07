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

drop type if exists type_frequency_of_category cascade;
create type type_frequency_of_category as
(
  covariate varchar(100),
  value smallint, --will be 1, 0 or null
  frequency bigint
);


drop function if exists store_frequencies_of_categories();
create function store_frequencies_of_categories() RETURNS type_frequency_of_category[] as $$
 declare
   
   --age_category and race to be handled differently

   n_covariates smallint := 0;
   array_index smallint := 0;
   i smallint;
   frequencies_of_categories type_frequency_of_category array[100] = '{}';
   frequency_of_category type_frequency_of_category;
   covariate_value smallint;
   frequency bigint;
   n_frequencies_of_categories integer;
   statement varchar(2000) := null;
   curFrequenciesOfValues refcursor;

   covariates varchar(100) array[100] := '{"child_disability", "count_previous_removal_episodes", 
                   "family_structure_married_couple", "family_structure_single_female", "parent_alcohol_abuse",
                   "parent_drug_abuse"}';
 
 begin
   select array_length(covariates, 1) into n_covariates;

     for i in 1..n_covariates loop

        statement := 'select cast(' || covariates[i] || ' as integer), count(*) ' || 'from klmk_metrics_copy ' ||
                     'group by cast(' || covariates[i] || ' as integer)';

        open curFrequenciesOfValues for execute statement;

        loop
            array_index := array_index + 1;
            fetch curFrequenciesOfValues into covariate_value, frequency;

            IF NOT FOUND THEN
              EXIT; -- exit loop
            END IF;

            frequency_of_category := row(covariates[i], covariate_value, frequency);
            select array_append(frequencies_of_categories, frequency_of_category) 
             into frequencies_of_categories; 
        end loop;
      close curFrequenciesOfValues;
     end loop;

    /*select array_length(frequencies_of_categories, 1) into n_frequencies_of_categories;
    for i in 1..n_frequencies_of_categories loop
       return next frequencies_of_categories[i];
    end loop;*/
    return frequencies_of_categories;
 end;
$$ LANGUAGE plpgsql;


drop function if exists find_frequency_of_category_and_value(type_frequency_of_category[], varchar, 
                                    integer);
create function find_frequency_of_category_and_value(type_frequency_of_category[], varchar, 
                                    integer) returns integer as $$
  declare

    i integer := 0;
    array_len integer := 0; 
    frequencies_of_categories alias for $1;
    v_covariate alias for $2;
    v_value alias for $3;

  begin
     select array_length(frequencies_of_categories, 1) into array_len;
     for i in 1..array_len loop
       if (frequencies_of_categories[i].covariate = v_covariate AND 
           frequencies_of_categories[i].value = v_value) then
         return frequencies_of_categories[i].frequency;
       end if;
     end loop;
  end;
$$ LANGUAGE plpgsql;

drop function if exists compute_pairwise_distances();
create function compute_pairwise_distances() returns void as $$
  declare

   non_matching_covariates integer; --the simple matching coefficient
   both_have_values integer;
   distance numeric(6, 4);
   n_recs_processed integer := 0;
   current_t varchar(100) := '';
   frequencies_of_categories type_frequency_of_category array[100] = '{}';
   weighted_distance numeric(7, 5);
   freq_my_kid_category integer;
   freq_other_kid_category integer;
   sum_frequencies bigint;
   prod_frequencies bigint;

   curKidsAndRemEps cursor for
    select klmk_metrics_copy.child_id, klmk_metrics_copy.episode_number, 
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
    from klmk_metrics_copy, people, removal_episodes_copy re
    where klmk_metrics_copy.child_id = people.id
    and re.child_id = klmk_metrics_copy.child_id 
    and re.episode_number = klmk_metrics_copy.episode_number
    and re.end_date is not null
    order by klmk_metrics_copy.child_id, klmk_metrics_copy.episode_number;
    --limit 100;

  curToCompareDistanceWith cursor(v_child_id bigint, v_episode_number integer) is
    select klmk_metrics_copy.child_id, klmk_metrics_copy.episode_number, 
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
    from klmk_metrics_copy, people, removal_episodes_copy re
    where ((klmk_metrics_copy.child_id > v_child_id) 
           or (klmk_metrics_copy.child_id = v_child_id and klmk_metrics_copy.episode_number > v_episode_number))
    and re.child_id = klmk_metrics_copy.child_id 
    and klmk_metrics_copy.child_id = people.id
    and re.episode_number = klmk_metrics_copy.episode_number
    and re.end_date is not null;
     
  begin

    frequencies_of_categories := store_frequencies_of_categories();

    for kids_and_rem_eps in curKidsAndRemEps loop
      /*Compute the distance with children with a higher child_id,
        or, if the child_id's are same, then the other one should have 
        a higher removal episode number.*/
        /*raise notice 'my kid id = %, episode number = %',
            kids_and_rem_eps.child_id, kids_and_rem_eps.episode_number;*/

        for compare_distance_with in curToCompareDistanceWith
            (kids_and_rem_eps.child_id, kids_and_rem_eps.episode_number) loop

          non_matching_covariates := 0;
          both_have_values := 0;
          weighted_distance := 0;

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
               freq_my_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'child_disability', 
                                cast(kids_and_rem_eps.child_disability as integer));

                 freq_other_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'child_disability', 
                                cast(compare_distance_with.child_disability as integer));

                 sum_frequencies := freq_my_kid_category + freq_other_kid_category;
                 prod_frequencies := freq_my_kid_category*freq_other_kid_category;

                 weighted_distance := weighted_distance + 
                 cast(sum_frequencies as numeric)/cast(prod_frequencies as numeric);

                 /*raise notice 'kids_and_rem_eps.child_disability = %, 
                               compare_distance_with.child_disability = %,
                              freq_my_kid_category = %, freq_other_kid_category = %, 
                               weighted_distance = %', kids_and_rem_eps.child_disability,
                               compare_distance_with.child_disability,
                               freq_my_kid_category, freq_other_kid_category,
                               weighted_distance;*/
            end if;
          end if;

          if (kids_and_rem_eps.count_previous_removal_episodes is not null 
              AND compare_distance_with.count_previous_removal_episodes is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.count_previous_removal_episodes != 
                compare_distance_with.count_previous_removal_episodes) then
               non_matching_covariates := non_matching_covariates + 1;

                freq_my_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'count_previous_removal_episodes', 
                                kids_and_rem_eps.count_previous_removal_episodes);

                 freq_other_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'count_previous_removal_episodes', 
                                compare_distance_with.count_previous_removal_episodes);

                 sum_frequencies := freq_my_kid_category + freq_other_kid_category;
                 prod_frequencies := freq_my_kid_category*freq_other_kid_category;

                 weighted_distance := weighted_distance + 
                 cast(sum_frequencies as numeric)/cast(prod_frequencies as numeric);
            end if;
          end if;

          if (kids_and_rem_eps.family_structure_married_couple is not null 
              AND compare_distance_with.family_structure_married_couple is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.family_structure_married_couple != 
                compare_distance_with.family_structure_married_couple) then
               non_matching_covariates := non_matching_covariates + 1;

                freq_my_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'family_structure_married_couple', 
                                kids_and_rem_eps.family_structure_married_couple);

                 freq_other_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'family_structure_married_couple', 
                                compare_distance_with.family_structure_married_couple);

                 sum_frequencies := freq_my_kid_category + freq_other_kid_category;
                 prod_frequencies := freq_my_kid_category*freq_other_kid_category;

                 weighted_distance := weighted_distance + 
                 cast(sum_frequencies as numeric)/cast(prod_frequencies as numeric);

            end if;
          end if;

          if (kids_and_rem_eps.family_structure_single_female is not null 
              AND compare_distance_with.family_structure_single_female is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.family_structure_single_female != 
                compare_distance_with.family_structure_single_female) then
               non_matching_covariates := non_matching_covariates + 1;

               freq_my_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'family_structure_single_female', 
                                kids_and_rem_eps.family_structure_single_female);

                 freq_other_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'family_structure_single_female', 
                                compare_distance_with.family_structure_single_female);

                 sum_frequencies := freq_my_kid_category + freq_other_kid_category;
                 prod_frequencies := freq_my_kid_category*freq_other_kid_category;

                 weighted_distance := weighted_distance + 
                 cast(sum_frequencies as numeric)/cast(prod_frequencies as numeric);

            end if;
          end if;

          if (kids_and_rem_eps.parent_alcohol_abuse is not null 
              AND compare_distance_with.parent_alcohol_abuse is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.parent_alcohol_abuse != 
                compare_distance_with.parent_alcohol_abuse) then
               non_matching_covariates := non_matching_covariates + 1;

               freq_my_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'parent_alcohol_abuse', 
                                cast(kids_and_rem_eps.parent_alcohol_abuse as numeric));

                 freq_other_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'parent_alcohol_abuse', 
                                cast(compare_distance_with.parent_alcohol_abuse as numeric));

                 sum_frequencies := freq_my_kid_category + freq_other_kid_category;
                 prod_frequencies := freq_my_kid_category*freq_other_kid_category;

                 weighted_distance := weighted_distance + 
                 cast(sum_frequencies as numeric)/cast(prod_frequencies as numeric);

            end if;
          end if;

          if (kids_and_rem_eps.parent_drug_abuse is not null 
              AND compare_distance_with.parent_drug_abuse is not null) then 

            both_have_values := both_have_values + 1;
            if (kids_and_rem_eps.parent_drug_abuse != 
                compare_distance_with.parent_drug_abuse) then
               non_matching_covariates := non_matching_covariates + 1;

               freq_my_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'parent_drug_abuse', 
                                cast(kids_and_rem_eps.parent_drug_abuse as numeric));

                 freq_other_kid_category := 
                 find_frequency_of_category_and_value(frequencies_of_categories, 
                                'parent_drug_abuse', 
                                cast(compare_distance_with.parent_drug_abuse as numeric));

                 sum_frequencies := freq_my_kid_category + freq_other_kid_category;
                 prod_frequencies := freq_my_kid_category*freq_other_kid_category;

                 weighted_distance := weighted_distance + 
                 cast(sum_frequencies as numeric)/cast(prod_frequencies as numeric);

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
 
        insert into pairwise_distances_copy
        (child_id_1, episode_number_1, child_id_2, episode_number_2, distance)
        values (kids_and_rem_eps.child_id, kids_and_rem_eps.episode_number,
                compare_distance_with.child_id, compare_distance_with.episode_number,
                distance);
                
        end loop;
        /*n_recs_processed := n_recs_processed + 1;
        if (n_recs_processed % 100 = 0) then
         select substring(timeofday() from 1 for 19) into current_t;
         raise notice 'n_recs_processed = %, current time = %', n_recs_processed, 
               current_t;
        end if;*/
    end loop;
  end;
$$ LANGUAGE plpgsql;
