drop function if exists get_from_initial_placement_quiz(bigint, bigint, bigint);
create function get_from_initial_placement_quiz(bigint, bigint, bigint) returns void as $$
  declare
    v_child_id alias for $1;
    v_episode_number alias for $2;
    v_initial_placement_quiz_id alias for $3;

    v_single_female smallint;
    v_single_male smallint;
    v_married_couple smallint;
    v_unmarried_couple smallint;
    v_parent_alcohol_abuse boolean;
    v_parent_drug_abuse boolean;
    v_child_behavioral_problem boolean;
    v_child_disability boolean;

  begin
    select ipq.parent_alcohol_abuse,
      ipq.parent_drug_abuse,
      ipq.child_behavioral_problem,
      ipq.child_disability,
      case when ipq.family_structure = 'Single Female' then 1 else 0
           end,
      case when ipq.family_structure = 'Single Male' then 1 else 0
           end,
      case when ipq.family_structure = 'Married Couple' then 1 else 0
           end,
      case when ipq.family_structure = 'Unmarried Couple' then 1 else 0
           end
      into v_parent_alcohol_abuse, v_parent_drug_abuse, v_child_behavioral_problem, v_child_disability, 
           v_single_female, v_single_male, v_married_couple, v_unmarried_couple
      from initial_placement_quizzes ipq where ipq.id = v_initial_placement_quiz_id;

      /*raise notice 'v_parent_alcohol_abuse = %, v_parent_drug_abuse = %, 
                    v_single_female = %, v_single_male = %, v_married_couple = %,
                    v_unmarried_couple = %', v_parent_alcohol_abuse, v_parent_drug_abuse, 
                    v_single_female, v_single_male, v_married_couple,
                    v_unmarried_couple;*/
      update klmk_metrics
      set family_structure_single_female = v_single_female,
      family_structure_single_male = v_single_male,
      family_structure_married_couple = v_married_couple,
      family_structure_unmarried_couple = v_unmarried_couple,
      parent_alcohol_abuse = v_parent_alcohol_abuse,
      parent_drug_abuse = v_parent_drug_abuse,
      child_behavioral_problem = v_child_behavioral_problem,
      child_disability = v_child_disability
      where child_id = v_child_id
      and episode_number = v_episode_number;
  end;
$$ LANGUAGE plpgsql;

drop function if exists case_county_for_kids_like_my_kids(bigint, bigint);
CREATE FUNCTION case_county_for_kids_like_my_kids(bigint, bigint) returns void as $$
  declare
    v_child_id alias for $1;
    v_episode_number alias for $2;

    v_case_county bigint;
    v_assessment_county bigint;

  begin
    select cs.county_id, assessments.county_id into v_case_county, v_assessment_county
    from cases cs
     inner join case_plans cl on (cl.case_id = cs.id)
     inner join case_plan_focus_children cpfc on (cpfc.case_plan_id = cl.id) 
     inner join case_linked_assessments cla on (cla.case_id = cs.id) 
     inner join assessments on (cla.assessment_id = assessments.id)
     where cpfc.person_id = v_child_id;

    update klmk_metrics
    set case_county_id = v_case_county,
        assessment_county_id = v_assessment_county
    where child_id = v_child_id
    and episode_number = v_episode_number;
  end;
$$ LANGUAGE plpgsql;

drop function if exists get_domestic_violence(bigint, bigint);
CREATE FUNCTION get_domestic_violence(bigint, bigint) returns void as $$
  declare
    v_child_id alias for $1;
    v_episode_number alias for $2;

    v_domestic_violence boolean;

  begin
    select intakes.domestic_violence into v_domestic_violence
    from removal_episodes, allegations, assessments, intakes
    where removal_episodes.child_id = allegations.victim_id
    and allegations.assessment_id = assessments.id
    and assessments.intake_id = intakes.id
    and removal_episodes.child_id = v_child_id;
    
    update klmk_metrics
    set domestic_violence_reported = v_domestic_violence
    where child_id = v_child_id
    and episode_number = v_episode_number;
  end;
$$ LANGUAGE plpgsql;


drop function if exists from_abuse_quizzes(bigint, bigint);
create function from_abuse_quizzes(bigint, bigint) returns void as $$
  declare
    v_child_id alias for $1;
    v_episode_number alias for $2;

    v_primary_caregiver_has_mental_health_problem boolean;

  begin
     select aq.n11 
     into v_primary_caregiver_has_mental_health_problem
     from abuse_quizzes aq, risk_assessments ra, case_plan_focus_children cpfc,
          case_plans cl, cases cs, case_linked_assessments cla, assessments
     where (cpfc.person_id = v_child_id)
     and (cpfc.case_plan_id = cl.id) 
     and (cl.case_id = cs.id)
     and (cla.case_id = cs.id) 
     and  (cla.assessment_id = assessments.id)
     and (ra.assessment_id = assessments.id)
     and (aq.risk_assessment_id = ra.id);

     update klmk_metrics
     set primary_caregiver_has_mental_health_problem = v_primary_caregiver_has_mental_health_problem
     where child_id = v_child_id
     and episode_number = v_episode_number;

  end;
$$ LANGUAGE plpgsql;

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


drop function if exists code_child_race(bigint,bigint,boolean, boolean, boolean, boolean, boolean, boolean);
CREATE FUNCTION code_child_race(bigint,bigint,boolean, boolean, boolean, boolean, boolean, boolean) RETURNS void as $$

  declare
  v_child_id alias for $1;
  v_episode_number alias for $2;
  v_multi_racial alias for $3;
  v_white alias for $4; 
  v_black alias for $5;
  v_american_indian alias for $6;
  v_pacific_islander alias for $7;
  v_asian alias for $8;

begin
     
     if not (v_american_indian is null and
        v_white is null and
        v_black is null and
        v_pacific_islander is null and
        v_asian is null and
        v_multi_racial is null)
     then
         update klmk_metrics
         set white =
            (case when v_white = 't' then 1
            else 0 end),
             black = 
            (case when v_black = 't' then 1
            else 0 end),
            american_indian = 
            (case when v_american_indian = 't' then 1
            else 0 end),
            pacific_islander =
            (case when v_pacific_islander = 't' then 1
            else 0 end),
            asian =
            (case when v_asian = 't' then 1
            else 0 end)
         where child_id = v_child_id
         and episode_number = v_episode_number;
      end if;
end;
$$ LANGUAGE plpgsql;


drop function if exists metrics_for_kids_like_my_kids();
CREATE FUNCTION metrics_for_kids_like_my_kids() RETURNS void AS $$

  declare
    age_in_years integer := 0;
    v_first_rem_loc_id bigint;
    v_initial_placement_quiz_id bigint;
    n_previous_removal_episodes smallint;
    v_initial_placement_setting varchar(255);
    n_recs integer := 0;
    current_t varchar(100) := '';


    curKidsWithRemEps cursor for
      select re.child_id, re.episode_number, re.start_date, p.multi_racial, p.white, 
             p.black, p.american_indian, p.pacific_islander, p.asian,
      case when p.gender = 'Male' then 1 
           when p.gender = 'Female' then 0
           else null
           end as gender,
      (re.end_date - re.start_date) length_of_stay 
      from removal_episodes re, people p
      where re.child_id = p.id
      order by re.child_id, re.episode_number;
      
    begin

      for kid_with_rem_ep in curKidsWithRemEps loop
     
        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before insert, current time = %', current_t;*/

        insert into klmk_metrics (child_id, episode_number, gender) 
        values (kid_with_rem_ep.child_id, kid_with_rem_ep.episode_number, kid_with_rem_ep.gender);

        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before querying removal_episodes, current time = %', current_t;*/

        select count(*) into n_previous_removal_episodes
        from removal_episodes re_prev 
        where re_prev.child_id = kid_with_rem_ep.child_id
        and re_prev.episode_number < kid_with_rem_ep.episode_number;

        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before calling get_first_rem_loc_within_rem_ep, current time = %', current_t;*/

        v_first_rem_loc_id := get_first_rem_loc_within_rem_ep(kid_with_rem_ep.child_id, kid_with_rem_ep.start_date); 
        
        v_initial_placement_quiz_id := null;

        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before querying removal_locations for initial_placement_quiz_id, current time = %', current_t;*/

        select initial_placement_quiz_id, provider_type 
        into v_initial_placement_quiz_id, v_initial_placement_setting
        from removal_locations
        where id = v_first_rem_loc_id;

        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before calling get_from_initial_placement_quiz, current time = %', current_t;*/

        perform get_from_initial_placement_quiz(kid_with_rem_ep.child_id, 
                  kid_with_rem_ep.episode_number, v_initial_placement_quiz_id);

        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before calling case_county_for_kids_like_my_kids, current time = %', current_t;*/

        perform case_county_for_kids_like_my_kids(kid_with_rem_ep.child_id, 
                  kid_with_rem_ep.episode_number);
        /*perform from_abuse_quizzes(kid_with_rem_ep.child_id, 
                  kid_with_rem_ep.episode_number);*/
        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before calling get_domestic_violence, current time = %', current_t;*/

        perform get_domestic_violence(kid_with_rem_ep.child_id, 
                  kid_with_rem_ep.episode_number);

        /*select substring(timeofday() from 1 for 26) into current_t;
        raise notice 'before updating klmk_metrics, current time = %', current_t;*/

        update klmk_metrics
        set count_previous_removal_episodes = n_previous_removal_episodes,
            initial_placement_setting = v_initial_placement_setting
        where child_id = kid_with_rem_ep.child_id 
        and episode_number = kid_with_rem_ep.episode_number;

        n_recs := n_recs + 1;
        if (n_recs % 100 = 0) then
         select substring(timeofday() from 1 for 19) into current_t;
         raise notice 'n_recs = %, current time = %', n_recs, current_t;
        end if; 
        
      end loop;
    end;
$$ LANGUAGE plpgsql;



