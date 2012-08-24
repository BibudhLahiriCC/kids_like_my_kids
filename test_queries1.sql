select people.id people_id,  
     rl.id rl_id,
     ipq.id ipq_id,  
     cpfc.id cpfc_id,  
     cl.id cl_id,
     cs.id cs_id,
     cla.id cla_id,  
     assessments.id assessment_id,  
ra.id ra_id,  
aq.id aq_id,  
eoq.id eoq_id, 
re.child_id, re.start_date, (re.end_date - re.start_date) length_of_stay,
extract(year from age(re.start_date, people.date_of_birth)) as age_in_years,
case when people.multi_racial = 't' then 1 else 0
end as multi_racial,
case when people.american_indian = 't' then 1 else 0
end as american_indian,
case when people.white = 't' then 1 else 0
end as white,
case when people.black = 't' then 1 else 0
end as black,
case when people.pacific_islander = 't' then 1 else 0
end as pacific_islander,
case when people.asian = 't' then 1 else 0
end as asian,
case when ipq.family_structure = 'Single Female' then 1 else 0
     end as single_female,
case when ipq.family_structure = 'Single Male' then 1 else 0
     end as single_male,
case when ipq.family_structure = 'Married Couple' then 1 else 0
     end as married_couple,
case when ipq.family_structure = 'Unmarried Couple' then 1 else 0
     end as unmarried_couple,
ipq.parent_alcohol_abuse,
ipq.parent_drug_abuse,
cpfc.person_id case_plan_focus_child, cs.id case_id, cl.id case_plan_id, cpfc.id case_plan_focus_child_id, 
cs.county_id, cpfc.concurrent_permanency_goal, cpfc.has_had_mental_health_assessment,
case when (select count(*) 
	   from medical_conditions mc, medical_condition_names mcn
	   where mc.medical_condition_name_id = mcn.id
	   and mcn.name like '%Mental%'
	   and mc.person_id = re.child_id
	   and (mc.start_date is null or mc.start_date <= re.start_date)
	   and (mc.end_date is null or (re.end_date is not null and mc.end_date >= re.end_date))) > 0 then TRUE else FALSE
end as child_has_mental_health_condition,
case when (select count(*) 
	   from medical_conditions mc, medical_condition_names mcn
	   where mc.medical_condition_name_id = mcn.id
	   and mcn.name not like '%behavior%'
	   and mcn.name not like '%psychosocial%'
	   and mcn.name not like '%developmental%'
	   and mcn.name not like '%mental%'
	   and mcn.name not like '%Death of birth family member that may affect medical history%'
	   and mcn.name not like '%emotional%'
	   and mcn.name not like '%pregnancy/birth history%'
	   and mcn.name not like '%genetic history%'
	   and mc.person_id = re.child_id
	   and (mc.start_date is null or mc.start_date <= re.start_date)
	   and (mc.end_date is null or (re.end_date is not null and mc.end_date >= re.end_date))) > 0 then TRUE else FALSE
end as child_has_physical_health_condition,
aq.n11 as primary_caregiver_has_mental_health_problem,
aq.n7 as domestic_violence_reported,
(select count(*) from removal_episodes re_prev 
	 where re_prev.child_id = re.child_id
	 and re_prev.episode_number < re.episode_number) as number_of_previous_episodes,
rl.provider_type intial_placement_setting,
case when (select count(*) 
           from removal_episodes re_following 
           where re_following.child_id = re.child_id
           and re_following.episode_number > re.episode_number
           and re_following.start_date - re.end_date <= 365) > 0 then TRUE else FALSE
end as re_entry_within_year_of_exit,
case when (select count(*)
           from allegations al_in_six_months
           where al_in_six_months.victim_id = re.child_id
           and date(al_in_six_months.date_of_alleged_incident) - re.end_date <= 180) > 0 then TRUE else FALSE
end as abuse_neglect_within_six_months,
case when (select count(*)
           from allegations al_in_one_year
           where al_in_one_year.victim_id = re.child_id
           and date(al_in_one_year.date_of_alleged_incident) - re.end_date <= 365) > 0 then TRUE else FALSE
end as abuse_neglect_within_one_year,
eoq.performing_at_grade_level,
case when (select count(*)
          from insurance_providers ip
          where ip.person_id = re.child_id
          and ip.start_date <= re.end_date
          and (ip.end_date is null or ip.end_date >= re.end_date)) > 0 then TRUE else FALSE
end as child_has_insurance_at_exit,
case when (select count(*)
           from medications med, medication_regimens mr
           where mr.medication_id = med.id
           and med.name like '%psycho%'
           and mr.person_id = re.child_id
           and mr.start_date <= re.end_date
          and (mr.end_date is null or mr.end_date >= re.end_date)) > 0 then TRUE else FALSE
end as child_on_psycho_med_at_exit
from removal_episodes re 
     inner join people on (re.child_id = people.id)
     inner join removal_locations rl on (rl.id = get_first_rem_loc_within_rem_ep(re.child_id, re.start_date))
     left outer join initial_placement_quizzes ipq on (rl.initial_placement_quiz_id = ipq.id)
     left outer join case_plan_focus_children cpfc on (cpfc.person_id = re.child_id)
     left outer join case_plans cl on (cpfc.case_plan_id = cl.id) 
     left outer join cases cs on (cl.case_id = cs.id)
     left outer join case_linked_assessments cla on (cla.case_id = cs.id) 
     left outer join assessments on (cla.assessment_id = assessments.id)
left outer join risk_assessments ra on (ra.assessment_id = assessments.id)
left outer join abuse_quizzes aq on (aq.risk_assessment_id = ra.id)
left outer join educational_overview_quizzes eoq on (eoq.case_plan_focus_child_id = cpfc.id)
where re.child_id = 10000008184
order by re.child_id

--select get_first_rem_loc_within_rem_ep(10000008184, to_date('2004-07-23', 'YYYY-MM-DD'))


