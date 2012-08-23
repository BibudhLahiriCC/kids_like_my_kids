select re.child_id, re.episode_number, people.date_of_birth, re.start_date, 
date(rl.started_at) removal_location_start_date,
date(rl.ended_at) removal_location_end_date,
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
(select count(*) 
 from medical_conditions mc, medical_condition_names mcn
 where mc.medical_condition_name_id = mcn.id
 and mcn.name like '%Mental%'
 and mc.person_id = re.child_id) number_of_medical_conditions,
aq.n11 as primary_caregiver_has_mental_health_problem
from removal_episodes re, people, removal_locations rl, initial_placement_quizzes ipq,
     case_plan_focus_children cpfc, case_plans cl, cases cs, case_linked_assessments cla, 
     assessments 
left outer join risk_assessments ra on (ra.assessment_id = assessments.id)
left outer join abuse_quizzes aq on (aq.risk_assessment_id = ra.id)
where re.child_id = people.id
and re.child_id = rl.person_id
and re.end_date is null
and date(rl.started_at) >= re.start_date
and rl.initial_placement_quiz_id = ipq.id
and cpfc.person_id = re.child_id
and cpfc.case_plan_id = cl.id 
and cl.case_id = cs.id 
and cs.county_id is not null
--and re.child_id = 10000008897
and cla.case_id = cs.id
and cla.assessment_id = assessments.id
order by number_of_medical_conditions desc, re.child_id, removal_location_start_date



