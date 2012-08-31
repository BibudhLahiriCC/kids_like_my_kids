select count(*) from assessments 

select count(*) from allegations

select person_id, date_of_birth
from intake_people, people
where person_id = people.id
and date_of_birth is not null
limit 10

select date_of_birth
from people
where id = 10001687750

select count(distinct assessments.id)
from assessments, allegations
where allegations.assessment_id = assessments.id

select count(distinct child_id) from removal_episodes

select to_char(re1.start_date, 'YYYY'), count(distinct re1.child_id)
from removal_episodes re1, people
where not exists (select 1 
from removal_episodes re, allegations, assessments
where re.child_id = allegations.victim_id
and allegations.assessment_id = assessments.id
and re.child_id = re1.child_id)
and re1.child_id = people.id
group by to_char(re1.start_date, 'YYYY')
order by count(distinct re1.child_id) desc

select cfc.person_id, count(distinct(cfci.involvement_type_id, cfci.start_date))
from removal_episodes re1, case_focus_child_involvements cfci, case_focus_children cfc, involvement_types it
where not exists (select 1 
from removal_episodes re, allegations, assessments
where re.child_id = allegations.victim_id
and allegations.assessment_id = assessments.id
and re.child_id = re1.child_id)
and cfci.case_focus_child_id = cfc.id
and cfc.person_id = re1.child_id
and cfci.involvement_type_id = it.id
--and cfc.person_id = 10000690957
group by cfc.person_id
having count(distinct(cfci.involvement_type_id, cfci.start_date)) > 1
order by count(distinct(cfci.involvement_type_id, cfci.start_date)) desc

select count(distinct removal_episodes.child_id)
from removal_episodes


select count(distinct removal_episodes.child_id)
from removal_episodes

select cfc.person_id, count(distinct cfci.involvement_type_id)
from case_focus_child_involvements cfci, case_focus_children cfc
where cfci.case_focus_child_id = cfc.id
group by cfc.person_id
having count(distinct cfci.involvement_type_id) > 1


select distinct cfc.person_id, cfci.start_date, it.name
from case_focus_child_involvements cfci, case_focus_children cfc, involvement_types it
where cfci.case_focus_child_id = cfc.id
and cfc.person_id = 10000690957
and cfci.involvement_type_id = it.id
order by cfc.person_id, cfci.start_date, it.name
