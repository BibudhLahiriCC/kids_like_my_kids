select  count(event_date)
from eligibility_determination_periods, eligibility_determination_points
where determination_point_id = eligibility_determination_points.id
and end_date is Null

select * from eligibility_determination_periods 
where focus_person_id = 10000537378

select * from eligibility_determination_points
where focus_person_id = 10000537378

select *
from removal_locations
where person_id = 10000537378
order by started_at

select ch.date as event_date
      from court_hearings ch, court_hearing_outcomes cho, 
      court_hearing_outcome_types chot 
      where ch.person_id =  10000537378  and 
      cho.court_hearing_id = ch.id 
      and cho.outcome_type_id = chot.id 
      and chot.ends_removal_episode = 't'  
      order by event_date;

select count(distinct focus_person_id) from eligibility_determination_periods

select abs(to_date('2011-03-28', 'YYYY-MM-DD') - to_date('2011-03-29','YYYY-MM-DD'))

select periods.focus_person_id, rl_for_start.id removal_location_id, rl_for_start.type removal_location_type, 
date(rl_for_start.started_at) removal_location_start_date, points.id el_point_id, points.event_date, ch.date court_hearing_date, periods.end_date
from eligibility_determination_periods periods, eligibility_determination_points points, removal_locations rl_for_start,
      court_hearings ch, court_hearing_outcomes cho, court_hearing_outcome_types chot 
where periods.determination_point_id = points.id
and points.initiating_event_type = 'Eligibility::RemovalLocation'
and rl_for_start.person_id = periods.focus_person_id
and abs(date(rl_for_start.started_at) - date(points.event_date)) <= 1
and ch.person_id =  periods.focus_person_id  and 
      cho.court_hearing_id = ch.id 
      and cho.outcome_type_id = chot.id 
      and chot.ends_removal_episode = 't'
and abs(ch.date - date(periods.end_date)) <= 1
order by periods.focus_person_id, rl_for_start.started_at, ch.date

select *
from removal_locations 
where person_id = 10000008203
order by started_at
 