select ch.date as event_date
      from court_hearings ch, court_hearing_outcomes cho, 
      court_hearing_outcome_types chot 
      where ch.person_id = 10001765385  and 
      cho.court_hearing_id = ch.id 
      and cho.outcome_type_id = chot.id 
      and chot.ends_removal_episode = 't'  
      order by event_date;


select creator_id from court_hearings limit 1

set audit.user_id = -2396

insert into court_hearings (date, person_id, creator_id) 
values (to_date('2012-08-25', 'YYYY-MM-DD'), 10001765385, 3968)

select * from court_hearings order by id desc limit 1

insert into court_hearing_outcomes (court_hearing_id, outcome_type_id)
values (653372, 192)

select * from removal_locations order by id desc limit 1

update court_hearing_outcomes 
set outcome_type_id = 191
where id = 169017

select ch.date, cho.outcome_type_id
      from court_hearings ch, court_hearing_outcomes cho, 
      court_hearing_outcome_types chot 
      where ch.person_id = 10001765385  and 
      cho.court_hearing_id = ch.id 
      and cho.outcome_type_id = chot.id 
      and chot.ends_removal_episode = 't'  

select * from court_hearing_outcome_types where ends_removal_episode = 't'

insert into removal_locations (started_at, type, person_id)
values (to_date('2012-08-25 04:15:00', 'YYYY-MM-DD HH:MI:SS'), 
         'RemovalLocation::Placement', 10001765385)
