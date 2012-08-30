select to_char(start_date, 'YYYY'), count(*)
from removal_episodes
group by to_char(start_date, 'YYYY')
order by to_char(start_date, 'YYYY')

select re.child_id, re.episode_number, re.start_date, re.end_date, (re.end_date -  re.start_date) length_of_stay
from removal_episodes re
where re.child_id in 
(select re1.child_id 
 from removal_episodes re1
 group by re1.child_id
 having count(*) = 5)
order by re.child_id, re.episode_number