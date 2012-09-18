select case when fn_remove_legacy(permanency_outcome) = 'Child is entering the Collaborative Care Program' then 'Entering_CC'
            when fn_remove_legacy(permanency_outcome) = 'Runaway with Wardship Dismissed' then 'Runaway'
            when fn_remove_legacy(permanency_outcome) = 'Transfer of Placement and Care to Another Indiana State Agency' then 'TPA'
            when fn_remove_legacy(permanency_outcome) = 'Permanent Placement with a Relative' then 'Relative'
            when fn_remove_legacy(permanency_outcome) = 'Death of Child' then 'Death'
            when fn_remove_legacy(permanency_outcome) = 'Reunification' then 'Reunification'
            else permanency_outcome
       end as processed_permanency_outcome,
count(people.id)
from people, removal_episodes, court_hearings, court_hearing_outcomes, court_hearing_outcome_types
where people.id = removal_episodes.child_id
and people.id = court_hearings.person_id
and court_hearings.id = court_hearing_outcomes.court_hearing_id
and court_hearing_outcomes.outcome_type_id = court_hearing_outcome_types.id
and court_hearing_outcome_types.ends_removal_episode = 't'
and removal_episodes.end_date = court_hearings.date
and start_date - date_of_birth > 0
and removal_episodes.end_date is not null
group by processed_permanency_outcome;

select trim(trailing 'x' from 'xTomxx')

select (trim(trailing '(Legacy)' from 'Permanent Placement with a Relative'))

select position('xy' in 'abcxy')

select position('xy' in 'abcxz')