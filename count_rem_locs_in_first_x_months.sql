drop function if exists count_rem_locs_in_first_x_months(bigint, varchar, varchar);
create function get_from_initial_placement_quiz(bigint, varchar, varchar) returns integer as $$
 declare
   v_child_id alias for $1;
   v_removal_episode_start_date alias for $2;
   v_removal_episode_end_date alias for $3;

   curKidsWithRemLocs cursor for
    select date(started_at) start_date, date(ended_at) end_date from removal_locations rl 
    where rl.person_id = v_child_id
    and rl.type = 'RemovalLocation::Placement' 
    and date(rl.started_at) between to_date('2004-07-23', 'YYYY-MM-DD') 
    and to_date('2008-11-25', 'YYYY-MM-DD')
    order by started_at;

 begin
  for kids_with_rem_locs in curKidsWithRemLocs loop
     
  end loop;
 end;
$$ LANGUAGE plpgsql;

