drop function if exists get_first_rem_loc_within_rem_ep(bigint, date);
CREATE FUNCTION get_first_rem_loc_within_rem_ep(bigint, date) RETURNS bigint AS $$
  declare
    child_id alias for $1;
    rem_ep_start_date alias for $2;
    first_removal_location_id bigint;

  begin
     select min(id) into first_removal_location_id 
     from removal_locations 
     where person_id = child_id
     and date(started_at) = rem_ep_start_date;

     return first_removal_location_id;
  end;
$$ LANGUAGE plpgsql;


