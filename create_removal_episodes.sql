 /*This method, given the relationship of a caregiver with a child who is in a 
   removal_location, returns whether the caregiver is a relative.*/
  drop function if exists is_caregiver_a_relative(varchar);
  CREATE FUNCTION is_caregiver_a_relative(varchar) RETURNS boolean AS $$
   declare
     v_relationship_to_child alias for $1;
     array_len integer := 0;

     relative_types varchar(100) array[100] := 
       '{"first cousin", "first cousin once removed", "grandparent", "great-grandparent", 
         "great-great-grandparent", "great-great-great-grandparent", "great uncle or aunt",
         "half-sibling", "parent", "parent-in-law", "Relative", "sibling", "sibling-in-law",
         "spouse", "step-parent", "step-sibling", "uncle or aunt"}';

   begin
     select array_length(relative_types, 1) into array_len;
     for i in 1..array_len loop
       if (relative_types[i] = v_relationship_to_child) then
         return TRUE;
       end if;
     end loop; 
     return FALSE;
   end;
  $$ LANGUAGE plpgsql;

  drop function if exists number_of_contacts_in_rem_ep(bigint, date, date);
  CREATE FUNCTION number_of_contacts_in_rem_ep(bigint, date, date) RETURNS integer AS $$

  declare
    child_id alias for $1; 
    rem_ep_start_date alias for $2;
    rem_ep_end_date alias for $3;
    v_n_contacts integer := 0;
  
  begin

    select count(distinct c.id) into v_n_contacts
    from contact_people cp, contacts c, contact_people cp1, relationships, relationship_types
    where cp.person_id = child_id
    and (c.id = cp.contact_id) 
    and (cp1.contact_id = cp.contact_id) 
    and cp1.person_id <> cp.person_id 
    and c.successful = 't' 
    and cp.present = 't'              
    and cp1.present = 't'
    and relationships.weak_side_person_id = cp.person_id
    and relationships.strong_side_person_id = cp1.person_id
    and relationships.type_id = relationship_types.id
    and relationship_types.strong_name = 'case worker'
    and relationship_types.weak_name = 'client'
    and date(c.occurred_at) between rem_ep_start_date and rem_ep_end_date;

    return v_n_contacts;
    
  end;
  $$ LANGUAGE plpgsql;

  drop function if exists number_of_visits_in_rem_ep(bigint, date, date);
  CREATE FUNCTION number_of_visits_in_rem_ep(bigint, date, date) RETURNS integer AS $$

  declare
    child_id alias for $1; 
    rem_ep_start_date alias for $2;
    rem_ep_end_date alias for $3;
    v_n_visits integer := 0;
  
  begin

    --raise notice 'child_id = %', child_id;

    select count(distinct c.id) into v_n_visits
    from contact_people cp, contacts c, contact_people cp1, relationships, relationship_types
    where cp.person_id = child_id
    and (c.id = cp.contact_id) 
    and (cp1.contact_id = cp.contact_id) 
    and cp1.person_id <> cp.person_id 
    and c.successful = 't' 
    and cp.present = 't'              
    and cp1.present = 't'
    and relationships.weak_side_person_id = cp.person_id
    and relationships.strong_side_person_id = cp1.person_id
    and relationships.type_id = relationship_types.id and
    ((relationship_types.strong_name = 'parent' and relationship_types.weak_name = 'child')
      or (relationship_types.strong_name = 'first cousin' and relationship_types.weak_name = 'first cousin')
      or (relationship_types.strong_name = 'first cousin once removed' and relationship_types.weak_name = 'first cousin once removed')
      or (relationship_types.strong_name = 'grandparent' and relationship_types.weak_name = 'grandchild')
      or (relationship_types.strong_name = 'great-grandparent' and relationship_types.weak_name = 'great-grandchild')
      or (relationship_types.strong_name = 'great-great-grandparent' and relationship_types.weak_name = 'great-great-grandchild')
      or (relationship_types.strong_name = 'great-great-great-grandparent' and relationship_types.weak_name = 'great-great-great-grandchild')
      or (relationship_types.strong_name = 'great uncle or aunt' and relationship_types.weak_name = 'great nephew or niece')
      or (relationship_types.strong_name = 'half-sibling' and relationship_types.weak_name = 'half-sibling')
      or (relationship_types.strong_name = 'parent-in-law' and relationship_types.weak_name = 'child-in-law')
      or (relationship_types.strong_name = 'sibling' and relationship_types.weak_name = 'sibling')
      or (relationship_types.strong_name = 'sibling-in-law' and relationship_types.weak_name = 'sibling-in-law')
      or (relationship_types.strong_name = 'specified relative' and relationship_types.weak_name = 'specified relative')
      or (relationship_types.strong_name = 'spouse' and relationship_types.weak_name = 'spouse')
      or (relationship_types.strong_name = 'step-parent' and relationship_types.weak_name = 'step-child')
      or (relationship_types.strong_name = 'step-sibling' and relationship_types.weak_name = 'step-sibling')
      or (relationship_types.strong_name = 'uncle or aunt' and relationship_types.weak_name = 'nephew or niece'))
    and date(c.occurred_at) between rem_ep_start_date and rem_ep_end_date;

    --raise notice 'v_n_visits = %', v_n_visits;
    return v_n_visits;
    
  end;
  $$ LANGUAGE plpgsql;

 drop type if exists type_event_in_rem_ep cascade;
  create type type_event_in_rem_ep as 
      (
        child_id bigint,
        event_type varchar(30),
        event_date date,
        placed_with_relative_this_rem_loc boolean,         
        county_of_this_rem_loc varchar(255)
        ----placed_with_relative_this_rem_loc and county_of_initial_removal have value only if 
        --event_type is 'location_start'
      );
  
  drop table if exists removal_episodes;
  create table removal_episodes
  (
    child_id bigint,
    episode_number integer, 
    start_date date,
    end_date date,
    placed_with_relative_this_rem_ep boolean,
    county_of_initial_removal varchar(255),
    n_contacts_in_rem_ep integer,
    n_visits_in_rem_ep integer
  );

  drop table if exists table_event_in_rem_ep;
  create 
       --temporary 
       table table_event_in_rem_ep 
       (
         event_row type_event_in_rem_ep
   );

  /*This method gets the removal episode start and end dates for a given
    child. We use "locations" to start removal episodes, but court outcomes
    to end removal episodes.*/
  drop function if exists get_removal_episodes(bigint);
  CREATE FUNCTION get_removal_episodes(bigint) RETURNS void AS $$

  declare
    v_person_id alias for $1;
    event_in_rem_ep type_event_in_rem_ep;
    raw_merged_df type_event_in_rem_ep array[100];
    merged_df type_event_in_rem_ep array[100];
    n_events integer := 0;
    array_len integer := 0;
    removal_episode_number integer;
    new_removal_episode_number integer;
    old_removal_episode_number integer;
    n_rem_eps_after_delete integer;
    n_merged_rows integer;
    i integer;
    episode_start_date date;
    v_placed_with_relative_this_rem_ep boolean;
    
   --Get the start dates (and times) of the locations
   curRemovalLocationStartDates cursor for 
      select date(started_at) as event_date, 
             is_caregiver_a_relative(relationship_to_child) as placed_with_relative_this_rem_loc,
             counties.name county_for_this_rem_loc
      from removal_locations 
      left outer join resources on removal_locations.provider_id = resources.id
      left outer join counties on resources.county_id = counties.id
      where person_id = v_person_id 
      order by event_date;

   curCourtHearingDates cursor for 
      select ch.date as event_date
      from court_hearings ch, court_hearing_outcomes cho, 
      court_hearing_outcome_types chot 
      where ch.person_id =  v_person_id  and 
      cho.court_hearing_id = ch.id 
      and cho.outcome_type_id = chot.id 
      and chot.ends_removal_episode = 't'  
      order by event_date;

   curSortEvents cursor for
      select (event_row).child_id, (event_row).event_type, (event_row).event_date,
             (event_row).placed_with_relative_this_rem_loc,
             (event_row).county_of_this_rem_loc
      from table_event_in_rem_ep 
      order by (event_row).event_date asc, (event_row).event_type desc; --if there is a 
      --removal location start date and a court hearing date that coincide, make the
      --removal location start date come first.

   curRemovalEpisodesAfterDelete cursor for
      select child_id, episode_number
      from removal_episodes
      order by start_date;

   begin

       for court_hearing_dates in curCourtHearingDates loop
        event_in_rem_ep := row(v_person_id, 'court_hearing', court_hearing_dates.event_date, 
                               null);
        select array_append(raw_merged_df, event_in_rem_ep) into raw_merged_df;
        n_events := n_events + 1;
      end loop;

      for location_start_dates in curRemovalLocationStartDates loop
        event_in_rem_ep := row(v_person_id, 'location_start', location_start_dates.event_date,
                               location_start_dates.placed_with_relative_this_rem_loc,
                               location_start_dates.county_for_this_rem_loc);
        select array_append(raw_merged_df, event_in_rem_ep) into raw_merged_df;
        n_events := n_events + 1;
      end loop;

      select array_length(raw_merged_df, 1) into array_len;

      truncate table table_event_in_rem_ep;
      insert into table_event_in_rem_ep (select unnest(raw_merged_df));
      for sort_events in curSortEvents loop
        event_in_rem_ep := row(sort_events.child_id, sort_events.event_type, sort_events.event_date,
                               sort_events.placed_with_relative_this_rem_loc,
                               sort_events.county_of_this_rem_loc);
        select array_append(merged_df, event_in_rem_ep) into merged_df;
      end loop;
      select array_length(merged_df, 1) into n_merged_rows;

     removal_episode_number := 0;
     /*Since we are storing removal episodes for all children in a single table, 
       we are not truncating it.*/
     --truncate table removal_episodes;

     /*In general, the start-date of a removal episode, 
     that is the outcome of a court hearing,
     is the first location_start_date
     that follows the court_hearing_date.
     The end date of this removal episode is the next court_hearing_date,
     and it should follow the start date of the removal episode by at least 
     2 days, since the length of a removal episode should be at least 24 hours.
     First, we identify all removal episodes, with start dates as the first 
     location_start_date between two consecutive values of court_hearing_date.
     Next, we will eliminate all removal episodes whose lengths are one day 
     or less.
    
     #If there are some court_hearing_dates even before the first 
     #location_start_date, because of junk data, proceed till we get the 
     #first location_start_date, because otherwise we will have no removal
     #episodes to finish.*/

     i := 1;
     while ((i <= n_merged_rows) 
             AND (merged_df[i].event_type = 'court_hearing')) loop
      i := i + 1;
     end loop;

     episode_start_date := null;
     v_placed_with_relative_this_rem_ep := FALSE;

     while (i <= n_merged_rows) loop
     
      if (merged_df[i].event_type = 'location_start') then

        v_placed_with_relative_this_rem_ep := v_placed_with_relative_this_rem_ep OR 
                                            merged_df[i].placed_with_relative_this_rem_loc;

        if (episode_start_date is null) then
          /*A removal episode starts. The last removal episode may not
            even end if the child is currently in a removal episode.*/
          removal_episode_number := removal_episode_number + 1;
          episode_start_date := merged_df[i].event_date;
          insert into removal_episodes (child_id, episode_number, start_date, county_of_initial_removal)
          values (v_person_id, removal_episode_number, episode_start_date, 
                   merged_df[i].county_of_this_rem_loc);

          /*raise notice 'creating removal episode for v_person_id = %, 
                        removal_episode_number = %, episode_start_date = %',
                        v_person_id, removal_episode_number, to_char(episode_start_date, 'YYYY-MM-DD');*/
        end if;
        /*Otherwise, this is just another removal location in
          the current removal episode. Do nothing.*/
              
      else 
      
        /*Encountered a court hearing date. End the current removal episode,
         if one exists.*/
        if (episode_start_date is not null) then
        
          --removal_episodes[removal_episode_number].end_date := merged_df[i].event_date;
          update removal_episodes set end_date =  merged_df[i].event_date,
                 placed_with_relative_this_rem_ep = v_placed_with_relative_this_rem_ep
          where child_id = v_person_id
          and episode_number = removal_episode_number;

          update removal_episodes 
          set n_contacts_in_rem_ep = number_of_contacts_in_rem_ep(v_person_id, start_date, COALESCE(end_date, 
                                             to_date('2100-12-31', 'YYYY-MM-DD'))),
              n_visits_in_rem_ep = number_of_visits_in_rem_ep(v_person_id, start_date, COALESCE(end_date, 
                                             to_date('2100-12-31', 'YYYY-MM-DD')))
          where child_id = v_person_id
          and episode_number = removal_episode_number;

          /*raise notice 'updating end date for child_id = %, removal episode start date = %, 
                        removal episode end date = %', v_person_id,  
                        to_char(episode_start_date, 'YYYY-MM-DD'), 
                        to_char(merged_df[i].event_date, 'YYYY-MM-DD');*/

          episode_start_date := null;
          v_placed_with_relative_this_rem_ep := FALSE;
        end if;

      end if;
      i := i + 1;
   end loop; --end while (i <= n_merged_rows)

   /*#Keep the removal episodes that 
    # 1) do not have an end date, or
    # 2) have an end date, but lasted for more than 1 day*/

    delete from removal_episodes 
    where child_id = v_person_id
    and (end_date is not null and
           extract(month from age(end_date, start_date)) = 0 and
           extract(day from age(end_date, start_date)) < 2);

    /*execute 'select count(*) from removal_episodes where child_id = ' || v_person_id 
            into n_rem_eps_after_delete;

    if (n_rem_eps_after_delete <> removal_episode_number) then
      new_removal_episode_number := 1;
      for removal_episodes_after_delete in curRemovalEpisodesAfterDelete loop

         old_removal_episode_number := removal_episodes_after_delete.episode_number;
         update removal_episodes
         set episode_number = new_removal_episode_number
         where episode_number = old_removal_episode_number
         and child_id = v_person_id;

         new_removal_episode_number := new_removal_episode_number + 1;
      end loop;
    end if;*/
   end;
  $$ LANGUAGE plpgsql;

  drop function if exists fn_create_removal_episodes();
  create function fn_create_removal_episodes() RETURNS void AS $$
   declare

     n_children_processed integer := 0;
     current_t varchar(100) := '';

     curChildrenInPlacement cursor for
       select distinct(person_id)
       from removal_locations;

   begin
      for children_in_placement in curChildrenInPlacement loop
        perform get_removal_episodes(children_in_placement.person_id);   
        n_children_processed := n_children_processed + 1;
        if (n_children_processed % 100 = 0) then
         select substring(timeofday() from 1 for 19) into current_t;
         raise notice 'n_children_processed = %, current time = %', n_children_processed, 
               current_t;
        end if;
      end loop;
   end;
  $$ LANGUAGE plpgsql;
