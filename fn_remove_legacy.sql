drop function if exists fn_remove_legacy(varchar);
CREATE FUNCTION fn_remove_legacy(varchar) RETURNS varchar AS $$
  declare
  
   permanency_outcome alias for $1;
   v_position integer := 0;

  begin
   v_position := position('(Legacy)' in permanency_outcome);
   if (v_position != 0) then
      return substring(permanency_outcome from 1 for (v_position-2));
   else
      return permanency_outcome;
   end if;
  end;
$$ LANGUAGE plpgsql;
