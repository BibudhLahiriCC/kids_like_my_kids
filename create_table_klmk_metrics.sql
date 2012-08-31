drop table if exists klmk_metrics; 
create table klmk_metrics(
 child_id bigint,
 episode_number integer,
 family_structure_single_female smallint,
 family_structure_single_male smallint,
 family_structure_married_couple smallint,
 family_structure_unmarried_couple smallint,
 parent_alcohol_abuse boolean,
 parent_drug_abuse boolean,
 child_behavioral_problem boolean,
 child_disability boolean,
 case_county_id bigint,
 assessment_county_id bigint,
 primary_caregiver_has_mental_health_problem boolean,
 domestic_violence_reported boolean, 
 count_previous_removal_episodes smallint,
 initial_placement_setting varchar(255)
);
