select *
from legally_mandated_reasons

select reason_for_change from removal_locations

select graduation_testing, standardized_testing, performing_at_grade_level, person_id, case_plan_id, case_id
from educational_overview_quizzes, case_plan_focus_children, case_plans
where (graduation_testing is not null or standardized_testing is not null)
and educational_overview_quizzes.case_plan_focus_child_id = case_plan_focus_children.id
and case_plan_focus_children.case_plan_id = case_plans.id

select distinct(performing_at_grade_level) from educational_overview_quizzes

select * from removal_episodes

select * from insurance_providers

