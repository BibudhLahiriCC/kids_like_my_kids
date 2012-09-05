drop table pairwise_distances;

create table pairwise_distances
(child_id_1 bigint,
 episode_number_1 integer, 
 child_id_2 bigint,
 episode_number_2 integer, 
 distance numeric(6,4));

create index idx_child_id_1 on pairwise_distances using btree(child_id_1, episode_number_1);
create index idx_child_id_2 on pairwise_distances using btree(child_id_2, episode_number_2);

create index idx_distance on pairwise_distances using btree(trunc(distance, 2));
