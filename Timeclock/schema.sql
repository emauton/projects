drop table if exists time_slots;

create table time_slots (
  id int not null auto_increment,
  clock_user_id int not null,
  start datetime not null,
  finish datetime not null,
  created_at timestamp not null,
  updated_at timestamp not null,
  constraint fk_time_slot_user foreign key (clock_user_id) references users(id),
  primary key (id)
);

drop table if exists clock_users;

create table clock_users (
  id int not null auto_increment,
  name varchar(64) not null,
  in_time datetime,
  created_at timestamp not null,
  updated_at timestamp not null,
  primary key (id)
);

