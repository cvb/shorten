create table users (id serial primary key, token text not null unique);

create table folders ( id serial primary key
                     , title text
                     , user_id integer not null references users (id)
                               on delete cascade
                     );
create index on folders (user_id);

create table links ( id        serial primary key
                   , url       text
                   , code      text unique
                   , user_id   integer not null references users (id)
                               on delete cascade
                   , folder_id integer references folders (id)
                               on delete set null
                   );
create index on links (user_id);
create index on links (folder_id);

create table clicks ( id serial primary key
                    , date      timestamptz not null default now()
                    , referer   text
                    , remote_ip text
                    , link_id   integer not null references links (id)
                                on delete cascade
                    );
create index on clicks (link_id);
