%% 聊天数据库中用户信息，聊天消息、频道信息的表结构如下：
-record(user,{name,password}).
-record(channel, {name, creator, alive}).
-record(msg, {user_name, channel_name, time, message}).
-record(channel_user, {channel_name, user_name}).
-record(channel_map, {channel_name, width,length}).

%% 缓存频道进程与对应pid的关系
-record(channel_pid, {channel_name, pid}).
-record(user_pid, {user_name, pid}).

