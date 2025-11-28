-- @create_users
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user TEXT NOT NULL,
    pass TEXT NOT NULL,
    db_name TEXT NOT NULL,
    hostname TEXT NOT NULL,
    token TEXT NOT NULL
);

-- @add_user
INSERT INTO users VALUES;

-- @user_by_name
SELECT * FROM users u WHERE u.user = @user_name;

-- @remove_user
DELETE FROM users WHERE user = @user_name;

