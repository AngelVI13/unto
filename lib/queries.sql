-- @create_athletes
CREATE TABLE IF NOT EXISTS athletes (
    id INTEGER PRIMARY KEY,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL,
    city TEXT NOT NULL,
    state TEXT NOT NULL,
    country TEXT NOT NULL,
    sex TEXT NOT NULL,
    created_at TEXT NOT NULL,
    weight FLOAT NOT NULL
);

-- TODO: 1. write all insert queries or maybe generate them?
-- TODO: 2. You can have queries that fetch all activities by
-- some criteria (between dates etc.) with their stats which is
-- much more performant than first fetching all activities and
-- then for each activity fetching their stats. For this we have
-- to use join. Example:
-- SELECT * FROM activity JOIN stats ON activity.stats_id = stats.id WHERE activity.id = ?

-- @list_athletes
SELECT * FROM athletes
ORDER BY id;

-- @create_stats
CREATE TABLE IF NOT EXISTS stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER PRIMARY KEY,
    FOREIGN KEY (activity_id) REFERENCES activities (id),
    data_points INTEGER NOT NULL,
    moving_time INTEGER NOT NULL,
    elapsed_time INTEGER NOT NULL,
    distance REAL,           -- optional
    elev_gain INTEGER,       -- optional
    elev_loss INTEGER,       -- optional
    elev_high INTEGER,       -- optional
    elev_low INTEGER,        -- optional
    start_lat REAL,          -- optional
    start_lng REAL,          -- optional
    end_lat REAL,            -- optional
    end_lng REAL,            -- optional
    average_speed REAL,      -- optional
    max_speed REAL,          -- optional
    average_cadence INTEGER, -- optional
    max_cadence INTEGER,     -- optional
    average_temp INTEGER,    -- optional
    average_heartrate INTEGER, -- optional
    max_heartrate INTEGER,     -- optional
    average_power INTEGER,     -- optional
    max_power INTEGER          -- optional
);

-- @create_activities
CREATE TABLE IF NOT EXISTS activities (
    id INTEGER PRIMARY KEY,
    athlete_id INTEGER,
    name TEXT NOT NULL,
    sport_type TEXT NOT NULL,
    start_date TEXT NOT NULL,
    timezone TEXT NOT NULL,
    map_id TEXT NOT NULL,
    map_summary_polyline TEXT NOT NULL,
    stats_id INTEGER NOT NULL,
    FOREIGN KEY (athlete_id) REFERENCES athletes (id) ON DELETE CASCADE,
    FOREIGN KEY (stats_id) REFERENCES stats (id) ON DELETE CASCADE
);

-- example query for activity that happened after certain date
-- All events after a certain UTC datetime
--SELECT * FROM events
--WHERE start_date > '2025-07-20T00:00:00Z';
-- here have to replace the hardcoded date with `?`

-- @create_laps
CREATE TABLE IF NOT EXISTS laps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    lap_index INTEGER NOT NULL,
    moving_time INTEGER NOT NULL,
    start INTEGER NOT NULL,
    len INTEGER NOT NULL,
    stats_id INTEGER NOT NULL,
    FOREIGN KEY (activity_id) REFERENCES activities (id) ON DELETE CASCADE,
    FOREIGN KEY (stats_id) REFERENCES stats (id) ON DELETE CASCADE
);


-- @create_splits
CREATE TABLE IF NOT EXISTS splits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    split_index INTEGER NOT NULL,
    start INTEGER NOT NULL,
    len INTEGER NOT NULL,
    stats_id INTEGER NOT NULL,
    FOREIGN KEY (activity_id) REFERENCES activities (id) ON DELETE CASCADE,
    FOREIGN KEY (stats_id) REFERENCES stats (id) ON DELETE CASCADE
);

