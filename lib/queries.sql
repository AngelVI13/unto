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

-- @add_athlete
INSERT INTO athletes VALUES;

-- @list_athletes
SELECT * FROM athletes
ORDER BY id;

-- @list_athlete_ids
SELECT id FROM athletes;

-- @create_stats
CREATE TABLE IF NOT EXISTS stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    data_points INTEGER NOT NULL,
    moving_time INTEGER NOT NULL,
    elapsed_time INTEGER NOT NULL,
    distance REAL NULL,           -- optional
    elev_gain INTEGER NULL,       -- optional
    elev_loss INTEGER NULL,       -- optional
    elev_high INTEGER NULL,       -- optional
    elev_low INTEGER NULL,        -- optional
    start_lat REAL NULL,          -- optional
    start_lng REAL NULL,          -- optional
    end_lat REAL NULL,            -- optional
    end_lng REAL NULL,            -- optional
    average_speed REAL NULL,      -- optional
    max_speed REAL NULL,          -- optional
    average_cadence INTEGER NULL, -- optional
    max_cadence INTEGER NULL,     -- optional
    average_temp INTEGER NULL,    -- optional
    average_heartrate INTEGER NULL, -- optional
    max_heartrate INTEGER NULL,     -- optional
    average_power INTEGER NULL,     -- optional
    max_power INTEGER NULL,          -- optional

    FOREIGN KEY (activity_id) REFERENCES activities (id) ON DELETE CASCADE
);

-- @add_stats
INSERT INTO stats VALUES;

-- @stats_id_for_activity
SELECT id FROM stats WHERE activity_id == @activity_id;

-- @create_activities
CREATE TABLE IF NOT EXISTS activities (
    id INTEGER PRIMARY KEY,
    athlete_id INTEGER NOT NULL,
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

-- @add_activity
INSERT INTO activities VALUES;

-- @activities_after
SELECT 
    a.*,
    s.moving_time,
    s.elapsed_time,
    s.distance,
    s.elev_gain,
    s.elev_loss,
    s.elev_high,
    s.elev_low,
    s.start_lat,
    s.start_lng,
    s.end_lat,
    s.end_lng,
    s.average_speed,
    s.max_speed,
    s.average_cadence,
    s.max_cadence,
    s.average_temp,
    s.average_heartrate,
    s.max_heartrate,
    s.average_power,
    s.max_power
FROM activities a
JOIN stats s ON a.stats_id = s.id
WHERE a.start_date > @start_date;

-- @list_activities
SELECT id FROM activities;

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

-- @add_lap
INSERT INTO laps VALUES;

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

-- @add_split
INSERT INTO splits VALUES;

-- @create_streams
CREATE TABLE IF NOT EXISTS streams (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    data BLOB NOT NULL,
    data_len INTEGER NOT NULL, -- decompressed string length
    FOREIGN KEY (activity_id) REFERENCES activities (id) ON DELETE CASCADE
);

-- @add_streams
INSERT INTO streams VALUES;
