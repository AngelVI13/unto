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

-- @create_stats
CREATE TABLE IF NOT EXISTS stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
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
    max_power INTEGER,          -- optional

    FOREIGN KEY (activity_id) REFERENCES activities (id) ON DELETE CASCADE
);

-- @add_stats
INSERT INTO stats VALUES;

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
