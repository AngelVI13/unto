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

-- @num_athletes
SELECT COUNT(*) FROM athletes;

-- @list_athlete_ids
SELECT id FROM athletes;

-- @create_stats
CREATE TABLE IF NOT EXISTS stats (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    lap_idx INTEGER DEFAULT NULL,
    split_idx INTEGER DEFAULT NULL,
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
    max_power INTEGER NULL          -- optional
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
    route_id INTEGER DEFAULT NULL,
    FOREIGN KEY (athlete_id) REFERENCES athletes (id) ON DELETE CASCADE
);

-- @add_activity
INSERT INTO activities VALUES;

-- @activities_between
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
JOIN stats s ON s.activity_id = a.id AND s.lap_idx IS NULL AND s.split_idx IS NULL
WHERE a.start_date > @start_date 
    AND a.start_date < @end_date;

-- @list_activities
SELECT id FROM activities;

-- @create_routes
CREATE TABLE IF NOT EXISTS routes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    route_id INTEGER NOT NULL,
    route_hash TEXT NOT NULL,
    route_start TEXT NOT NULL,
    route_distance FLOAT NOT NULL
);

-- @activities_on_route
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
JOIN stats s ON s.activity_id = a.id AND s.lap_idx IS NULL AND s.split_idx IS NULL
WHERE a.route_id IS NOT NULL AND a.route_id == @route_id
ORDER BY a.start_date ASC;

-- @add_route
INSERT INTO routes VALUES;

-- @list_similar_routes
SELECT * FROM routes
WHERE route_start == @start_hash
    AND route_distance BETWEEN @distance * 0.90 AND @distance * 1.10;

-- @create_laps
CREATE TABLE IF NOT EXISTS laps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    lap_index INTEGER NOT NULL,
    moving_time INTEGER NOT NULL,
    start INTEGER NOT NULL,
    len INTEGER NOT NULL
);

-- @add_lap
INSERT INTO laps VALUES;

-- @laps_by_activity_id
SELECT 
    l.lap_index,
    l.start,
    l.len,
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
FROM laps l
JOIN stats s ON s.activity_id = l.activity_id AND s.lap_idx = l.lap_index
WHERE l.activity_id = @activity_id
ORDER BY l.lap_index;

-- @create_splits
CREATE TABLE IF NOT EXISTS splits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    split_index INTEGER NOT NULL,
    start INTEGER NOT NULL,
    len INTEGER NOT NULL
);

-- @add_split
INSERT INTO splits VALUES;

-- @splits_by_activity_id
SELECT 
    sp.split_index,
    sp.start,
    sp.len,
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
FROM splits sp
JOIN stats s ON s.activity_id = sp.activity_id AND s.split_idx = sp.split_index
WHERE sp.activity_id = @activity_id
ORDER BY sp.split_index;

-- @create_streams
CREATE TABLE IF NOT EXISTS streams (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_id INTEGER NOT NULL,
    data TEXT NOT NULL,
    data_len INTEGER NOT NULL -- decompressed string length
);

-- @add_streams
INSERT INTO streams VALUES;

-- @streams_for_activity
SELECT * FROM streams WHERE activity_id == @activity_id;

-- @activity_by_id
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
    s.max_power,
    st.data,
    st.data_len
FROM activities a
JOIN stats s ON a.id = s.activity_id AND s.lap_idx IS NULL and s.split_idx IS NULL
JOIN streams st ON a.id = st.activity_id
WHERE a.id == @activity_id;
