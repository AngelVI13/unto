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
