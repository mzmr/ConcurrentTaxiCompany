-record(coords, {x, y}).

% taxi
-define(CITY_WIDTH, 10000). % meters
-define(CITY_LENGTH, 10000).
-define(WORK_TIME, 7000). % miliseconds
-define(BREAK_TIME, 7000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 150). % meters

% client
-define(MIN_INTERVAL, 10). % miliseconds
-define(MAX_INTERVAL, 500).