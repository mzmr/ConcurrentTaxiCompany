-record(coords, {x, y}).
-record(hr_office, {pid}).
-record(order_receiver, {pid}).
-record(taxi_db_access, {pid}).
-record(taxi_db, {pid}).

-define(DEBUG, off).

-define(CITY_WIDTH, 10000). % meters
-define(CITY_LENGTH, 10000).

-define(CITIES_NUMBER, 3).
-define(TAXI_DB_PER_CITY, 2).
-define(RECEIVERS_PER_CITY, 3).

-define(INITIAL_TAXI_NUMBER, 2).

% taxi
-define(WORK_TIME, 7000). % miliseconds
-define(BREAK_TIME, 7000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 150). % meters

% client
-define(MIN_ORDER_INTERVAL, 5000). % miliseconds
-define(MAX_ORDER_INTERVAL, 10000).

% hr_office
-define(CHANCE_FOR_ACCEPTANCE, 20). % procents

% applicant
-define(MIN_APPLICATION_INTERVAL, 10000). % miliseconds
-define(MAX_APPLICATION_INTERVAL, 15000).

% client_supervisor
-define(CLIENTS_NUMBER, 5).

% applicant_supervisor
-define(APPLICANTS_NUMBER, 4).

% panel
-define(REFRESH_INTERVAL, 1000). % miliseconds