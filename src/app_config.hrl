-record(coords, {x, y}).
-record(hr_office, {pid}).
-record(order_receiver, {pid}).
-record(taxi_db_access, {pid}).
-record(taxi_db, {pid}).

-define(DEBUG, off).

-define(CITY_WIDTH, 500). % meters
-define(CITY_LENGTH, 500).

-define(CITIES_NUMBER, 10).
-define(TAXI_DB_PER_CITY, 3).
-define(RECEIVERS_PER_CITY, 3).

-define(INITIAL_TAXI_PER_CITY_NUMBER, 20).

% taxi
-define(WORK_TIME, 15000). % miliseconds
-define(BREAK_TIME, 15000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 150). % meters

% client
-define(MIN_ORDER_INTERVAL, 5000). % miliseconds
-define(MAX_ORDER_INTERVAL, 20000).

% hr_office
-define(CHANCE_FOR_ACCEPTANCE, 5). % procents

% applicant
-define(MIN_APPLICATION_INTERVAL, 4000). % miliseconds
-define(MAX_APPLICATION_INTERVAL, 20000).

% client_supervisor
-define(CLIENTS_NUMBER, 50). % this is the total number of clients which will be shared between all cities

% applicant_supervisor
-define(APPLICANTS_NUMBER, 3).

% panel
-define(REFRESH_INTERVAL, 1000). % miliseconds
-define(FUEL_LITER_COST, 4.65). % PLN
-define(ORDER_CONST_COST, 7). % PLN
-define(ONE_KM_COST, 1.9). % PLN
-define(LITERS_PER_KM, 6).