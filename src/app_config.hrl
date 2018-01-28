-record(coords, {x, y}).
-record(hr_office, {pid}).
-record(order_receiver, {pid}).
-record(taxi_sup, {pid}).

-define(DEBUG, off).
% TERAZ JEST SUPER
-define(CITY_WIDTH, 800). % meters
-define(CITY_LENGTH, 800).

-define(CITIES_NUMBER, 40).
-define(RECEIVERS_PER_CITY, 3).

-define(INITIAL_TAXI_PER_CITY_NUMBER, 400).

-define(MAX_DISTANCE_TO_CLIENT, 300). % meters

% taxi
-define(MIN_WORK_TIME, 10000). % miliseconds
-define(MAX_WORK_TIME, 30000).
-define(MIN_BREAK_TIME, 6000).
-define(MAX_BREAK_TIME, 25000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 600). % meters

% client
-define(MIN_ORDER_INTERVAL, 100). % miliseconds
-define(MAX_ORDER_INTERVAL, 700).

% hr_office
-define(CHANCE_OF_ACCEPTANCE, 15). % procents

% applicant
-define(MIN_APPLICATION_INTERVAL, 300). % miliseconds
-define(MAX_APPLICATION_INTERVAL, 1000).

% client_supervisor
-define(CLIENTS_NUMBER, 160). % total number of clients to split between all the cities

% applicant_supervisor
-define(APPLICANTS_NUMBER, 80).

% panel
-define(REFRESH_INTERVAL, 1000). % miliseconds
-define(FUEL_LITER_COST, 4.65). % PLN
-define(ORDER_CONST_COST, 7). % PLN
-define(ONE_KM_COST, 1.9). % PLN
-define(LITERS_PER_KM, 11).
-define(DAY_WAGE, 5). % PLN