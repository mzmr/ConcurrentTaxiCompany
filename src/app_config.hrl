-record(coords, {x, y}).

-define(CITY_WIDTH, 10000). % meters
-define(CITY_LENGTH, 10000).

% taxi
-define(WORK_TIME, 7000). % miliseconds
-define(BREAK_TIME, 7000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 150). % meters

% client
-define(MIN_ORDER_INTERVAL, 10000). % miliseconds
-define(MAX_ORDER_INTERVAL, 10001).

% hr_office
-define(CHANCE_FOR_ACCEPTANCE, 20). % procents

% applicant
-define(MIN_APPLICATION_INTERVAL, 10000). % miliseconds
-define(MAX_APPLICATION_INTERVAL, 15000).

% client_supervisor
-define(CLIENTS_NUMBER, 100).