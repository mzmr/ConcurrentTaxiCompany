# ConcurrentTaxiCompany
This is a model of taxi company created using Erlang concurrent programming.


## Running the application
In the project [root folder](./) (e.g. C:\Users\TheUser\ConcurrentTaxiCompany):
```
erl -make
erl -pa ebin
```

Then in erlang shell:
```erl
application:start(taxi_company).
```

## Stopping the application
```erl
application:stop(taxi_company).
q(). % to quit the shell
```
