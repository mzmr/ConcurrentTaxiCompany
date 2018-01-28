{ application, taxi_company, [
  { description, "The simple simulation of the taxi company's running." },
  { vsn, "1.0" },
  {
    modules, [
      app_config,
      applicant,
      applicant_supervisor,
      client,
      client_supervisor,
      first_level_supervisor,
      hiring_supervisor,
      hr_office,
      hr_office_supervisor,
      main_supervisor,
      order_handler,
      order_receiver,
      order_receiver_supervisor,
      ordering_supervisor,
      panel,
      second_level_supervisor,
      stats,
      taxi,
      taxi_company,
      taxi_supervisor,
      utils
    ]
  }, {
    registered, [
      main_supervisor,
      first_level_supervisor,
      second_level_supervisor,
      ordering_supervisor,
      hiring_supervisor,
      order_receiver_supervior,
      client_supervisor,
      hr_office_supervior,
      applicant_supervisor
    ]
  }, {
    applications, [
      kernel,
      stdlib
    ]
  },
  {mod, {taxi_company, []}},
  {env, []}
]}.