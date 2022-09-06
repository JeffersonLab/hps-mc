A-prime generation to slic  {#apgentoslic}
==========================
This is an example of how to use ap_gen_to_slic_job.py.
By modifying the job script (including subsequent recompilation of hps-mc), this example can also be used to produce prompt or delayed signals. For the delayed signals, the parameter “ctau” needs to be set in jobs.json.

#### Job parameters
This table contains parameters special to the ap\_gen\_to\_slic job. The general parameters are discussed on the [examples main page](@ref examples).

| param    |                              |
|----------|------------------------------|
| apmass   | A-prime mass in MeV?         |
| detector | detector setup used for slic |
