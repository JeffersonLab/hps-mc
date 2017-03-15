mw_run  1    integer  0      #cluster type
mw_run  4    logical  T      #normalize wegiht by cross section
mw_run  5    integer  10000  #number of points for MadEvent integration
mw_run  6    integer  10000  #number of points for MadWeight integration
mw_run  22   logical  0      #put on 1 to add a new events in the analysis (for step 5)

mw_parameter 2 logical 0     #put on 1 to add new param_card.dat (for step 1) creates the corresponding new events dir if step 5 already performs
