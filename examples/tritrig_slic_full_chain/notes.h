/**
 * \page tritrigslicfullchain tritrig\_slic\_full\_chain
 * 
 * The tritrig\_slic\_full\_chain job generates tritrig events using MadGraph5 and transforms the output from lhe to stdhep to then tag the mother particle and rotate the events to beam coordinates. Then, slic is use dto simulate the detector response, and empty bunches are added to simulate the signal pile-up. Lastly, the readout is simulated, hps-java is used to run the reconstruction, and hpstr is run to convert the reconstruction output files to root files and to run the analysis on those files.
 * 
 * \section params Job parameters
 * This table contains parameters special to the tritrig\_beam job. The general parameters are discussed on the \ref examples "examples main page".
 * 
 * | param           |                                                                                |
 * |-----------------|--------------------------------------------------------------------------------|
 * | steering\_files | readout and reconstruction steering files                                      |
 * | config\_files   | configuration files for recon and ana                                          |
 * | output\_files   | list of readout and reco slcio, as well as reco and analysis root output files |
 */