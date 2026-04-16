#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdhep_util.hh>

#include <unistd.h>
#include <iostream>
#include <vector>
using namespace std;

/**
 * Adds mother particles (623 scattered electron/muon and 622 A-prime/virtual photon) to events
 * and builds proper genealogy from LHE and STDHEP input files.
 * Handles both A-prime (nup=7) and radiative (nup=6) cases.
 */

const double ELECTRON_MASS = 0.000511;
const double BEAM_ENERGY = 3.74;

struct LHEParticle {
    int idhep;
    int isthep;
    int jmohep[2];
    double phep[5];  // px, py, pz, E, mass
};

enum EventType {
    EVENT_APRIME,     // nup = 7
    EVENT_RADIATIVE,  // nup = 6
    EVENT_UNKNOWN
};

bool read_lhe_event_aprime(FILE* lhe_file, int nup, LHEParticle& scattered_electron,
                           LHEParticle& aprime, int id_pair) {
    char line[1000];

    bool found_aprime = false;
    bool found_scattered = false;

    // Read all particles in the event
    for (int i = 0; i < nup; i++) {
        if (fgets(line, 1000, lhe_file) == NULL) return false;

        LHEParticle temp;
        sscanf(line, "%d %d %d %d %*d %*d %lf %lf %lf %lf %lf",
               &temp.idhep, &temp.isthep,
               &temp.jmohep[0], &temp.jmohep[1],
               &temp.phep[0], &temp.phep[1], &temp.phep[2],
               &temp.phep[3], &temp.phep[4]);

        // Find A-prime (id_pair, status 2)
        if (temp.idhep == id_pair && temp.isthep == 2) {
            aprime = temp;
            found_aprime = true;
        }

        // Find scattered electron (11, status 1, energy < beam energy)
        if (temp.idhep == 11 && temp.isthep == 1 && temp.phep[3] < BEAM_ENERGY) {
            scattered_electron = temp;
            found_scattered = true;
        }
    }

    if (!found_aprime || !found_scattered) {
        fprintf(stderr, "Error: Could not find required particles in A-prime LHE event\n");
        fprintf(stderr, "  Found pair particle (%d): %d, Found scattered electron: %d\n",
                id_pair, found_aprime, found_scattered);
        return false;
    }

    return true;
}

bool read_lhe_event_radiative(FILE* lhe_file, int nup, LHEParticle& scattered_lepton,
                              LHEParticle& electron, LHEParticle& positron) {
    char line[1000];

    bool found_scattered = false;
    bool found_electron = false;
    bool found_positron = false;

    // Read all particles in the event
    for (int i = 0; i < nup; i++) {
        if (fgets(line, 1000, lhe_file) == NULL) return false;

        LHEParticle temp;
        sscanf(line, "%d %d %d %d %*d %*d %lf %lf %lf %lf %lf",
               &temp.idhep, &temp.isthep,
               &temp.jmohep[0], &temp.jmohep[1],
               &temp.phep[0], &temp.phep[1], &temp.phep[2],
               &temp.phep[3], &temp.phep[4]);

        // Find scattered lepton (13, status 1, energy < beam energy)
        if (temp.idhep == 13 && temp.isthep == 1 && temp.phep[3] < BEAM_ENERGY) {
            scattered_lepton = temp;
            found_scattered = true;
        }

        // Find electron (11, status 1)
        if (temp.idhep == 11 && temp.isthep == 1) {
            electron = temp;
            found_electron = true;
        }

        // Find positron (-11, status 1)
        if (temp.idhep == -11 && temp.isthep == 1) {
            positron = temp;
            found_positron = true;
        }
    }

    if (!found_scattered || !found_electron || !found_positron) {
        fprintf(stderr, "Error: Could not find required particles in radiative LHE event\n");
        fprintf(stderr, "  Found scattered lepton: %d, Found electron: %d, Found positron: %d\n",
                found_scattered, found_electron, found_positron);
        return false;
    }

    return true;
}

bool read_lhe_event(FILE* lhe_file, EventType& event_type, int& nup,
                   LHEParticle& scattered_particle, LHEParticle& pair_or_electron,
                   LHEParticle& positron, int id_pair) {
    char line[1000];

    // Find <event> tag
    bool found_event = false;
    while (fgets(line, 1000, lhe_file) != NULL) {
        if (strstr(line, "<event") != NULL) {
            found_event = true;
            break;
        }
    }

    if (!found_event) return false;

    // Read event header
    if (fgets(line, 1000, lhe_file) == NULL) return false;
    sscanf(line, "%d", &nup);

    // Determine event type
    if (nup == 7) {
        event_type = EVENT_APRIME;
        return read_lhe_event_aprime(lhe_file, nup, scattered_particle, pair_or_electron, id_pair);
    } else if (nup == 6) {
        event_type = EVENT_RADIATIVE;
        return read_lhe_event_radiative(lhe_file, nup, scattered_particle, pair_or_electron, positron);
    } else {
        fprintf(stderr, "Error: Unknown event type with nup=%d (expected 6 or 7)\n", nup);
        event_type = EVENT_UNKNOWN;
        return false;
    }
}

int main(int argc, char** argv)
{
    int id_beam = 623;
    int id_pair = 622;

    int c;

    while ((c = getopt(argc, argv, "hi:j:")) != -1) {
        switch (c) {
            case 'h':
                printf("-h: print this help\n");
                printf("-i: PDG ID of beam particle (scattered electron/muon, default 623)\n");
                printf("-j: PDG ID of pair particle (A-prime/virtual photon, default 622)\n");
                printf("Usage: %s [-i beam_id] [-j pair_id] <input stdhep> <input lhe> <output stdhep>\n", argv[0]);
                return 0;
            case 'i':
                id_beam = atoi(optarg);
                break;
            case 'j':
                id_pair = atoi(optarg);
                break;
            case '?':
                printf("Invalid option or missing option argument; -h to list options\n");
                return 1;
            default:
                abort();
        }
    }

    if (argc - optind < 3) {
        printf("Usage: %s [-i beam_id] [-j pair_id] <input stdhep> <input lhe> <output stdhep>\n", argv[0]);
        printf("Use -h for help\n");
        return 1;
    }

    int istream = 0;
    int ostream = 1;

    // Open input STDHEP file
    int n_events = open_read(argv[optind], istream);
    if (n_events <= 0) {
        fprintf(stderr, "Error: Could not open STDHEP file %s\n", argv[optind]);
        return 1;
    }

    // Open input LHE file
    FILE* lhe_file = fopen(argv[optind + 1], "r");
    if (!lhe_file) {
        fprintf(stderr, "Error: Could not open LHE file %s\n", argv[optind + 1]);
        close_read(istream);
        return 1;
    }

    // Open output STDHEP file
    open_write(argv[optind + 2], ostream, n_events);

    printf("Processing events with:\n");
    printf("  Beam particle ID: %d\n", id_beam);
    printf("  Pair particle ID: %d\n", id_pair);

    int event_count = 0;
    int aprime_count = 0;
    int radiative_count = 0;

    // Process each event
    while (true) {
        // Read STDHEP event
        if (!read_next(istream)) {
            break;  // No more events
        }

        vector<stdhep_entry> input_event;
        int nevhep = read_stdhep(&input_event);

        // Read LHE event
        EventType event_type;
        int nup;
        LHEParticle scattered_particle, pair_or_electron, positron;

        if (!read_lhe_event(lhe_file, event_type, nup, scattered_particle,
                           pair_or_electron, positron, id_pair)) {
            fprintf(stderr, "Error reading LHE event %d\n", event_count);
            break;
        }

        // Build output event with unified logic
        vector<stdhep_entry> output_event;

        if (event_type == EVENT_APRIME) {
            aprime_count++;

            // ===== Find particles in STDHEP by mother relationship =====

            // Find scattered electron: jmohep[0] = 0 AND idhep = 11
            int scattered_idx = -1;
            for (size_t i = 0; i < input_event.size(); i++) {
                if (input_event[i].jmohep[0] == 0 && input_event[i].idhep == 11) {
                    scattered_idx = i;
                    break;
                }
            }

            // Find e+e- pair: jmohep[0] = 1 AND idhep = ±11
            int electron_idx = -1;
            int positron_idx = -1;
            int pair_count = 0;

            for (size_t i = 0; i < input_event.size(); i++) {
                if (input_event[i].jmohep[0] == 1) {
                    if (input_event[i].idhep == 11) {
                        if (electron_idx == -1) {
                            electron_idx = i;
                            pair_count++;
                        }
                    } else if (input_event[i].idhep == -11) {
                        if (positron_idx == -1) {
                            positron_idx = i;
                            pair_count++;
                        }
                    }
                }
            }

            // Validation — skip event if a daughter was absorbed in the target
            if (electron_idx == -1 || positron_idx == -1) {
                event_count++;
                continue;
            }

            // Determine vertex for scattered electron (623)
            double scattered_vertex[4];
            if (scattered_idx != -1) {
                // Use scattered electron's vertex
                for (int j = 0; j < 4; j++) {
                    scattered_vertex[j] = input_event[scattered_idx].vhep[j];
                }
            } else {
                // No scattered electron found (N=2 case), use (0, 0, 0.02, 0)
                scattered_vertex[0] = 0.0;
                scattered_vertex[1] = 0.0;
                scattered_vertex[2] = 0.02;
                scattered_vertex[3] = 0.0;
            }

            // ===== Entry 0: Documentation particle (id_beam, isthep=3) =====
            // SLIC ignores isthep=3; jdahep points to scattered electron if present
            stdhep_entry entry0;
            entry0.isthep = 3;
            entry0.idhep = id_beam;
            entry0.jmohep[0] = 0;
            entry0.jmohep[1] = 0;
            // jdahep set below after we know whether scattered electron is included
            entry0.phep[0] = scattered_particle.phep[0];
            entry0.phep[1] = scattered_particle.phep[1];
            entry0.phep[2] = scattered_particle.phep[2];
            entry0.phep[3] = scattered_particle.phep[3];
            entry0.phep[4] = ELECTRON_MASS;
            for (int j = 0; j < 4; j++) {
                entry0.vhep[j] = scattered_vertex[j];
            }

            // ===== Entry 1: A-prime (id_pair) =====
            stdhep_entry entry1;
            entry1.isthep = 2;
            entry1.idhep = id_pair;
            entry1.jmohep[0] = 0;
            entry1.jmohep[1] = 0;
            // jdahep set below
            entry1.phep[0] = pair_or_electron.phep[0];
            entry1.phep[1] = pair_or_electron.phep[1];
            entry1.phep[2] = pair_or_electron.phep[2];
            entry1.phep[3] = pair_or_electron.phep[3];
            entry1.phep[4] = pair_or_electron.phep[4];
            for (int j = 0; j < 4; j++) {
                entry1.vhep[j] = input_event[electron_idx].vhep[j];
            }

            // ===== A' daughters =====
            stdhep_entry entry_pos = input_event[positron_idx];
            entry_pos.jmohep[1] = 0;
            entry_pos.phep[4] = ELECTRON_MASS;

            stdhep_entry entry_ele = input_event[electron_idx];
            entry_ele.jmohep[1] = 0;
            entry_ele.phep[4] = ELECTRON_MASS;

            if (scattered_idx != -1) {
                // 5-particle structure: 623, 622, scattered_e, positron, electron
                entry0.jdahep[0] = 3;  // scattered electron at position 3
                entry0.jdahep[1] = 3;
                entry1.jdahep[0] = 4;  // A' daughters at positions 4,5
                entry1.jdahep[1] = 5;
                entry_pos.jmohep[0] = 2;
                entry_ele.jmohep[0] = 2;

                stdhep_entry entry_sc = input_event[scattered_idx];
                entry_sc.jmohep[0] = 1;  // mother is 623
                entry_sc.jmohep[1] = 0;

                output_event.push_back(entry0);
                output_event.push_back(entry1);
                output_event.push_back(entry_sc);
                output_event.push_back(entry_pos);
                output_event.push_back(entry_ele);
            } else {
                // 4-particle structure: 623, 622, positron, electron
                entry0.jdahep[0] = 0;
                entry0.jdahep[1] = 0;
                entry1.jdahep[0] = 3;  // A' daughters at positions 3,4
                entry1.jdahep[1] = 4;
                entry_pos.jmohep[0] = 2;
                entry_ele.jmohep[0] = 2;

                output_event.push_back(entry0);
                output_event.push_back(entry1);
                output_event.push_back(entry_pos);
                output_event.push_back(entry_ele);
            }

        } else if (event_type == EVENT_RADIATIVE) {
            radiative_count++;

            // ===== Find particles in STDHEP by mother relationship =====

            // Find scattered lepton: jmohep[0] = 0 AND idhep = 13
            int scattered_idx = -1;
            for (size_t i = 0; i < input_event.size(); i++) {
                if (input_event[i].jmohep[0] == 0 && input_event[i].idhep == 13) {
                    scattered_idx = i;
                    break;
                }
            }

            // Find e+e- pair: jmohep[0] = 1 AND idhep = ±11
            // For radiative, if no particles with jmohep[0]=1, check jmohep[0]=0
            int electron_idx = -1;
            int positron_idx = -1;
            int pair_count = 0;

            // First try jmohep[0] = 1
            for (size_t i = 0; i < input_event.size(); i++) {
                if (input_event[i].jmohep[0] == 1) {
                    if (input_event[i].idhep == 11) {
                        if (electron_idx == -1) {
                            electron_idx = i;
                            pair_count++;
                        }
                    } else if (input_event[i].idhep == -11) {
                        if (positron_idx == -1) {
                            positron_idx = i;
                            pair_count++;
                        }
                    }
                }
            }

            // If not found with jmohep[0]=1, try jmohep[0]=0 (radiative default case)
            if (electron_idx == -1 || positron_idx == -1) {
                for (size_t i = 0; i < input_event.size(); i++) {
                    if (input_event[i].jmohep[0] == 0) {
                        if (input_event[i].idhep == 11 && electron_idx == -1) {
                            electron_idx = i;
                        } else if (input_event[i].idhep == -11 && positron_idx == -1) {
                            positron_idx = i;
                        }
                    }
                }
            }

            // Validation — skip event if a daughter was absorbed in the target
            if (electron_idx == -1 || positron_idx == -1) {
                event_count++;
                continue;
            }

            // Determine vertex for scattered lepton (623)
            double scattered_vertex[4];
            if (scattered_idx != -1) {
                // Use scattered lepton's vertex
                for (int j = 0; j < 4; j++) {
                    scattered_vertex[j] = input_event[scattered_idx].vhep[j];
                }
            } else {
                // No scattered lepton found, use e+e- vertex
                for (int j = 0; j < 4; j++) {
                    scattered_vertex[j] = input_event[electron_idx].vhep[j];
                }
            }

            // ===== Entry 0: Scattered lepton (id_beam) =====
            // isthep=3: documentation particle — SLIC ignores it, preventing unintended simulation
            stdhep_entry entry0;
            entry0.isthep = 3;
            entry0.idhep = id_beam;
            entry0.jmohep[0] = 0;
            entry0.jmohep[1] = 0;
            entry0.jdahep[0] = 0;
            entry0.jdahep[1] = 0;
            entry0.phep[0] = scattered_particle.phep[0];
            entry0.phep[1] = scattered_particle.phep[1];
            entry0.phep[2] = scattered_particle.phep[2];
            entry0.phep[3] = scattered_particle.phep[3];
            entry0.phep[4] = ELECTRON_MASS;
            for (int j = 0; j < 4; j++) {
                entry0.vhep[j] = scattered_vertex[j];
            }
            output_event.push_back(entry0);

            // ===== Entry 1: Virtual photon (id_pair) =====
            // Calculate 4-momentum sum from STDHEP e+e- (more accurate than LHE)
            stdhep_entry entry1;
            entry1.isthep = 2;
            entry1.idhep = id_pair;
            entry1.jmohep[0] = 0;
            entry1.jmohep[1] = 0;
            entry1.jdahep[0] = 3;  // 1-indexed: electron at output_event[2]
            entry1.jdahep[1] = 4;  // 1-indexed: positron at output_event[3]

            // Sum 4-momenta from STDHEP particles
            entry1.phep[0] = input_event[electron_idx].phep[0] + input_event[positron_idx].phep[0];  // px
            entry1.phep[1] = input_event[electron_idx].phep[1] + input_event[positron_idx].phep[1];  // py
            entry1.phep[2] = input_event[electron_idx].phep[2] + input_event[positron_idx].phep[2];  // pz
            entry1.phep[3] = input_event[electron_idx].phep[3] + input_event[positron_idx].phep[3];  // E

            // Calculate invariant mass
            double E2 = entry1.phep[3] * entry1.phep[3];
            double px2 = entry1.phep[0] * entry1.phep[0];
            double py2 = entry1.phep[1] * entry1.phep[1];
            double pz2 = entry1.phep[2] * entry1.phep[2];
            entry1.phep[4] = sqrt(E2 - px2 - py2 - pz2);

            // Vertex same as e+e- pair
            for (int j = 0; j < 4; j++) {
                entry1.vhep[j] = input_event[electron_idx].vhep[j];
            }
            output_event.push_back(entry1);

            // ===== Entry 2: Electron (11) =====
            stdhep_entry entry2 = input_event[electron_idx];
            entry2.jmohep[0] = 2;  // Mother is entry 1 (the 622)
            entry2.jmohep[1] = 0;
            // Mass already correct (0.000511)
            output_event.push_back(entry2);

            // ===== Entry 3: Positron (-11) =====
            stdhep_entry entry3 = input_event[positron_idx];
            entry3.jmohep[0] = 2;  // Mother is entry 1 (the 622)
            entry3.jmohep[1] = 0;
            // Mass already correct (0.000511)
            output_event.push_back(entry3);

        } else {
            fprintf(stderr, "Error: Unknown event type for event %d\n", event_count);
            break;
        }

        // Write output event
        write_stdhep(&output_event, event_count);
        write_file(ostream);

        event_count++;

        if (event_count % 1000 == 0) {
            printf("Processed %d events (%d A-prime, %d radiative)\n",
                   event_count, aprime_count, radiative_count);
        }
    }

    // Clean up
    close_read(istream);
    fclose(lhe_file);
    close_write(ostream);

    printf("\nSuccessfully processed %d events\n", event_count);
    printf("  A-prime events: %d\n", aprime_count);
    printf("  Radiative events: %d\n", radiative_count);

    return 0;
}
