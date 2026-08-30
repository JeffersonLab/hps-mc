plot_all_featured_and_2d_debug.py
Plot unit normalized distributions for all variables; now needs signal and background normalization.

plot_all_features_and_do_tables_2.py
Plots some accurately normalized distributions. First script to obtain accurate yields in signal and background, an iteration that worked is included in STUFF_WORKS.

plot_all_features_and_do_tables_3_MULTI_just_z_fit.py
Does the above task but only for the z distribution, allowing the z profile to be fit and checked against the expected post-resampling behavior.
Dependencies: plot_all_features_and_do_tables_3_MULTI.py

plot_all_features_and_do_tables_3_MULTI.py
Does the tables with the additional constraint that multiple signals are overlaid for differing pairs of masses and epsilon.

plot_all_features_and_do_tables_3.py
Performs tight selection and operation of previous scripts, but with two val cuts, one for significance and one for min y0.
Dependencies: decayLength7sel.py

plot_all_features_and_do_tables_4_MULTI_just_z_fit.py
Same as the 3_MULTI_just_z_fit version, but for the version 4 workflow, which uses reweighting rather than down-sampling, with no cutoffs and true_vd.
Dependencies: plot_all_features_and_do_tables_4_MULTI.py

plot_all_features_and_do_tables_4_MULTI.py
Same as the version 3 MULTI workflow, but uses reweighting rather than down-sampling, with no cutoffs and true_vd.

plot_all_features_and_do_tables_4.py
Same as the version 3 workflow, but uses reweighting rather than down-sampling, with no cutoffs and true_vd.

plot_all_features_and_do_tables_BDT.py
Does the tables for the BDT to compare its performance against the selection-based cuts.
Dependencies: decayLength8sel.py, /bdt_trainer/train_bdt_classifier.py

plot_all_features_and_do_tables_BK_w_CUTs.py
Plots the invM of the background as selection cuts are increased.

plot_all_features_and_do_tables.py

plot_all_together_vals.py
Plots the contour levels for a given value for all vals, useful for checking whether one cut is better than the rest.

plot_ann_vs_bdt.py

plot_bg_shell_feature_diffs_ann_bdt2.py

plot_bg_shell_feature_diffs_ann_bdt.py

plot_features_normed.py
Plots histograms for cut variables, weighted by their actual physical values after tight selection.

plot_psum_compare.py
Compares the psum for different interpretations.

plot_psum_from_vertex_only.py
Plots the psum from quantities calculated independently, rather than using the psum already stored in the ROOT file.

plot_psum_from_vertex.py
Plots the psum, used to evaluate whether it had the desired shape.

plot_significance_surfaces.py
Makes 25 contour plots for the various cuts, with 2D histograms and color contours for significance as a function of cut value.

plot_sig_vs_mass.py
Plots the significance as a function of mass, i.e. cut val, for various values of epsilon and val that are fed in.

plot_sig_vs_val.py
Plots the significance as a function of val, i.e. cut val, for various values of mass and epsilon that are fed in.

plot_z0_dist.py
Plots unit normalized distributions of cut variables; they are not weighted by physical weighting.

plot_z0_signal_v_back.py
Plots unit normalized distributions of cut variables; at this point it is min y0.
