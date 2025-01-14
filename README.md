# ProPickML
ProPickML is a machine-learning based tool for chromatogram analysis in label-Free targeted Proteomics

ProPickML uses XGBoost models to acurately predict peptide peaks in your data 
In order to get predictions on your dataset we recommend running the full process from mzXML to Ml input file. 
We also recommend placing all code and mzXML files in one directory for ease of execution.

1. Convert Raw files into mzXML using proteoWizard
2. Run NEW_mzXML_to_csv.R on mzXML files
3. Use Make_graphs.R on the output to create chromatograms for each peptide and sample (optional)
4. Run NEW_csv_to_ml.R on the output summary csv files
5. Run NEW_ML_final_prep.R on output of step 4 to have final input tables for Machine learning
6. Run XGBoostModel.R with the datsets from dataset.zip to run the models and verify results. Add in your own preprocessed datasets to train a personnalized model.
7. Use xgb_urine.model or xgb_general.model to try out the pre-trained Urine specific and generalizable model on your data

Please read comments carefully in each code file for precise instructions
