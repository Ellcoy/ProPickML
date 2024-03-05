# ProPickML
ProPickML is a machine-learning based tool for chromatogram analysis in label-Free targeted Proteomics

DRF_1_AutoML_3_20240216_151135 is the file containing the pretrained machine learning model. 
In order to get predictions on your dataset we recommend running the full process from mzXML to Ml input file. 
We also recommend placing all code and mzXML files in one directory for ease of execution.

1. Convert Raw files into mzXML using proteoWizard
2. Run mzXML_to_csv.R on mzXML files
3. Use Make_graphs.R on the output to create chromatograms for each peptide and sample (optional)
4. Run csv_to_ML.R on the output summary csv files
5. Run ML_file_to_predictions.R on output of step 4 to get predictions on your dataset

Please read comments carefully in each code file for precise instructions
