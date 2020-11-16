# H515_Final

MOTIVATION
Identifying causes of radiology exam workflow outliers. We are looking at the factor Turnaround Time-hours between the study images being collected and a radiologist finalizing the exam-as a categorical variable stratified by standard deviations (SD--, SD-, Mean, SD+, SD++). We will predict this factor using the remaining variables in the dataset described below.
Additionally we will look at predicting billing errors, based on a binary variable ‘addendum.’ Addendum indicates the radiologist was required by billing services to submit an update to their report to allow for appropriate billing to take place. Potentially these addendums are influenced by procedure code, the radiologist creating the report, or other variables in our dataset.
THE DATASET
Dig Our RIS (DORIS) is an electronic tool for searching radiology reports with a Google-like web interface. Using this tool, we acquired a CSV of RIS metadata at IU Health. We limited our search to 1 month (September, 2020), 4 hospitals within the IU Health system (Methodist, University, North, and West Hospitals), and non-procedural diagnostic radiology. In total approximately 43 thousand exams were included in this dataset. Columns of data are described appendix A. Dataset hosted at github repo in Appendix B.
