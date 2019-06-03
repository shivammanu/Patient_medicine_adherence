# Patient_medicine_adherence
Predicting the patients who are non adherent to their prescribed medicines from the transactional records at a pharmacy

## WHY MEDICAL ADHERENCE?

1. Next frontier in healthcare innovation
2. Medical adherence is the most vital element in bridging the gap between the patient’s health and the evolution in medical sciences.
3. Platforms for exchanging info increases the ability of the healthcare system to educate patients and importance of adherence
4. Intelligent diagnostics linked with monitoring facilities
5. Combination of compliance and persistence.
6. Compliance is the degree to which a patient follows or completes a prescribed diagnostic


## APPROACH

1. These studies can only be transformed into systems when there is a system which can segregates adherent and non-adherents patients. (Classification)
2. The client needs a model that can account for the risk of non-adherence per patient and predict if the patient is likely to non-adhere.
3. Irregularity in buying the next dosages at the supposed intervals.
4. Predicting the non-adherents correctly would be the prime objective of the model, so that appropriate actions can be taken by the client.
5. Key parameters which would drive the model can be age, income, awareness scale, education, location, family size, severity of ailment etc.
6. Several modelling techniques would be needed to check from which the data can be most accurately predicted.


## DATA PREPROCESSING – FEATURE ENGGINEERING

1. Ordering dataset in chronological order of transaction dates for patient-medication combination.
2. Checking for missing values.
3. Converting the attributes in factors, numeric and date.
4. Removing the redundant level in PurchasedBy attribute.
5. Calculating delay for every transaction using Date and For_How_many_days.
6. Adding new feature t for giving order of transactions for all patient-medication combinations.
7. Create dummies from categorical attributes and standardize numeric data.
8. Removing Date and For_How_many_days from dataset.
9. Removing PatientID, MedicationID, PatientID-MedicationID.
