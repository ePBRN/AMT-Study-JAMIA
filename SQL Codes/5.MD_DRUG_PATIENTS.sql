--Create table of Patients and drugs used

SELECT * INTO MD_DRUG_PATIENTS FROM (
	SELECT Patient_UUID, DRUG_NO FROM DB_Medical_Director_V3_10_5_ePBRN_PRESCRIPTION_V1
	union all
	SELECT Patient_UUID, DRUG_NO FROM DB_Medical_Director_V3_10_5_ePBRN_REPEAT_PRESCRIPTION_V1
	) a
