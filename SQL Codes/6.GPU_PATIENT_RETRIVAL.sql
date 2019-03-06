-- RETREIVE OMOP_MD PATIENTS
SELECT P.Patient_UUID FROM MD_DRUG_PATIENTS P
INNER JOIN OMOP_MD_ATC_AMT_V3 T ON P.DRUG_NO = T.MD_DRUG_NO  --SELECT ATC DRUG LIST
INNER JOIN GPU_MD_ACTIVEPATIENTS A ON A.Patient_UUID = P.Patient_UUID
WHERE ATC_CODE LIKE 'C07%' -- ENTER ATC CODE
GROUP BY P.PATIENT_UUID
order by Patient_UUID asc

-- RETREIVE PBS_MD PATIENTS

SELECT P.Patient_UUID FROM MD_DRUG_PATIENTS P
INNER JOIN PBS_MD_ATC_AMT T ON P.DRUG_NO = T.MD_DRUG_NO  --SELECT ATC DRUG LIST
INNER JOIN GPU_MD_ACTIVEPATIENTS A ON A.Patient_UUID = P.Patient_UUID
WHERE ATC_CODE LIKE 'C07%' -- ENTER ATC CODE
GROUP BY P.PATIENT_UUID
order by Patient_UUID asc
