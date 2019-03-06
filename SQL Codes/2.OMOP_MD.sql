/** THIS CODE IS USED FOR OMOP_MAPPING FOR MD**/

--JOIN OMOP MAPPING WITH MD DRUGS

SELECT MD_DRUG_NO, MD_SOURCE_AMT, ATC_CODE INTO OMOP_MD_ATC_AMT_V3 FROM MD_AMT B
INNER JOIN OMOP_ATC_AMT_V3 P ON P.concept_code = B.MD_SOURCE_AMT COLLATE DATABASE_DEFAULT

---------------------------------------------------------------------------------------------------------------------------------------




