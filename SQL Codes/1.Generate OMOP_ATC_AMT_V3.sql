

SELECT
 c1.concept_id                 Class_Concept_Id,
 c1.concept_name               Class_Name,
 c1.concept_code               Class_Code,
 c1.concept_class_id              Classification,
 c1.vocabulary_id              Class_vocabulary_id,
 v1.vocabulary_name            Class_vocabulary_name,
 ca.min_levels_of_separation  Levels_of_Separation,
 ca.descendant_concept_id     concept_id
 into #temp
FROM concept_ancestor   ca,
 concept                c1,
 vocabulary             v1
WHERE   
ca.ancestor_concept_id = c1.concept_id
AND    c1.vocabulary_id = 'ATC'
-- AND    c1.concept_class_id IN ('ATC','VA Class','Mechanism of Action','Chemical Structure','ETC','Physiologic Effect')
AND    c1.vocabulary_id = v1.vocabulary_id

select c.concept_id_1 concept_id, c.concept_name, c.concept_code, c.concept_class_id, c.invalid_reason, c.standard_concept, c.vocabulary_id, t.Class_Concept_Id, t.Class_Name, t.Class_Code ATC_CODE, t.Classification 
INTO #[GPE_FF_WOL_PRACTICE_DATA_NOV_2017_REPORTS].[dbo].[OMOP_ATC_AMT_V3]
from (
select r.concept_id_1, r.concept_id_2, con.concept_name, con.concept_code, con.concept_class_id, con.invalid_reason, con.standard_concept, con.vocabulary_id from concept_relationship r
join concept con on r.concept_id_1 = con.concept_id
where relationship_id = 'Maps to' 
 ) c
join #temp t on t.concept_id = c.concept_id_2
where c.vocabulary_id = 'AMT' 
group by c.concept_id_1, c.concept_name, c.concept_code, c.concept_class_id, c.invalid_reason, c.standard_concept, c.vocabulary_id, t.Class_Concept_Id, t.Class_Name, t.Class_Code, t.Classification
drop table #temp