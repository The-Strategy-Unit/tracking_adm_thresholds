/*
README
Query to return table that will provide descriptions for SNOMED codes in the ECDS.
*/

SELECT 
   [Sheet_Name],
   [SNOMED_Code],
   CASE
      WHEN [SNOMED_Description] IS NULL
      AND [SNOMED_TERM] IS NULL THEN [SNOMED_UK_Preferred_Term]
      WHEN [SNOMED_Description] IS NULL
      AND [SNOMED_UK_Preferred_Term] IS NULL THEN [SNOMED_TERM]
      ELSE [SNOMED_Description]
   END AS 'DERIVED_SNOMED_DESCR'
/*
   ,[ECDS_Description]
   ,[SNOMED_Description]
   ,[SNOMED_TERM]  -- FOR SPOKEN LANGUAGE
   ,[SNOMED_UK_Preferred_Term]
   ,[ICD10_Mapping]
    ,[ICD10_Description]
*/
FROM [UKHD_ECDS_TOS].[CODE_SETS]
ORDER BY [Sheet_Name]