/*
README
Returns a table showing number of attendances due to
the exclusions applied in the primary ecds query.
Note: has no date cut-off. 
*/

SELECT
  [Der_Provider_Code],
  [Der_Provider_Site_Code],
  CASE 
    WHEN [Der_Dupe_Flag] = 0
      THEN 0 
      ELSE 1
    END AS 'dupe_flag',
  CASE 
    WHEN [Finished_Indicator] = 1
      THEN 0 
      ELSE 1
    END AS 'ongoing',
  CASE 
    WHEN [EC_Department_Type] = '01'
      THEN 0 
      ELSE 1
    END AS 'not_type_1',
  CASE 
    WHEN [EC_AttendanceCategory] = '1'
      THEN 0 
      ELSE 1
    END AS 'planned',
  CASE 
    WHEN [Sex] IN ('1', '2')
      THEN 0 
      ELSE 1
    END AS 'sex_other',
  CASE 
    WHEN [Der_Age_At_CDS_Activity_Date] BETWEEN 0 AND 110
      THEN 0 
      ELSE 1
    END AS 'age_error',
  CASE 
    WHEN [Der_AEA_Patient_Group] != '70' 
      OR [Der_AEA_Patient_Group] IS NULL
      THEN 0 
      ELSE 1
    END AS 'doa',
  CASE 
    WHEN [Discharge_Destination_SNOMED_CT] != '305398007' 
      OR [Discharge_Destination_SNOMED_CT] IS NULL
      THEN 0 
      ELSE 1
    END AS 'died_ed',
  CASE 
    WHEN [EC_Discharge_Status_SNOMED_CT] IN (
       '182992009',
       '1077081000000104',
       '1077031000000103',  
       '1324201000000109',
       '1077021000000100',
       '1077781000000101',
       '1077061000000108',
       '1077041000000107',
       '1077101000000105',
       '1077051000000105',
       '1077071000000101',
       '1077091000000102',
       '2304481000000106'
       ) 
      OR [EC_Discharge_Status_SNOMED_CT] IS NULL
      THEN 0 
      ELSE 1
    END AS 'left/died',
  COUNT(*) AS 'n'
FROM [MESH_ECDS].[EC_Core]
  WHERE 1 = 1
    AND [Der_EC_Arrival_Date_Time] >= '2025-01-01 00:00:00.000'

  GROUP BY 
   [Der_Provider_Code],
   [Der_Provider_Site_Code],
  CASE 
    WHEN [Der_Dupe_Flag] = 0
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [Finished_Indicator] = 1
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [EC_Department_Type] = '01'
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [EC_AttendanceCategory] = '1'
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [Sex] IN ('1', '2')
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [Der_Age_At_CDS_Activity_Date] BETWEEN 0 AND 110
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [Der_AEA_Patient_Group] != '70'
      OR [Der_AEA_Patient_Group] IS NULL
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [Discharge_Destination_SNOMED_CT] != '305398007'
      OR [Discharge_Destination_SNOMED_CT] IS NULL
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [EC_Discharge_Status_SNOMED_CT] IN (
       '182992009',
       '1077081000000104',
       '1077031000000103',  
       '1324201000000109',
       '1077021000000100',
       '1077781000000101',
       '1077061000000108',
       '1077041000000107',
       '1077101000000105',
       '1077051000000105',
       '1077071000000101',
       '1077091000000102',
       '2304481000000106'
       ) 
      OR [EC_Discharge_Status_SNOMED_CT] IS NULL
      THEN 0 
      ELSE 1
    END