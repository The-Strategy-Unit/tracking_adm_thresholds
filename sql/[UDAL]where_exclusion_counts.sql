SELECT
  [Der_Provider_Code],
  CASE 
    WHEN [SUS_Excluded] = 'False'
      THEN 0 
      ELSE 1 
    END AS 'sus_excluded',
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
      THEN 0 
      ELSE 1
    END AS 'doa',
  CASE 
    WHEN [Discharge_Destination_SNOMED_CT] != '305398007'
      THEN 0 
      ELSE 1
    END AS 'died_ed',
  CASE 
    WHEN [EC_Discharge_Status_SNOMED_CT] = '182992009' 
        OR [EC_Discharge_Status_SNOMED_CT] IS NULL
      THEN 0 
      ELSE 1
    END AS 'streamed/left/died',
  CASE 
    WHEN [EC_Discharge_Status_SNOMED_CT] IS NULL
      THEN 1 
      ELSE 0
    END AS 'disstat_null',
  COUNT(*) AS 'n'
FROM [MESH_ECDS].[EC_Core]
  WHERE 1 = 1
    AND [Der_EC_Arrival_Date_Time] >= '2025-01-01 00:00:00.000'

  GROUP BY 
   [Der_Provider_Code],
   CASE 
    WHEN [SUS_Excluded] = 'False'
      THEN 0 
      ELSE 1 
    END,
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
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [Discharge_Destination_SNOMED_CT] != '305398007'
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [EC_Discharge_Status_SNOMED_CT] = '182992009' 
        OR [EC_Discharge_Status_SNOMED_CT] IS NULL
      THEN 0 
      ELSE 1
    END,
  CASE 
    WHEN [EC_Discharge_Status_SNOMED_CT] IS NULL
      THEN 1 
      ELSE 0
    END