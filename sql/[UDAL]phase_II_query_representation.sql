/*
 README 
 How representative are the provider samples used in our analyses?
 This script is used to query [Reporting_MESH_ECDS].[EC_Core_Monthly_Snapshot]
 which is the monthly version of the ECDS on UDAL.
 Based on financial year 2024/25.
 */

SELECT [Der_Provider_Code],
  [Der_Provider_Site_Code],
  CASE
    WHEN Discharge_Destination_SNOMED_CT IN (
      '306706006',
      '1874161000000104',
      '1066361000000104',
      '1066371000000106',
      '1066381000000108',
      '1066391000000105',
      '1066401000000108'
    ) THEN 1
    ELSE 0
  END AS is_adm,
  CASE
    WHEN ec_arrival_mode_snomed_ct IN (
      '1048031000000100',
      '1048041000000109',
      '1048021000000102',
      '1048051000000107',
      '1048081000000101'
    ) THEN 1
    ELSE 0
  END AS is_ambulance,
  /*CASE
    WHEN EC_Department_Type = '01' THEN 1
    ELSE 0
  END AS is_type1,
  */
  CASE
    WHEN [EC_Department_Type] = '03' THEN 1
    ELSE 0
  END AS is_type3,
  CASE
    WHEN Der_Age_At_CDS_Activity_Date < 18 THEN 1
    ELSE 0
  END AS age_under_18,
  CASE
    WHEN Der_Age_At_CDS_Activity_Date > 74 THEN 1
    ELSE 0
  END AS age_75_plus,
  CASE
    WHEN Der_EC_Duration >= 0
    AND Der_EC_Duration < 240 THEN 1
    WHEN Der_EC_Duration IS NULL THEN NULL
    ELSE 0
  END AS 'under_4hrs',
  CASE
    WHEN Der_EC_Duration >= 0
    AND Der_EC_Duration < 720 THEN 1
    WHEN Der_EC_Duration IS NULL THEN NULL
    ELSE 0
  END AS 'under_12hrs',
  CASE
    WHEN [Index_Of_Multiple_Deprivation_Decile] IN (1, 2) THEN 1
    ELSE 0
  END AS imd_quint_1,
  COUNT (*) AS n
FROM UDAL_Warehouse.Reporting_MESH_ECDS.EC_Core_Monthly_Snapshot ec
WHERE 1 = 1
  AND [Der_Financial_Year] = '2024/25'
  AND [Der_Dupe_Flag] = 0
  AND [Finished_Indicator] = 1
  /* AN UNPLANNED FIRST (I.E. NOT FOLLOW-UP / UNKNOWN):*/
  AND [EC_Department_Type] IN ('01', '03')
  AND [EC_AttendanceCategory] = '1'
  AND [Sex] IN ('1', '2')
  AND [Der_Age_At_CDS_Activity_Date] BETWEEN 0 AND 110
  /* NOT (BROUGHT IN DEAD OR DIED DURING ATTENDANCE):*/
  AND (
    (NOT [Der_AEA_Patient_Group] = '70')
    OR [Der_AEA_Patient_Group] IS NULL
  )
  AND (
    (
      NOT [Discharge_Destination_SNOMED_CT] = '305398007'
    )
    OR [Discharge_Destination_SNOMED_CT] IS NULL
  )
  /*
   TREATMENT COMPLETED OR 
   STREAMED: TO SDEC / TO UTC / IP / GP / ED / OPHTH / MH /
   FRAILTY / DENT / PHARM / FALLS / SCHEDULED RETURN
   */
  AND (
    [EC_Discharge_Status_SNOMED_CT] IN (
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
  )
GROUP BY [Der_Provider_Code],
  [Der_Provider_Site_Code],
  CASE
    WHEN Discharge_Destination_SNOMED_CT IN (
      '306706006',
      '1874161000000104',
      '1066361000000104',
      '1066371000000106',
      '1066381000000108',
      '1066391000000105',
      '1066401000000108'
    ) THEN 1
    ELSE 0
  END,
  CASE
    WHEN ec_arrival_mode_snomed_ct IN (
      '1048031000000100',
      '1048041000000109',
      '1048021000000102',
      '1048051000000107',
      '1048081000000101'
    ) THEN 1
    ELSE 0
  END,
  /*CASE
    WHEN EC_Department_Type = '01' THEN 1
    ELSE 0
  END,
  */
  CASE
    WHEN [EC_Department_Type] = '03' THEN 1
    ELSE 0
  END,
  CASE
    WHEN Der_Age_At_CDS_Activity_Date < 18 THEN 1
    ELSE 0
  END,
  CASE
    WHEN Der_Age_At_CDS_Activity_Date > 74 THEN 1
    ELSE 0
  END,
  CASE
    WHEN Der_EC_Duration >= 0
    AND Der_EC_Duration < 240 THEN 1
    WHEN Der_EC_Duration IS NULL THEN NULL
    ELSE 0
  END,
  CASE
    WHEN Der_EC_Duration >= 0
    AND Der_EC_Duration < 720 THEN 1
    WHEN Der_EC_Duration IS NULL THEN NULL
    ELSE 0
  END,
  CASE
    WHEN [Index_Of_Multiple_Deprivation_Decile] IN (1, 2) THEN 1
    ELSE 0
  END