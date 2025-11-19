/*
 README 
 This script is used to query [UDAL_Warehouse].[MESH_ECDS].[EC_Core]
 which is the "faster (returns)" version of the ECDS on UDAL.
 This is the primary query for the project. 
 */
 
SELECT 
  [EC_Ident],
  [NHS_Number_Is_Valid],
  [Index_Of_Multiple_Deprivation_Decile],
  [Sex],
  [Ethnic_Category],
  [EC_Arrival_Mode_SNOMED_CT],
  [EC_Attendance_Source_SNOMED_CT],
  [EC_Discharge_Status_SNOMED_CT],
  [Discharge_Destination_SNOMED_CT],
  [EC_Chief_Complaint_SNOMED_CT],
  [EC_Acuity_SNOMED_CT],
  [Der_Pseudo_NHS_Number],
  [Der_Provider_Code],
  [TRUST_NAME],
  [Der_Provider_Site_Code],
  [SITE_NAME],
  [Der_Age_At_CDS_Activity_Date],
  [Der_Financial_Year],
  [Der_EC_Diagnosis_All],
  [Der_EC_Arrival_Date_Time],
  [Der_EC_Departure_Date_Time]
FROM [MESH_ECDS].[EC_Core] core
  /* TAKE 'CONFIRMED' TYPE 1s ONLY (PROVENANCE OF TABLE UNKNOWN)*/
  INNER JOIN (
    SELECT 
      [SITE_CODE],
      [SITE_NAME],
      [TRUST_NAME]
      FROM [Internal_Reference].[AeMasterSiteList]
    WHERE 1 = 1
      AND CURRENT_FLAG = 1
      AND DEPARTMENT_TYPE = 1
      AND CONFIRMED_T1 = 'Y'
  ) sites 
  ON core.[Der_Provider_Site_Code] = sites.[SITE_CODE]
WHERE 1 = 1
  AND [Der_EC_Arrival_Date_Time] >= '2025-01-01 00:00:00.000'
  /* PROVISIONAL CLAUSES - TO BE CHECKED :*/
  /* NOTE: [SUS_Excluded] CAN BE PAIRED WITH [Exclusions] TO GIVE REASON:*/
  /*AND [SUS_Excluded] = 'False'*/
  AND [Der_Dupe_Flag] = 0
  AND [Finished_Indicator] = 1
  AND [EC_Department_Type] = '01'
  /* AN UNPLANNED FIRST (I.E. NOT FOLLOW-UP / UNKNOWN):*/
  AND [EC_AttendanceCategory] = '1'
  AND [Sex] IN ('1', '2')
  AND [Der_Age_At_CDS_Activity_Date] BETWEEN 0 AND 110
  /* NOT (BROUGHT IN DEAD OR DIED DURING ATTENDANCE):*/
  AND (
      (
      NOT [Der_AEA_Patient_Group] = '70'
      ) 
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
  