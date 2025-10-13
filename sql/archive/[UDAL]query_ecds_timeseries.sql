/*
README 
This script is used to query [UDAL_Warehouse].[MESH_ECDS].[EC_Core]
which is the "faster (submission)" version of the ECDS on UDAL.
This is gives daily timeseries of attendances for given WHERE clause
from start 2023.
*/

SELECT 
  [Arrival_Date], 
  COUNT(*) AS 'n'

 FROM [MESH_ECDS].[EC_Core]
  WHERE 1 = 1
    AND [Der_EC_Arrival_Date_Time] >= '2023-01-01 00:00:00.000'
    
    /* PROVISIONAL CLAUSES - TO BE CHECKED :*/

    /* NOTE: [SUS_Excluded] CAN BE PAIRED WITH [Exclusions] TO GIVE REASON:*/
    AND [SUS_Excluded] = 'False'
    AND [Der_Dupe_Flag] = 0
    AND [Finished_Indicator] = 1
    
    AND [EC_Department_Type] = '01'
    
    /* AN UNPLANNED FIRST (I.E. NOT FOLLOW-UP / UNKNOWN):*/
    AND [EC_AttendanceCategory] = '1'
    
    AND [Sex] IN ('1', '2')
    AND [Der_Age_At_CDS_Activity_Date] BETWEEN 0 AND 110
    
    /* NOT (BROUGHT IN DEAD OR DIED DURING ATTENDANCE):*/
    AND (
      NOT ([Der_AEA_Patient_Group] = '70' OR [Discharge_Destination_SNOMED_CT] = '305398007') 
      )
      
    /*TREATMENT COMPLETED (CHECK WHETHER TO ALSO INCLUDE NULLs):*/
    AND [EC_Discharge_Status_SNOMED_CT] = '182992009'
  
  GROUP BY [Arrival_Date]
