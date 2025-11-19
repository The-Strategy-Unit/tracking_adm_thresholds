/*
README 
This script is used to query [UDAL_Warehouse].[MESH_ECDS].[EC_Core]
which is the "faster (submission)" version of the ECDS on UDAL.
This is the primary query for the project. 
*/

SELECT
      [EC_Ident]
      /* 
     ,[EC_Load_Id]
     ,[EC_PCD_Indicator]
     ,[Generated_Record_ID]
     ,[Interchange_Identifier]
     ,[Interchange_Received_Date]
     ,[CDS_Message_Version_Number]
     ,[Protocol_identifier]
     ,[Unique_CDS_identifier]
     ,[Bulk_Replacement_CDS_Group]
       [CDS_Extract_Date]
      ,[CDS_Extract_Time]
      ,[Report_Period_Start_Date]
      ,[Report_Period_End_Date]
      ,[Update_Type]
      ,[CDS_Applicable_Date]
      ,[CDS_Applicable_Time]
      [Record_Version]
      ,[Record_Confidentiality_Category]
      ,[CDS_Record_Identifier]
      ,[CDS_Type]
      ,[CDS_Group_Indicator]
      ,[Finished_Indicator]
      ,[CDS_Activity_Date]
      ,[CDS_Interchange_Sender_Identity]
      ,[CDS_Prime_Recipient_Identity]
      */
      ,[Provider_Code]
      /*
      ,[Commissioner_Code]
      ,[Query_Identifier]
      ,[Query_Date]
      ,[QueryTime]
      ,[SNOMED_Version]
      ,[Unique_Booking_Reference_Number_Converted]
      ,[UBRNIssuer]
      ,[Org_Code_Patient_Pathway_ID_Issuer]
      ,[RTT_Period_Status]
      ,[Wait_Time_Measurement_Type]
      ,[RTT_Period_Start_Date]
      ,[RTT_Period_End_Date]
      ,[RTT_Period_Length]
      ,[NHS_Number_Status_Indicator_Code]
       */
      ,[NHS_Number_Is_Valid]
         /*
      ,[Local_Patient_ID]
      ,[Org_Code_Local_Patient_ID]
      ,[Withheld_Identity_Reason]
      */
       /*
      ,[Month_of_Birth]
      ,[Year_of_Birth]
      ,[Patient_Type]
      ,[Age_at_CDS_Activity_Date]
      ,[Age_At_Arrival]
      ,[Fractional_Age_At_Arrival]
      ,[HES_Age_At_Arrival]
      ,[HES_Age_At_Departure]
      ,[Postcode_District]
      ,[2011_Output_Area]
      ,[2011_MS0A]
      ,[2011_LSOA]
      ,[Country]
      ,[Government_Office_Region]
       */
         /*
      ,[Residence_County]
      ,[Local_Authority_District]
      ,[Parliamentary_Constituency]
      ,[ED_County_Code]
      ,[ED_District_Code]
      ,[Electoral_Ward_1998]
       */
      ,[Index_Of_Multiple_Deprivation_Decile]
       /*
      ,[Index_Of_Multiple_Deprivation_Decile_Description]
      ,[Rural_Urban_Indicator]
      ,[Cancer_Registry]
      ,[Grid_Link_Reference]
      ,[Org_Code_Residence_Responsibility]
      ,[Organisation_Id_From_Patient_Postcode]
      ,[HES_CCG_From_Patient_Postcode]
       */
      ,[Sex]
      ,[Ethnic_Category]
         /*
      ,[Accommodation_Status_SNOMED_CT]
      ,[Accommodation_Status_Code_Approved]
      ,[Preferred_Spoken_Language_SNOMED_CT]
       */
         /*
      ,[Preferred_Spoken_Language_Code_Approved]
      ,[Accessible_Information_Professional_Required_SNOMED_CT]
      ,[Accessible_Information_Professional_Required_Code_Approved]
      ,[Interpreter_Language_SNOMED_CT]
      ,[Interpreter_Language_Code_Approved]
      ,[Overseas_Visitor_Charging_Category]
      ,[GP_Code]
      ,[GP_Practice_Code]
      ,[PDS_General_Practice_Code]
      ,[Responsible_CCG_From_General_Practice]
      ,[HES_CCG_From_General_Practice]
      ,[HES_Responsible_CCG_Value]
      ,[HES_Responsible_CCG_Origin]
      ,[Attendance_Unique_Identifier]
      ,[Site_Code_of_Treatment]
      ,[Attendance_Postcode_District]
      ,[Attendance_Government_Office_Region]
      ,[Attendance_SHA_Provider]
      ,[Attendance_HES_CCG_From_Treatment_Site_Code]
      ,[Attendance_HES_CCG_From_Treatment_Origin]
      ,[EC_Department_Type]
      ,[Attendance_LSOA_Provider_Distance]
      ,[Attendance_LSOA_Provider_Distance_Origin]
      */
      /*
      ,[Attendance_LSOA_Treatment_Site_Distance]
      ,[Attendance_LSOA_Treatment_Site_Distance_Origin]
      ,[EC_Attendance_Number]
      ,[Arrival_Date]
      ,[Arrival_Time]
      ,[Arrival_Month] 
      */
      ,[EC_Arrival_Mode_SNOMED_CT]
         /*
      ,[Arrival_Mode_Approved]
      ,[EC_AttendanceCategory]
      ,[Arrival_Planned]
      ,[Ambulance_Incident_Number]
      ,[Org_Code_Ambulance_Trust]
       */
      ,[EC_Attendance_Source_SNOMED_CT]
         /*
      ,[Attendance_Source_Approved]
      ,[Attendance_Source_Organisation]
      ,[EC_Initial_Assessment_Date]
      ,[EC_Initial_Assessment_Time]
      ,[EC_Initial_Assessment_Time_Since_Arrival]
      ,[EC_Seen_For_Treatment_Date]
      ,[EC_Seen_For_Treatment_Time]
      ,[EC_Seen_For_Treatment_Time_Since_Arrival]
      ,[EC_Conclusion_Date]
      ,[EC_Conclusion_Time]
      ,[EC_Conclusion_Time_Since_Arrival]
      ,[EC_Departure_Date]
      ,[EC_Departure_Time]
      ,[EC_Departure_Time_Since_Arrival]
      ,[EC_Decision_To_Admit_Date]
      ,[EC_Decision_To_Admit_Time]
      ,[EC_Decision_To_Admit_Time_Since_Arrival]
      ,[Decision_To_Admit_Treatment_Function_Code]
      ,[Decision_To_Admit_Receiving_Site]
       */
      ,[EC_Discharge_Status_SNOMED_CT]
         /*
      ,[EC_Discharge_Status_Approved]
       */
      ,[Discharge_Destination_SNOMED_CT]
         /*
      ,[DischargeDestinationApproved]
      ,[Discharge_Follow_Up_SNOMED_CT]
      ,[Discharge_Follow_Up_Approved]
      ,[Discharge_Information_Given_SNOMED_CT]
      ,[Discharge_Information_Given_Approved]
       */
      ,[EC_Chief_Complaint_SNOMED_CT]
      ,[EC_Acuity_SNOMED_CT]
         /*
      ,[EC_Injury_Date]
      ,[EC_Injury_Time]
      ,[EC_Injury_Activity_Status_SNOMED_CT]
      ,[EC_Injury_Activity_Type_SNOMED_CT]
      ,[EC_Injury_Intent_SNOMED_CT]
      ,[EC_Injury_Mechanism_SNOMED_CT]
      ,[EC_Place_Of_Injury_SNOMED_CT]
      ,[EC_Place_Of_Injury_Latitude]
      ,[EC_Place_Of_Injury_Longitude]
      ,[Clinical_Trial_Code]
      ,[Clinical_Disease_Notification]
      ,[Service_Agreement_Provider]
      ,[Service_Agreement_Commissioner]
      ,[Commissioning_Serial_Number]
      ,[Agreement_Line_Number]
      ,[Provider_Reference_Number]
      ,[Commissioner_Reference_Number]
      ,[SHA_Commissioner]
      ,[Is_Commissioner_Recognised]
      ,[Commissioning_Government_Office_Region]
      ,[SUS_Grouper_Version]
      ,[SUS_Code_Cleaning_Applied]
      ,[SUS_HRG_Code]
      ,[SUS_Costing_Period]
      ,[SUS_Excluded]
      ,[SUS_Tariff]
      ,[SUS_Tariff_Description]
      ,[SUS_MFF]
      ,[SUS_MFAdjustment]
      ,[SUS_Final_Price]
      ,[Age_Range]
      ,[Clinical_Chief_Complaint_Code_Approved]
      ,[Clinical_Chief_Complaint_Injury_Related]
      ,[Clinical_Chief_Complaint_Extended_Code]
      ,[Clinical_Acuity_Code_Approved]
      ,[Clinical_Injury_Activity_Status_Code_Approved]
      ,[Clinical_Injury_Activity_Type_Code_Approved]
      ,[Clinical_Injury_Intent_Code_Approved]
      ,[Clinical_Injury_Place_Type_Approved]
      ,[DQ_Chief_Complaint_Expected]
      ,[DQ_Chief_Complaint_Completed]
      ,[DQ_Chief_Complaint_Valid]
      ,[DQ_Primary_Diagnosis_Expected]
      ,[DQ_Primary_Diagnosis_Completed]
      ,[DQ_Primary_Diagnosis_Valid]
      ,[Exclusions]
      ,[Reason_For_Access]
       */
      ,[Der_Pseudo_NHS_Number]
      ,[Der_Provider_Code]
      ,[Der_Provider_Site_Code]
         /*
      ,[Der_Commissioner_Code]*/
      ,[Der_Age_At_CDS_Activity_Date]
      /*, [Der_Activity_Month]*/
      ,[Der_Financial_Year]
      /* ,[Der_Number_AEA_Diagnosis]
      ,[Der_Number_EC_Diagnosis]
      */
      /*
      ,[Der_Number_AEA_Investigation]
      ,[Der_Number_EC_Investigation]
      ,[Der_Number_AEA_Treatment]
      ,[Der_Number_EC_Treatment]
      ,[Der_AEA_Diagnosis_All]
       */
      ,[Der_EC_Diagnosis_All]
       /*
      ,[Der_AEA_Investigation_All]
      ,[Der_EC_Investigation_All]
      ,[Der_AEA_Treatment_All]
      ,[Der_EC_Treatment_All]*/
      ,[Der_EC_Arrival_Date_Time]
      ,[Der_EC_Departure_Date_Time]
         /*
      ,[Der_EC_Duration]
      ,[Der_Postcode_CCG_Code]
      ,[Der_Postcode_PCT_Code]
      ,[Der_Postcode_Sector]
      ,[Der_Postcode_Local_Auth]
      ,[Der_Postcode_Dist_Unitary_Auth]
      ,[Der_Postcode_Electoral_Ward]
      */
      ,[Der_Postcode_LSOA_Code]
         /*
      ,[Der_Postcode_MSOA_Code]
      ,[Der_Postcode_Constituency_Code]
      ,[Der_Dupe_Flag]
      */
      ,[Der_Record_Type]
      /*
      ,[Der_Pseudo_Patient_Pathway_ID]
      ,[Der_AEA_Patient_Group]
      */
      ,[Der_Postcode_LSOA_2011_Code]
         /*
      ,[Der_Postcode_MSOA_2011_Code]
      */
      ,[Der_Postcode_LSOA_2021_Code]
         /*
      ,[Der_Postcode_MSOA_2021_Code]
      ,[Clinically_Ready_To_Proceed_Timestamp]
      ,[Clinically_Ready_To_Proceed_Time_Since_Arrival]
      */
      /*
      ,[Ambulance_Care_Contact_Identifier]
      ,[Clinical_Disease_Notification_Code]
      ,[Clinical_Disease_Notification_Description]
      ,[Ethnic_Category_2021]
      ,[Deleted]
      ,[pseudo_nhs_number_pet]
      ,[UDALFileID]
      */
  FROM [MESH_ECDS].[EC_Core]
  WHERE 1 = 1
    AND [Der_EC_Arrival_Date_Time] >= '2025-01-01 00:00:00.000'
    
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
    
    /* NOT (BROUGHT IN DEAD OR DIED DURING ATTENDANCE): */
    AND (
      (NOT [Der_AEA_Patient_Group] = '70') OR [Der_AEA_Patient_Group] IS NULL
      )
    AND (
      (NOT [Discharge_Destination_SNOMED_CT] = '305398007') OR [Discharge_Destination_SNOMED_CT] IS NULL
      )
    /*AND (
      NOT ([Der_AEA_Patient_Group] = '70' OR [Discharge_Destination_SNOMED_CT] = '305398007') 
      )
    */
    
    /*TREATMENT COMPLETED (CHECK WHETHER TO ALSO INCLUDE NULLs):*/
    AND [EC_Discharge_Status_SNOMED_CT] = '182992009'
