create or replace PACKAGE BODY       Msosect
IS
/* 7/1/2004     JMB      Change campus translation to include new 'U' campus */
/* 1/20/2004   Tang  Add new field ws_sched_code */
/* 4/20/2004    Tang Removed update of 20_percent_date to speed up refresh  */
/* 6/21/2006    Tang  Added three columns; intg_code, intg_desc, intp_code   */
/* 3/3/2010     CM    Modified logic used to determine campuses for classes held at Shady Grove   */
/* 3/3/2010     CM    Modified logic used to determine campuses for rsts = XX records to match logic used to determine campuses earlier in procedure */
/* 3/3/2010     CM    Removed join to SFRSTCR when looking up description for RSTS code */
/* 9/18/2011    CM   Removed ELSE statement in SCBCRSE section.  Was causing contact hours to be set to zero when a non-zero contact
                      hours value set in SSBSECT.  This ELSE was overriding previous value retrieved from SSBSECT. */
/* 4/10/2012	Tang  when ws_crse_type_flag = 'N', treat it same as 'D'   */
/* 10/9/2015  Tang  testing, no change to code, only added this comment line */


   PROCEDURE p_update (
      p_stcr_term_code   VARCHAR2,
      p_stcr_pidm        NUMBER,
      p_stcr_crn         VARCHAR2
   )
   IS
      ws_crn_levl                SFRSTCR.sfrstcr_levl_code%TYPE;
      ws_crse_type_flag          AELP_DEV.crse_type_flag%TYPE;
      ws_aelp_ind                AELP_DEV.crse_type_flag%TYPE;
      ws_dev_ind                 AELP_DEV.crse_type_flag%TYPE;
      ws_ptrm_code               SFRSTCR.sfrstcr_ptrm_code%TYPE;
      ws_rsts_code               SFRSTCR.sfrstcr_rsts_code%TYPE;
      ws_rsts_desc               STVRSTS.stvrsts_desc%TYPE;
      ws_rsts_date               SFRSTCR.sfrstcr_rsts_date%TYPE;
      ws_rsts_count              STVRSTS.stvrsts_incl_sect_enrl%TYPE;
      ws_gmod_code               SFRSTCR.sfrstcr_gmod_code%TYPE;
      ws_crse_camp_detail_code   SFRSTCR.sfrstcr_camp_code%TYPE;
      ws_course_campus_code      SFRSTCR.sfrstcr_camp_code%TYPE;
      ws_course_campus           STVCAMP.stvcamp_desc%TYPE;
      ws_bill_hr                 SFRSTCR.sfrstcr_bill_hr%TYPE          := 0;
      ws_credit_hr               SFRSTCR.sfrstcr_credit_hr%TYPE        := 0;
      ws_contact_hr              SSBSECT.ssbsect_cont_hr%TYPE          := 0;
      ws_scbcrse_contact_hr      SCBCRSE.scbcrse_cont_hr_low%TYPE      := 0;
      ws_subject_code            SSBSECT.ssbsect_subj_code%TYPE;
      ws_course_number           SSBSECT.ssbsect_crse_numb%TYPE;
      ws_subject_course_number   VARCHAR2 (9);
      ws_course_title            SCBCRSE.scbcrse_title%TYPE;
      ws_reg_modality            SFRSTCR.sfrstcr_user%TYPE;
      ws_start_date              SSRMEET.ssrmeet_start_date%TYPE;
      ws_end_date                SSRMEET.ssrmeet_end_date%TYPE;
      ws_begin_time              SSRMEET.ssrmeet_begin_time%TYPE;
      ws_end_time                SSRMEET.ssrmeet_end_time%TYPE;
      --   ws_20_percent_date         DATE;
      ws_day_eve_ind             VARCHAR2 (1);
      ws_bldg_code               SSRMEET.ssrmeet_bldg_code%TYPE;
      ws_bldg_desc               STVBLDG.stvbldg_desc%TYPE;
      ws_room_code               SSRMEET.ssrmeet_room_code%TYPE;
      ws_meet_no                 SSRMEET.ssrmeet_meet_no%TYPE;
      /* 3 new fields added Feb 2003*/
      ws_grde_code_mid           SFRSTCR.sfrstcr_grde_code_mid%TYPE;
      ws_grde_code_eot           SFRSTCR.sfrstcr_grde_code%TYPE;
      ws_course_dept_code        SSBOVRR.ssbovrr_dept_code%TYPE;
      /*end of fields added Feb 2003*/
      ws_sect_camp_detail_code   SSBSECT.ssbsect_camp_code%TYPE;
      ws_sect_camp_code          SSBSECT.ssbsect_camp_code%TYPE;
      ws_sect_ptrm_code          SSBSECT.ssbsect_ptrm_code%TYPE;
      ws_crse_crn_levl           SFRSTCR.sfrstcr_levl_code%TYPE;
      ws_dept_camp_code          STVCAMP.stvcamp_code%TYPE;
      ws_section_numb            SSBSECT.ssbsect_seq_numb%TYPE;
      ws_schd_code               SSBSECT.ssbsect_schd_code%TYPE;
      ws_intg_code               SSBSECT.ssbsect_intg_cde%TYPE;
      ws_intg_desc               GORINTG.gorintg_desc%TYPE;
      ws_intp_code               GORINTG.gorintg_intp_code%TYPE;
      SECTION                    VARCHAR2 (30);
      errcode                    NUMBER (5);
   BEGIN
/*************************************/
      BEGIN
         SECTION := 'SFRSTCR';

         SELECT sfrstcr_bill_hr, sfrstcr_camp_code, sfrstcr_credit_hr,
                sfrstcr_rsts_code, sfrstcr_rsts_date, sfrstcr_gmod_code,
                sfrstcr_levl_code, sfrstcr_ptrm_code, LTRIM (sfrstcr_user),
/**midterm and end-of-term grades added Jan. 2003*/
                sfrstcr_grde_code_mid, sfrstcr_grde_code
           INTO ws_bill_hr, ws_crse_camp_detail_code, ws_credit_hr,
                ws_rsts_code, ws_rsts_date, ws_gmod_code,
                ws_crn_levl, ws_ptrm_code, ws_reg_modality,
                ws_grde_code_mid, ws_grde_code_eot
           FROM SFRSTCR
          WHERE sfrstcr_term_code = p_stcr_term_code
            AND sfrstcr_pidm = p_stcr_pidm
            AND sfrstcr_crn = p_stcr_crn;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_bill_hr := 0;
            ws_crse_camp_detail_code := NULL;
            ws_credit_hr := 0;
            ws_rsts_code := 'XX';
            ws_rsts_date := SYSDATE;
            ws_gmod_code := NULL;
            ws_crn_levl := NULL;
            ws_ptrm_code := NULL;
            ws_reg_modality := NULL;
            ws_course_campus_code := NULL;
            ws_course_campus := NULL;
            ws_grde_code_mid := NULL;
            ws_grde_code_eot := NULL;
      END;

/*************************************/
      BEGIN
         SECTION := 'SSRMEET';
           /*there is an unresolved problem regarding bldg_code, room_code */

         SELECT
           /*and meet_no when there are multiple rows in SSRMEET           */
                MIN (ssrmeet_start_date),
                                         /*i have only chosen to use min here so that data will load     */
                                         MAX (ssrmeet_end_date),
                MIN (ssrmeet_begin_time), MAX (ssrmeet_end_time),
                MIN (ssrmeet_bldg_code), MIN (ssrmeet_room_code),
                MIN (ssrmeet_meet_no)
           INTO ws_start_date, ws_end_date,
                ws_begin_time, ws_end_time,
                ws_bldg_code, ws_room_code,
                ws_meet_no
           FROM SSRMEET
          WHERE ssrmeet_term_code = p_stcr_term_code
            AND ssrmeet_crn = p_stcr_crn;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_start_date := NULL;
            ws_end_date := NULL;
            ws_begin_time := NULL;
            ws_end_time := NULL;
            ws_bldg_code := NULL;
            ws_room_code := NULL;
            ws_meet_no := NULL;
      END;

/*************************************/
-- 1/20/2004 Add new field ws_sched_code
      BEGIN
         SECTION := 'SSBSECT';

         SELECT ssbsect_subj_code, ssbsect_crse_numb,
                ssbsect_subj_code || ssbsect_crse_numb, ssbsect_cont_hr,
                ssbsect_camp_code, ssbsect_seq_numb, ssbsect_schd_code
           INTO ws_subject_code, ws_course_number,
                ws_subject_course_number, ws_contact_hr,
                ws_sect_camp_detail_code, ws_section_numb, ws_schd_code
           FROM SSBSECT
          WHERE ssbsect_crn = p_stcr_crn
            AND ssbsect_term_code = p_stcr_term_code;

         --follwing condition added 9/7/01 JMB to account for deleted course registration
         IF ws_rsts_code = 'XX'
         THEN
            ws_contact_hr := 0;
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_subject_code := NULL;
            ws_course_number := NULL;
            ws_subject_course_number := NULL;
            ws_contact_hr := 0;
            ws_sect_camp_detail_code := NULL;
      END;

/*************************************/
   --this section enabled Feb 2003 to populate new department field
      SECTION := 'GET_COURSE_DEPT';

      BEGIN
         SECTION := 'student_dept_data';
         ws_course_dept_code :=
            Mc_F_Student_Dept_Data (p_stcr_term_code,
                                    p_stcr_crn,
                                    ws_subject_code,
                                    ws_course_number
                                   );
         ws_dept_camp_code := SUBSTR (ws_course_dept_code, 1, 1);
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_course_dept_code := NULL;
            ws_dept_camp_code := NULL;
         WHEN OTHERS
         THEN
            ws_course_dept_code := NULL;
            ws_dept_camp_code := NULL;
      END;

   /*************************************/
--new section added Aug 2003 due to addition of 'D' campus code (see above)
-- CM 03/2010  Modified section to handle camp detail code = S in the same manner as camp detail code = D.
--
      SECTION := 'CAMPUS_TRANSLATION';

      IF     (ws_crse_camp_detail_code = 'D'
            OR ws_crse_camp_detail_code = 'S')
         AND ws_dept_camp_code = 'R'
      THEN
         ws_crse_camp_detail_code := 'L';
         ws_course_campus_code := 'R';
         ws_course_campus := 'Rockville';
      ELSIF     (ws_crse_camp_detail_code = 'D'
              OR ws_crse_camp_detail_code = 'S')
            AND ws_dept_camp_code = 'T'
      THEN
         ws_crse_camp_detail_code := 'M';
         ws_course_campus_code := 'T';
         ws_course_campus := 'Takoma Park/Silver Spring';
      ELSIF     (ws_crse_camp_detail_code = 'D'
              OR ws_crse_camp_detail_code = 'S')
            AND ws_dept_camp_code = 'G'
      THEN
         ws_crse_camp_detail_code := 'K';
         ws_course_campus_code := 'G';
         ws_course_campus := 'Germantown';
      ELSIF     (ws_crse_camp_detail_code = 'D'
            OR ws_crse_camp_detail_code = 'S')
            AND ws_dept_camp_code = 'C'
      THEN
         ws_crse_camp_detail_code := 'C';
         ws_course_campus_code := 'C';
         ws_course_campus := 'Continuing Education';
      ELSIF     (ws_crse_camp_detail_code = 'D'
               OR ws_crse_camp_detail_code = 'S')
            AND (   ws_dept_camp_code NOT IN ('R', 'T', 'G', 'C')
                 OR ws_dept_camp_code IS NULL
                )
      THEN
         ws_crse_camp_detail_code := ws_dept_camp_code;
         ws_course_campus_code := '?';
         ws_course_campus := 'Unknown';
--
--  code below commented out
--
      /*
      ELSIF ws_crse_camp_detail_code IN ('G', 'P', 'K')
      THEN
         ws_course_campus_code := 'G';
         ws_course_campus := 'Germantown';
      ELSIF ws_crse_camp_detail_code IN ('O', 'C', 'H', 'E', 'R', 'L')
      THEN
         ws_course_campus_code := 'R';
         ws_course_campus := 'Rockville';
      ELSIF ws_crse_camp_detail_code IN ('T', 'N', 'M', 'U')
      THEN
         ws_course_campus_code := 'T';
         ws_course_campus := 'Takoma Park/Silver Spring';
      ELSE
         ws_course_campus_code := 'R';
         ws_course_campus := 'Rockville';*/
--
-- end of commented section
--
      ELSE
         ws_course_campus_code :=
            Mc_F_Decode_Camp_Code (ws_crse_camp_detail_code,
                                   ws_dept_camp_code
                                  );

         SELECT stvcamp_desc
           INTO ws_course_campus
           FROM STVCAMP
          WHERE stvcamp_code = ws_course_campus_code;
      END IF;

/*************************************/
      BEGIN
         SECTION := 'STVRSTS';

--
--  CM 03/2010  Removed join to SFRSTCR - extra work
--

         SELECT stvrsts_desc,
		stvrsts_incl_sect_enrl
           INTO ws_rsts_desc,
		ws_rsts_count
           FROM STVRSTS
--                SFRSTCR
          WHERE stvrsts_code = ws_rsts_code;
--
-- sfrstcr_pidm = p_stcr_pidm
--            AND sfrstcr_term_code = p_stcr_term_code
--            AND sfrstcr_crn = p_stcr_crn
--            AND stvrsts_code = ws_rsts_code;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_rsts_desc := 'Deleted by MC';
            ws_rsts_count := 'N';
      END;

/*************************************/
      BEGIN
         SECTION := 'STVBLDG';

         SELECT stvbldg_desc
           INTO ws_bldg_desc
           FROM STVBLDG
          WHERE stvbldg_code = ws_bldg_code;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_bldg_desc := NULL;
      END;

/*************************************/
      BEGIN
         SECTION := 'AELP_DEV';

         SELECT crse_type_flag
           INTO ws_crse_type_flag
           FROM AELP_DEV
          WHERE SUBSTR (p_stcr_term_code, 1, 4) = ay
            AND ws_subject_code = subj_code
            AND SUBSTR (ws_course_number, 1, 3) = crse_numb;

         IF ws_crse_type_flag = 'A'
         THEN
            ws_aelp_ind := 'A';
         ELSIF ws_crse_type_flag in ('N', 'D')
         THEN
            ws_dev_ind := ws_crse_type_flag;
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_crse_type_flag := NULL;
            ws_aelp_ind := NULL;
            ws_dev_ind := NULL;
         WHEN OTHERS
         THEN
            ws_aelp_ind := NULL;
            ws_dev_ind := NULL;
      END;

  /*************************************/
/*   IF ws_contact_hr is null THEN      /* this section was incorporated into the next one (SCBCRSE)6/20/2001 */
/*   BEGIN
     section := 'SCBCRSE_CONT_HR';
     SELECT
            a.scbcrse_cont_hr_low
       INTO
            ws_contact_hr
       FROM SCBCRSE a
      WHERE a.scbcrse_subj_code = ws_subject_code
        AND a.scbcrse_crse_numb = ws_course_number
        AND a.scbcrse_eff_term = (select max(b.scbcrse_eff_term)
                                from scbcrse b
                            where a.scbcrse_subj_code = b.scbcrse_subj_code
                              and a.scbcrse_crse_numb = b.scbcrse_crse_numb
                       and b.scbcrse_eff_term <= p_stcr_term_code);
  EXCEPTION
     WHEN NO_DATA_FOUND THEN
      ws_contact_hr := NULL;
  END;
  END IF;
  /*************************************/
      BEGIN
         SECTION := 'SCBCRSE';

         SELECT a.scbcrse_title, a.scbcrse_cont_hr_low,
                DECODE (a.scbcrse_ceu_ind, 'Y', 'NC', NULL, 'CR', '?')
           INTO ws_course_title, ws_scbcrse_contact_hr,
                ws_crse_crn_levl
           FROM SCBCRSE a
          WHERE a.scbcrse_subj_code = ws_subject_code
            AND a.scbcrse_crse_numb = ws_course_number
            AND a.scbcrse_eff_term =
                   (SELECT MAX (b.scbcrse_eff_term)
                      FROM SCBCRSE b
                     WHERE a.scbcrse_subj_code = b.scbcrse_subj_code
                       AND a.scbcrse_crse_numb = b.scbcrse_crse_numb
                       AND b.scbcrse_eff_term <= p_stcr_term_code);

         --following condition modified 9/7/01 to account for deleted course registrations
         IF (    ws_contact_hr IS NULL
             AND ws_rsts_code <> 'XX')
         THEN
            ws_contact_hr := ws_scbcrse_contact_hr;
--
--  CM 08/11  Removed ELSE statement.  Was causing contact hours to be set to zero when a non-zero contact
--            hours value set in SSBSECT.  This ELSE was overriding previous value retrieved from SSBSECT.
--
--         ELSE
--            ws_contact_hr := 0;
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            ws_course_title := NULL;
            ws_scbcrse_contact_hr := 0;
            ws_crse_crn_levl := NULL;
      END;

/*************************************/
      BEGIN
         SECTION := 'SECT DAY/EVENING IND';

         IF ws_begin_time >= 1700
         THEN
            ws_day_eve_ind := 'E';
         ELSIF ws_begin_time < 1700
         THEN
            ws_day_eve_ind := 'D';
         /*logic added to assign "U - Unknown" value to classes with no begin time*/
         /*requested by Spike Yancy of OIRA*/
         /*change completed 8/2/01 by J Bruno*/
         ELSIF ws_begin_time IS NULL
         THEN
            ws_day_eve_ind := 'U';
         END IF;
      EXCEPTION
         WHEN OTHERS
         THEN
            ws_day_eve_ind := NULL;
      END;

   /*************************************/
--this section enabled 9/10/01, using rsts code 'DN' in function to calculate 20% date  JMB
 /*
      BEGIN
      section := 'GET_20_PERCENT_DATE';
      IF ((p_stcr_crn is not null) and (p_stcr_term_code is not null) and (ws_ptrm_code is not null)) THEN
      ws_20_percent_date := f_get_percent_date(p_stcr_crn, p_stcr_term_code,
      ws_ptrm_code, 'DN');
         ELSE ws_20_percent_date := NULL;
      END IF;
    EXCEPTION
     WHEN NO_DATA_FOUND THEN
      ws_20_percent_date := NULL;
    END;
*/
   /*************************************/
   --this section enabled 11/30/01, using new 20_pct_function
 /*

 This section commented out to speed up refresh    4/20/2004

 IF ws_start_date IS NOT NULL
      THEN
         BEGIN
            section := 'GET_20_PERCENT_DATE';
            ws_20_percent_date := f_get_20_pct_date (p_stcr_crn, p_stcr_term_code);
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               ws_20_percent_date := NULL;
            WHEN OTHERS
            THEN
               ws_20_percent_date := NULL;
         END;
      END IF;
*/
/*************************************/
      IF ws_rsts_code = 'XX'
      THEN
         /*this section enabled Feb 2003 to retain course info for deleted registrations*/
--
--  CM 03/2010  Now uses the same logic to determine campuses as used earlier in procedure
--

         BEGIN
            SECTION := 'GET DELETED COURSE INFO';

            ws_crse_camp_detail_code := ws_sect_camp_detail_code;

            ws_crn_levl := ws_crse_crn_levl;
            ws_ptrm_code := ws_sect_ptrm_code;


      IF     (ws_crse_camp_detail_code = 'D'
            OR ws_crse_camp_detail_code = 'S')
         AND ws_dept_camp_code = 'R'
      THEN
         ws_crse_camp_detail_code := 'L';
         ws_course_campus_code := 'R';
         ws_course_campus := 'Rockville';
      ELSIF     (ws_crse_camp_detail_code = 'D'
              OR ws_crse_camp_detail_code = 'S')
            AND ws_dept_camp_code = 'T'
      THEN
         ws_crse_camp_detail_code := 'M';
         ws_course_campus_code := 'T';
         ws_course_campus := 'Takoma Park/Silver Spring';
      ELSIF     (ws_crse_camp_detail_code = 'D'
              OR ws_crse_camp_detail_code = 'S')
            AND ws_dept_camp_code = 'G'
      THEN
         ws_crse_camp_detail_code := 'K';
         ws_course_campus_code := 'G';
         ws_course_campus := 'Germantown';
      ELSIF     (ws_crse_camp_detail_code = 'D'
            OR ws_crse_camp_detail_code = 'S')
            AND ws_dept_camp_code = 'C'
      THEN
         ws_crse_camp_detail_code := 'C';
         ws_course_campus_code := 'C';
         ws_course_campus := 'Continuing Education';
      ELSIF     (ws_crse_camp_detail_code = 'D'
               OR ws_crse_camp_detail_code = 'S')
            AND (   ws_dept_camp_code NOT IN ('R', 'T', 'G', 'C')
                 OR ws_dept_camp_code IS NULL
                )
      THEN
         ws_crse_camp_detail_code := ws_dept_camp_code;
         ws_course_campus_code := '?';
         ws_course_campus := 'Unknown';
      ELSE
         ws_course_campus_code :=
            Mc_F_Decode_Camp_Code (ws_crse_camp_detail_code,
                                   ws_dept_camp_code
                                  );

         SELECT stvcamp_desc
           INTO ws_course_campus
           FROM STVCAMP
          WHERE stvcamp_code = ws_course_campus_code;
      END IF;
  END;

END IF;


--
-- old  CODE
--
--            IF ws_crse_camp_detail_code IN ('G', 'P', 'K')
--            THEN
--               ws_course_campus_code := 'G';
--               ws_course_campus := 'Germantown';
--            ELSIF ws_crse_camp_detail_code IN ('O', 'C', 'H', 'E', 'R', 'L')
--            THEN
--               ws_course_campus_code := 'R';
--               ws_course_campus := 'Rockville';
--            ELSIF ws_crse_camp_detail_code IN
--                                  ('T', 'N', 'M', 'U') --'U' code added 7/1/04
--            THEN
--               ws_course_campus_code := 'T';
--               ws_course_campus := 'Takoma Park/Silver Spring';
--            ELSE
--               ws_course_campus_code := 'R';
--               ws_course_campus := 'Rockville';
----            END IF;
--         EXCEPTION
--            WHEN NO_DATA_FOUND
--            THEN
--               ws_sect_camp_code := NULL;
--               ws_crse_camp_detail_code := NULL;
--               ws_course_campus_code := NULL;
--               ws_course_campus := NULL;
--               ws_crse_crn_levl := NULL;
--               ws_crn_levl := NULL;
--               ws_ptrm_code := NULL;
--               ws_sect_ptrm_code := NULL;
--         END;
--      END IF;



/*************************************/
/* This section added Jun-21-2006    Tang */
      BEGIN
         SECTION := 'Intergration Code';

         SELECT ssbsect_intg_cde
           INTO ws_intg_code
           FROM SSBSECT
          WHERE ssbsect_term_code = p_stcr_term_code
            AND ssbsect_crn = p_stcr_crn;

         SELECT gorintg_desc, gorintg_intp_code
           INTO ws_intg_desc, ws_intp_code
           FROM GORINTG
          WHERE gorintg_integration_cde = ws_intg_code;

      EXCEPTION
         WHEN OTHERS
         THEN
            ws_intg_code := NULL;
            ws_intg_desc := NULL;
            ws_intp_code := NULL;
      END;

/*************************************/
      SECTION := 'UPDATE_MSFSECT';

      UPDATE MSFSECT
         SET msfsect_crn_levl = ws_crn_levl,
             msfsect_aelp_ind = ws_aelp_ind,
             msfsect_dev_ind = ws_dev_ind,
             msfsect_rsts_code = ws_rsts_code,
             msfsect_rsts_desc = ws_rsts_desc,
             msfsect_rsts_date = ws_rsts_date,
             msfsect_rsts_count = ws_rsts_count,
             msfsect_gmod_code = ws_gmod_code,
             msfsect_grde_code_mid = ws_grde_code_mid,
             msfsect_grde_code_eot = ws_grde_code_eot,
             msfsect_crse_camp_detail_code = ws_crse_camp_detail_code,
             msfsect_course_campus_code = ws_course_campus_code,
             msfsect_course_campus = ws_course_campus,
             msfsect_course_dept_code = ws_course_dept_code,
             msfsect_bill_hr = ws_bill_hr,
             msfsect_credit_hr = ws_credit_hr,
             msfsect_contact_hr = ws_contact_hr,
             msfsect_subject_code = ws_subject_code,
             msfsect_course_number = ws_course_number,
             msfsect_subject_course_number = ws_subject_course_number,
             msfsect_course_title = ws_course_title,
             msfsect_reg_modality = ws_reg_modality,
             msfsect_start_date = ws_start_date,
             msfsect_end_date = ws_end_date,
             msfsect_begin_time = ws_begin_time,
             msfsect_end_time = ws_end_time,
               --        The following line was commented out to speed up refresh time
             --     msfsect_20_percent_date = ws_20_percent_date,
             msfsect_day_eve_ind = ws_day_eve_ind,
             msfsect_bldg_code = ws_bldg_code,
             msfsect_bldg_desc = ws_bldg_desc,
             msfsect_room_code = ws_room_code,
             msfsect_meet_no = ws_meet_no,
             msfsect_activity_date = SYSDATE,
             msfsect_change_flag = 'N',
             msfsect_section_numb = ws_section_numb,
             msfsect_schd_code = ws_schd_code,
             msfsect_intg_code = ws_intg_code,
             msfsect_intg_desc = ws_intg_desc,
             msfsect_intp_code = ws_intp_code
       WHERE msfsect_term_code = p_stcr_term_code
         AND msfsect_pidm = p_stcr_pidm
         AND msfsect_crn = p_stcr_crn;
   EXCEPTION
      WHEN OTHERS
      THEN
         BEGIN
            P_Merrors ('MSOSECT', p_stcr_pidm, p_stcr_term_code, p_stcr_crn);

            UPDATE MSFSECT
               SET msfsect_change_flag = 'N'
             WHERE msfsect_term_code = p_stcr_term_code
               AND msfsect_pidm = p_stcr_pidm
               AND msfsect_crn = p_stcr_crn;
         END;
   END p_update;

/*********************************************************************************/
   PROCEDURE p_populate
   IS
      tot_recs          NUMBER (20);
      lv_row_count      NUMBER (9);
      lv_refresh_time   DATE;
   BEGIN
      SELECT SYSDATE
        INTO lv_refresh_time
        FROM DUAL;

      SELECT COUNT (*)
        INTO lv_row_count
        FROM MSFSECT
       WHERE msfsect_change_flag = 'Y';

      <<outer_loop>>
      LOOP
         COMMIT;
         DBMS_TRANSACTION.begin_discrete_transaction;

         DECLARE
            CURSOR c1
            IS
               SELECT msfsect_term_code, msfsect_pidm, msfsect_crn
                 FROM MSFSECT
                WHERE msfsect_change_flag = 'Y'
                  AND ROWNUM <= 1000;
         BEGIN
            FOR c1_index IN c1
            LOOP
               Msosect.p_update (c1_index.msfsect_term_code,
                                 c1_index.msfsect_pidm,
                                 c1_index.msfsect_crn
                                );
               COMMIT;
            END LOOP;

            SELECT COUNT (*)
              INTO tot_recs
              FROM MSFSECT
             WHERE msfsect_change_flag = 'Y';

            EXIT outer_loop WHEN tot_recs = 0;
         END;

         COMMIT;
      END LOOP;

      INSERT INTO TRACK_REFRESH_ROWS
           VALUES ('MSFSECT', lv_row_count, lv_refresh_time, SYSDATE);

      COMMIT;
   END;
END Msosect;

