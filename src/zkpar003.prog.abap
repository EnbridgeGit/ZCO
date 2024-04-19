REPORT ZKPAR003 line-count 58 LINE-Size 255.

******************************************************************
*       Owner: Union Gas                                         *
*  Programmer: Dan Pitre                                         *
*        Date: Dec 2, 1997                                       *
*  Request ID:                                                   *
*                                                                *
* This program produces a report which will display PA data      *
* based on System of Origin (user-defined characteristic within  *
* PA).                                                           *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
*          -        -      -                                     *
******************************************************************
******************************************************************

TABLES: CE11100.   "Union/Centra Operating Line Items

DATA:   LN_CNTR    TYPE I,                        "Line Counter
        Total_Billed_Rev   like ce11100-vvbrv,    "Total Billed Revenue
        Total_Unbilled_Rev like ce11100-vvurv,    "Total Unbilled Rev.
        Total_Billed_Vol   like ce11100-vvbvl,    "Total Billed Volume
        Total_Unbilled_Vol like ce11100-vvuvl,    "Total Unbilled Vol.
        Total_Other_Rev    like ce11100-vvord,    "Total Other Revenue
        Total_Cust         like ce11100-vvcut.    "Total Customers

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 31(30) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  26(30).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block: *

SELECTION-SCREEN BEGIN OF BLOCK COMPANY
                 WITH FRAME TITLE TEXT-002.

PARAMETERS:
* SELECT-OPTIONS:
  S_COMPNY like ce11100-bukrs                       "company code
            obligatory modif id abc.
*           OBLIGATORY NO INTERVALS MODIF ID ABC.

* Second block ends after this statement. *

SELECTION-SCREEN END OF BLOCK COMPANY.

SELECTION-SCREEN SKIP.

* The following third block produces a frame within the first block: *

SELECTION-SCREEN BEGIN OF BLOCK ORIGIN
                 WITH FRAME TITLE TEXT-005.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 17(13) TEXT-009.
PARAMETERS:
  P_ORIGIN      LIKE CE11100-WWSID               "COMPANY OF ORIGIN
                OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* Third block ends after this statement. *

SELECTION-SCREEN END OF BLOCK ORIGIN.

SELECTION-SCREEN SKIP.

* The following fourth block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK POSTING
                 WITH FRAME TITLE TEXT-006.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(20) TEXT-007.
SELECTION-SCREEN: COMMENT 17(13) TEXT-003.
PARAMETERS:
  P_CO_DT1    LIKE CE11100-BUDAT              "COPA From Date
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-004.
PARAMETERS:
  P_CO_DT2    LIKE CE11100-BUDAT              "COPA To Date
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(20) TEXT-008.
SELECTION-SCREEN: COMMENT 17(13) TEXT-003.
PARAMETERS:
  P_INV_D1    LIKE CE11100-FADAT              "COPA From Invoice Date
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-004.
PARAMETERS:
  P_INV_D2    LIKE CE11100-FADAT               "COPA To Invoice Date
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* This statement ends the third block. *

SELECTION-SCREEN END OF BLOCK POSTING.

SELECTION-SCREEN SKIP.

* This statement ends the first block. *

SELECTION-SCREEN END OF BLOCK INTRO.

* The following will highlight the screen's output for certain texts. *

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.


* Output the report headings. *
  PERFORM WRT_HDG1.
  PERFORM WRT_HDG2.

  SELECT * FROM CE11100
  WHERE  BUKRS = S_COMPNY
  AND    BUDAT BETWEEN P_CO_DT1 AND P_CO_DT2
  AND    FADAT BETWEEN P_INV_D1 AND P_INV_D2
  AND    WWSID = p_origin.

    add 1 to ln_cntr.

    if LN_CNTR > 58.
       perform wrt_hdg2.
       LN_CNTR = 1.
    endif.

    write: /2 ce11100-vrgar,                        "doc type
            5 ce11100-posnr,                        "item number
           13 ce11100-hzdat,                        "date created
           25 ce11100-usnam,                        "user
           39 ce11100-budat,                        "posting date
           51 ce11100-fadat,                        "invoice date
           63 ce11100-belnr,                        "PA doc number
           75 ce11100-kndnr,                        "customer
           87 ce11100-artnr,                        "mat number
           108 ce11100-wwdvn,                        "divison/district
           114 ce11100-wwsct,                       "sector
           120 ce11100-wwrat,                       "rate class
           125 ce11100-vvbvl,                       "billed volume
           144 ce11100-vvbrv,                       "billed revenue
           165 ce11100-vvuvl,                       "unbilled volume
           184 ce11100-vvurv,                       "unbilled revenue
           205 ce11100-vvord,                       "other revenue
           216 ce11100-vvcut.                       "# of custs

     add ce11100-vvbvl to total_billed_vol.
     add ce11100-vvbrv to total_billed_rev.
     add ce11100-vvuvl to total_unbilled_vol.
     add ce11100-vvurv to total_unbilled_rev.
     add ce11100-vvord to total_other_rev.
     add ce11100-vvcut to total_cust.

  ENDSELECT.

PERFORM WRT_TRAILER.

*************************BEGINNING OF FORMS*****************************

* This routine writes the report headings. *

FORM WRT_HDG1.
  NEW-PAGE WITH-TITLE.
  add 6 TO LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /56 TEXT-001.
  SKIP.
  WRITE: /1  TEXT-002, 15 S_COMPNY.
  WRITE: /1  TEXT-006,  6 ':',
                       14 TEXT-007, 22 'FROM', 27 P_CO_DT1,
                                    37 '  TO', 42 P_CO_DT2.
  WRITE: /14  text-008, 22 'FROM', 27 P_INV_D1,
                                    37 '  TO', 42 P_INV_D2.
  ULINE: /.
  FORMAT INTENSIFIED OFF.
  ENDFORM.

FORM WRT_HDG2.
  FORMAT INTENSIFIED ON.
  WRITE: /1   text-110,   6 text-011,
                         16 text-112,  25 text-113,
          40  text-114,  52 text-008,
                         64 text-115,  75 text-116,
          87  text-117, 107 text-118, 119 text-120,
          135 text-121, 155 text-121, 184 text-124,
          195 text-124, 217 text-125, 233 text-123.
  WRITE: /1   text-010,   5 TEXT-015,  14 TEXT-012,
          27  text-013,
          41  text-014,  53 TEXT-014,  65 TEXT-015,
          76  text-015,  88 TEXT-015, 107 text-018,
          112 text-019, 119 text-020, 135 text-021,
          155 text-022, 185 text-021, 195 text-022,
          216 text-022, 233 text-023.
  ULINE: /.
  ADD 5 TO LN_CNTR.
  FORMAT INTENSIFIED OFF.
ENDFORM.

FORM WRT_TRAILER.
   ULINE: /.
   write: /125 total_billed_vol, 144 total_billed_rev,
           165 total_unbilled_vol, 184 total_unbilled_rev,
           205 total_other_rev, 226 total_cust.
   ULINE: /.
ENDFORM.
