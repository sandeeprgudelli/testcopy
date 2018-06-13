00001 *          BEGINNING OF MEMBER  FNSELPCS04   KEPT IN AHS.PANLIB   00000‹‹*
00002 *          THIS IS SAME AS MEMBER MSM79839   KEPT IN MCS.PANLIB   00000€€.
00003 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 07101194
00004 *   MEMBER    ALL UPDATES    COPIES OF TEMP UPDATES TO BE SENT  * 5574462V
00005 *    NAME    TO BE DONE BY       FOLLOWING AREAS FOR REVIEW      *00035001
00006 *                                                                *0000600
00007 * FNSELPCS04   E. ROOS                                           *0000700
00008 *                            A. JAFFE       CLAIM & PROVIDER     *0000800
00009 *                            E. ROOS        FINAN.CLMS.RPTG.     *0000900
00010 *                                                                *0001000
00011 ******************************************************************0001100
00012 ******************************************************************0001200
00013 *                                                                *0001300
00014 *    CREATION DATE:                           OCTOBER 1989       *0001400
00015 *    PURPOSE:  DEFINE A COMMON DATA FORMAT FOR PAID CLAIMS FOR   *0001500
00016 *              ALL LINES OF BUSINESS, INCLUDING FIELDS REQUIRED  *0001600
00017 *              TO POPULATE A PAID CLAIMS TRIANGULATION HISTORY   *0001700
00018 *              FILE AND TO FEED THE ALBANY INCENTIVE RATED       *0001800
00019 *              CHARGE REGISTER APPLICATION                       *0001900
00020 ******************************************************************0002000
00021 *                                                                *0002100
00023    02  IRS-RECORD-SECTION.                                        0002200
00024 * CHAR 1 - 6                                                      0002300
00025      05  IRS-DETAIL-RCD-INDICATOR           PIC X(06).            0002400
00026          88  IRS-DETAIL-RECORD              VALUE 'DETAIL'.       0002500
00027 * CHAR 7 - 11                                                     0002600
00028      05  IRS-DETAIL-RCD-IDENTIFICATION      PIC X(5).             0002700
00029          88 IRS-PC-RCD-IDENT                VALUE SPACES.         0002800
00030 * CHAR 12 - 15                                                    0002900
00031      05  IRS-PAID-DATE.                                           0003000
00032          10  IRS-PAID-YEAR                  PIC X(02).            0003100
00033          10  IRS-PAID-MONTH                 PIC X(02).            0003200
00034 * CHAR 16 - 19                                                    0003300
00035      05  IRS-INCURRED-DATE.                                       0003400
00036          10  IRS-INCURRED-YEAR              PIC X(02).            0003500
00037          10  IRS-INCURRED-MONTH             PIC X(02).            0003600
00038 * CHAR 16 - 19                                                    0003700
00039      05  IRS-FROM-DATE  REDEFINES  IRS-INCURRED-DATE.             0003800
00040          10  IRS-FROM-YEAR                  PIC X(02).            0003900
00041          10  IRS-FROM-MONTH                 PIC X(02).            0004000
00042 * CHAR 20                                                         0004100
00043      05  IRS-LINE-OF-BUSINESS               PIC X(01).            0004200
00044          88  IRS-HOSPITAL                   VALUE '1'.            0004300
00045          88  IRS-DENTAL                     VALUE '2'.            0004400
00046          88  IRS-DRUG                       VALUE '3'.            0004500
00047          88  IRS-SURGICAL-MEDICAL           VALUE '4'.            0004600
00048          88  IRS-MAJOR-MEDICAL              VALUE '5'.            0004700
00048          88  IRS-MAIL-ORDER-DRUG            VALUE '6'.            0004800
00049          88  IRS-MANUAL-MEDICAL             VALUE '7'.            0004900
00050          88  IRS-VALID-LINES-OF-BUSINESS                          0005000
00051               VALUES  '1'  '2'  '3'  '4'  '5' '6' '7'.            0005100
00052 * CHAR 21                                                         0005200
00053      05  IRS-CLAIM-SERVICE-CATEGORY         PIC X(01).            0005300
00054          88  IRS-OTHER                      VALUE SPACE.          0005400
00055          88  ALB-MIGRATION                  VALUE 'A'.            0005500
00056 * CHAR 22                                                         0005600
00057      05  IRS-TYPE-OF-SERVICE                PIC X(01).            0005700
00058          88  IRS-INPATIENT                  VALUE LOW-VALUES.     0005800
00059          88  IRS-OUTPATIENT                 VALUE '0'.            0005900
00060          88  IRS-SURGERY                    VALUE '1'.            0006000
00061          88  IRS-ASSISTANT-SURGERY          VALUE '2'.            0006100
00062          88  IRS-MATERNITY                  VALUE '3'.            0006200
00063          88  IRS-ANESTHESIA                 VALUE '4'.            0006300
00064          88  IRS-RADIOLOGY                  VALUE '5'.            0006400
00065          88  IRS-HOME-CARE-PGM              VALUE '6'.            0006500
00066          88  IRS-HOME-OFF-NURSING-CARE      VALUE '7'.            0006600
00067          88  IRS-PATHOLGY                   VALUE '8'.            0006700
00068          88  IRS-CONSULTATION               VALUE '9'.            0006800
00069          88  IRS-OTHER-HOSP-SERVICE         VALUE 'A'.            0006900
00070          88  IRS-SPEECH-THERAPY             VALUE 'B'.            0007000
00071          88  IRS-HOME-CARE                  VALUE 'C'.            0007100
00072          88  IRS-NON-SURG-DENTAL-CARE       VALUE 'D'.            0007200
00073          88  IRS-CHIROP-OFFICE-ONLY         VALUE 'E'.            0007300
00074          88  IRS-AMBULANCE                  VALUE 'F'.            0007400
00075          88  IRS-DME-PURCHASE               VALUE 'G'.            0007500
00076          88  IRS-DME-RENTAL                 VALUE 'H'.            0007600
00077          88  IRS-EXPAND-MED-SUPPLIES        VALUE 'J'.            0007700
00078          88  IRS-PRESCRIPTION-DRUG          VALUE 'K'.            0007800
00079          88  IRS-VISION-CARE                VALUE 'M'.            0007900
00080          88  IRS-PRIVATE-DUTY-NURSING       VALUE 'N'.            0008000
00081          88  IRS-PHYSIOTHERAPY              VALUE 'P'.            0008100
00082          88  IRS-HOSPITAL-CHARGES           VALUE 'R'.            0008200
00083          88  IRS-MEDICAL-SUPPLIES           VALUE 'S'.            0008300
00084          88  IRS-CONCURRENT-CARE            VALUE 'W'.            0008400
00085          88  IRS-OTHER-SYS-GENERATED        VALUE 'Z'.            0008500
00086 * CHAR 23 - 25                                                    0008600
00087      05  IRS-PLAN-CODE                      PIC X(03).            0008700
00088 * CHAR 26 - 31                                                    0008800
00089      05  IRS-GROUP-NUMBER                   PIC X(06).            0008900
00090          88  IRS-VALID-GROUP-NUMBERS        VALUE '000000' THRU   0009000
00091                                                   '999999'.       0009100
00092      05  IRS-GROUP-NUMBER-REDEFINED       REDEFINES               0009200
00093              IRS-GROUP-NUMBER.                                    0009300
00094          10  IRS-GROUP-NUMBER-POS-1-2       PIC XX.               0009400
00095              88  IRS-ALBANY-GROUP-NUMBER-SERIES                   0009500
00096                      VALUES  '91'  '98'  '99'.                    0009600
00097              88  IRS-ALBANY-GROUP-NUMBER-91    VALUE '91'.        0009700
00098              88  IRS-ALBANY-GROUP-NUMBER-98    VALUE '98'.        0009800
00099              88  IRS-ALBANY-GROUP-NUMBER-99    VALUE '99'.        0009900
00100          10  FILLER                         PIC X(04).            0010000
00101 * CHAR 32 - 45                                                    0010100
00102      05  IRS-SUBSCRIBER-ID                  PIC X(14).            0010200
00103 * CHAR 46 - 62                                                    0010300
00104      05  MISCELLANEOUS-FIELDS-1.                                  0010400
00105          10  IRS-DETAIL-PATIENT-INITIAL     PIC X.                0010500
00106          10  IRS-DETAIL-PATIENT-LAST-NAME   PIC X(15).            0010600
00107          10  IRS-DETAIL-MED-SURG-BREAKDOWN  PIC X.                0010700
00108 * NOTE : ONE INPUT MCS RECORD FROM THE 453 BYTES "CSS" FILE       0010800
00109 * CAN RESULT IN 1,2,OR 3 OUTPUT RECORDS BEING WRITTEN IN          0010900
00110 * MSM79839 FORMAT, AS FOLLOWS :                                   0011000
00111 *  IF "BASIC",  AMOUNT PAID WILL BE FROM THE FIELD CALLED         0011100
00112 *  MS-CSS-PAID-AMOUNT-BASIC-ONLY IF GREATER THAN ZERO             0011200
00113 *  IF "RIDER",  AMOUNT PAID WILL BE FROM THE FIELD CALLED         0011300
00114 *  MS-CSS-PAID-AMOUNT-RIDER IF GREATER THAN ZERO                  0011400
00115 *  IF "MAJ.MED",  AMOUNT PAID WILL BE FROM THE FIELD CALLED       0011500
00116 *  MS-CSS-PAID-AMOUNT-MAJOR-MED IF GREATER THAN ZERO              0011600
00117              88  IRS-DETAIL-MED-SURG-BASIC         VALUE '1'.     0011700
00118              88  IRS-DETAIL-MED-SURG-RIDER         VALUE '2'.     0011800
00119              88  IRS-DETAIL-MED-SURG-MAJOR-MED     VALUE '3'.     0011900
00119              88  IRS-DETAIL-MED-SURG-MAJOR-REJ     VALUE '4'.     0011900
00120              88  IRS-DETAIL-MED-SURG-NO            VALUE ' '.     0012000
00121              88  IRS-DETAIL-MED-SURG-FROM-MCS                     0012100
00122                                     VALUES  '1'  '2'  '3' '4'.    0012200
00123 * CHAR 63 - 64                                                    0012300
00124      05  IRS-NUMBER-OF-DAYS-VISITS          PIC S9(03)    COMP-3. 0012400
00125 * CHAR 65 - 66                                                    0012500
00126      05  IRS-NUMBER-OF-CLAIMS               PIC S9(03)    COMP-3. 0012600
00127 * CHAR 67 - 68                                                    0012700
00128      05  IRS-NUMBER-OF-SERVICES             PIC S9(03)    COMP-3. 0012800
00129 * CHAR 69 - 80                                                    0012900
00130      05  MISCELLANEOUS-FIELDS-2.                                  0013000
00131 * SUB-DIVISION CODE OF ALL SPACES OR ALL LOW-VALUES HAS BEEN      0013100
00132 *  CONVERTED TO THREE ZEROS                                       0013200
00133          10  IRS-DETAIL-SUB-DIVISION         PIC XXX.             0013300
00134          10  IRS-DETAIL-PAID-DAY             PIC XX.              0013400
00135          10  IRS-DETAIL-INCURRED-DAY         PIC XX.              0013500
00136          10  IRS-ICHIS-PRESSO-FLAG           PIC X.               0013600
00137              88  IRS-ICHIS-PRESSO-NOT-APPL           VALUE  ' '.  0013700
00138              88  IRS-ICHIS-PRESSO-NO                 VALUE  '0'.  0013800
00139              88  IRS-ICHIS-PRESSO-YES                VALUE  '1'.  0013900
00140          10  IRS-ICHIS-RIDER-FLAG            PIC X.               0014000
00141              88  IRS-ICHIS-RIDER-NOT-APPL            VALUE  ' '.  0014100
00142              88  IRS-ICHIS-RIDER-NO                  VALUE  '0'.  0014200
00143              88  IRS-ICHIS-RIDER-YES                 VALUE  '1'.  0014300
00144          10  IRS-ICHIS-SUPP-PAYMENT-FLAG     PIC X.               0014400
00145              88  IRS-ICHIS-SUPP-PAY-NOT-APPL         VALUE  ' '.  0014500
00146              88  IRS-ICHIS-SUPP-PAY-NO               VALUE  '0'.  0014600
00147              88  IRS-ICHIS-SUPP-PAY-YES              VALUE  '1'.  0014700
00148          10  IRS-MCS-ASO-FLAG                                     0014800
00149              REDEFINES IRS-ICHIS-SUPP-PAYMENT-FLAG   PIC X.       0014900
00150              88  IRS-MCS-ASO-YES                     VALUE  'Y'.  0015000
00151          10  IRS-CSS-SOURCE-CODE             PIC X(1).            0015100
00152              88  IRS-CSS-MANUAL-MEDICAL         VALUE 'N'.        0015200
00153              88  IRS-CSS-MAIL-ORDER-DRUG        VALUE '4'.        0015300
00154          10  IRS-SENIOR-CARE-IND            PIC X.                0015400
00155              88  IRS-NOT-SENIOR-CARE        VALUE ' ' '0' '2'.    0015500
00156 * CHAR 81 - 86                                                    0015600
00157      05  IRS-PAID-CLAIM-LIABILITY           PIC S9(09)V99 COMP-3. 0015700
00158 * CHAR 87 - 100                                                   0015800
00159      05  MISCELLANEOUS-FIELDS-3.                                  0015900
00160 *   THIS FIELD IS IN   D  I  S  P  L  A  Y    MODE                0016000
00161          10  IRS-DETAIL-CLAIM-NUMBER        PIC X(14).            0016100
00162          10  HR-CASE-NO   REDEFINES IRS-DETAIL-CLAIM-NUMBER.      0016200
00163              15  HR-BR-ID                  PIC X.                 0016300
00164                  88  HR-NEW-YORK-BR-ID               VALUE ' '.   0016400
00165                  88  HR-RF-IN-PAT-BR-ID              VALUE 'R'.   0016500
00166                  88  HR-RF-OT-PAT-BR-ID              VALUE 'W'.   0016600
00167                  88  HR-IBM-TBS                      VALUE 'A'.   0016700
00168                  88  HR-GMEDS-ID                     VALUE 'B'.   0016800
00169                  88  HR-ALBANY-MCAUTO                VALUE 'C'.   0016900
00170                  88  HR-ALBANY-TRADITIONAL           VALUE 'D'.   0017000
00171                  88  HR-ALBANY-DIVISION              VALUE 'E'.   0017100
00172                  88  HR-ATTEDS-ID                    VALUE 'F'.   0017200
00173                  88  HR-NYSTATE                      VALUE 'G'.   0017300
00174                  88  HR-BFITBS                       VALUE 'H'.   0017400
00175                  88  HR-BE-DIGEST                    VALUE 'I'.   0017500
00176                  88  HR-NYNEX-E                      VALUE 'J'.   0017600
00177                  88  HR-EDSUPS-ID                    VALUE 'K'.   0017700
00178                  88  HR-MACY-ID                      VALUE 'L'.   0017800
00179                  88  HR-BLOOMINGDALE-ID              VALUE 'M'.   0017900
00180                  88  HR-NYNEX-NET-ID                 VALUE 'N'.   0018000
00181                  88  HR-PAR-NASCO-ID                 VALUE 'P'.   0018100
00182                  88  HR-CS90-ID                      VALUE 'S'.   0018200
00183                  88  HR-ITS-PAR-ID                   VALUE 'Q'.   0018300
00184                  88  HR-HEALTHNET-MIGRAT             VALUE 'T'.   0018400
00185                  88  HR-CONTROL-NASCO-ID             VALUE 'U'.   0018500
00186                  88  HR-BR-ID-VALID                               0018600
00187                         VALUE ' ', 'R', 'W', 'A', 'C',            0018700
00188                              'B', 'D', 'E', 'F', 'G', 'H'         0018800
00189                             'I', 'J', 'K', 'L', 'M', 'N'          0018900
00190                              'P', 'Q' 'S' 'T' 'U'.                0019000
00191              15  HR-CASE-ID                PIC X.                 0019100
00192                    88  HR-OT-PAT-CASE           VALUE 'A'.        0019200
00193                    88  HR-BANK-B-CASE           VALUE 'B'.        0019300
00194                    88  HR-CITY-HOSP-CASE        VALUE 'C'.        0019400
00195                    88  HR-BLOOD-E-CASE          VALUE 'E'.        0019500
00196                    88  HR-HOME-CARE-CASE        VALUE 'H'.        0019600
00197                    88  HR-HEMO-CASE             VALUE 'K'.        0019700
00198                    88  HR-NON-MEMBER-CASE       VALUE 'N'.        0019800
00199                    88  HR-SEN-CARE-CASE         VALUE 'S'.        0019900
00200                    88  HR-MEMBER-CASE         VALUE '0' THRU '9'. 0020000
00201                    88  HR-CASE-ID-VALID                           0020100
00202                     VALUE 'A', 'B', 'C', 'E', 'H', 'K', 'N', 'S', 0020200
00203                              '0' THRU '9'.                        0020300
00204              15  HR-CASENO-LST6       PIC X(6).                   0020400
00205              15  HR-ALBANY-CLAIM-DIGIT REDEFINES                  0020500
00206                      HR-CASENO-LST6.                              0020600
00207                   20  FILLER               PIC X(5).              0020700
00208                   20  HR-ALBANY-LAST-DIGIT    PIC X.              0020800
00209              15  HR-CS90-CLAIM-PACKED    REDEFINES                0020900
00210                      HR-CASENO-LST6       PIC S9(11) COMP-3.      0021000
00211 * CHAR 101 - 106                                                  0021100
00212      05  IRS-ACCOUNT-PAID-DATE.                                   0021200
00213          10  IRS-ACCOUNT-PAID-DATE-YY       PIC X(02).            0021300
00214          10  IRS-ACCOUNT-PAID-DATE-MM       PIC X(02).            0021400
00215          10  IRS-ACCOUNT-PAID-DATE-DD       PIC X(02).            0021500
00216 * CHAR 107                                                        0021600
00217      05  IRS-LOB-ADJUSTMENT-CODE            PIC X(1).             0021700
00218          88  IRS-HOSPITAL-ADJUSTED-CLAIM    VALUE  '1'.           0021800
00219          88  IRS-DENTAL-ADJUSTED-CLAIM      VALUE  '2'.           0021900
00220          88  IRS-DRUG-ADJUSTED-CLAIM        VALUE  '3'.           0022000
00221          88  IRS-SRGMED-ADJUSTED-CLAIM      VALUE  '4'.           0022100
00222          88  IRS-MAJMED-ADJUSTED-CLAIM      VALUE  '5'.           0022200
00223 * CHAR 108                                                        0022300
00224      05  IRS-POS-INDICATOR                  PIC X(01).            0022400
00225          88  IRS-TRADITIONAL           VALUE '0'.                 0022500
00226 * CHAR 109                                                        0022600
00227      05  IRS-BANK-B-INDICATOR               PIC X(01).            0022700
00228          88  IRS-BANK-B-ICHIS          VALUE '1'.                 0022800
00229          88  IRS-BANK-EMPTY            VALUE SPACE.               0022900
00230 * CHAR 110                                                        0023000
00231      05  IRS-ICHIS-COLLECTION-CODE                PIC X(01).      0023100
00232          88  IRS-COLL-GROUP            VALUE '1'.                 0023200
00233          88  IRS-COLL-DIRECT-PAYMENT   VALUE '0', '2' THRU '8'.   0023300
00234          88  IRS-COLL-CERT-EXP         VALUE '9'.                 0023400
00235          88  IRS-COLL-EMPTY            VALUE SPACE.               0023500
00236              88 IRS-COLL-GROUP           VALUE '1'.               0023600
00237              88 IRS-COLL-DIRECT-PAYMENT  VALUE '0', '2' THRU '8'. 0023700
00238              88 IRS-COLL-CERT-EXP        VALUE '9'.               0023800
00239              88 IRS-COLL-EMPTY           VALUE SPACE.             0023900
00240      05  IRS-RECORD-TYPE                PIC X(01).                0024000
00241          88 IRS-ICHIS                   VALUE '1'.                0024100
00242          88 IRS-MANREP                  VALUE '2'.                0024200
00243          88 IRS-DR                      VALUE '3'.                0024300
00244          88 IRS-DT                      VALUE '4'.                0024400
00245          88 IRS-MMM                     VALUE '5'.                0024500
00245          88 IRS-REJ                     VALUE '6'.                0024500
00245          88 IRS-ICHIS-GME               VALUE 'G'.                0024500
00246          88 IRS-VALID-RECORD-TYPES                                0024600
00247                VALUES '1' '2' '3' '4' '5' '6' 'G'.                0024700
00240      05  IRS-HR-SORT-GROUP-NO           PIC X(06).                0024800
00240      05  IRS-HR-CONTROL-IND             PIC X(01).                0024900
00240      05  IRS-AUDIT-IND                  PIC X(01).                0025000
00240      05  IRS-AUDIT-GRP                  PIC X(06).                0025100
00240      05  IRS-AUDIT-REPORT-AS-SUB-DIV    PIC X(03).                0025200
002200* CHAR. 90.                                                       0002300
002300     03  HR-DISP-CDE                   PIC X.                     0002400
002400* CHAR. 91.                                                       0002500
002500     03  HR-MARR-STATUS                PIC X.                     0002600
002600* CHAR. 92.                                                       0002700
002700     03  HR-TYPE-CONTRACT              PIC X.                     0002800
002800* CHAR. 93.                                                       0002900
002900     03  HR-TYPE-BUSINESS              PIC X.                     0003000
003000* CHAR. 94.                                                       0003100
003100     03  HR-COLLECTION-GRPCDE          PIC X.                     0003200
003200* FROM CHAR. 95 TO 96.                                            0003300
003300     03  HR-AGE                        PIC XX.                    0003400
003400* CHAR. 97.                                                       0003500
003500     03  HR-STILL-IN-IND               PIC X.                     0003600
003600* CHAR. 98.                                                       0003700
003700     03  HR-CONTINUATION-IND           PIC X.                     0003800
003800* CHAR. 99.                                                       0003900
003900     03  HR-INDEMNITY-IND              PIC X.                     0004000
004000* CHAR. 100.                                                      0004100
004100     03  HR-ACCOMODATION               PIC X.                     0004200
                 88 HR-DMFEE-PROCESS         VALUE 'H'.                 0008800
004200* CHAR. 101.                                                      0004300
004300     03  HR-LMTD-APPROVAL-IND          PIC X.                     0004400
004400* FROM CHAR. 102 TO 103.                                          0004500
004500********* NUMBER OF VISITS FOR HOME-CARE CASES                ****0004600
004600     03  HR-FULL-DAYS                  PIC S999            COMP-3.0004700
004700     03  HR-UNITS-OF-BLOOD    REDEFINES  HR-FULL-DAYS             0004800
004800                                       PIC S999            COMP-3.0004900
004900* FROM CHAR. 104 TO 105.                                          0005000
005000     03  HR-DISCOUNT-DAYS              PIC S999            COMP-3.0005100
005100     03  HR-PAID-VISITS REDEFINES                                 0005200
005200         HR-DISCOUNT-DAYS              PIC S99V9           COMP-3.0005300
005300* FROM CHAR. 106 TO 107.                                          0005400
005400     03  HR-BIRTH-YR                   PIC XX.                    0005500
005500* FROM CHAR. 108 TO 123.                                          0005600
005600    03  HR-DRG-DATA-PART-1.                                       0005700
005700* FROM CHAR. 108 TO 111.                                          0005800
005800        05  HR-DRG-FILLER-1.                                      0005900
005900            10  HR-OP-RATE-TYPE              PIC X.               0006000
006200            10  HR-DRG-CODE-NEW              PIC X(3).            0006100
006300* FROM CHAR. 112.                                                 0006200
006400        05  HR-DRG-STATE-CODE-INDIC          PIC X.               0006300
006500* FROM CHAR. 113.                                                 0006400
006600        05  HR-DRG-STATE-EXEMPTED-INDIC      PIC X.               0006500
006700* FROM CHAR. 114 TO 116.                                          0006600
006800        05  HR-DRG-MAJOR-DIAG-CATAGORY       PIC X(3).            0006700
006900* FROM CHAR. 117 TO 120.                                          0006800
007000        05  HR-DRG-CODE-INTENSITY-WEIGHT     PIC S9(3)V9(4)       0006900
007100                                             COMP-3.              0007000
007200* FROM CHAR. 121.                                                 0007100
007300        05  HR-DRG-CLM-CLASS-INDIC           PIC X.               0007200
007400* FROM CHAR. 122 TO 123.                                          0007300
007500        05  HR-DRG-OL-DAYS                   PIC S9(3) COMP-3.    0007400
007600* FROM CHAR. 124 TO 127.                                          0007500
007700     03  HR-TOT-HOSP-BILL              PIC S9(7)           COMP-3.0007600
007800     03  HR-POOL-B-DIFFERENTIAL    REDEFINES  HR-TOT-HOSP-BILL    0007700
007900                                       PIC S9(5)V99        COMP-3.0007800
008000* FROM CHAR. 128 TO 131.                                          0007900
008100     03  HR-TOT-CHGD-TO-PAT            PIC S9(7)           COMP-3.0000000
008200     03  HR-POOL-C-DIFFERENTIAL    REDEFINES  HR-TOT-CHGD-TO-PAT  0000000
008300                                       PIC S9(5)V99        COMP-3.0000000
008400* CHAR. 132.                                                      0008300
008500     03  HR-SEX-IND                    PIC X.                     0008400
008600* CHAR. 133.                                                      0008500
008700     03 HR-SPEC-PROCESS-IND                   PIC X.              0008600
                 88 HR-SPEC-PROCESS-NOT-CODED      VALUE ' '.           0008700
                 88 HR-NOT-A-SPEC-PROCESS           VALUE '0'.          0008800
                 88 HR-NYS-JOINT-VENTURE-CLAIM      VALUE '1'.          0008900
                 88 HR-NYS-DEDICATED-CENTER         VALUE '2'.          0009000
                 88 HR-TBS-IBM                      VALUE '3'.          0009100
                 88 HR-MCS-CORE-SYSTEM              VALUE '4'.          0009200
                 88 HR-ALBANY-HEALTHNET             VALUE '5'.          0009300
                 88 HR-ALBANY-GROUP                 VALUE '6'.          0009400
                 88 HR-ALBANY-DIRECT-PAY            VALUE '7'.          0009500
                 88 HR-ALBANY-NY-STATE              VALUE '8'.          0009600
                 88 HR-GM-EDS                       VALUE '9'.          0009700
                 88 HR-BFI-TBS                      VALUE 'B'.          0009800
                 88 HR-ATT-EDS                      VALUE 'F'.          0009900
                 88 HR-NYNEX-EDS                    VALUE 'J'.          0010000
                 88 HR-EDS-UPS                      VALUE 'K'.          0010100
                 88 HR-MACY-NASCO                   VALUE 'L'.          0010200
                 88 HR-BEACON-NASCO                 VALUE 'I'.          0010300
                 88 HR-BLOOMINGDALE-NASCO           VALUE 'M'.          0010400
                 88 HR-PAR-NASCO                    VALUE 'P'.          0010500
                 88 HR-NYNEX-NET-NASCO              VALUE 'N'.          0010600
                 88 HR-CONTROL-NASCO                VALUE 'A'.          0010700
                 88 HR-CS90-PROCESS                 VALUE 'C'.          0010800
                 88 HR-ITS-PAR                      VALUE 'Q'.          0010900
                 88 HR-PSI-PROCESS                  VALUE 'V'.          0011000
                 88 HR-ITS-HOST                     VALUE 'X'.          0011100
                 88 HR-GME-PROCESS                  VALUE 'G'.          0011200
                 88 HR-EMHC-CAPITATION              VALUE 'E'.          0011300
                 88 HR-SPEC-PROCESS-CODE-VALID      VALUES              0011400
                    ' ' '0' '1' '2' '3' '4'                             0011500
                    '5' '6' '7' '8' '9' 'F' 'B'                         0011600
                    'I' 'J' 'K' 'L' 'M' 'P' 'N' 'A' 'C'                 0011700
                    'Q' 'V' 'X' 'G' 'E'.                                0011800
008800* FROM CHAR. 134 TO 139.                                          0011900
008900     03  HR-DISCHARGE-DATE.                                       0012000
009000         05  HR-DIS-YR                 PIC XX.                    0012100
009100         05  HR-DIS-MO                 PIC XX.                    0012200
009200         05  HR-DIS-DA                 PIC XX.                    0012300
009400     03  HR-SUPP-ORIG-PAID-DATE    REDEFINES  HR-DISCHARGE-DATE.  0000000
009500         05  HR-SUPP-ORIG-PAID-YR      PIC XX.                    0012500
009600         05  HR-SUPP-ORIG-PAID-MO      PIC XX.                    0012600
009700         05  HR-SUPP-ORIG-PAID-DA      PIC XX.                    0012700
009800* FROM CHAR. 140 TO 143.                                          0012800
009900     03  HR-HOSP-CDE                   PIC X(4).                  0012900
010000* CHAR. 144.                                                      0013000
010100     03  HR-ALBANY-SUPP-IND            PIC X.                     0013100
010200* CHAR. 145.                                                      0013200
010300     03  HR-ECR-IND                    PIC X.                     0013300
010400         88 HR-NON-RIDER                     VALUE '0'.           0013400
010500         88 HR-RIDER                         VALUE '1'.           0013500
010600         88 HR-ECR-IND-VALID                 VALUE '0' '1'.       0013600
010700* CHAR. 146.                                                      0013700
010800     03  HR-SUPPS-IND                  PIC X.                     0013800
010900* CHAR. 147.                                                      0013900
011000     03  HR-SVCNG-ACCT-IND             PIC X.                     0014000
011100* CHAR. 148.                                                      0014100
011200     03  HR-COBRA-IND                  PIC X.                     0014200
011300* CHAR. 149.                                                      0014300
011400     03  HR-ERRCDE                     PIC X.                     0014400
011500* CHAR. 150.                                                      0014500
011600     03  HR-REFUND-STAT-COB            PIC X.                     0014600
011700* FROM CHAR. 151 TO 153.                                          0014700
011800     03  HR-ECR-NO                     PIC XXX.                   0014800
011900     03  HR-RIDER-PACK-NO REDEFINES                               0014900
012000         HR-ECR-NO                     PIC 9(5) COMP-3.           0015000
012100* CHAR. 154.                                                      0015100
012200     03  HR-CASE-CTR                   PIC S9     COMP-3.         0015200
012300* CHAR. 155.                                                      0015300
012400     03  HR-CBC-BLOOD-IND              PIC X.                     0015400
012500* CHAR. 156.                                                      0015500
012600     03  HR-PATIENT-DIED-IND           PIC X.                     0015600
012700* CHAR. 157.                                                      0015700
012800     03  HR-SC-DEDUCT-IND              PIC X.                     0015800
012900* CHAR. 158.                                                      0015900
013000     03  HR-SC-ALLOW-IND               PIC X.                     0016000
013100* CHAR. 159.                                                      0016100
013300     03  HR-PRESSO-IND                 PIC X.                     0016200
013400* FROM CHAR. 160 TO 164.                                          0016300
013500     03  HR-ZIP-CODE                   PIC X(5).                  0016400
013600* FROM CHAR. 165 TO 170.                                          0016500
013700     03  HR-ICD9-DIAG-CODE.                                       0016600
013800         05  HR-ICD9-F2                PIC X(2).                  0016700
013900         05  HR-ICD9-L3                PIC X(3).                  0016800
014000         05  FILLER                    PIC X.                     0016900
014100* FROM CHAR. 171 TO 174.                                          0017000
014200     03  HR-PATIENT-BIRTH-MONTH-DAY.                              0017100
014300         05  HR-PAT-BIRTH-MONTH        PIC X(2).                  0017200
014400         05  HR-PAT-BIRTH-DAY          PIC X(2).                  0017300
014500* FROM CHAR. 175 TO 187.                                          0017400
014600     03  HR-SECONDARY-SUB-ID.                                     0017500
014700         05  HR-SEC-ID-POS-1-9         PIC X(9).                  0017600
014800         05  HR-SEC-ID-POS-10-13.                                 0017700
014900             10  HR-SEC-ID-POS-10      PIC X.                     0017800
015000             10  HR-SEC-ID-POS-11-13 PIC X(3).                    0017900
015100         05  HR-PAYROLL-LOC-LST4       REDEFINES                  0018000
014800             HR-SEC-ID-POS-10-13      PIC X(4).                   0017700
015100         05  HR-IP-CO-PAY-DEFINITION REDEFINES                    0018000
015200             HR-SEC-ID-POS-10-13.                                 0018100
015300             10  HR-IP-CO-PAY-OPTION       PIC X.                 0018200
015400             10  HR-IP-CO-PAY-NO-OF-DAYS PIC X.                   0018300
015500             10  HR-IP-CO-PAY-AMT          PIC 99.                0018400
015600         05  HR-IP-FEP83 REDEFINES                                0018500
015700             HR-SEC-ID-POS-10-13.                                 0018600
015800             10  HR-IP-FEP83-OPTION        PIC X.                 0018700
015900             10  HR-IP-FEP83-DEDUCTIBLE    PIC 999.               0018800
016000         05  HR-OP-CO-PAY-DEFINITION REDEFINES                    0018900
016100             HR-IP-CO-PAY-DEFINITION.                             0019000
016200             10  HR-OP-CO-PAY-OPTION       PIC X.                 0019100
016300             10  HR-OP-CO-PAY-IDENT        PIC X.                 0019200
016400             10  HR-OP-CO-PAY-PCT          PIC V99.               0019300
016500* CHAR. 188.                                                      0019400
016700     03  HR-MAIN-OPERATING-RM-IND      PIC X.                     0019500
016800* FROM CHAR. 189 TO 190.                                          0019600
016900     03  HR-OP-TREATMENT-CODE          PIC XX.                    0019700
017000* FROM CHAR. 191 TO 192.                                          0019800
017200     03  HR-DIAG-CLASSIFICATION-CODE PIC XX.                      0019900
017300* FROM CHAR. 193 TO 197.                                          0020000
017400     03  HR-NON-POOL-PAYMT             PIC S9(7)V99    COMP-3.    0020100
017500* CHAR. 198 TO 213.                                               0020200
017600     03  HR-FLEX-AREA.                                            0020300
017700*                                                                 0020400
017800* CHAR. 198.                                                      0020500
017900         05  HR-FLEX-COST-SHARE-IND            PIC X.             0020600
018000* FROM CHAR. 199 TO 203.                                          0020700
018100         05  HR-FLEX-DEDUCT-AMT               PIC S9(7)V99 COMP-3.0020800
018200* FROM CHAR. 204 TO 208.                                          0020900
018300         05  HR-FLEX-CO-PAY-AMT               PIC S9(7)V99 COMP-3.0021000
018400         05  HR-TC-POOL-PENALTY REDEFINES                         0021100
018500             HR-FLEX-CO-PAY-AMT               PIC S9(7)V99 COMP-3.0021200
018600                                                                  0021300
018700* FROM CHAR. 209 TO 213.                                          0021400
018800         05  HR-FLEX-CO-INSUR-AMT             PIC S9(7)V99 COMP-3.0021500
018900         05  HR-TC-NON-POOL-PENALTY REDEFINES                     0021600
019000             HR-FLEX-CO-INSUR-AMT             PIC S9(7)V99 COMP-3.0021700
019100* FROM CHAR. 214 TO 219.                                          0021800
019200     03 HR-PRIM-DTE-OF-SURG.                                      0021900
019300         05  HR-SURGERY-YR                    PIC XX.             0022000
019400         05  HR-SURGERY-MO                    PIC XX.             0022100
019500         05  HR-SURGERY-DA                    PIC XX.             0022200
019600* FROM CHAR. 220 TO 224.                                          0022300
019700     03 HR-PRIM-PROCEDURE-CODE                PIC X(5).           0022400
019800* FROM CHAR. 225 TO 226.                                          0022500
019900     03 HR-PRODUCT-VARIATION-2BYTE             PIC X(2).          0022600
020000* FROM CHAR. 227 TO 234.                                          0022700
020100     03  HR-COST-SHARE-AREA.                                      0022800
020200*                                                                 0022900
020300* FROM CHAR. 227 TO 230.                                          0023000
020400         05  HR-DEDUCTIBLE-AMT               PIC S9(5)V99 COMP-3. 0023100
020500*                                                                 0023200
020600* FROM CHAR. 231 TO 234.                                          0023300
020700         05  HR-OTHER-DEDUCTIBLE-AMT         PIC S9(5)V99 COMP-3. 0023400
020800* FROM CHAR. 235 TO 256.                                          0023500
020900     03  STOP-MASK-MOVE.                                          0023600
021000* FROM CHAR. 235 TO 237.                                          0023700
021100         05  HR-CORE-SYS-ORIG-PLAN           PIC X(3).            0023800
021200*                                                                 0023900
021300* FROM CHAR. 238 TO 255.                                          0024000
021400         05  HR-CORE-SYS-CLAIM-NUMBER.                            0024100
021500* FROM CHAR. 238 TO 251.                                          0024200
021600             10  HR-CORE-SYS-CLAIM-NO      PIC X(14).             0024300
021700             10  HR-CS90-CONV-DATA REDEFINES                      0024400
021800                 HR-CORE-SYS-CLAIM-NO.                            0024500
021900                 15  FILLER                PIC X(13).             0024600
022000                 15  HR-CS90-CONV-IND      PIC X.                 0024700
022100* FROM CHAR. 252 TO 254.                                          0024800
022200             10  HR-ADJUST-NO              PIC S9(5) COMP-3.      0024900
022300* CHAR. 255.                                                      0025000
022400             10  FILLER                      PIC X.               0025100
022500* CHAR. 256.                                                      0025200
022600         05  HR-MAJOR-MEDICAL-IND        PIC X.                   0025300
022700             88  HR-NON-MAJOR-MEDICAL         VALUE '0'.          0025400
022800             88  HR-MAJOR-MEDICAL-LIAB-PAYMT VALUE '1'.           0025500
022900             88  HR-MAJOR-MEDICAL-RECORD      VALUE '2'.          0025600
023000* FROM CHAR. 257 TO 266.                                          0025700
023100     03  FILLER.                                                  0025800
023300         05  HR-CORE-SYS-LOCAL-PROV-ID       PIC X(10).           0025900
023400* FROM CHAR. 267 TO 272.                                          0026000
023500     03  HR-PROV-NO.                                              0026100
023600         05  HR-MEDICARE-PROV-NO             PIC X(6).            0026200
023700* FROM CHAR. 273 TO 276.                                          0026300
023800     03  HR-ICS-DATA.                                             0026400
024000         05  HR-OTHER-CO-INS-AMOUNT          COMP-3               0026500
024100                                             PIC 9(5)V99.         0026600
024300* FROM CHAR. 277 TO 282.                                          0026700
024400     03  HR-250-PLUS-DATA.                                        0026800
024500* FROM CHAR. 277 TO 281.                                          0026900
024600         05  HR-PATIENT-FIRST-NAME           PIC X(5).            0027000
024800* CHAR. 282.                                                      0027100
024900         05  HR-OLD-ERG-IND                  PIC X.               0027200
025000*                                                                 0027300
025100* FROM CHAR. 283 TO 291.                                          0027400
025200     03  HR-POINT-OF-SERVICE.                                     0027500
025300*                                                                 0027600
025400* CHAR. 283.                                                      0027700
025500         05  HR-POS-NETWORK-IND              PIC X(1).            0027800
025600* FROM CHAR. 284 TO 285.                                          0027900
025700         05  HR-PADDE-PATIENT-ID             PIC S9(3) COMP-3.    0028000
025800*                                                                 0028100
025900* FROM CHAR. 286 TO 291.                                          0028200
026000         05  HR-PRIMARY-CARE-PHYSICIAN       PIC X(6).            0028300
026100*                                                                 0028400
026200* FROM CHAR. 292 TO 356.                                          0028500
026300     03  HR-DRG-DATA-PART-2.                                      0028600
026400*                                                                 0028700
026500* CHAR. 292.                                                      0028800
026600        05  HR-DRG-ALT-LEVEL-CARE            PIC X.               0028900
026700* FROM CHAR. 293 TO 298.                                          0029000
026800        05  HR-DRG-ALT-CARE-DATE.                                 0029100
026900            10  HR-DRG-ALT-CARE-YY           PIC XX.              0029200
027000            10  HR-DRG-ALT-CARE-MM           PIC XX.              0029300
027100            10  HR-DRG-ALT-CARE-DD           PIC XX.              0029400
027200        05  HR-DRG-ALT-CARE-DATE-JUL   REDEFINES                  0029500
027300            HR-DRG-ALT-CARE-DATE.                                 0029600
027400            10  HR-DRG-ALT-CARE-YYY          PIC S9(3) COMP-3.    0029700
027500            10  HR-DRG-ALT-CARE-DDD          PIC S9(3) COMP-3.    0029800
027600* FROM CHAR. 299 TO 303.                                          0029900
027700        05  HR-DRG-DIAGNOSIS-2               PIC X(5).            0030000
027800* CHAR. 304.                                                      0030100
027900        05  FILLER                           PIC X.               0030200
028000* FROM CHAR. 305 TO 309.                                          0030300
028100        05  HR-DRG-DIAGNOSIS-3               PIC X(5).            0030400
028200* CHAR. 310.                                                      0030500
028300        05  FILLER                           PIC X.               0030600
028400* FROM CHAR. 311 TO 315.                                          0030700
028500        05  HR-DRG-DIAGNOSIS-4               PIC X(5).            0030800
028600* CHAR. 316.                                                      0030900
028700        05  FILLER                           PIC X.               0031000
028800* FROM CHAR. 317 TO 321.                                          0031100
028900        05  HR-DRG-DIAGNOSIS-5               PIC X(5).            0031200
029000* CHAR. 322.                                                      0031300
029100        05  FILLER                           PIC X.               0031400
029200* FROM CHAR. 323 TO 327.                                          0031500
029300        05  HR-DRG-PROCEDURE-2               PIC X(5).            0031600
029400* FROM CHAR. 328 TO 329.                                          0031700
029500        05  FILLER                           PIC XX.              0031800
029600* FROM CHAR. 330 TO 335.                                          0031900
029700        05  HR-DRG-PROC-DATES-2              PIC X(6).            0032000
029800* FROM CHAR. 336 TO 340.                                          0032100
029900        05  HR-DRG-PROCEDURE-3               PIC X(5).            0032200
030000* FROM CHAR. 341 TO 342.                                          0032300
030100        05  FILLER                           PIC XX.              0032400
030200* FROM CHAR. 343 TO 348.                                          0032500
030300        05  HR-DRG-PROC-DATES-3              PIC X(6).            0032600
030400* FROM CHAR. 349 TO 350.                                          0032700
030500        05  HR-DRG-DISCHARGE-STATUS          PIC X(2).            0032800
030600* CHAR. 351.                                                      0032900
030700        05  HR-DRG-TYPE-ADMISSION            PIC S9    COMP-3.    0033000
030800* FROM CHAR. 352 TO 356.                                          0033100
030900        05  HR-UBF-ADMIT-DIAG-CODE           PIC X(5).            0033200
031000* CHAR. 357.                                                      0033300
031100    03  HR-PMT-VOUCHER-ERROR-CODE            PIC X.               0033400
031200* FROM CHAR. 358 TO 359.                                          0033500
031300    03  HR-CORE-PROCESS-IND                  PIC X(2).            0033600
031400* FROM CHAR. 360 TO 361.                                          0033700
031500    03  HR-TC-RELATED-FIELDS.                                     0033800
031700* CHAR. 360.                                                      0033900
031800        05  HR-TEAM-CARE-REPORTING-IND       PIC X.               0034000
031900* CHAR. 361.                                                      0034100
032000        05  HR-TC-INDICATOR                  PIC X.               0034200
032100* CHAR. 362.                                                      0034300
032200     03  HR-TC-COINS-MOD                     PIC X.               0034400
032300     03  HR-IRS-PENALTY-INDICATOR REDEFINES HR-TC-COINS-MOD       0034500
032400                                             PIC X.               0034600
032500*                                                                 0034700
032600* FROM CHAR. 363 TO 365.                                          0034800
032700     03  HRCR-RIDER-CODE                     PIC X(3).            0034900
032800         88 HRCR-NO-RIDER                    VALUE ZEROES.        0035000
032900* CHAR. 366.                                                      0035100
033000     03  HRCR-CONVERTED-RIDER-IND            PIC X.               0035200
033100         88 HRCR-CONVERTED-RIDER             VALUE '1'.           0035300
033200         88 HRCR-NOT-CONVERTED-RIDER         VALUE '0'.           0035400
033300*                                                                 0035500
033300* FROM CHAR. 367 TO 625                                           0035600
033400******************************************************************0035700
033500***                   CONVERSION NEW FIELDS                    ***0035800
033600******************************************************************0035900
033700* FROM CHAR. 367 TO 368.                                          0036000
033800     03  HR-EMHC-PROCESSING-IND              PIC X(2).            0036100
033900* FROM CHAR. 369 TO 373.                                          0036200
034000     03  HR-DIAG-CODE-265                    PIC X(5).            0036300
034100* CHAR. 374.                                                      0036400
034200     03  HR-TC-PENALTY-INDICATOR             PIC X(1).            0036500
034300* FROM CHAR. 375 TO 376.                                          0036600
034400     03  HR-TC-PENALTY-CODE                  PIC S9(3) COMP-3.    0036700
034500* CHAR. 377.                                                      0036800
034600     03  HR-ERG-IND                          PIC X(1).            0036900
034700* CHAR. 378.                                                      0037000
034800     03  HR-FINAN-IND                        PIC X(1).            0037100
034900* CHAR. 379.                                                      0037200
035000     03  HR-MATERNITY                        PIC X(1).            0037300
035100* CHAR. 380.                                                      0037400
035200     03  HR-HOSPICE-IND                      PIC X(1).            0037500
035300* CHAR. 381.                                                      0037600
035400     03  HR-RATE-TYPE-IND                    PIC X(1).            0037700
035500* FROM CHAR. 382 TO 387.                                          0037800
035600     03  HR-ERIC-GROUP-NO                    PIC X(6).            0037900
035700* FROM CHAR. 388 TO 390.                                          0038000
035800     03  HR-ERIC-SUB-DIV                     PIC X(3).            0038100
035900* CHAR. 391.                                                      0038200
036000     03  HR-REMUN-SAVINGS-CODE-1             PIC X.               0038300
036100                                                                  0038400
036200* FROM CHAR. 392 TO 396.                                          0038500
036300     03  HR-REMUN-SAVINGS-AMT-1              COMP-3               0038600
036400                                             PIC S9(6)V99.        0038700
036500* CHAR. 397.                                                      0038800
036600     03  HR-REMUN-SAVINGS-CODE-2             PIC X.               0038900
036700                                                                  0039000
036800* FROM CHAR. 398 TO 402.                                          0039100
036900     03  HR-REMUN-SAVINGS-AMT-2              COMP-3               0039200
037000                                             PIC S9(6)V99.        0039300
037100* CHAR. 403.                                                      0039400
037200     03  HR-REMUN-SAVINGS-CODE-3             PIC X.               0039500
037300                                                                  0039600
037400* FROM CHAR. 404 TO 408.                                          0039700
037500     03  HR-REMUN-SAVINGS-AMT-3              COMP-3               0039800
037600                                             PIC S9(6)V99.        0039900
037700* FROM CHAR. 409 TO 412.                                          0040000
037800     03  HR-ADMIN-EXPENSE                    PIC S9(5)V99 COMP-3. 0040100
037900* FROM CHAR. 413 TO 416.                                          0040200
038000     03  HR-ACCESS-FEE                       PIC S9(5)V99 COMP-3. 0040300
038100* CHAR. 417                                                       0040400
038200     03  HR-SC-CROSOVR-IND                   PIC X.               0040500
038300* CHAR. 418                                                       0040600
038400     03  HR-PGP                              PIC X.               0040700
038700******************************************************************0040800
038400* NEW FIELDS AS OF MAY 94 MONTH END                               0040900
038500******************************************************************0041000
038600* CHAR. 419                                                       0041100
038700     03  HR-CHARGE-REGISTER-IND              PIC X.               0041200
038800            88  HR-SUB-DIVISION              VALUE 'S'.           0041300
038900            88  HR-ROLLUP                    VALUE ' '.           0041400
039000* FROM CHAR. 420 TO 421.                                          0041500
039100     03  HR-PRODUCT-CODE                     PIC X(2).            0041600
039200            88  HR-PRODUCT-HMO               VALUE '01'.          0041700
039300            88  HR-PRODUCT-POS               VALUE '02'.          0041800
039400            88  HR-PRODUCT-PPO               VALUE '03'.          0041900
039500            88  HR-PRODUCT-CODE-VALID                             0042000
039600                VALUE '01' '02' '03'.                             0042100
039700* CHAR. 422                                                       0042200
039800     03  HR-NEGOTD-RATE-IND                  PIC X.               0042300
039900* FROM CHAR. 423 TO 427.                                          0042400
040000     03  HR-NEGOTD-NET-SAVINGS-AMT           PIC S9(7)V99 COMP-3. 0042500
040100* FROM CHAR. 428 TO 431.                                          0042600
040200     03  HR-REMUN-PAYROLL-LOC                PIC X(04).           0042700
040210* CHAR. 432                                                       0042800
040200     03  HR-GROUP-QUARTILE                   PIC X(01).           0042900
040210* CHAR. 433                                                       0043000
040200     03  HR-GROUP-TIER                       PIC X(01).           0043100
040210* FROM CHAR. 434 TO 434.                                          0043200
040200     03  HR-BILLING-IND                      PIC X(01).           0043500
040200             88  HR-ALABAMA-BILING           VALUE 'A'.           0044800
040210* FROM CHAR. 435 TO 436.                                          0043400
040200     03  HR-GROUP-COUNTY                     PIC X(02).           0043500
040210* FROM CHAR. 437 TO 441.                                          0043600
040200     03  HR-REMUN-COVERED-CHARGES            PIC S9(06)V99 COMP-3.0043700
040210* FROM CHAR. 442 TO 453.                                          0043800
040200     03  HR-TOPPS-DATA-FIELDS.                                    0043900
040210* FROM CHAR. 442 TO 446.                                          0044000
040200         05  HR-TOPPS-GROUP-NO              PIC X(05).            0044100
040210* FROM CHAR. 447 TO 448.                                          0044200
040200         05  HR-TOPPS-TREATM-TYPE           PIC X(02).            0044300
040210* FROM CHAR. 449 TO 452.                                          0044400
040200         05  HR-TOPPS-BENEFIT-PKG           PIC X(04).            0044500
040210* CHAR. 453                                                       0044600
040200         05  HR-TOPPS-NETWORK-CHOICE        PIC X(01).            0044700
040200             88  HR-NETWORK-CHOICE-PRESTIGE VALUE 'P'.            0044800
040200             88  HR-NETWORK-CHOICE-SELECT    VALUE 'S'.           0044900
040210* CHAR. 454                                                       0045000
040200         05  HR-SPC-CHK-PAY-SUBS-IND        PIC X(01).            0045100
040210* CHAR. 455                                                       0045200
040200         05  HR-DED-CARRYOVER-IND           PIC X(01).            0045300
040210* CHAR. 456                                                       0045400
040200         05  HR-ORIGINAL-CLAIM-IND          PIC X(01).            0045500
040210* FROM CHAR. 457 TO 463.                                          0045600
040220         05   HR-ADJUSTM-REASON-CODE         PIC X(03).           0045700
040210* FROM CHAR. 464 TO 468.                                          0045800
040220     03   HR-MAGELLAN-RATE        PIC S9(05)V99 COMP-3.           0045900
040210* FROM CHAR. 464 TO 469.                                          0046000
040200     03  HR-ORIGINAL-GROUP-NUMBER       PIC X(06).                0046100
00090          88  HR-VALID-GROUP-NUMBERS         VALUE '000000' THRU   0046200
00091                                                   '999999'.       0046300
040210* FROM CHAR. 470 TO 475.                                          0046400
040200     03  HR-SBU-INDICATOR                    PIC X(02).           0046500
040200     03  HR-SBU-SUBCAT1                      PIC X(02).           0046600
040200     03  HR-SBU-SUBCAT2                      PIC X(02).           0046700
040300******************************************************************0046800
025100* FROM CHAR. 476 TO 625 - 150 BYTES ADDED FOR NOV 96 EOM.         0046900
040300******************************************************************0047000
039200* FROM CHAR. 476 TO 481.                                          0047100
 25200     03  HR-CRS-DATA-ELEMENT.                                     0047200
               05 HR-ADM-RCVD-DATE.                                     0047300
039300             10 HR-ADM-RCVD-YR   PIC XX.                          0047400
039300             10 HR-ADM-RCVD-MO   PIC XX.                          0047500
039300             10 HR-ADM-RCVD-DA   PIC XX.                          0047600
039200* CHAR. 482                                                       0047700
               05 HR-CHGBK-TO-AHS-IND PIC X.                            0047800
039200* FROM CHAR. 483 TO 487.                                          0047900
               05 HR-CORE-SYS-FIN-DAYS PIC S9(5)V9(4) COMP-3.           0048000
039200* FROM CHAR. 488 TO 490.                                          0048100
               05 HR-FINANCIAL-CASES   PIC S9V9(4)    COMP-3.           0048200
039200* FROM CHAR. 491 TO 493.                                          0048300
               05 HR-DIAG-CDE          PIC XXX.                         0048400
      * CHAR. 494                                                       0048500
               05 HR-NEW-BORN-CODE     PIC X.                           0048600
039200* FROM CHAR. 495 TO 625                                           0048700
           03  HR-SRS-ELEMENTS.                                         0048800
      * CHAR. 495                                                       0048900
039300         05  HR-PLS-PROG-CD            PIC X.                     0049000
      * CHAR. 496 TO 497.                                               0049100
039300         05  HR-PLS-UWRT-CORP-CD       PIC XX.                    0049200
      * CHAR. 498                                                       0049300
039300         05  HR-FILLER                 PIC X.                     0049400
      * FROM CHAR. 499 TO 500.                                          0049500
039300         05  HR-PLS-CLS-RISK-CD        PIC XX.                    0049600
      * FROM CHAR. 501 TO 502.                                          0049700
039300         05  HR-PLS-RISK-POOL-CD       PIC XX.                    0049800
      * FROM CHAR. 503 TO 504.                                          0049900
039300         05  HR-PLS-CONTRACT-TYP-CD    PIC XX.                    0050000
      * FROM CHAR. 505 TO 507.                                          0050100
039300         05  HR-PLS-CONTRACT-NO        PIC S9(5) COMP-3.          0050200
      * CHAR. 508                                                       0050300
039300         05  HR-PLS-COV-TYP-CD         PIC X.                     0050400
      * FROM CHAR. 509 TO 511.                                          0050500
039300         05  HR-PLS-CATEG-NO           PIC X(3).                  0050600
      * CHAR. 512                                                       0050700
039300         05  HR-PLS-BENE-LEV-NO        PIC X.                     0050800
      * FROM CHAR. 513 TO 516.                                          0050900
039300         05  HR-PLS-REP-CODE           PIC X(4).                  0051000
      * FROM CHAR. 517 TO 522.                                          0051100
039300         05  HR-PLS-PROD-COMB-CODE     PIC X(6).                  0051200
      * FROM CHAR. 523 TO 524                                           0051300
039300         05  HR-PLS-MARKET-SEGMENT     PIC XX.                    0051400
      * FROM CHAR. 525 TO 528                                           0051500
039300         05  HR-DRG-IL-POOL-A-PYMT     PIC S9(05)V99 COMP-3.      0051600
      * FROM CHAR. 525 TO 528                                           0051700
039300         05  HR-POOL-A-PAYMT REDEFINES                            0051800
039300             HR-DRG-IL-POOL-A-PYMT     PIC S9(05)V99 COMP-3.      0051900
      * FROM CHAR. 529 TO 532                                           0052000
039300         05  HR-DRG-OL-POOL-A-PYMT     PIC S9(05)V99 COMP-3.      0052100
      * FROM CHAR. 533 TO 536                                           0052200
               05  HR-DRG-IL-POOL-B-PYMT           PIC S9(5)V99 COMP-3. 0052300
               05  HR-POOL-B-PAYMT                 REDEFINES            0052400
                   HR-DRG-IL-POOL-B-PYMT           PIC S9(5)V99 COMP-3. 0052500
      * FROM CHAR. 537                                                  0052600
               05  HR-POOL-REGION-CODE             PIC X(01).           0052700
      * FROM CHAR. 538                                                  0052800
               05  HR-GME-GROUP-IND                PIC X(01).           0052900
      * FROM CHAR. 539 TO 542.                                          0053000
               05  HR-GME-SUBSCRIBER-COUNT         PIC S9(7) COMP-3.    0053100
      * FROM CHAR. 543 TO 545.                                          0053200
               05  HR-BLUE-SHIELD-PLAN-CODE        PIC X(03).           0053300
      * FROM CHAR. 547 TO 550.                                          0053400
               05  HR-DRG-IL-POOL-C-PYMT           PIC S9(05)V99 COMP-3.0053500
      * FROM CHAR. 551 TO 554.                                          0053600
               05  HR-DRG-IL-POOL-D-PYMT           PIC S9(05)V99 COMP-3.0053700
      * FROM CHAR. 555 TO 555.                                          0053800
               05  HR-CAPITATION-IND               PIC X(01).           0053900
      * FROM CHAR. 556 TO 556.                                          0054000
               05  HR-MSK-CATEGORY                 PIC X(01).           0054100
011840             88  HR-MSK-NOT-CANCER-RELATED     VALUE '1'.         0054200
011840             88  HR-MSK-IN-NETWORK            VALUE '2'.          0054300
011840             88  HR-MSK-ENCOUNTER             VALUE '3'.          0054400
011840             88  HR-MSK-GROSS-CAPITATION      VALUE '4'.          0054500
011840             88  HR-MSK-OUT-NETWORK           VALUE '5'.          0054600
      * FROM CHAR. 557 TO 557.                                          0054700
               05  HR-ACCTG-REPORT-IND             PIC X(01).           0054800
011840             88  HR-NOT-ACCTG-CATEGORY        VALUE ' '.          0054900
011840             88  HR-NOT-ACCTG-OLD-RECORD      VALUE LOW-VALUES.   0055000
011840             88  HR-ACCTG-REPORT-YES          VALUE 'Y'.          0055100
011840             88  HR-ACCTG-REPORT-NO           VALUE 'N'.          0055200
      * FROM CHAR. 558 TO 558.                                          0055300
               05  HR-BYPASS-SUPP-AND-POOL-IND     PIC X(01).           0055400
      * FROM CHAR. 559 TO 560.                                          0055500
               05  HR-TOPPS-PROGRAM-NUM            PIC X(02).           0055600
      * FROM CHAR. 561 TO 563.                                          0055700
               05  HR-TOPPS-SUB-PROG-NUM           PIC X(03).           0055800
      * FROM CHAR. 564 TO 568.                                          0055900
               05  HR-MCARE-INTEREST-AMT    PIC S9(07)V99 COMP-3.       0056000
      * FROM CHAR. 561 TO 563.                                          0056100
               05  HR-TOPPS-MARKET-SEGMENT         PIC X(02).           0056200
      * FROM CHAR. 569 TO 572.                                          0056300
               05  HR-DRG-OL-POOL-B-PYMT    PIC S9(05)V99 COMP-3.       0056400
      * FROM CHAR. 573 TO 576.                                          0056500
               05  HR-DRG-OL-POOL-C-PYMT    PIC S9(05)V99 COMP-3.       0056600
      * FROM CHAR. 577 TO 580.                                          0056700
               05  HR-DRG-OL-POOL-D-PYMT    PIC S9(05)V99 COMP-3.       0056800
      * FROM CHAR. 581 TO 582.                                          0056900
               05  HR-SCHEDULE-IND          PIC X(02).                  0057000
      * FROM CHAR. 583 TO 585.                                          0057100
027210         05  HR-NO-DAYS-PROMPT-PAY-INT COMP-3 PIC S9(5).          0057200
      * FROM CHAR. 586 TO 593.                                          0057300
027210         05  HR-CLK-START-DAT-PROM-PAY.                           0057400
027210             15  HR-CLK-START-DAT-PROM-PAY-CC    PIC X(2).        0057500
027210             15  HR-CLK-START-DAT-PROM-PAY-YY    PIC X(2).        0057600
027210             15  HR-CLK-START-DAT-PROM-PAY-MM    PIC X(2).        0057700
027210             15  HR-CLK-START-DAT-PROM-PAY-DD    PIC X(2).        0057800
      * FROM CHAR. 594 TO 594.                                          0057900
               05  HR-EMHC-IND                 PIC X.                   0058000
                      88  HR-EMHC-YES             VALUE 'Y'.            0058100
      * FROM CHAR. 595 TO 598                                           0058200
039300         05  HR-DRG-IL-POOL-F-PYMT     PIC S9(05)V99 COMP-3.      0058300
039300         05  HR-OTHER-STATE-SURCHARGE REDEFINES                   0058400
039300             HR-DRG-IL-POOL-F-PYMT     PIC S9(05)V99  COMP-3.     0058500
      * FROM CHAR. 600 TO 600.                                          0058600
039300         05  HR-SOURCE-CODE            PIC X(01).                 0058700
                      88  HR-SOURCE-GMEDS             VALUE '2'.        0058800
                      88  HR-SOURCE-TBS               VALUE '3'.        0058900
                      88  HR-SOURCE-MAIL-ORDER-DRUG VALUE '4'.          0059000
                      88  HR-SOURCE-NASCO-CONTROL     VALUE '5'.        0059100
                      88  HR-SOURCE-ALB-CONV-HIST     VALUE '6'.        0059200
                      88  HR-SOURCE-CS90              VALUE '7'.        0059300
                      88  HR-SOURCE-ITS               VALUE '8'.        0059400
                      88  HR-SOURCE-TOPPS             VALUE '9'.        0059500
                      88  HR-SOURCE-GME               VALUE 'A'.        0059600
                      88  HR-SOURCE-CAPITATION        VALUE 'C'.        0059700
                      88  HR-SOURCE-MSK               VALUE 'E'.        0059800
      * FROM CHAR. 601 TO 605                                           0059900
00039          05  HR-PROVIDER-TAX-ID             PIC S9(09) COMP-3.    0060000
      * FROM CHAR. 606 TO 606                                           0060100
00039          05  HR-ITSHOME-ECRP-IND            PIC X(01).            0060200
                   88  HR-ITS-HOME                 VALUE 'I'.           0060300
                   88  HR-ECRP                     VALUE 'E'.           0060400
      * FROM CHAR. 607 TO 607                                           0060500
00039          05  FILLER                         PIC X(01).            0060600
      * FROM CHAR. 608 TO 608                                           0060700
00039          05  HR-SPC-CHK-IND                 PIC X(01).            0060800
      * FROM CHAR. 609 TO 609                                           0060900
027210         05  HR-PAY-TO-PROVIDER-IND          PIC X.               0061000
                   88  HR-PAY-PROVIDER             VALUE 'Y'.           0061100
                   88  HR-PAY-SUBSCRIBER           VALUE 'N'.           0061200
      * FROM CHAR. 610 TO 612                                           0061300
027210         05  HR-ITS-SCCF-SERIAL-NUMBER       PIC X(03).           0061400
      * FROM CHAR. 613 TO 613.                                          0061500
027210         05  HR-NASCO-BANK-ACCT-TYPE         PIC X(01).           0061600
      * FROM CHAR. 614 TO 614.                                          0061700
027210         05  HR-PRODUCT-CLASSIF-CODE         PIC X(01).           0061800
      * FROM CHAR. 615 TO 615.                                          0061900
027210         05  HR-INTEREST-IND                 PIC X(01).           0062000
027210              88  HR-PROMPT-PAY-INT          VALUE 'P'.           0062100
027210              88  HR-MCARE-RISK-INT          VALUE 'M'.           0062200
      * CHAR. 616-618                                                   0062300
039300         05  HR-PLS-LOB-CD             PIC X(03).                 0062400
      * CHAR. 619-624                                                   0062500
039300         05  HR-HOSP-CODE-1-6          PIC X(06).                 0062600
      * FROM CHAR. 625 TO 625.                                          0062700
           03  FILLER                      PIC X(01).                   0062800
      *                                                                 0062900
      ******************************************************************0063000
RS1298* FROM CHAR. 626 TO 700.******************************************0063100
      * *********ADDITIONAL SRS ELEMENTS************                    0063200
      * FROM CHAR. 626 TO 627.                                          0063300
           03  HR-PLS-BPI-NUMBER           PIC S9(3) COMP-3.            0063400
      * FROM CHAR. 628 TO 631.                                          0063500
           03  HR-PLS-AGENCY-NUMBER.                                    0063600
                05  HR-PLS-MARKET-SEGEMNT       PIC X(01).              0063700
                05  HR-PLS-ACCOUNT-TYPE         PIC X(01).              0063800
                05  HR-PLS-WP-MKT-SEGMENT      PIC X(02).               0063900
      * FROM CHAR. 632 TO 640.                                          0064000
           03  HR-PLS-GRP-KEY-ID           PIC X(09).                   0064100
      * FROM CHAR. 641 TO 645.                                          0064200
           03  HR-CAP-PAY-VENDOR-AMT      PIC S9(7)V99 COMP-3.          0064300
      * FROM CHAR. 646 TO 647.                                          0064400
           03  HR-PROFITABILITY-CODE       PIC X(02).                   0064500
      * FROM CHAR. 648 TO 649.                                          0064600
           03  HR-HI-PROFITABILITY-CODE    PIC X(02).                   0064700
      * FROM CHAR. 650 TO 650.                                          0064800
           03  HR-BOOK-OF-BUSINESS         PIC X(01).                   0064900
      * FROM CHAR. 651 TO 652.                                          0065000
           03  HR-PACKAGE-NO               PIC S9(04) COMP.             0065100
      * FROM CHAR. 653 TO 653.                                          0065200
           03  HR-COLLECTION-GRPCDE-OLD    PIC X(01).                   0065300
      * FROM CHAR. 654 TO 655.                                          0065400
           03  HR-PLS-UWRT-CORP-CD-OLD     PIC X(02).                   0065500
      * FROM CHAR. 656 TO 658.                                          0065600
           03  HR-BC-PLAN-COVERAGE-CODE-OLD   PIC X(03).                0065700
      * FROM CHAR. 659 TO 661.                                          0065800
           03  HR-BLUE-SHIELD-PLAN-CODE-OLD   PIC X(03).                0065900
      * FROM CHAR. 662 TO 665.                                          0066000
           03  HR-TC-PENALTY-AMT              PIC S9(05)V99 COMP-3.     0066100
      * FROM CHAR. 666 TO 670.                                          0066200
           03  HR-PRODUCT-FUND-CODE           PIC X(05).                0066300
      * FROM CHAR. 671 TO 675.                                          0066400
           03  HR-RATING-COMB                 PIC X(05).                0066500
      * FROM CHAR. 676 TO 680.                                          0066600
019900     03 HR-PRODUCT-VARIATION-CODE       PIC X(5).                 0022600
      * FROM CHAR. 681 TO 684.                                          0066600
019900     03 HR-DEST-ID                      PIC X(4).                 0022600
      * FROM CHAR. 685 TO 685.                                          0066600
019900     03 HR-NO-SHIELD-IND                PIC X(1).                 0022600
019900        88 HR-NO-SHIELD             VALUE 'N'.                    0022600
      * FROM CHAR. 686 TO 715.                                          0066600
019900     03 HR-PROVIDER-NAME             PIC X(30).                   0022600
      * FROM CHAR. 716 TO 720.                                          0066600
019900     03 HR-PROVIDER-ZIP-CODE         PIC X(05).                   0022600
      * FROM CHAR. 721 TO 733.                                          0066600
019900     03 HR-MEDICAL-REC-18-30         PIC X(13).                   0022600
      * FROM CHAR. 734 TO 738.                                          0066800
           03  HR-TOTAL-DIFF-AMT           PIC S9(07)V99 COMP-3.        0066100
      * FROM CHAR. 739 TO 747.                                          0066800
019900     03 HR-HIPAA-ALT-ID              PIC X(09).                   0022600
      * FROM CHAR. 748 TO 749.                                          0066800
019900     03 HR-DISEASE-MGMT-IND          PIC X(02).                   0022600
019900         88 HR-RARE-CHRONIC-DISEASE        VALUE '01'.            0022600
019900         88 HR-KIDNEY-DISEASE              VALUE '02'.            0022600
019900         88 HR-CONGESTIVE-HEART-FALURE     VALUE '03'.            0022600
019900         88 HR-PULMONARY-DISEASE           VALUE '04'.            0022600
019900         88 HR-DIABETES                    VALUE '05'.            0022600
019900         88 HR-CORONARY-ARTERY-DISEASE     VALUE '06'.            0022600
019900         88 HR-ASTHMA                      VALUE '07'.            0022600
019900         88 HR-IMPACT-CONDITIONS           VALUE '08'.            0022600
019900         88 HR-KIDNEY-DISEASE-TELEPHONC     VALUE '09'.           0022600
      * FROM CHAR. 750 TO 754.                                          0066800
019900     03 HR-DISEASE-MGMT-RATE      PIC S9(07)V99 COMP-3.           0022600
      * FROM CHAR. 755 TO 758.                                          0066800
019900     03 HR-CDHP-HRA-AMOUNT        PIC S9(05)V99 COMP-3.           0022600
      * FROM CHAR. 759 TO 761.                                          0066800
019900     03 HR-DEPENDENT-NUMBER          PIC X(03).                   0022600
      *** FOLLOWING TWO FIELDS ARE ADDED FOR MULTIPURSE PROJECT         0107400
      * FROM CHAR. 762 TO 762.                                          0066800
           03 HR-CDHP-IND                     PIC X.                    0066900
               88 HR-AXP-MULTIPURSE-HSA       VALUE 'C'.                0066900
               88 HR-AXP-DEBIT-HSA            VALUE 'E'.                0066900
      * FROM CHAR. 763 TO 766.                                          0066800
019900     03 HR-CDHP-HSA-AMOUNT        PIC S9(05)V99 COMP-3.           0022600
           03 HR-SIC-CODE              PIC 9(4).                        0066800
      * NPI CHANGES STARTS HERE.                                        0066800
      * FROM CHAR. 773 TO 782.                                          0066800
           03  HR-NPI-CODE             PIC X(10).                       0066800
      * NPI CHANGES ENDS HERE.                                          0066800
      * FROM CHAR. 783 TO 784.                                          0066800
           03  HR-LINE-CATEGORY-CODE      PIC X(02).                    0066800
      * FROM CHAR. 785 TO 785.                                          0066800
           03  HR-ACCESS-FEE-IND          PIC X(01).                    0066800
      * FROM CHAR 786 TO 786.
           03  HR-NIA-CAP-IND                 PIC X.
               88  HR-NIA-INSCOPE-PAR         VALUE '1'.
               88  HR-NIA-INSCOPE-NONPAR      VALUE '2'.
               88  HR-NIA-OUTSCOPE            VALUE '3'.
               88  HR-NIA-NON-CAP             VALUE '4'.
      * FROM CHAR. 787 TO 791.                                          0066800
           03  HR-CAP-PAY-NIA-AMT         PIC S9(7)V99 COMP-3.          0066800
      * FROM CHAR. 792 TO 796.
           03  HR-MCS-PROMPT-PAY-INT      PIC S9(7)V99 COMP-3.
      * FROM CHAR. 797 - 797.
           03  HR-NCN-INDICATOR              PIC X.
               88  HR-NCN-CLM-ADJSTMNT         VALUE 'A'.
               88  HR-BYPASS-NCN-SUSPENDS      VALUE 'B'.
               88  HR-NCN-DENIED-LINE          VALUE 'D'.
               88  HR-NCN-ITS-PROF-NEG         VALUE 'F'.
               88  HR-NCN-NON-ITS-INST-NEG     VALUE 'G'.
               88  HR-NCN-ITS-DIS-PAY-PROV     VALUE 'H'.
               88  HR-NCN-ITS-DIS-PAY-MEM      VALUE 'I'.
               88  HR-NCN-ITS-INST-NEG         VALUE 'J'.
               88  HR-NCN-NON-ITS-INST-DIS-MEM VALUE 'M'.
               88  HR-NCN-NO-NEG               VALUE 'N'.
               88  HR-NCN-NON-ITS-DIS-PAY-PVDR VALUE 'P'.
               88  HR-NCN-ITS-PAYEE-FLIP-NEG   VALUE 'Q'.
               88  HR-NCN-ITS-PAYEE-FLIP-DIS   VALUE 'R'.
               88  HR-CLAIM-SENT-TO-NCN        VALUE 'S'.
               88  HR-NCN-DIS-PRICE-NO-CHRG    VALUE 'T'.
               88  HR-NCN-NEG-FLIPD-TO-DIS     VALUE 'U'.
               88  HR-NCN-NON-ITS-PROF-NEG     VALUE 'Y'.
               88  HR-NCN-NO-PRICE             VALUE 'X'.
      * FROM CHAR. 798 - 802.
      * NCN COMMISSION ADDED   - CR#160671
           03  HR-NCN-FEE                 PIC S9(7)V99 COMP-3.
      * NCN GROUP FEE ADDED    - CR#160671
      * FROM CHAR. 803 - 807.
           03  HR-NCN-GROUP-FEE           PIC S9(7)V99 COMP-3.
      * CHANGES FOR NCN EXPANSION END
      * FROM CHAR. 808 TO 808.
           03  HR-PLS-DRG-SVY-IN          PIC X.
      * CHANGES FOR APR DRG END
      * CHANGES FOR RSI INDICATOR START
      * FROM CHAR. 809 TO 809.
           03  HR-RSI-IND                 PIC X.
               88  HR-RSI-NEWYORK           VALUE 'N'.
               88  HR-RSI-CONNECTICUT       VALUE 'C'.
      * CHANGES FOR RSI INDICATOR END
      * CHANGES FOR NARROW NETWORK START
      * FROM CHAR. 810 TO 810.
           03  NARROW-NET-IND          PIC X.
               88  UNITEHERE             VALUE 'U'.
               88  NARROWNET             VALUE 'N'.
               88  NON-NARROW-UNITE      VALUE ' '.
      * CHANGES FOR NARROW NETWORK END
      * FROM CHAR. 811 TO 812.
           03  H-BLOOM-EXCH-IND          PIC X(02).
               88  H-BLOOM-EXCH            VALUE 'BL'.
      * CHANGES FOR BLOOM LG PROJECT END
      * FROM CHAR. 813 TO 813.
           03  H-EFT-IND                 PIC X(01).
               88  H-EFT-CLAIM             VALUE 'Y'.
               88  H-NON-EFT-CLAIM         VALUE 'N'.
      * CHANGES FOR ICD10 IRS EXPANSION START
      * FROM CHAR. 814 TO 820.
           03  H-ICD10-PRIMARY-DIAG-CODE PIC X(07).
      * FROM CHAR. 821 TO 822.
           03  H-ICD9-ICD10-IND          PIC X(02).
      * FROM CHAR. 823 TO 829.
           03  H-ICD10-PRIMARY-PROC-CODE PIC X(07).
      * FROM CHAR. 830 TO 837.
           03  H-ICD10-PRM-PROC-DATE     PIC X(08).
      * FROM CHAR. 838 TO 844.
           03  H-EXPND-CHECK-NUMBER      PIC S9(13) COMP-3.
      * CHANGES FOR ICD10 IRS EXPANSION END
      * FROM CHAR. 845 TO 850.
           03  H-PC2-PROGRAM-CODE        PIC X(6).
      * FROM CHAR. 851 TO 856.
           03  H-PC2-FUND-TYPE           PIC X(6).
      * CHANGES FOR PC2 PROJECT START END
      * CHANGES FOR ITS PROJECT START
           03  H-ITS-SUPP-AMT            PIC S9(7)V99 COMP-3.
      * CHANGES FOR ITS PROJECT END
      * FROM CHAR. 857 TO 970.
           03  FILLER                      PIC X(109).
