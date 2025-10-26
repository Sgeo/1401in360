//HERC01B JOB (1),'SIM1401 TOM BROWN',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=HERC01,PASSWORD=CUL8TR,
//             REGION=1024K
//STEP1 EXEC ASMFCLG PARM.ASM=(OBJ,NODECK),MAC1='SYS2.MACLIB'
//ASM.SYSIN DD *
*SIM1401  START 0                                                       00000100
***********************************************************************
*                                                                     *
*         1401 SIMULATOR AS MODIFIED BY TOM BROWN WRO AS OF 71182     *
*                                                                     *
*********************************************************************** 00000200
*                                                                     * 00000300
*                                                                     * 00000400
*     1 4 0 1   S I M U L A T O R   F O R   S Y S T E M / 3 6 0       * 00000500
*                                                                     * 00000600
*                                                                     * 00000700
*                                                                     * 00000800
*     THIS PROGRAM WILL SIMULATE A 1401 ON A SYSTEM/360.  THE         * 00000900
* SYSTEM/360 MUST HAVE AT LEAST 65K, STANDARD INSTURCTION SET, ONE    * 00001000
* 1052, ONE 2540, AND ONE PRINTER. THE 1401 FEATURES SUPPORTED ARE    * 00001100
* ADVACED PROGRAMMING, SENSE SWITCHES, TAPES, MULTIPLY, DIVIDE,       * 00001200
* 16K CORE, AND ALL STANDARD INSTRUCTIONS EXCEPT SELECT STACKER.      * 00001300
* OPERATOR CONTROL IS THROUGH THE 1052, USING THE FOLLOWING ENTRIES   * 00001400
*                                                                     * 00001500
*                                                                     * 00001600
*          SRS  -  START RESET                                        * 00001700
*          STT  -  START                                              * 00001800
*          LDC  -  LOAD FROM CARDS                                    * 00001900
*          LDT  -  LOAD FROM TAPE                                     * 00002000
*          SSS  -  SET SENSE SWITCHES                                 * 00002100
*          TAS  -  TAPE ASSIGNMENT                                    * 00002200
*          CLR  -  CLEAR ALL 1401 CORE                                * 00002300
*          DIS  -  DISPLAY 1401 CORE ON THE PRINTER                   * 00002400
*          ALT  -  ALTER 1401 CORE                                    * 00002500
*          WTM  -  WRITE TAPE MARK                                    * 00002600
*          RWD  -  REWIND TAPE                                        * 00002700
*          TRM  -  TERMINATE THE SIMULATOR                            * 00002800
*                                                                     * 00002900
*                                                                     * 00003000
*                                                                     * 00003100
* 16K BYTES ARE SET ASIDE FOR SIMULATED CORE, WITH EACH BYTE HAVING   * 00003200
* THE FOLOWING FORMAT.                                                * 00003300
*     360 BIT        1401 BIT                                         * 00003400
*        0            UNUSED                                          * 00003500
*        1           WORD MARK                                        * 00003600
*        2               B                                            * 00003700
*        3               A                                            * 00003800
*        4               8                                            * 00003900
*        5               4                                            * 00004000
*        6               2                                            * 00004100
*        7               1                                            * 00004200
*                                                                     * 00004300
*                                                                     * 00004400
*********************************************************************** 00004500
       EJECT                                                            00004600
       USING   SETBS1,15                                                00004700
       USING   SETBS1+4096,14                                           00004800
       USING   SIMCOR,7                                                 00004900
       TITLE  'ADD'                                                     00005000
       USING   A,13                                                     00005100
A      CH      9,=H'7'           DETERMINE INSTRUCTION LENGTH           00005200
       BE      AL7               *                                      00005300
       CH      9,=H'1'           *                                      00005400
       BE      AL1               *                                      00005500
       CH      9,=H'4'           *                                      00005600
       BNE     ILEGLN            *                                      00005700
       LA      6,1(10)           4 CHARACTERS, SET A AND B EQUAL        00005800
       BAL     8,CVAD43          *                                      00005900
       LR      11,5              *                                      00006000
       LR      12,11             *                                      00006100
       B       AL1               *                                      00006200
AL7    LA      6,1(10)           CONVERT ADDRESSES                      00006300
       BAL     8,CVAD43          *                                      00006400
       LR      11,5              *                                      00006500
       LA      6,4(10)           *                                      00006600
       BAL     8,CVAD43          *                                      00006700
       LR      12,5              *                                      00006800
AL1    MVI     POS1,1            SET 1-POSITION INDICATOR               00006900
       MVI     AEND,0            CLEAR A-FIELD ENDED INDICATOR          00007000
       LA      0,1               SET REGISTER FOR FAST SUBTRACTION      00007100
       IC      4,0(10)           GET OP CODE                            00007200
       SRDL    4,1               SAVE LOW ORDER BIT                     00007300
       IC      4,0(11)           GET A-FIELD SIGN                       00007400
       SRL     4,4               *                                      00007500
       SRDL    4,2               *                                      00007600
       IC      4,0(12)           GET B-FIELD SIGN                       00007700
       SRL     4,4               *                                      00007800
       SLDL    4,3               TEST TABLE                             00007900
       N       4,=F'31'          *                                      00008000
       A       4,=A(TBTRCP)      *                                      00008100
       TM      0(4),X'1'         *                                      00008200
       BO      AL1H              COMPLEMENT ADD                         00008300
*                                                                       00008400
*      PERFORM TRUE ADD                                                 00008500
*                                                                       00008600
       MVI     AL1C+1,X'70'      SET TO KEEP SIGN                       00008700
       LA      1,0               CLEAR CARRY                            00008800
AL1A   IC      3,0(12)           GET B-FIELD CHARACTER                  00008900
       LR      6,3               SAVE B-FIELD ZONE                      00009000
       N       3,=F'15'          ISOLATE DIGIT                          00009100
         C     3,=F'11'          Q/ IS DIGIT NUMERIC                    00009200
         BL    *+8               YES                                    00009300
         S     3,=F'8'           NO, ELIMINATE 8 BIT                    00009400
       CH      3,=H'10'          Q/ ZERO                                00009500
       BNE     *+6               NO                                     00009600
       SR      3,3               YES, CLEAR IT                          00009700
       CLI     AEND,1            Q/ IS THERE STILL AN A-FIELD           00009800
       BE      AL1B              NO                                     00009900
       IC      4,0(11)           YES, GET DIGIT                         00010000
       LR      5,4               *                                      00010100
       N       4,=F'15'          *                                      00010200
         C     4,=F'11'          Q/ IS DIGIT NUMERIC                    00010300
         BL    *+8               YES                                    00010400
         S     4,=F'8'           NO, ELIMINATE 8 BIT                    00010500
       CH      4,=H'10'          Q/ ZERO                                00010600
       BNE     *+6               NO                                     00010700
       SR      4,4               YES, CLEAR IT                          00010800
       AR      3,4               ADD A TO B                             00010900
AL1B   AR      3,1               ADD CARRY                              00011000
       LA      1,0               CLEAR CARRY                            00011100
       CH      3,=H'9'           Q/ IS RESULT GREATER THAN 9            00011200
       BNH     AL1C              NO, OK                                 00011300
       SH      3,=H'10'          YES, SUBTRACT 10                       00011400
       LA      1,1               SET CARRY                              00011500
AL1C   NI      0(12),X'00'       STORE RESULT DIGIT                     00011600
       STC     3,AL1D+1          *                                      00011700
       TM      AL1D+1,X'0F'      Q/ IS RESULT ZERO                      00011800
       BC      5,AL1D            NO                                     00011900
       OI      AL1D+1,X'0A'      YES, SET 8-2 BITS                      00012000
AL1D   OI      0(12),0           *                                      00012100
       MVI     AL1C+1,X'40'      SET TO ELIMINATE ZONES                 00012200
       CLI     AEND,1            Q/ HAS A-FIELD ALREADY ENDED           00012300
        BE      AL1E             YES                                    00012400
       SR      11,0              DECREMENT A-FIELD ADDRESS              00012500
       TM      1(11),X'40'       Q/ END OF A-FIELD                      00012600
       BZ      AL1E              NO                                     00012700
       MVI     AEND,1            YES, SET A-FIELD ENDED INDICATOR       00012800
AL1E   SR      12,0              DECREMENT B-FIELD ADDRESS              00012900
       TM      1(12),X'40'       Q/ END OF B-FIELD                      00013000
       BO      AL1F              YES                                    00013100
       MVI     POS1,0            NO, TURN OFF 1-POSITION INDICATOR      00013200
       CLI     AEND,1            Q/ A-FIELD ENDED                       00013300
       BNE     AL1A              NO                                     00013400
       SR      5,5               YES, CLEAR A-FIELD CHARACTER           00013500
       B       AL1A              ADD NEXT POSITION                      00013600
AL1F   CLI     POS1,1            Q/ WAS THIS A 1-POSITION FIELD         00013700
         BE    AL1G1             YES, DONE                              00013800
       N       5,=F'48'          NO, ADD HIGH ORDER ZONES               00013900
       N       6,=F'48'          *                                      00014000
       AR      5,6               *                                      00014100
       SLL     1,4               ADD CARRY                              00014200
       AR      5,1               *                                      00014300
       STC     5,AL1G+1          STORE NEW ZONE                         00014400
       NI      AL1G+1,X'30'      *                                      00014500
AL1G   OI      1(12),0           *                                      00014600
AL1G1    LTR   1,1               Q/ WAS THERE A CARRY                   00014700
       BC      8,NXTOP           NO                                     00014800
       MVI     OVRFLO,1          YES, SET OVERFLOW INDICATOR            00014900
       B       NXTOP                                                    00015000
*                                                                       00015100
*      PERFORM COMPLEMENT ADDITION                                      00015200
*                                                                       00015300
AL1H   LA      1,1               SET CARRY                              00015400
       ST      12,SAVB           SAVE B-FIELD UNITS ADDRESS             00015500
       MVI     AL1L+1,X'70'      SET TO KEEP B-FIELD SIGN               00015600
       IC      3,0(12)           GET B-FIELD SIGN                       00015700
       N       3,=F'48'          *                                      00015800
       CH      3,=H'32'          Q/ IS IT MINUS                         00015900
       BE      AL1I              YES                                    00016000
       OI      0(12),X'30'       NO, PUT PLUS SIGN IN STANDARD FORM     00016100
AL1I   IC      2,0(12)           GET B-FIELD DIGIT                      00016200
       N       2,=F'15'          *                                      00016300
         C     2,=F'11'          Q/ IS DIGIT NUMERIC                    00016400
         BL    *+8               YES                                    00016500
         S     2,=F'8'           NO, ELIMINATE 8 BIT                    00016600
       CH      2,=H'10'          Q/ ZERO                                00016700
       BNE     *+6               NO                                     00016800
       SR      2,2               YES, CLEAR IT                          00016900
       LA      3,9               SET COMPLEMENT                         00017000
       CLI     AEND,1            Q/ HAS A-FIELD PREVIOUSLY ENDED        00017100
       BE      AL1J              YES                                    00017200
       IC      4,0(11)           NO, GET A-FIELD DIGIT                  00017300
       N       4,=F'15'          *                                      00017400
         C     4,=F'11'          Q/ IS DIGIT NUMERIC                    00017500
         BL    *+8               YES                                    00017600
         S     4,=F'8'           NO, ELIMINATE 8 BIT                    00017700
       CH      4,=H'10'          Q/ ZERO                                00017800
       BNE     *+6               NO                                     00017900
       SR      4,4               YES, CLEAR IT                          00018000
       SR      3,4               COMPLEMENT A-FIELD DIGIT               00018100
AL1J   AR      2,3               ADD COMPLEMENT TO B-FIELD DIGIT        00018200
       AR      2,1               ADD CARRY                              00018300
       LA      1,0               CLEAR CARRY                            00018400
       CH      2,=H'9'           Q/ RESULT GREATER THAN 9               00018500
       BNH     AL1K              NO, OK                                 00018600
       SH      2,=H'10'          YES, SUBTRACT 10                       00018700
       LA      1,1               SET CARRY                              00018800
AL1K   STC     2,AL1M+1          STORE RESULT DIGIT                     00018900
AL1L   NI      0(12),0           *                                      00019000
       TM      AL1M+1,X'0F'      Q/ IS RESULT ZERO                      00019100
       BC      5,AL1M            NO                                     00019200
       OI      AL1M+1,X'0A'      YES, SET 8-2 BITS                      00019300
AL1M   OI      0(12),0           *                                      00019400
       MVI     AL1L+1,X'40'      SET TO ELIMINATE B-FIELD ZONES         00019500
       CLI     AEND,1            Q/ HAS A-FIELD ALREADY ENDED           00019600
       BE      AL1N              YES                                    00019700
       SR      11,0              NO, DECREMENT A-FIELD ADDRESS          00019800
       TM      1(11),X'40'       Q/ IS THIS THE END OF THE A-FIELD      00019900
       BZ      AL1N              NO                                     00020000
       MVI     AEND,1            YES, SET A-FIELD ENDED INDICATOR       00020100
AL1N   SR      12,0              DECREMENT B-FIELD ADDRESS              00020200
       TM      1(12),X'40'       Q/ IS THIS THE END OF THE B-FIELD      00020300
       BO      AL1O              YES                                    00020400
       MVI     POS1,0            NO, CLEAR 1-POSITION INDICATOR         00020500
       B       AL1I                                                     00020600
AL1O   LTR     1,1               Q/ CARRY                               00020700
       BC      6,NXTOP           YES, DONE                              00020800
*                                                                       00020900
*      PERFORM RECOMPLEMENT CYCLE                                       00021000
*                                                                       00021100
       LA      1,1               SET CARRY                              00021200
       L       12,SAVB           RESTORE B-FIELD UNITS ADDRESS          00021300
       IC      2,0(12)           GET B-FIELD SIGN                       00021400
       N       2,=F'48'          *                                      00021500
       NI      0(12),X'CF'       SET SIGN TO MINUS                      00021600
       OI      0(12),X'20'       *                                      00021700
       CH      2,=H'32'          Q/ WAS THE B-FIELD SIGN MINUS          00021800
       BNE     AL1P              NO, LEAVE IT MINUS                     00021900
       OI      0(12),X'30'       YES, SET IT PLUS                       00022000
AL1P   IC      3,0(12)           GET B-FIELD DIGIT                      00022100
       N       3,=F'15'          *                                      00022200
       CH      3,=H'10'          Q/ ZERO                                00022300
       BNE     *+6               NO                                     00022400
       SR      3,3               YES, CLEAR IT                          00022500
       LA      4,9               SET COMPLEMENT                         00022600
       SR      4,3               COMPLEMENT THE DIGIT                   00022700
       AR      4,1               ADD CARRY                              00022800
       LA      1,0               CLEAR CARRY                            00022900
       CH      4,=H'9'           Q/ IS THE RESULT GREATER THAN 9        00023000
       BNH     AL1Q              NO, OK                                 00023100
       SH      4,=H'10'          YES, SUBTRACT 10                       00023200
       LA      1,1               SET CARRY                              00023300
AL1Q   STC     4,AL1R+1          STORE RESULT                           00023400
       NI      0(12),X'70'       *                                      00023500
       TM      AL1R+1,X'0F'      Q/ IS RESULT ZERO                      00023600
       BC      5,AL1R            NO                                     00023700
       OI      AL1R+1,X'0A'      YES, SET 8-2 BITS                      00023800
AL1R   OI      0(12),0           *                                      00023900
       SR      12,0              DECREMENT B-FIELD ADDRESS              00024000
       TM      1(12),X'40'       Q/ IS THIS THE END OF THE B-FIELD      00024100
       BZ      AL1P              NO                                     00024200
       B       NXTOP             YES                                    00024300
TBTRCP DC      X'01000100000101000100010000010100'                      00024400
       DC      X'00010001010000010100010000010100'                      00024500
       TITLE  'ZERO AND ADD'                                            00024600
       USING   ZA,13                                                    00024700
ZA     CH      9,=H'1'                                                  00024800
       BE      ZAL1                                                     00024900
       CH      9,=H'7'                                                  00025000
       BE      ZAL7                                                     00025100
       CH      9,=H'4'                                                  00025200
       BNE     ILEGLN                                                   00025300
ZAL7   LA      6,1(10)                                                  00025400
       BAL     8,CVAD43                                                 00025500
       LR      11,5                                                     00025600
       LR      12,5                                                     00025700
       CH      9,=H'4'                                                  00025800
       BE      ZAL1                                                     00025900
       LA      6,4(10)                                                  00026000
       BAL     8,CVAD43                                                 00026100
       LR      12,5                                                     00026200
ZAL1   LR      6,12                                                     00026300
       LR      5,11                                                     00026400
       LA      0,1                                                      00026500
       IC      3,0(11)           SAVE LOW CHARACTER OF A-FIELD          00026600
       STC     3,TEMP1           *                                      00026700
ZAL1A  MVN     0(1,6),0(5)       MOVE NUMERIC                           00026800
       NI      0(6),X'4F'        ELIMINATE ZONE                         00026900
       SR      5,0                                                      00027000
       SR      6,0                                                      00027100
       TM      1(5),X'40'        Q/ END OF A-FIELD                      00027200
       BO      ZAL1E             YES                                    00027300
       TM      1(6),X'40'        NO, END OF B-FIELD                     00027400
       BZ      ZAL1A             NO, MOVE NEXT DIGIT                    00027500
ZAL1C  OI      0(12),X'20'       SET B-FIELD SIGN MINUS                 00027600
       NI      TEMP1,X'30'       Q/ IS A-FIELD MINUS                    00027700
       CLI     TEMP1,X'20'       *                                      00027800
       BE      ZAL1D             YES                                    00027900
       OI      0(12),X'30'       NO, SET B-FIELD SIGN PLUS              00028000
ZAL1D  LR      11,5              SET A-ADDRESS                          00028100
       LR      12,6              SET B-ADDRESS                          00028200
       B       NXTOP                                                    00028300
ZAL1E  TM      1(6),X'40'        ZERO B-FIELD BEYOND RANGE OF A-FIELD   00028400
       BO      ZAL1C             *                                      00028500
       NI      0(6),X'40'        *                                      00028600
       OI      0(6),X'0A'                                               00028700
       SR      6,0                                                      00028800
       B       ZAL1E             *                                      00028900
       TITLE  'ZERO AND SUBTRACT'                                       00029000
       USING   ZS,13                                                    00029100
ZS     CH      9,=H'7'                                                  00029200
       BE      ZS1                                                      00029300
         CH    9,=H'1'                                                  00029400
         BE    ZSL4                                                     00029500
       CH      9,=H'4'                                                  00029600
       BNE     ILEGLN                                                   00029700
ZS1    LA      6,1(10)                                                  00029800
       BAL     8,CVAD43                                                 00029900
       LR      11,5                                                     00030000
       LR      12,11                                                    00030100
       CH      9,=H'4'                                                  00030200
       BE      ZSL4                                                     00030300
       LA      6,4(10)                                                  00030400
       BAL     8,CVAD43                                                 00030500
       LR      12,5                                                     00030600
ZSL4   LR      5,11                                                     00030700
       LR      6,12                                                     00030800
       LA      0,1               SET ONE IN REG 0 FOR SUBTRACTING       00030900
       IC      3,0(11)           SAVE LOW CHARACTER OF A-FIELD          00031000
       STC     3,TEMP1           *                                      00031100
ZSL4A  MVN     0(1,6),0(5)       MOVE NUMERIC                           00031200
       NI      0(6),X'4F'        ELIMINATE ZONE                         00031300
       SR      5,0               DECREMENT A-ADDRESS                    00031400
       TM      1(5),X'40'                                               00031500
       BO      ZSL4F                                                    00031600
       SR      6,0               DECREMENT B-ADDRESS                    00031700
       TM      1(6),X'40'                                               00031800
       BZ      ZSL4A                                                    00031900
ZSL4C  OI      0(12),X'20'       SET B-FIELD SIGN MINUS                 00032000
       NI      TEMP1,X'30'       Q/ WAS A-FIELD MINUS                   00032100
       CLI     TEMP1,X'20'       *                                      00032200
       BNE     ZSL4D             LEAVE IT MINUS IF IT WAS PLUS          00032300
       OI      0(12),X'30'       MAKE B-FIELD PLUS                      00032400
ZSL4D  LR      11,5                                                     00032500
       LR      12,6                                                     00032600
       B       NXTOP                                                    00032700
ZSL4E  NI      0(6),X'40'                                               00032800
       OI      0(6),X'0A'                                               00032900
ZSL4F  SR      6,0                                                      00033000
       TM      1(6),X'40'                                               00033100
       BO      ZSL4C                                                    00033200
       B       ZSL4E                                                    00033300
       TITLE  'BRANCH, CONDITIONAL BRANCH, AND BRANCH ON CHARACTER'     00033400
       USING   B,13                                                     00033500
B        CH      9,=H'1'               CHAINED BCE?
         BE      BCE1A                 YES, GO CHECK IT
         CH      9,=H'4'               UNCOND 4-POS BRANCH?
         BE      BL5BCH                YES, DO IT
         BL      ILEGLN                ILLEGAL LENGTH OF 2 OR 3
         CLI     4(10),0               IS POS 5 BLANK?
         BE      BL5BCH                YES, UNCOND BRANCH
         CH      9,=H'7'               CHAINED 7-POS BRANCH?
         BE      BCE7                  YES, GO CHECK IT
         CH      9,=H'8'               8-POS BCE?
         BE      BCE8                  YES, DO IT
         BH      ILEGLN                >8, NO GOOD
         CH      9,=H'5'               5-POS COND BRANCH?
         BNE     ILEGLN                NO, ILLEGAL 6-POS INST
         IC    3,4(10)           GET D CHARACTER                        00034500
         N     3,=F'63'          *                                      00034600
         SLL   3,2               MULTIPLY BY 4                          00034700
         L     4,DCHARTBL(3)     GET ADDRESS OF CONDITIONAL BRANCH RTN  00034800
         BR    4                 GO TO ROUTINE OF NXTOP                 00034900
BL5A   TM      SENSEA,1          Q/ IS SENSE SWITCH A ON                00035000
       BZ      NXTOP             NO, CANNOT BRANCH                      00035100
       TM      CRDEOF,1          YES, IS READER EMPTY                   00035200
       BO      BL5BCH            YES, BRANCH                            00035300
       B       NXTOP             NO                                     00035400
BL5B   CLI     SENSEB,1                                                 00035500
       B       BL5CKB                                                   00035600
BL5C   CLI     SENSEC,1                                                 00035700
       B       BL5CKB                                                   00035800
BL5D   CLI     SENSED,1                                                 00035900
       B       BL5CKB                                                   00036000
BL5E   CLI     SENSEE,1                                                 00036100
       B       BL5CKB                                                   00036200
BL5F   CLI     SENSEF,1                                                 00036300
       B       BL5CKB                                                   00036400
BL5G   CLI     SENSEG,1                                                 00036500
       B       BL5CKB                                                   00036600
BL5K   CLI     TPEOF,1                                                  00036700
       MVI     TPEOF,0                                                  00036800
       B       BL5CKB                                                   00036900
BL5L   CLI     TPERR,1                                                  00037000
       B       BL5CKB                                                   00037100
BL5S   CLI     CPR,0                                                    00037200
       B       BL5CKB                                                   00037300
BL5T   CLI     CPR,1                                                    00037400
       B       BL5CKB                                                   00037500
BL5U   CLI     CPR,2                                                    00037600
       B       BL5CKB                                                   00037700
BL51   CLI     CPR,0                                                    00037800
       BE      NXTOP                                                    00037900
       B       BL5BCH                                                   00038000
BL5Z   CLI     OVRFLO,1                                                 00038100
       MVI     OVRFLO,0                                                 00038200
       B       BL5CKB                                                   00038300
BL59     CLI   PRTP9,1                                                  00038400
         B     BL5CKB                                                   00038500
BL52   CLI     PRTP12,1                                                 00038600
       B       BL5CKB                                                   00038700
BL5RER CLI     RDRERR,1                                                 00038800
       MVI     RDRERR,0                                                 00038900
       B       BL5CKB                                                   00039000
BL5PER CLI     PCHERR,1                                                 00039100
       MVI     PCHERR,0                                                 00039200
BL5P     B     NXTOP                                                    00039300
BL53   CLI     PRTERR,1          Q/ PRINT ERROR                         00039400
       MVI     PRTERR,0          CLEAR ERROR INDICATOR                  00039500
       B       BL5CKB            CHECK CONDITION CODE                   00039600
BL5CKB BNE     NXTOP                                                    00039700
BL5BCH LA      6,1(10)                                                  00039800
       B       SETBCH            SET CONDITIONS FOR BRANCH              00039900
BCE8     MVC   DCHAR,7(10)                                              00040800
BCE7     LA    6,4(10)           NO, TREAT AS BCE                       00040200
       BAL     8,CVAD43                                                 00040300
       LR      12,5                                                     00040400
       LA      6,1(10)                                                  00040500
       BAL     8,CVAD43                                                 00040600
       LR      11,5                                                     00040700
BCE1A  MVC     TEMP1(1),0(12)                                           00040900
       NI      TEMP1,X'BF'                                              00041000
         CLC   TEMP1,DCHAR       COMPARE D CHARACTER TO CORE LOCATION   00041100
       BNE     BCE1B                                                    00041200
       LR      12,10                                                    00041300
       AR      12,9                                                     00041400
       ST      10,LSTBCH         STORE LOCATION COUNTER BEFORE BRANCH   00041500
       LR      10,11                                                    00041600
       LA      9,0                                                      00041700
       B       NXTOP                                                    00041800
BCE1B  SH      12,=H'1'                                                 00041900
       B       NXTOP                                                    00042000
DCHARTBL DC    A(BL5BCH),8A(NXTOP),A(BL59),2A(NXTOP),A(BL52)            00042100
         DC    4A(NXTOP),A(BL51,BL5S)                                   00042200
         DC    A(BL5T,BL5U),4A(NXTOP),A(BL5Z,BL53),7A(NXTOP)            00042300
         DC    A(BL5K,BL5L),3A(NXTOP),A(BL5P,NXTOP,BL5P,BL5PER)         00042400
         DC    6A(NXTOP),A(BL5A,BL5B,BL5C,BL5D,BL5E,BL5F,BL5G)          00042500
         DC    2A(NXTOP),A(BL5RER),5A(NXTOP)                            00042600
       TITLE  'BRANCH ON WORD MARK / ZONE'                              00042700
       USING   BWZ,13                                                   00042800
BWZ    CH      9,=H'1'                                                  00042900
       BE      BWZL1                                                    00043000
       CH      9,=H'8'                                                  00043100
       BNE     ILEGLN                                                   00043200
       LA      6,1(10)                                                  00043300
       BAL     8,CVAD43                                                 00043400
       LR      11,5                                                     00043500
       LA      6,4(10)                                                  00043600
       BAL     8,CVAD43                                                 00043700
       LR      12,5                                                     00043800
       MVC     DCHAR(1),7(10)                                           00043900
BWZL1  SH      12,=H'1'                                                 00044000
       CLI     DCHAR,X'01'                                              00044100
       BE      BWZW                                                     00044200
       CLI     DCHAR,X'02'                                              00044300
       BE      BWZ0                                                     00044400
       CLI     DCHAR,X'32'                                              00044500
       BE      BWZBA                                                    00044600
       CLI     DCHAR,X'22'                                              00044700
       BE      BWZB                                                     00044800
       CLI     DCHAR,X'12'                                              00044900
       BE      BWZA                                                     00045000
       CLI     DCHAR,X'03'                                              00045100
       BE      BWZW0                                                    00045200
       CLI     DCHAR,X'33'                                              00045300
       BE      BWZWBA                                                   00045400
       CLI     DCHAR,X'23'                                              00045500
       BE      BWZWB                                                    00045600
       CLI     DCHAR,X'13'                                              00045700
       BE      BWZWA                                                    00045800
       B       ILEGOP                                                   00045900
BWZW   TM      1(12),X'40'                                              00046000
       BO      BWZBCH                                                   00046100
       B       NXTOP                                                    00046200
BWZ0   TM      1(12),X'30'                                              00046300
       BZ      BWZBCH                                                   00046400
       B       NXTOP                                                    00046500
BWZBA  TM      1(12),X'30'                                              00046600
       BO      BWZBCH                                                   00046700
       B       NXTOP                                                    00046800
BWZB   TM      1(12),X'20'                                              00046900
       BZ      NXTOP                                                    00047000
       TM      1(12),X'10'                                              00047100
       BO      NXTOP                                                    00047200
       B       BWZBCH                                                   00047300
BWZA   TM      1(12),X'20'                                              00047400
       BO      NXTOP                                                    00047500
       TM      1(12),X'10'                                              00047600
       BO      BWZBCH                                                   00047700
       B       NXTOP                                                    00047800
BWZW0  TM      1(12),X'40'                                              00047900
       BO      BWZBCH                                                   00048000
       B       BWZ0                                                     00048100
BWZWBA TM      1(12),X'40'                                              00048200
       BO      BWZBCH                                                   00048300
       B       BWZBA                                                    00048400
BWZWB  TM      1(12),X'40'                                              00048500
       BO      BWZBCH                                                   00048600
       B       BWZB                                                     00048700
BWZWA  TM      1(12),X'40'                                              00048800
       BO      BWZBCH                                                   00048900
       B       BWZA                                                     00049000
BWZBCH ST      10,LSTBCH         STORE LOCATION COUNTER BEFORE BRANCH   00049100
       LR      12,10             SET B-REG                              00049200
       AR      12,9              *                                      00049300
       LR      10,11             SET LOCATION COUNTER FOR BRANCH        00049400
       LA      9,0               *                                      00049500
       B       NXTOP                                                    00049600
       TITLE  'COMPARE'                                                 00049700
       USING   C,13                                                     00049800
C        CH    9,=H'1'                                                  00049900
         BE    CL1                                                      00050000
         CH    9,=H'4'                                                  00050100
         BE    CL4                                                      00050200
         CH    9,=H'7'                                                  00050300
       BNE     ILEGLN                                                   00050400
       LA      6,4(10)                                                  00050500
       BAL     8,CVAD43                                                 00050600
       LR      12,5                                                     00050700
CL4      LA    6,1(10)           CONVERT A-ADDR TO 360 FORMAT           00050800
         BAL   8,CVAD43          *                                      00050900
         LR    11,5              *                                      00051000
         CH    9,=H'4'           Q/ IS INSTRUCTION 4 CHARACTERS         00051100
         BNE   CL1               NO                                     00051200
         LR    12,11             YES, FORS                              00051300
         LR    12,11             YES, FORCE B/ADDR = A/ADDR             00051400
CL1      MVI   TCPR,0            INITIALIZE COMPARE RESULT TO EQUAL     00051500
       LA      4,0                                                      00051600
       LA      0,1                                                      00051700
C1     SR      11,0                                                     00051800
       SR      12,0                                                     00051900
       TM      1(12),X'40'                                              00052000
       BO      C2                                                       00052100
       TM      1(11),X'40'                                              00052200
       BO      C5                LONG B-FIELD                           00052300
       LA      4,1(4)                                                   00052400
       B       C1                                                       00052500
C2     LR      5,11                                                     00052600
       LR      6,12                                                     00052700
       LA      4,1(4)                                                   00052800
C3     MVC     TCR(1),1(6)                                              00052900
       MVC     TCR+1(1),1(5)                                            00053000
       TR      TCR(2),CPRTBL     CONVERT DIGITS TO SORT SEQUENCE        00053100
       CLC     TCR(1),TCR+1                                             00053200
       BH      C5                                                       00053300
       BL      C6                                                       00053400
       LA      5,1(5)                                                   00053500
       LA      6,1(6)                                                   00053600
       BCT     4,C3                                                     00053700
C4       CH    9,=H'1'                                                  00053800
         BNE   C4A                                                      00053900
         CLI   TCPR,0                                                   00054000
         BE    NXTOP                                                    00054100
C4A      MVC   CPR,TCPR                                                 00054200
       B       NXTOP                                                    00054300
C5     MVI     TCPR,2            SET HIGH                               00054400
       B       C4                                                       00054500
C6     MVI     TCPR,1            SET LOW                                00054600
       B       C4                                                       00054700
TCPR   DC      X'00'                                                    00054800
TCR    DS      CL2                                                      00054900
CPRTBL DC      HL1'0,55,56,57,58,59,60,61,62,63,54,20,21,22,23,24'      00055000
       DC      HL1'19,13,46,47,48,49,50,51,52,53,45,14,15,16,17,18'     00055100
       DC      HL1'12,36,37,38,39,40,41,42,43,44,35,7,8,9,10,11'        00055200
       DC      HL1'6,26,27,28,29,30,31,32,33,34,25,1,2,3,4,5'           00055300
       DC      HL1'0,55,56,57,58,59,60,61,62,63,54,20,21,22,23,24'      00055400
       DC      HL1'19,13,46,47,48,49,50,51,52,53,45,14,15,16,17,18'     00055500
       DC      HL1'12,36,37,38,39,40,41,42,43,44,35,7,8,9,10,11'        00055600
       DC      HL1'6,26,27,28,29,30,31,32,33,34,25,1,2,3,4,5'           00055700
       TITLE  'HALT'                                                    00055800
       USING   H,13                                                     00055900
H      CH      9,=H'1'                                                  00056000
       BE      H1                                                       00056100
         CH    9,=H'2'                 IS IT A 2-POS HALT?       TAB    00056110
         BE    H1                      YES, GO PROCESS           TAB    00056120
       CH      9,=H'4'                                                  00056200
       BE      H1                                                       00056300
       CH      9,=H'7'                                                  00056400
       BNE     ILEGLN                                                   00056500
H1       LR    5,10              CONVERT I ADDRESS                      00056600
         AR    5,9                     ADD LENGTH TO I - LOC     TAB    00056610
         BAL   8,H5                     *                               00056700
         MVC   HLTIAR,HLTADARA         IAR TO SAVE AREA          TAB    00056710
         MVC   HLTWTO+15(6),HLTADARA    MOVE I ADDRESS TO OUTPUT        00056800
         MVI   HLTWTO+5,17       MOVE LENGTH TO WTO                     00056900
         CH    9,=H'2'                 2-POS HALT?               TAB    00057005
         BL    H2                      NO, 1-POS IAR ONLY        TAB    00057010
         BH    H1A                     > 2 POS - PRINT AAR & BAR TAB    00057015
         MVI   HLTWTO+23,C'D'          2-POS HALT - PRINT D-MOD  TAB    00057020
         MVC   HLTWTO+25(1),1(10)      D-MOD TO WTO              TAB    00057025
         TR    HLTWTO+25(1),TRIE       TRANS D-MOD TO EBCDIC     TAB    00057030
         MVI   HLTWTO+5,22             SET WTO LENGTH            TAB    00057035
         B     H2                      GO PRINT THE MESSAGE      TAB    00057040
H1A      MVI   HLTWTO+23,C'A'          REPLACE THE A FOR 7-POS HLT TAB  00057045
         BL    H2                       NO                              00057100
         LA    6,1(10)           CONVERT 1401 ADDRESS                   00057200
         BAL   8,CVAD43          *                                      00057300
         BAL   8,H5                     *                               00057400
         MVC   HLTWTO+24(6),HLTADARA    MOVE A ADDRESS TO OUTPUT        00057500
         LA    6,4(10)           CONVERT 1401 B ADDRESS                 00057600
         BAL   8,CVAD43          *                                      00057700
         BAL   8,H5                     *                               00057800
         MVC   HLTWTO+33(6),HLTADARA    MOVE B ADDRESS TO OUTPUT        00057900
         MVI   HLTWTO+5,35       MOVE LENGTH TO WTO                     00058000
H2       BAL   8,HALTWTO                PRINT HALT ON TYPEWRITER        00058100
       CH      9,=H'4'                                                  00058200
       BNE     H3                                                       00058300
       LA      6,1(10)                                                  00058400
       BAL     8,CVAD43                                                 00058500
       ST      5,ADR360                                                 00058600
H3       CLC   HLTIAR,EOJIAR           IS THIS EOJ?              TAB    00058700
         BE    TERMINAT                YES, GO END SIM1401       TAB    00058710
         MVC   RETURN,=A(H4)                                            00058720
         B     WTORTN                                                   00058800
H4     CH      9,=H'4'           Q/ BRANCH                              00058900
       BNE     NXTOP                                                    00059000
         LR    12,10                                                    00059100
         AR    12,9                                                     00059200
       L       10,ADR360                                                00059300
       LA      9,0                                                      00059400
       B       NXTOP                                                    00059500
H5       SR    5,7               GET 1401 ADDRESS                       00059600
         CVD   5,PAKT            CONVERT TO DECIMAL                     00059700
         UNPK  HLTADARA(6),PAKT+5(3)    UNPACK 1401 ADDRESS             00059800
         OI    HLTADARA+5,X'F0'         MAKE SIGN NUMERIC               00059900
         LA    1,HLTADARA               BLANK LEADING ZEROS             00060000
H6       CLI   0(1),C'0'         *                                      00060100
         BCR   6,8                      *                               00060200
         MVI   0(1),X'40'               *                               00060300
         LA    1,1(1)                   *                               00060400
         B     H6                *                                      00060500
HLTADARA DC    CL6' '                                                   00060600
EOJIAR   DC    CL6' '                  EOJ IAR FROM PARM FIELD   TAB    00060610
HLTIAR   DC    CL6' '                  HALT IAR                  TAB    00060620
       TITLE  'CLEAR STORAGE'                                           00060700
       USING   CS,13                                                    00060800
CS     CH      9,=H'1'                                                  00060900
       BE      CSL1                                                     00061000
       CH      9,=H'4'                                                  00061100
       BE      CSL4                                                     00061200
       CH      9,=H'7'                                                  00061300
       BL      ILEGLN                                                   00061400
       MVC     HLDBCH(3),1(10)                                          00061500
       LA      6,4(10)                                                  00061600
       B       CSCOM                                                    00061700
CSL4   LA      6,1(10)                                                  00061800
CSCOM  BAL     8,CVAD43                                                 00061900
       LR      12,5                                                     00062000
CSL1   LR      3,12                                                     00062100
       SR      3,7               SUBTRACT SIMULATED CORE BASE LOCATION  00062200
       LA      2,0                                                      00062300
       D       2,=F'100'                                                00062400
       SR      12,2                                                     00062500
       STC     2,CSL1A+1                                                00062600
CSL1A  XC      0(0,12),0(12)     CLEAR CORE BLOCK                       00062700
       CR      12,7              Q/ DID B-REG GO TO 0                   00062800
       BNE     CS2               NO                                     00062900
         L     12,=F'15999'                                             00063000
         AR    12,7                                                     00063100
       B       CS3               *                                      00063200
CS2    SH      12,=H'1'          SUBTRACT 1 FROM B-REG                  00063300
CS3    CH      9,=H'7'           Q/ IS THERE A BRANCH                   00063400
       BL      NXTOP                                                    00063500
       LA      6,HLDBCH                                                 00063600
         B     SETBCH                                                   00063700
HLDBCH DS      CL3                                                      00063800
       TITLE  'SET WORD MARK'                                           00063900
       USING   SW,13                                                    00064000
SW     CH      9,=H'6'                                                  00064100
       BNL     SWL7                                                     00064200
       CH      9,=H'4'                                                  00064300
       BE      SWL4                                                     00064400
       CH      9,=H'1'                                                  00064500
       BE      SWL1                                                     00064600
       B       ILEGLN                                                   00064700
SWL4   LA      6,1(10)                                                  00064800
       BAL     8,CVAD43                                                 00064900
       LR      11,5                                                     00065000
       OI      0(11),X'40'                                              00065100
       SH      11,=H'1'                                                 00065200
       LR      12,11                                                    00065300
       B       NXTOP                                                    00065400
SWL7   LA      6,1(10)                                                  00065500
       BAL     8,CVAD43                                                 00065600
       LR      11,5                                                     00065700
       LA      6,4(10)                                                  00065800
       BAL     8,CVAD43                                                 00065900
       LR      12,5                                                     00066000
SWL1   OI      0(11),X'40'                                              00066100
       OI      0(12),X'40'                                              00066200
       SH      11,=H'1'                                                 00066300
       SH      12,=H'1'                                                 00066400
       CH      9,=H'7'                                                  00066500
       BNH     NXTOP                                                    00066600
       LA      9,7                                                      00066700
       B       NXTOP                                                    00066800
       TITLE  'CLEAR WORD MARK'                                         00066900
       USING   CW,13                                                    00067000
CW     CH      9,=H'6'                                                  00067100
       BNL     CWL7                                                     00067200
       CH      9,=H'4'                                                  00067300
       BE      CWL4                                                     00067400
       CH      9,=H'1'                                                  00067500
       BE      CWL1                                                     00067600
       B       ILEGLN                                                   00067700
CWL4   LA      6,1(10)                                                  00067800
       BAL     8,CVAD43                                                 00067900
       LR      11,5                                                     00068000
       NI      0(11),X'BF'                                              00068100
       SH      11,=H'1'                                                 00068200
       LR      12,11                                                    00068300
       B       NXTOP                                                    00068400
CWL7   LA      6,1(10)                                                  00068500
       BAL     8,CVAD43                                                 00068600
       LR      11,5                                                     00068700
       LA      6,4(10)                                                  00068800
       BAL     8,CVAD43                                                 00068900
       LR      12,5                                                     00069000
CWL1   NI      0(11),X'BF'                                              00069100
       NI      0(12),X'BF'                                              00069200
       SH      11,=H'1'                                                 00069300
       SH      12,=H'1'                                                 00069400
       B       NXTOP                                                    00069500
       TITLE  'MOVE CHARACTERS TO A WORD MARK'                          00069600
       USING   MCW,13                                                   00069700
MCW    CH      9,=H'7'                                                  00069800
       BE      MCWL7                                                    00069900
       CH      9,=H'4'                                                  00070000
       BE      MCWL4                                                    00070100
       CH      9,=H'1'                                                  00070200
       BE      MCWL1                                                    00070300
       CH      9,=H'8'                                                  00070400
       BE      MCW8                                                     00070500
       B       ILEGLN                                                   00070600
MCWL7  LA      6,4(10)                                                  00070700
       BAL     8,CVAD43                                                 00070800
       LR      12,5                                                     00070900
MCWL4  LA      6,1(10)                                                  00071000
       BAL     8,CVAD43                                                 00071100
       LR      11,5                                                     00071200
MCWL1  LA      0,1                                                      00071300
MCWL1B MVC     MCWL1A+1(1),0(11)                                        00071400
       NI      MCWL1A+1,X'3F'                                           00071500
       NI      0(12),X'40'                                              00071600
MCWL1A OI      0(12),0                                                  00071700
       SR      11,0                                                     00071800
       SR      12,0                                                     00071900
       TM      1(11),X'40'                                              00072000
       BO      NXTOP                                                    00072100
       TM      1(12),X'40'                                              00072200
       BZ      MCWL1B                                                   00072300
       B       NXTOP                                                    00072400
MCW8   MVC     DCHAR(1),7(10)                                           00072500
       CLI     DCHAR,X'29'                                              00072600
       BE      RT                                                       00072700
       CLI     DCHAR,X'16'                                              00072800
       BE      WT                                                       00072900
       CLI     DCHAR,X'31'                                              00073000
       BE      MBD                                                      00073100
       CLI     DCHAR,X'32'                                              00073200
       BE      MBD                                                      00073300
       B       ILEGOP                                                   00073400
*                                                                       00073500
*      READ TAPE WITHOUT WORD MARKS                                     00073600
*                                                                       00073700
RT     LA      6,4(10)           CONVERT CORE LOCATION FOR TAPE READ    00073800
       BAL     8,CVAD43          *                                      00073900
       LR      12,5              *                                      00074000
       BAL     8,FNDRIV          GET DEVICE ADDRESS                     00074100
         BAL   8,TSTOPEN                                                00074200
         BAL   8,LOADMD                                                 00074300
       MVI     BCDTAP,1          *                                      00074400
       TM      2(10),X'14'       *                                      00074500
       BO      RT1               *                                      00074600
         BAL   8,BINMOD                                                 00074700
       MVI     BCDTAP,0          SET BINARY                             00074800
RT1      ST    3,TMDCB                                                  00074900
         MVC   TPCCW,=A(RTCCW)                                          00075000
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00075100
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00075200
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00075300
         EXCP  TMIOB                                                    00075400
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00075500
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00075600
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00075700
         BAL   8,TPTEST                                                 00075800
       BAL     8,FNDLNG          FIND LENGTH OF B-FIELD                 00075900
       LR      3,6               *                                      00076000
         L     1,TAPEAREA        SET SENDING ADDRESS                    00076100
       LH      5,SAVCSW+6        FIND NUMBER OF BYTES READ              00076200
         LH    4,=H'18000'       *                                      00076300
       SR      4,5               *                                      00076400
       CR      3,4               USE SMALLER FIELD                      00076500
       BNH     RT3               *                                      00076600
       LR      3,4               *                                      00076700
RT3    CH      3,=H'256'         Q/ MORE THAN 256 BYTES                 00076800
       BNH     RT4               NO                                     00076900
       NC      0(256,12),WM256   YES, MOVE 256 BYTES                    00077000
       CLI     BCDTAP,1          *                                      00077100
       BNE     RT3A              *                                      00077200
       TR      0(256,1),TREI                                            00077300
RT3A   OC      0(256,12),0(1)    *                                      00077400
       LA      1,256(1)          *                                      00077500
       LA      12,256(12)        *                                      00077600
       SH      3,=H'256'         *                                      00077700
       B       RT3               *                                      00077800
RT4    SH      3,=H'1'           MOVE REMAINING BYTES                   00077900
       STC     3,RT5+1           *                                      00078000
       STC     3,RT6+1           *                                      00078100
       STC     3,RT7+1           *                                      00078200
RT5    NC      0(0,12),WM256     *                                      00078300
       CLI     BCDTAP,1          *                                      00078400
       BNE     RT7               *                                      00078500
RT6    TR      0(0,1),TREI                                              00078600
RT7    OC      0(0,12),0(1)      *                                      00078700
       AR      12,3              SET GROUP MARK AFTER DATA              00078800
       NI      1(12),X'40'       *                                      00078900
       OI      1(12),X'3F'       *                                      00079000
       LA      12,2(12)          SET B-ADDRESS                          00079100
       B       NXTOP             END OF TAPE READ INSTRUCTION           00079200
*                                                                       00079300
*      WRITE TAPE WITHOUT WORD MARKS                                    00079400
*                                                                       00079500
WT     LA      6,4(10)                                                  00079600
       BAL     8,CVAD43                                                 00079700
       LR      12,5                                                     00079800
       BAL     8,FNDLNG                                                 00079900
       STH     6,WTCCW2+6        STORE LENGTH IN CCW                    00080000
       LR      4,12                                                     00080100
       AR      12,6              SET B-ADDRESS                          00080200
       LA      12,1(12)          *                                      00080300
         L     3,TAPEAREA                                               00080400
       MVI     BCDTAP,1          *                                      00080500
       CLI     2(10),X'14'       Q/ IS INSTRUCTION BCD                  00080600
       BE      WT1               YES                                    00080700
       MVI     BCDTAP,0          *                                      00080800
WT1    CH      6,=H'256'                                                00080900
       BNH     WT2                                                      00081000
       MVC     0(256,3),0(4)                                            00081100
       CLI     BCDTAP,1          Q/ BCD                                 00081200
       BNE     WT1A              NO                                     00081300
       TR      0(256,3),TRIE     YES, CHANGE X'00' TO X'10' FOR TAPE    00081400
WT1A   LA      3,256(3)          UP REG 3 BY 256                        00081500
       LA      4,256(4)                                                 00081600
       SH      6,=H'256'                                                00081700
       B       WT1                                                      00081800
WT2    STC     6,WT3+1                                                  00081900
       STC     6,WT4+1                                                  00082000
WT3    MVC     0(0,3),0(4)                                              00082100
       CLI     BCDTAP,1          Q/ BCD                                 00082200
       BNE     WT4A              NO                                     00082300
WT4    TR      0(0,3),TRIE       YES, CHANGE X'00' TO X'10' FOR TAPE    00082400
WT4A   BAL     8,FNDRIV          GET DEVICE ADDRESS                     00082500
         BAL   8,TSTOPEN                                                00082600
         BAL   8,LOADMD                                                 00082700
         CLI   2(10),X'14'                                              00082800
         BE    WT1B                                                     00082900
         BAL   8,BINMOD                                                 00083000
WT1B     EQU   *                                                        00083100
         ST    3,TMDCB                                                  00083200
         MVC   TPCCW,=A(WTCCW1)                                         00083300
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00083400
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00083500
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00083600
         EXCP  TMIOB                                                    00083700
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00083800
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00083900
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00084000
         BAL   8,TPTEST                                                 00084100
       B       NXTOP                                                    00084200
MBD    LA      6,1(10)                                                  00084300
       BAL     8,CVAD43                                                 00084400
       LR      11,5                                                     00084500
       LA      6,4(10)                                                  00084600
       BAL     8,CVAD43                                                 00084700
       LR      12,5                                                     00084800
       LA      0,1                                                      00084900
       LR      6,12                                                     00085000
       SH      6,=H'100'                                                00085100
       CLI     DCHAR,X'32'                                              00085200
       BE      MBC                                                      00085300
       LR      6,11                                                     00085400
       SH      6,=H'100'                                                00085500
MBD1   IC      3,0(11)                                                  00085600
       STC     3,MBD2+1                                                 00085700
       NI      MBD2+1,X'BF'                                             00085800
       NI      0(12),X'40'                                              00085900
MBD2   OI      0(12),0                                                  00086000
       SR      12,0                                                     00086100
       IC      3,0(6)                                                   00086200
       STC     3,MBD3+1                                                 00086300
       NI      MBD3+1,X'BF'                                             00086400
       NI      0(12),X'40'                                              00086500
MBD3   OI      0(12),0                                                  00086600
       SR      12,0                                                     00086700
       SR      11,0                                                     00086800
       SR      6,0                                                      00086900
       TM      1(6),X'40'                                               00087000
       BC      8,MBD1                                                   00087100
       B       NXTOP                                                    00087200
MBC    IC      3,0(11)                                                  00087300
       STC     3,MBC1+1                                                 00087400
       NI      MBC1+1,X'BF'                                             00087500
       NI      0(12),X'40'                                              00087600
MBC1   OI      0(12),0                                                  00087700
       SR      11,0                                                     00087800
       IC      3,0(11)                                                  00087900
       STC     3,MBC2+1                                                 00088000
       NI      MBC2+1,X'BF'                                             00088100
       NI      0(6),X'40'                                               00088200
MBC2   OI      0(6),0                                                   00088300
       SR      12,0                                                     00088400
       SR      11,0                                                     00088500
       SR      6,0                                                      00088600
       TM      1(6),X'40'                                               00088700
       BO      NXTOP                                                    00088800
       TM      1(12),X'40'                                              00088900
       BZ      MBC                                                      00089000
       B       NXTOP                                                    00089100
       TITLE  'MOVE CHARACTERS AND SUPPRESS LEADING ZEROS'              00089200
       USING   MCS,13                                                   00089300
MCS    CH      9,=H'1'                                                  00089400
       BE      MCSL1                                                    00089500
       CH      9,=H'7'                                                  00089600
         BE    MCSL7                                                    00089700
         CH    9,=H'4'                                                  00089800
         BNE   ILEGLN                                                   00089900
         LA    6,1(10)                                                  00090000
         BAL   8,CVAD43                                                 00090100
         LR    11,5                                                     00090200
         LR    12,5                                                     00090300
         B     MCSL1                                                    00090400
MCSL7    LA    6,1(10)                                                  00090500
       BAL     8,CVAD43                                                 00090600
       LR      11,5                                                     00090700
       LA      6,4(10)                                                  00090800
       BAL     8,CVAD43                                                 00090900
       LR      12,5                                                     00091000
MCSL1  LA      0,1                                                      00091100
       MVI     SUPRES,1                                                 00091200
       IC      3,0(11)           MOVE ONLY DIGIT OF FIRST CHARACTER     00091300
       STC     3,0(12)           *                                      00091400
       NI      0(12),X'0F'       *                                      00091500
       STC     3,TEMP1           SAVE A-CHARACTER                       00091600
       OI      0(12),X'40'       SET WORD MARK TO STOP REVERSE SCAN     00091700
       B       MCSL1B                                                   00091800
MCSL1A IC      3,0(11)           MOVE CHARACTER                         00091900
       STC     3,0(12)           *                                      00092000
       STC     3,TEMP1           SAVE A-CHARACTER                       00092100
       NI      0(12),X'3F'       *                                      00092200
MCSL1B SR      11,0                                                     00092300
        SR     12,0                                                     00092400
       TM      TEMP1,X'40'       Q/ END OF A-FIELD                      00092500
       BZ      MCSL1A            NO                                     00092600
       LA      12,1(12)          YES                                    00092700
MCSL1C MVC     TEMP1(1),0(12)                                           00092800
       NI      TEMP1,X'3F'                                              00092900
       CLI     SUPRES,1          Q/ IS ZERO SUPPRESSION ON              00093000
       BE      MCSL1G            YES                                    00093100
       CLI     TEMP1,X'0A'       NO, IS IT SIGNIFICANT DIGIT,BLANK 0    00093200
       BNH     MCSL1E            YES                                    00093300
       CLI     TEMP1,X'1B'       Q/ COMMA                               00093400
       BE      MCSL1E            YES                                    00093500
       CLI     TEMP1,X'20'       Q/ HYPHEN                              00093600
       BE      MCSL1E            YES                                    00093700
         MVI   SUPRES,1        TURN ON ZERO SUPRESSION                  00093800
MCSL1E TM      0(12),X'40'       Q/ LAST DIGIT                          00093900
       BO      MCSL1F            YES                                    00094000
       LA      12,1(12)          NO, PROCESS NEXT DIGIT                 00094100
       B       MCSL1C            *                                      00094200
MCSL1F NI      0(12),X'BF'       CLEAR WORD MARK                        00094300
       LA      12,1(12)          SET B-ADDRESS                          00094400
       B       NXTOP             GET NEXT INSTRUCTION                   00094500
MCSL1G CLI     0(12),X'09'       Q/ SIGNIFICANT DIGIT                   00094600
       BH      MCSL1H            *                                      00094700
       CLI     0(12),X'00'       *                                      00094800
       BE      MCSL1H            *                                      00094900
       MVI     SUPRES,0          YES, TURN OFF ZERO SUPPRESSION         00095000
       B       MCSL1E            *                                      00095100
MCSL1H CLI     TEMP1,X'00'       Q/ BLANK                               00095200
       BE      MCSL1I            BLANK                                  00095300
       CLI     TEMP1,X'0A'       Q/ ZERO                                00095400
       BE      MCSL1I            ZERO                                   00095500
       CLI     TEMP1,X'1B'       Q/ COMMA                               00095600
       BNE     MCSL1E            NO                                     00095700
MCSL1I NI      0(12),X'40'                                              00095800
       B       MCSL1E                                                   00095900
       TITLE  'MOVE NUMERIC'                                            00096000
       USING   MN,13                                                    00096100
MN     CH      9,=H'1'                                                  00096200
       BE      MNL1                                                     00096300
         CH    9,=H'4'                                                  00096400
         BE    MNL4                                                     00096500
       CH      9,=H'7'                                                  00096600
       BNE     ILEGLN                                                   00096700
       LA      6,4(10)                                                  00096800
       BAL     8,CVAD43                                                 00096900
       LR      12,5                                                     00097000
MNL4     LA    6,1(10)                                                  00097100
         BAL   8,CVAD43                                                 00097200
         LR    11,5                                                     00097300
         CH    9,=H'4'                                                  00097400
         BNE   MNL1                                                     00097500
         LR    12,11             4 CHARACTERS, SET B ADR = A ADR        00097600
MNL1   MVN     0(1,12),0(11)     MOVE NUMERIC                           00097700
       SH      11,=H'1'                                                 00097800
       SH      12,=H'1'                                                 00097900
       B       NXTOP                                                    00098000
       TITLE  'MOVE ZONE'                                               00098100
       USING   MZ,13                                                    00098200
MZ     CH      9,=H'1'                                                  00098300
       BE      MZL1                                                     00098400
       CH      9,=H'7'                                                  00098500
       BNE     ILEGLN                                                   00098600
       LA      6,1(10)                                                  00098700
       BAL     8,CVAD43                                                 00098800
       LR      11,5                                                     00098900
       LA      6,4(10)                                                  00099000
       BAL     8,CVAD43                                                 00099100
       LR      12,5                                                     00099200
MZL1   IC      3,0(11)                                                  00099300
       STC     3,MZL1A+1                                                00099400
       NI      0(12),X'CF'                                              00099500
       NI      MZL1A+1,X'30'                                            00099600
MZL1A  OI      0(12),0                                                  00099700
       SH      11,=H'1'                                                 00099800
       SH      12,=H'1'                                                 00099900
       B       NXTOP                                                    00100000
       TITLE  'LOAD CHARACTERS TO AN A-FIELD WORD MARK'                 00100100
       USING   LCA,13                                                   00100200
LCA    CH      9,=H'7'                                                  00100300
       BE      LCAL7                                                    00100400
       CH      9,=H'4'                                                  00100500
       BE      LCAL4                                                    00100600
       CH      9,=H'1'                                                  00100700
       BE      LCAL1                                                    00100800
       CH      9,=H'8'                                                  00100900
       BE      LCA8                                                     00101000
       B       ILEGLN                                                   00101100
LCAL7  LA      6,4(10)                                                  00101200
       BAL     8,CVAD43                                                 00101300
       LR      12,5                                                     00101400
LCAL4  LA      6,1(10)                                                  00101500
       BAL     8,CVAD43                                                 00101600
       LR      11,5                                                     00101700
LCAL1  LA      0,1                                                      00101800
LCAL1A IC      3,0(11)                                                  00101900
       STC     3,0(12)                                                  00102000
       SR      11,0                                                     00102100
       SR      12,0                                                     00102200
       TM      1(11),X'40'                                              00102300
       BZ      LCAL1A                                                   00102400
       B       NXTOP                                                    00102500
LCA8   CLI     7(10),X'29'                                              00102600
       BE      RTW                                                      00102700
       CLI     7(10),X'16'                                              00102800
       BE      WTW                                                      00102900
       B       ILEGOP                                                   00103000
*                                                                       00103100
*      READ TAPE WITH WORD MARKS                                        00103200
*                                                                       00103300
RTW    LA      6,4(10)                                                  00103400
       BAL     8,CVAD43                                                 00103500
       LR      12,5                                                     00103600
       BAL     8,FNDRIV                                                 00103700
         BAL   8,TSTOPEN                                                00103800
         LA    8,LOADMD                                                 00103900
         BALR  8,8                                                      00104000
         ST    3,TMDCB                                                  00104100
         MVC   TPCCW,=A(RTCCW)                                          00104200
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00104300
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00104400
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00104500
         EXCP  TMIOB                                                    00104600
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00104700
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00104800
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00104900
         BAL   8,TPTEST                                                 00105000
       LH      3,SAVCSW+6        FIND NUMBER OF BYTES READ              00105100
         LH    4,=H'18000'       *                                      00105200
       SR      4,3                                                      00105300
         L     1,TAPEAREA        SET SENDING ADDRESS                    00105400
RTW1   CLI     0(12),X'7F'       Q/ GP MK - WD MK IN CORE               00105500
       BE      RTW3              YES                                    00105600
       CLI     0(1),X'6D'        Q/ WORD SEPARATOR                      00105700
       BNE     RTW2              NO                                     00105800
       LA      1,1(1)            YES                                    00105900
       IC      3,0(1)                                                   00106000
       STC     3,0(12)                                                  00106100
       TR      0(1,12),TREI                                             00106200
       OI      0(12),X'40'                                              00106300
       SH      4,=H'1'                                                  00106400
       B       RTW2A                                                    00106500
RTW2   IC      3,0(1)                                                   00106600
       STC     3,0(12)                                                  00106700
       TR      0(1,12),TREI                                             00106800
RTW2A  LA      1,1(1)                                                   00106900
       LA      12,1(12)                                                 00107000
       BCT     4,RTW1                                                   00107100
       CLI     0(12),X'7F'       RECORD MOVED, IS GROUP MARK NEXT CHAR  00107200
       BE      RTW3              YES, LEAVE IT ALONE                    00107300
       MVI     0(12),X'3F'       NO, MOVE IN A GROUP MARK               00107400
RTW3   LA      12,1(12)          SET B-ADDRESS                          00107500
       B       NXTOP                                                    00107600
*                                                                       00107700
*      WRITE TAPE WITH WORD MARKS                                       00107800
*                                                                       00107900
WTW    LA      6,4(10)                                                  00108000
       BAL     8,CVAD43                                                 00108100
       LR      12,5                                                     00108200
         L     1,TAPEAREA                                               00108300
       LR      2,12                                                     00108400
WTW1   TM      0(2),X'7F'        Q/ GROUP MARK WORD MARK                00108500
       BO      WTW3              YES, FIELD DONE                        00108600
       TM      0(2),X'40'        Q/ WORD MARK                           00108700
       BZ      WTW2              NO                                     00108800
       MVI     0(1),X'6D'        YES, INSERT WORD SEPARATOR             00108900
       LA      1,1(1)            *                                      00109000
WTW2   MVC     0(1,1),0(2)                                              00109100
       TR      0(1,1),TRIE                                              00109200
       LA      1,1(1)                                                   00109300
       LA      2,1(2)                                                   00109400
       B       WTW1                                                     00109500
WTW3     S     1,TAPEAREA                                               00109600
       STH     1,WTCCW2+6                                               00109700
       BAL     8,FNDRIV                                                 00109800
         BAL   8,TSTOPEN                                                00109900
         BAL   8,LOADMD                                                 00110000
         ST    3,TMDCB                                                  00110100
         MVC   TPCCW,=A(WTCCW1)                                         00110200
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00110300
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00110400
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00110500
         EXCP  TMIOB                                                    00110600
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00110700
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00110800
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00110900
         BAL   8,TPTEST                                                 00111000
       LA      12,1(2)                                                  00111100
       B       NXTOP                                                    00111200
       TITLE   'MOVE CHARACTERS AND EDIT'                               00111300
       USING   MCE,13                                                   00111400
MCE    CH      9,=H'7'           Q/ IS LENGTH CORRECT                   00111500
       BNE     ILEGLN            NO                                     00111600
       LA      6,1(10)           YES, CONVERT ADDRESSES                 00111700
       BAL     8,CVAD43          *                                      00111800
       LR      11,5              *                                      00111900
       LA      6,4(10)           *                                      00112000
       BAL     8,CVAD43          *                                      00112100
       LR      12,5              *                                      00112200
       LA      0,1                                                      00112300
       MVI     AEND,0            CLEAR A-FIELD END INDICATOR            00112400
       MVI     BODY,0            CLEAR BODY TRIGGER                     00112500
       MVI     SUPRES,0          CLEAR ZERO SUPPRESSION INDICATOR       00112600
         MVI   FLOAT,0        CLEAR FLOATING DOLLAR SIGN INDICATOR      00112700
         MVI   SIGDIG,0       CLEAR SIGNIFICANT DIGIT IND               00112800
         MVI   ASTER,0        CLEAR ASTERISK PROTECTION IND             00112900
         MVI   AMINUS,0       CLEAR A-FIELD MINUS INDICATOR             00113000
         MVI   DECIMAL,0       DECIMAL POINT INDICATOR                  00113100
         MVI   FIRSTDOL,0          CLEAR $  INFIRST CHAR INDICATOR      00113200
         MVI   SIGNDOL,0           CLEAR DOLLAR SIGN INDICATOR          00113300
         IC    2,0(11)        Q/ A-FIELD MINUS                          00113400
         N     2,=F'48'                                                 00113500
         CH    2,=H'32'                                                 00113600
         BNE   MCE1           NO                                        00113700
         MVI   AMINUS,1       YES,SET A-FIELD MINUS INDICATOR           00113800
MCE1   IC      1,0(12)           SAVE B-FIELD CHARACTER                 00113900
       STC     1,TEMP1           *                                      00114000
       NI      0(12),X'3F'       CLEAR WORD MARK                        00114100
         CLI   0(12),X'3B'     Q/ DECIMAL POINT                         00114200
         BNE   MCE1A           NO                                       00114300
         MVI   DECIMAL,1       YES,SET DECIMAL INDICATOR                00114400
           ST  12,DECADD       STORE ADDRESS OF DECIMAL POINT           00114500
MCE1A    CLI   0(12),X'00'     Q/ BLANK                                 00114600
       BE      MCE6              YES                                    00114700
       CLI     0(12),X'0A'       Q/ ZERO                                00114800
       BE      MCE6              YES                                    00114900
       CLI     0(12),X'30'       Q/ AMPERSAND                           00115000
       BE      MCE3              YES                                    00115100
       CLI     BODY,1            Q/ BODY TRIGGER ON                     00115200
       BE      MCE3A             YES                                    00115300
       CLI     0(12),X'1B'       Q/ COMMA                               00115400
       BE      MCE3              YES                                    00115500
       CLI     0(12),X'33'       Q/ C                                   00115600
       BE      MCE2              YES                                    00115700
       CLI     0(12),X'29'       Q/ R                                   00115800
       BE      MCE2              YES                                    00115900
       CLI     0(12),X'20'       Q/ -                                   00116000
       BNE     MCE3A             NO                                     00116100
MCE2     CLI   AMINUS,1       Q/ A-FIELD MINUS                          00116200
       BE      MCE3A             YES                                    00116300
MCE3   MVI     0(12),X'00'       MOVE BLANK TO B-FIELD                  00116400
         SR    12,0            DECREMENT B-FIELD                        00116500
         B     MCE5                                                     00116600
MCE3A    CLI   0(12),X'2C'     Q/ *                                     00116700
         BNE   MCE3B           NO                                       00116800
         CLI   BODY,1          Q/ BODY TRIGGER ON                       00116900
         BNE   MCE3B           NO                                       00117000
         MVI   ASTER,1         SET ASTERISK PRORECTION INDICATOR        00117100
         B     MCE6                                                     00117200
MCE3B    CLI   0(12),X'2B'     Q/ DOLLAR SIGN                           00117300
         BE    MCE5C           YES                                      00117400
         SR    12,0            DECREMENT B-FIELD                        00117500
         B     MCE5                                                     00117600
MCE4A  SR      11,0                                                     00117700
MCE5A    SR    12,0                                                     00117800
       TM      1(11),X'40'       Q/ END OF A-FIELD                      00117900
       BZ      MCE5              NO                                     00118000
       MVI     AEND,1            YES, SET A-FIELD ENDED INDICATOR       00118100
MCE5   TM      TEMP1,X'40'       Q/ END OF B-FIELD                      00118200
       BZ      MCE1              NO                                     00118300
       B       MCE8              YES                                    00118400
MCE5C    MVI   SIGNDOL,1           SET DOLLAR SIGN INDICATOR            00118500
         ST    12,DOLSIGN          STORE ADDRESS OF DOLLAR SIGN         00118600
         TM    1(12),X'40'         Q/ FLOATING DOLLAR SIGN              00118700
         BZ    MCE5A                                                    00118800
         MVI   FLOAT,1                                                  00118900
         MVC   0(1,12),0(11)                                            00119000
         B     MCE4A                                                    00119100
MCE6   CLI     AEND,1            Q/ HAS A-FIELD ALREADY ENDED           00119200
       BE      MCE3              YES                                    00119300
       MVC     0(1,12),0(11)     MOVE CHARACTER                         00119400
       NI      0(12),X'3F'       *                                      00119500
         CLI   0(12),X'00'         BLANK                                00119600
         BE    MCE6A               YES                                  00119700
         CLI   0(12),X'09'         DIGIT                                00119800
         BH    MCE6A               NO                                   00119900
         MVI   SIGDIG,1            YES SET SIG DIGIT INDICATOR          00120000
MCE6A    CLI   BODY,1              Q/ BODY TRIGGER ON                   00120100
       CLI     BODY,1            Q/ IS BODY TRIGGER ON                  00120200
       BE      MCE7              YES                                    00120300
       MVI     BODY,1            NO                                     00120400
         ST    12,LASTDIG     STORE ADDRESS OF LOW ORDER DIGIT          00120500
       NI      0(12),X'0F'       REMOVE ZONE                            00120600
MCE7   TM      TEMP1,X'0A'       Q/ IS DIGIT ZERO                       00120700
         BC    12,MCE4A        NO                                       00120800
       TM      TEMP1,X'35'                                              00120900
         BC    5,MCE4A         NO                                       00121000
       OI      0(12),X'40'       YES, SET ZERO SUPPRESSION WORD MARK    00121100
         ST    12,ZEROSUP      STORE ZERO SUPPRESSION ADDRESS           00121200
       MVI     SUPRES,1          SET ZERO SUPPRESSION INDICATOR         00121300
         B     MCE4A              INDICATOR                             00121400
MCE8   CLI     SUPRES,1       Q/ WAS THERE ZERO SUPPRESSION             00121500
       BNE     NXTOP             NO, GET NEXT INSTRUCTION               00121600
         MVI   FIRST,1        SET FIRST CHARACTER OF SCAN INDICATOR     00121700
       LA      12,1(12)                                                 00121800
         CLI   0(12),X'2B'         DOLLAR SIGN                          00121900
         BNE   MCE8A                                                    00122000
         MVI   FIRSTDOL,1          YES                                  00122100
MCE8A    MVC   TEMP1(1),0(12)      SAVE CHARACTER                       00122200
       NI      0(12),X'3F'       CLEAR WORD MARK                        00122300
       CLI     0(12),X'00'       Q/ BLANK                               00122400
       BE      MCE9              YES                                    00122500
       CLI     0(12),X'0A'       Q/ ZERO                                00122600
       BE      MCE11             YES                                    00122700
       CLI     0(12),X'09'       Q/ SIGNIFICANT DIGIT                   00122800
       BH      MCE9              NO                                     00122900
       MVI     SUPRES,0          TURN OFF ZERO SUPPRESSION              00123000
         MVI   SIGDIG,1        SET SIGNIFICANT DIGIT INDICATOR          00123100
       B       MCE10                                                    00123200
MCE9   CLI     0(12),X'1B'       Q/ COMMA                               00123300
       BE      MCE11             YES                                    00123400
         CLI   0(12),X'20'     Q/ -                                     00123500
         BE    MCE10C          YES                                      00123600
         CLC   0(12),=X'3329' Q/ CR SYMBOL                              00123700
         BE    MCE13          YES                                       00123800
         CLI   0(12),X'3B'                                              00123900
         BE    MCE11                                                    00124000
       MVI     SUPRES,1          TURN ON ZERO SUPPRESSION               00124100
MCE10  LA      12,1(12)                                                 00124200
         MVI   FIRST,0        TURN OFF FIRST TIME INDICATOR             00124300
MCE10B   TM    TEMP1,X'40'     Q/ W/RD MARK                             00124400
         BO    FLDOL           YES,TEST FOR FLOATING DOLLAR SIGN        00124500
       B       MCE8A             NO, PROCESS NEXT DIGIT                 00124600
MCE10C   CLI   FIRST,1         Q/ FIRST CHARACTER IN STRING             00124700
         BE    MCE10D          YES                                      00124800
         B     MCE10                                                    00124900
MCE10D   CLI   AMINUS,1        Q/ A-FIELD MINUS                         00125000
         BE    MCE10                                                    00125100
         MVI   0(12),X'00'     NO,BLANK MINUS SIGN                      00125200
         B     MCE10                                                    00125300
MCE11  CLI     SUPRES,1          Q/ ZERO SUPPRESSION ON                 00125400
       BNE     MCE10             NO                                     00125500
       MVI     0(12),X'00'       YES, BLANK CHARACTER                   00125600
         CLI   FIRST,1        Q/ FIRST CHARACTER IN STRING              00125700
         BE    MCE12          YES                                       00125800
         CLI   ASTER,1        Q/ ASTERISK PROTECTION ON                 00125900
         BNE   MCE10          NO                                        00126000
         MVI   0(12),X'2C'    YES, INSERT ASTERISK                      00126100
       B       MCE10                                                    00126200
MCE12    CLI   AMINUS,1       Q/ A-FIELD MINUS                          00126300
         BE    MCE10          YES                                       00126400
         MVI   0(12),X'00'     NO,BLANK CHARACTER                       00126500
         B     MCE10                                                    00126600
MCE13    CLI   SUPRES,1       Q/ ZERO SUPPRESSION ON                    00126700
         BNE   MCE10          NO                                        00126800
         CLI   FIRST,1        Q/ 1ST CHARACTER IN STRING                00126900
         BE    MCE14          YES                                       00127000
         MVC   0(2,12),=C'  ' NO,BLANK CR                               00127100
         B     MCE10                                                    00127200
MCE14    CLI   AMINUS,1       Q/ A-FIELD MINUS                          00127300
         BE    MCE10A         YES                                       00127400
         MVC   0(2,12),=C'  ' NO,BLANK CR                               00127500
         B     MCE10                                                    00127600
MCE10A   LA    12,1(12)                                                 00127700
         B     MCE10                                                    00127800
FLDOL    CLI   FLOAT,1        Q/ FLOATING DOLLAR SIGN                   00127900
         BNE   DECON          NO, GO TO DECIMAL CONTR                   00128000
DOLLAR   CLI   0(12),X'00'    Q/ BLANK                                  00128100
         BNE   MOVDOL          NO,GO TO NEXT POSITION IN B-FIELD        00128200
         MVI   0(12),X'2B'    MOVE DOLLAR SIGN INTO B-FIELD             00128300
         B     DECON                                                    00128400
MOVDOL   SR    12,0           DECREMENT B-FIELD                         00128500
         B     DOLLAR                                                   00128600
DECON    CLI   DECIMAL,1       IS DECIMAL CONTROL NEEDED                00128700
         BNE   NXTOP           NO                                       00128800
         CLI   SIGDIG,1        Q/ SIGNIFICANT DIGIT                     00128900
         BNE   MCE16           NO                                       00128900
         L     3,DECADD        TEST TENS POSITION                       00129100
MCE16C   CLI   1(3),X'00'          FOR BLANK                            00129200
         BNE   NXTOP          NOT BLANK                                 00129300
         MVI   1(3),X'0A'     YES INSERT ZERO                           00129400
         LA    3,1(3)         LOOP                                      00129500
         B     MCE16C                                                   00129600
MCE16    L     5,LASTDIG                                                00129700
         CLC   DECADD,ZEROSUP                                           00129800
         BH    MCE16A                                                   00129900
         L     4,DECADD                                                 00130000
         B     MCE16B                                                   00130100
MCE16A   L     4,ZEROSUP                                                00130200
MCE16B   SR    5,4                                                      00130300
         AH    5,=H'1'                                                  00130400
MCE16D   MVC   0(1,4),=X'00'                                            00130500
         AR    4,0                                                      00130600
         BCT   5,MCE16D                                                 00130700
         TM    1(12),X'40'         Q/ FLOATING DOLLAR SIGN              00130800
         CLI   SIGNDOL,1           Q/  DOLLAR SIGN                      00130900
         BNE   NXTOP               NO                                   00131000
         CLI   FIRSTDOL,1          Q/ DOLLAR SIGN OK                    00131100
         BE    NXTOP                                                    00131200
         L     3,DOLSIGN           NO                                   00131300
         MVI   0(3),X'00'          BLANK DOLLAR SIGN                    00131400
         B     NXTOP                                                    00131500
ZEROSUP  DS    F               ZERO SUPPRESSION ADDRESS                 00131600
DECADD   DS    F               DECIMAL POINT ADDRESS                    00131700
DECIMAL  DC    X'00'           DECIMAL INDICATOR                        00131800
FLOAT    DC    X'00'          FLOATING DOLLAR SIGN INDICATOR            00131900
FIRST    DC    X'00'          FIRST CHARACTER OF SCAN INDICATOR         00132000
AMINUS   DC    X'00'          A-FIELD MINUS INDICATOR                   00132100
BODY     DC    X'00'           BODY TRIGGER                             00132200
ASTER    DC    X'00'          ASTERISK PROTECTION INDICATOR             00132300
SIGDIG   DC    X'00'           SIGNIFICANT DIGIT INDICATOR              00132400
FIRSTDOL DC    X'00'                                                    00132500
DOLSIGN  DS    F                                                        00132600
LASTDIG  DS    F              ADDRESS OF LOW ORDER DIGIT                00132700
SIGNDOL  DC    X'00'                                                    00132800
       TITLE  'READ A CARD'                                             00132900
       USING   R,13                                                     00133000
R      CH      9,=H'1'                                                  00133100
       BE      RL1                                                      00133200
       CH      9,=H'4'                                                  00133300
       BE      RL4                                                      00133400
       B       ILEGLN                                                   00133500
RL1    BAL     8,READ                                                   00133600
       B       NXTOP                                                    00133700
RL4    MVC     ADR140(3),1(10)                                          00133800
       BAL     8,READ                                                   00133900
       LA      6,ADR140          GET BRANCH ADDRESS                     00134000
       B       SETBCH            SET CONDITIONS FOR BRANCH              00134100
       TITLE  'PUNCH A CARD'                                            00134200
       USING   P,13                                                     00134300
P      CH      9,=H'1'                                                  00134400
       BE      PL1                                                      00134500
       CH      9,=H'4'                                                  00134600
       BNE     ILEGLN                                                   00134700
       BAL     8,PUNCH                                                  00134800
       LA      6,1(10)           REFERENCE BRANCH ADDRESS               00134900
       B       SETBCH            SET CONDITIONS FOR BRANCH              00135000
PL1    BAL     8,PUNCH                                                  00135100
       B       NXTOP                                                    00135200
       TITLE  'READ AND PUNCH'                                          00135300
       USING   RP,13                                                    00135400
RP     CH      9,=H'1'                                                  00135500
       BE      RPL1                                                     00135600
       CH      9,=H'4'                                                  00135700
       BNE     ILEGLN                                                   00135800
       MVC     ADR140(3),1(10)   SAVE BRANCH ADDRESS                    00135900
       BAL     8,READ                                                   00136000
       BAL     8,PUNCH                                                  00136100
       LA      6,ADR140          REFERENCE BRANCH ADDRESS               00136200
       B       SETBCH            SET CONDITIONS FOR BRANCH              00136300
RPL1   BAL     8,READ                                                   00136400
       BAL     8,PUNCH                                                  00136500
       B       NXTOP                                                    00136600
       TITLE  'PRINT A LINE'                                            00136700
       USING   W,13                                                     00136800
W      CH      9,=H'1'                                                  00136900
       BE      WL1                                                      00137000
       CH      9,=H'2'                                                  00137100
       BE      WM                                                       00137200
       CH      9,=H'5'                                                  00137300
       BE      WM                                                       00137400
       CH      9,=H'4'                                                  00137500
       BNE     ILEGLN                                                   00137600
WL4    BAL     8,WRITE                                                  00137700
       LA      6,1(10)           REFERENCE BRANCH ADDRESS               00137800
       B       SETBCH            SET CONDITIONS FOR BRANCH              00137900
WL1    BAL     8,WRITE                                                  00138000
       B       NXTOP                                                    00138100
WM     MVC     DCHAR(1),1(10)                                           00138200
       CH      9,=H'2'                                                  00138300
       BE      WML2                                                     00138400
       MVC     DCHAR(1),4(10)                                           00138500
WML2     STM   13,15,MACREGSV  SAVE MACRO REGS                          00138600
         MVI   PRTP12,0            CLEAR CHANNEL 12 INDICATOR           00138700
         CLI   DCHAR,X'3C'                                              00138800
       BE      WML20A                                                   00138900
       CLI     DCHAR,X'12'                                              00139000
       BNE     ILEGOP                                                   00139100
       CH      9,=H'5'                                                  00139200
       BE      WL4                                                      00139300
       B       WL1                                                      00139400
WML20A   MVC   PRNTBUFF+1(132),SIMCOR+201    MOVE WORD MARKS TO PRINT   00139500
         TR    PRNTBUFF+1(132),TRWDMK        *                          00139600
         LA    3,PRNTDCB               GET ADDR OF PRINT DCB     TAB    00139610
         BAL   5,UROPEN                GO OPEN PRINTER           TAB    00139620
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00139700
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00139800
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00139900
         MVI   PRNTBUFF,X'09'    SET CARRIAGE CONTROL                   00140000
         PUT   PRNTDCB,PRNTBUFF  WRITE WORD MARKS                       00140100
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00140200
DUMPSW   NOPR  4                       RETURN TO DUMP ROUTINE           00140210
         BAL   8,COUNTER                                                00140300
       LA      12,SIMCOR+333                                            00140400
       CH      9,=H'2'                                                  00140500
       BE      NXTOP                                                    00140600
       LA      6,1(10)                                                  00140700
       B       SETBCH            SET CONDITIONS FOR BRANCH              00140800
       TITLE  'READ AND PRINT'                                          00140900
       USING   WR,13                                                    00141000
WR     CH      9,=H'1'                                                  00141100
       BE      WRL1                                                     00141200
       CH      9,=H'4'                                                  00141300
       BNE     ILEGLN                                                   00141400
       MVC     ADR140(3),1(10)   SAVE BRANCH ADDRESS                    00141500
       BAL     8,WRITE                                                  00141600
       BAL     8,READ                                                   00141700
       LA      6,ADR140          REFERENCE BRANCH ADDRESS               00141800
       B       SETBCH            SET CONDITIONS FOR BRANCH              00141900
WRL1   BAL     8,WRITE                                                  00142000
       BAL     8,READ                                                   00142100
       B       NXTOP                                                    00142200
       TITLE  'PRINT AND PUNCH'                                         00142300
       USING   WP,13                                                    00142400
WP     CH      9,=H'1'                                                  00142500
       BE      WPL1                                                     00142600
       CH      9,=H'4'                                                  00142700
       BNE     ILEGLN                                                   00142800
       BAL     8,WRITE                                                  00142900
       BAL     8,PUNCH                                                  00143000
       LA      6,1(10)           REFERENCE BRANCH ADDRESS               00143100
       B       SETBCH            SET CONDITIONS FOR BRANCH              00143200
WPL1   BAL     8,WRITE                                                  00143300
       BAL     8,PUNCH                                                  00143400
       B       NXTOP                                                    00143500
       TITLE  'WRITE,READ, AND PUNCH'                                   00143600
       USING   WRP,13                                                   00143700
WRP    CH      9,=H'1'                                                  00143800
       BE      WRPL1                                                    00143900
       CH      9,=H'4'                                                  00144000
       BNE     ILEGLN                                                   00144100
       MVC     ADR140(3),1(10)   SAVE BRANCH ADDRESS                    00144200
       BAL     8,WRITE                                                  00144300
       BAL     8,READ                                                   00144400
       BAL     8,PUNCH                                                  00144500
       LA      6,ADR140          REFERENCE BRANCH ADDRESS               00144600
       B       SETBCH            SET CONDITIONS FOR BRANCH              00144700
WRPL1  BAL     8,WRITE                                                  00144800
       BAL     8,READ                                                   00144900
       BAL     8,PUNCH                                                  00145000
       B       NXTOP                                                    00145100
       TITLE  'SELECT STACKER'                                          00145200
       USING   SS,13                                                    00145300
SS     CH      9,=H'2'                                                  00145400
         BE    SSDG2                                                    00145500
       CH      9,=H'5'                                                  00145600
       BNE     ILEGLN                                                   00145700
SSDG4    CLI   4(10),X'04'                                              00145800
         BE    SSDG5                                                    00145900
         CLI   4(10),X'08'                                              00146000
         BE    SSDG6                                                    00146100
SSDG7    EQU   *                                                        00146200
       LA      6,1(10)                                                  00146300
         B     SETBCH                                                   00146400
SSDG2    CLI   1(10),X'04'        4 POCKET                              00146500
         BE    SSDG1              YES                                   00146600
         CLI   1(10),X'08'        8 POCKET                              00146700
         BE    SSDG3              YES                                   00146800
         B     NXTOP                                                    00146900
SSDG1    MVI   PCHARAA,X'41'      SELECT POCKET 4                       00147000
         B     NXTOP                                                    00147100
SSDG3    MVI   PCHARAA,X'81'      SELECT POCKET 8                       00147200
         B     NXTOP                                                    00147300
SSDG5    MVI   PCHARAA,X'41'      SELECT POCKET 4                       00147400
         B     SSDG7                                                    00147500
SSDG6    MVI   PCHARAA,X'81'      SELECT POCKET 8                       00147600
         B     SSDG7                                                    00147700
       TITLE  'CONTROL CARRIAGE'                                        00147800
       USING   CC,13                                                    00147900
CC     MVC     DCHAR(1),1(10)                                           00148000
       CH      9,=H'2'                                                  00148100
       BE      CCL2                                                     00148200
       CH      9,=H'5'                                                  00148300
       BNE     ILEGLN                                                   00148400
       MVC     DCHAR(1),4(10)                                           00148500
CCL2   TM      DCHAR,X'30'                                              00148600
       BZ      CCIMSK                                                   00148700
       BO      CCAFSK                                                   00148800
       TM      DCHAR,X'20'                                              00148900
       BO      CCIMSP                                                   00149000
       IC      3,DCHAR                                                  00149100
       N       3,=F'3'                                                  00149200
       SLL     3,3                                                      00149300
       O       3,=F'1'                                                  00149400
         STC   3,PRNTBUFF                                               00149500
       B       CCDONE                                                   00149600
CCIMSP IC      3,DCHAR                                                  00149700
       N       3,=F'3'                                                  00149800
       SLL     3,3                                                      00149900
         STC   3,PRNTBUFF                                               00150000
         OI    PRNTBUFF,X'03'                                           00150100
       B       CCNOW                                                    00150200
CCAFSK IC      3,DCHAR                                                  00150300
       N       3,=F'15'                                                 00150400
       TM      DCHAR,X'0F'                                              00150500
       BM      CC1                                                      00150600
       LA      3,10                                                     00150700
CC1    SLL     3,3                                                      00150800
         STC   3,PRNTBUFF                                               00150900
         OI    PRNTBUFF,X'81'                                           00151000
       B       CCDONE                                                   00151100
CCIMSK IC      3,DCHAR                                                  00151200
       CLI     DCHAR,X'00'                                              00151300
       BE      NXTOP                                                    00151400
       TM      DCHAR,X'0F'                                              00151500
       BM      CC2                                                      00151600
       LA      3,10                                                     00151700
CC2    N       3,=F'15'                                                 00151800
       SLL     3,3                                                      00151900
       O       3,=F'131'                                                00152000
         STC   3,PRNTBUFF                                               00152100
CCNOW    EQU   *                       NEXT 2 LINES ADDED        TAB    00152200
         LA    3,PRNTDCB               GET ADDR OF PRINT DCB     TAB    00152210
         BAL   5,UROPEN                GO OPEN THE PRINTER       TAB    00152220
         STM   13,15,MACREGSV          SAVE MACRO REGS           TAB    00152230
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00152300
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00152400
         PUT   PRNTDCB,PRNTBUFF                                         00152500
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00152600
         BAL   8,COUNTER                                                00152700
         MVI   PRNTBUFF,X'09'                                           00152800
*      MVI     PRTP12,0          CLEAR CHANNEL 12 INDICATOR             00152900
CCDONE CH      9,=H'2'                                                  00153000
       BE      NXTOP                                                    00153100
       LA      6,1(10)                                                  00153200
         B     SETBCH                                                   00153300
       TITLE   'TAPE CONTROL OPERATIONS'                                00153400
       USING   CU,13                                                    00153500
CU     CH      9,=H'5'                                                  00153600
       BNE     ILEGLN                                                   00153700
       CLI     4(10),X'29'                                              00153800
       BE      RWD                                                      00153900
       CLI     4(10),X'24'                                              00154000
       BE      WTM                                                      00154100
       CLI     4(10),X'14'                                              00154200
       BE      RWU                                                      00154300
       CLI     4(10),X'32'                                              00154400
       BE      BSP                                                      00154500
       CLI     4(10),X'35'                                              00154600
       BE      SKP                                                      00154700
       B       ILEGOP                                                   00154800
RWD    MVI     CUCCW,X'07'                                              00154900
       B       CU1                                                      00155000
WTM    MVI     CUCCW,X'1F'                                              00155100
       B       CU1                                                      00155200
BSP    MVI     CUCCW,X'27'                                              00155300
       B       CU1                                                      00155400
SKP    MVI     CUCCW,X'17'                                              00155500
CU1    BAL     8,FNDRIV                                                 00155600
         ST    3,CUDCB                                                  00155700
         MVI   CUECB,0                                                  00155800
         MVI   CUIOB,X'42'       SET COMMAND CHAIN + UNRELATED BITS     00155900
         BAL   8,TSTOPEN                                                00156000
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00156100
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00156200
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00156300
         EXCP  CUIOB                                                    00156400
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00156500
         WAIT  1,ECB=CUECB                                              00156600
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00156700
       B       NXTOP                                                    00156800
RWU      IC    2,3(10)           GET 1401 DRIVE NUMBER                  00156900
         N     2,=F'7'           *                                      00157000
         CLI   3(10),X'0A'                                              00157100
         BNE   RWUA                                                     00157200
         SR    2,2                                                      00157300
RWUA     EQU   *                                                        00157400
         SLL   2,3                                                      00157500
         L     4,TAPADR+4(2)                                            00157600
         B     RWUCLOSE          GO CLOSE                               00157700
CUCCW    CCW   0,0,X'60',1             CONTROL UNIT OP           TAB    00157800
         CCW   X'03',0,X'20',1         CHAINED TO A NOP          TAB    00157900
       TITLE   'MULTIPLY'                                               00158000
       USING   M,13                                                     00158100
M      CH      9,=H'7'                                                  00158200
       BNE     ILEGLN                                                   00158300
       LA      6,1(10)                                                  00158400
       BAL     8,CVAD43                                                 00158500
       LR      11,5                                                     00158600
       LA      6,4(10)                                                  00158700
       BAL     8,CVAD43                                                 00158800
       LR      12,5                                                     00158900
       ST      12,MPYSAV         SAVE UNITS ADDRESS OF PRODUCT          00159000
       LR      5,11              INTIALIZE PRODUCT AREA                 00159100
       LR      6,12              *                                      00159200
M1     MVI     0(6),X'0A'        *                                      00159300
       TM      0(5),X'40'        *                                      00159400
       BO      M2                *                                      00159500
       SH      5,=H'1'           *                                      00159600
       SH      6,=H'1'           *                                      00159700
       B       M1                *                                      00159800
M2     SH      6,=H'2'           *                                      00159900
       MVI     1(6),X'0A'        *                                      00160000
       LA      1,0               COMPARE SIGNS                          00160100
       LA      2,0               *                                      00160200
       TM      0(6),X'20'        *                                      00160300
       BZ      M3                *                                      00160400
       TM      0(6),X'10'        *                                      00160500
       BO      M3                *                                      00160600
       LA      1,1               *                                      00160700
M3     TM      0(11),X'20'       *                                      00160800
       BZ      M4                *                                      00160900
       TM      0(11),X'10'       *                                      00161000
       BO      M4                *                                      00161100
       LA      2,1               *                                      00161200
M4     MVI     MINPRD,0                                                 00161300
       CR      1,2                                                      00161400
       BE      M5                SIGNS EQUAL                            00161500
       MVI     MINPRD,1          SIGNS UNEQUAL                          00161600
M5     IC      1,0(6)                                                   00161700
       N       1,=F'15'                                                 00161800
       CH      1,=H'10'          Q/ ZERO                                00161900
       BNE     *+6               NO                                     00162000
       SR      1,1               YES, CLEAR                             00162100
M6     LA      0,0                                                      00162200
       LTR     1,1               Q/ IS MULTIPLICAND DIGIT ZERO          00162300
       BZ      M9                                                       00162400
       LR      5,12              SET REGISTERS FOR ADD                  00162500
       LR      4,11                                                     00162600
       LR      8,12              LOAD PRODUCT POINTER                   00162700
M7     IC      2,0(4)                                                   00162800
       N       2,=F'15'                                                 00162900
       CH      2,=H'10'          Q/ ZERO                                00163000
       BNE     *+6               NO                                     00163100
       SR      2,2               YES, CLEAR                             00163200
       IC      3,0(5)                                                   00163300
       N       3,=F'15'                                                 00163400
       CH      3,=H'10'          Q/ ZERO                                00163500
       BNE     *+6               NO                                     00163600
       SR      3,3               YES, CLEAR IT                          00163700
       AR      3,2                                                      00163800
       AR      3,0                                                      00163900
       LA      0,0                                                      00164000
       CH      3,=H'9'                                                  00164100
       BNH     M8                                                       00164200
       SH      3,=H'10'                                                 00164300
       LA      0,1                                                      00164400
M8     STC     3,0(8)            STORE RESULT                           00164500
       CLI     0(8),X'00'        Q/ RESULT ZERO                         00164600
       BNE     *+8               NO                                     00164700
       MVI     0(8),X'0A'        YES, SET 8-2 BITS                      00164800
       SH      4,=H'1'                                                  00164900
       SH      5,=H'1'                                                  00165000
       SH      8,=H'1'                                                  00165100
       TM      1(4),X'40'                                               00165200
       BZ      M7                                                       00165300
       IC      3,0(5)            ADD CARRY TO NEXT PRODUCT DIGIT        00165400
       CH      3,=H'10'          Q/ ZERO                                00165500
       BNE     *+6               NO                                     00165600
       SR      3,3               YES, CLEAR                             00165700
       AR      3,0                                                      00165800
       STC     3,0(8)            *                                      00165900
       CLI     0(8),X'00'        Q/ RESULT ZERO                         00166000
       BNE     *+8               NO                                     00166100
       MVI     0(8),X'0A'        YES, SET 8-2 BITS                      00166200
       SH      1,=H'1'                                                  00166300
       BC      6,M6              COUNT NOT ZERO, ADD NEXT DIGIT         00166400
M9     SH      6,=H'1'                                                  00166500
       NI      1(6),X'40'        CLEAR LAST USED MULTIPLICAND DIGIT     00166600
       OI      1(6),X'0A'        *                                      00166700
       TM      1(6),X'40'                                               00166800
       BO      M10                                                      00166900
       SH      12,=H'1'                                                 00167000
       B       M5                                                       00167100
M10    LR      11,4                                                     00167200
       L       12,MPYSAV         RELOAD UNITS ADDRESS OF PRODUCT        00167300
       OI      0(12),X'20'                                              00167400
       CLI     MINPRD,1                                                 00167500
       BE      M11                                                      00167600
       OI      0(12),X'30'                                              00167700
M11    LR      12,6                                                     00167800
       B       NXTOP                                                    00167900
MINPRD DS      C                                                        00168000
MPYSAV DS      F                                                        00168100
       TITLE   'DIVIDE'                                                 00168200
       USING   D,13                                                     00168300
D      CH      9,=H'7'           Q/ IS LENGTH ( BYTES                   00168400
       BNE     ILEGLN            NO                                     00168500
       LA      6,1(10)           YES, CONVERT ADDRESSES                 00168600
       BAL     8,CVAD43          *                                      00168700
       LR      11,5              *                                      00168800
       LA      6,4(10)           *                                      00168900
       BAL     8,CVAD43          *                                      00169000
       LR      12,5              *                                      00169100
       LA      0,1               SET REG TO 1 FOR + OR - 1              00169200
       LR      1,11              SCAN DIVISOR FOR LENGTH AND IS IT ZERO 00169300
       MVI     TEMP1,0           *                                      00169400
         MVI   TEMP2,0           *                                      00169500
D1       MVN   TEMP2,0(1)        *                                      00169600
         CLI   TEMP2,X'0A'       *                                      00169700
         BE    D1A               *                                      00169800
         CLI   TEMP2,X'00'       *                                      00169900
       BE      D1A               *                                      00170000
       MVI     TEMP1,1           *                                      00170100
D1A    SR      1,0               *                                      00170200
       TM      1(1),X'40'        *                                      00170300
       BZ      D1                *                                      00170400
       CLI     TEMP1,0           Q/ IS DIVISOR ZERO                     00170500
       BNE     D2                NO, OK                                 00170600
       MVI     OVRFLO,1          YES, SET OVERFLOW INDICATOR            00170700
       B       NXTOP                                                    00170800
D2     LR      6,12              FIND HIGH ORDER QUOTIENT LOCATION      00170900
       AR      6,1               *                                      00171000
       SR      6,11              *                                      00171100
       SR      6,0               *                                      00171200
D3     MVI     TEMP1,0           PREPARE TO COMPARE DIVISOR + DVDND     00171300
       LR      1,11                                                     00171400
       LR      2,12                                                     00171500
D4     IC      3,0(1)            GET DIGITS                             00171600
       IC      4,0(2)            *                                      00171700
       N       3,=F'15'          *                                      00171800
       N       4,=F'15'          *                                      00171900
       CH      3,=H'10'          Q/ ZERO                                00172000
       BNE     *+6               NO                                     00172100
       SR      3,3               YES, CLEAR IT                          00172200
       CH      4,=H'10'          Q/ ZERO                                00172300
       BNE     *+6               NO                                     00172400
       SR      4,4               YES, CLEAR                             00172500
       CR      3,4               COMPARE                                00172600
       BE      D5                EQUAL, DO NOT CHANGE INDICATOR         00172700
       BH      D4A               A-DIGIT GREATER                        00172800
       MVI     TEMP1,0           A-DIGIT LESS                           00172900
       B       D5                *                                      00173000
D4A    MVI     TEMP1,1           SET A GREATER THAN B                   00173100
D5     SR      1,0               DECREMENT FIELD POINTERS               00173200
       SR      2,0               *                                      00173300
       TM      1(1),X'40'        Q/ END OF A-FIELD                      00173400
       BZ      D4                NO                                     00173500
       TM      0(2),X'0A'        TEST 1 MORE DIVIDEND DIGIT             00173600
       BO      D6                ZERO                                   00173700
       TM      0(2),X'0F'        Q/ BLANK                               00173800
       BZ      D6                YES, TREAT SAME AS ZERO                00173900
       MVI     TEMP1,0           1, DIVIDEND GREATER THAN DIVISOR       00174000
D6     CLI     TEMP1,1           Q/ IS DIVISOR TOO LARGE                00174100
       BE      D10               YES                                    00174200
       LR      1,11              SET REGISTERS FOR COMPLEMENT ADD       00174300
       LR      2,12              *                                      00174400
       LA      8,1               SET CARRY                              00174500
D7     IC      5,0(2)            GET B-FIELD DIGIT                      00174600
       N       5,=F'15'          *                                      00174700
       CH      5,=H'10'          Q/ ZERO                                00174800
       BNE     *+6               NO                                     00174900
       SR      5,5               YES, CLEAR                             00175000
       LA      4,9               GET COMPLEMENT OF A-FIELD DIGIT        00175100
       IC      3,0(1)            *                                      00175200
       N       3,=F'15'          *                                      00175300
       CH      3,=H'10'          Q/ ZERO                                00175400
       BNE     *+6               NO                                     00175500
       SR      3,3               YES, CLEAR IT                          00175600
       SR      4,3               *                                      00175700
       AR      5,4               ADD TO B-FIELD DIGIT                   00175800
       AR      5,8               ADD CARRY                              00175900
       LA      8,0               CLEAR CARRY                            00176000
       CH      5,=H'9'           Q/ RESULT GREATER THAN 9               00176100
       BNH     D8                NO                                     00176200
       SH      5,=H'10'          YES, SUBTRACT 10 FROM RESULT           00176300
       LA      8,1               SET CARRY                              00176400
D8     STC     5,D9+1            STORE RESULT                           00176500
       NI      0(2),X'F0'        *                                      00176600
       CLI     D9+1,X'00'        Q/ RESULT ZERO                         00176700
       BNE     D9                NO                                     00176800
       OI      D9+1,X'0A'        YES, SET 8-2 BITS                      00176900
D9     OI      0(2),0            *                                      00177000
       SR      2,0               DECREMENT A- AND B-ADDRESSES           00177100
       SR      1,0               *                                      00177200
       TM      1(1),X'40'        Q/ END OF A-FIELD                      00177300
       BZ      D7                NO, PROCESS NEXT DIGIT                 00177400
       IC      3,0(2)            YES, ADD 1 MORE DIVIDEND DIGIT         00177500
       N       3,=F'15'          *                                      00177600
       CH      3,=H'10'          Q/ ZERO                                00177700
       BNE     *+6               NO                                     00177800
       SR      3,3               YES, CLEAR IT                          00177900
       LA      3,9(3)            *                                      00178000
       AR      3,8               *                                      00178100
       CH      3,=H'9'           Q/ RESULT GREATER THAN 9               00178200
       BNH     D9A               NO                                     00178300
       SH      3,=H'10'          YES, SUBTRACT 10                       00178400
D9A    STC     3,0(2)            STORE RESULT                           00178500
       CLI     0(2),X'00'        Q/ RESULT ZERO                         00178600
       BNE     *+8               NO                                     00178700
       MVI     0(2),X'0A'        YES, SET 8-2 BITS                      00178800
       IC      3,0(6)            ADD 1 TO QUOTIENT DIGIT                00178900
       N       3,=F'15'          *                                      00179000
       CH      3,=H'10'          Q/ ZERO                                00179100
       BNE     *+6               NO                                     00179200
       SR      3,3               YES, CLEAR IT                          00179300
       AR      3,0               *                                      00179400
       STC     3,TEMP1           STORE RESULT                           00179500
       MVN     0(1,6),TEMP1      *                                      00179600
       B       D3                                                       00179700
D10    TM      0(12),X'30'       Q/ ZONE BITS                           00179800
       BC      5,D11             YES, DIVIDE DONE                       00179900
       AR      6,0               NO, UP REFERENCE TO NEXT DIGIT         00180000
       AR      12,0              *                                      00180100
       B       D3                                                       00180200
D11    IC      2,0(11)           COMPARE DIVISOR AND DIVIDEND SIGNS     00180300
       IC      3,0(12)           *                                      00180400
       N       2,=F'48'          *                                      00180500
       N       3,=F'48'          *                                      00180600
       SRDL    2,4               *                                      00180700
       LA      4,SINTBL          *                                      00180800
       IC      2,0(4,2)          *                                      00180900
       IC        3,0(4,3)          *                                    00181000
       OI      0(6),X'30'        SET QUOTIENT PLUS                      00181100
       CR      2,3               Q/ ARE SIGNS EQUAL                     00181200
       BE      D12               YES, LEAVE QUOTIENT PLUS               00181300
       NI      0(6),X'EF'        UNEQUAL, SET QUOTIENT MINUS            00181400
D12    LR      11,1              SET A- AND B-ADDRESSES                 00181500
       SR      11,0              *                                      00181600
       LR      12,6              *                                      00181700
       B       NXTOP                                                    00181800
SINTBL DC      X'00000100'                                              00181900
       TITLE  'MODIFY ADDRESS'                                          00182000
       USING   MA,13                                                    00182100
MA     CH      9,=H'7'                                                  00182200
       BE      MA1                                                      00182300
       CH      9,=H'1'                                                  00182400
       BE      MAL4                                                     00182500
       CH      9,=H'4'                                                  00182600
       BNE     ILEGLN                                                   00182700
MA1    LA      6,1(10)                                                  00182800
       BAL     8,CVAD43                                                 00182900
       LR      11,5                                                     00183000
       LR      12,11                                                    00183100
       CH      9,=H'4'                                                  00183200
       BE      MAL4                                                     00183300
       LA      6,4(10)                                                  00183400
       BAL     8,CVAD43                                                 00183500
       LR      12,5                                                     00183600
MAL4   SH      11,=H'3'                                                 00183700
       SH      12,=H'3'                                                 00183800
       LA      0,15              UNITS                                  00183900
       LA      1,0               *                                      00184000
       IC      2,3(11)           *                                      00184100
       IC      3,3(12)           *                                      00184200
       NR      2,0               *                                      00184300
       NR      3,0               *                                      00184400
       CH      2,=H'10'                                                 00184500
       BNE     *+6                                                      00184600
       SR      2,2                                                      00184700
       CH      3,=H'10'                                                 00184800
       BNE     *+6                                                      00184900
       SR      3,3                                                      00185000
       AR      3,2               *                                      00185100
       CH      3,=H'9'           *                                      00185200
       BNH     MAL4A             *                                      00185300
       SH      3,=H'10'          *                                      00185400
       LA      1,1               *                                      00185500
MAL4A  STC     3,MAL4B+1         *                                      00185600
       NI      3(12),X'70'       *                                      00185700
       TM      MAL4B+1,X'0F'                                            00185800
       BC      5,MAL4B                                                  00185900
       OI      MAL4B+1,X'0A'                                            00186000
MAL4B  OI      3(12),0           *                                      00186100
       IC      2,2(11)           TENS                                   00186200
       IC      3,2(12)           *                                      00186300
       NR      2,0               *                                      00186400
       NR      3,0               $                                      00186500
       CH      2,=H'10'                                                 00186600
       BNE     *+6                                                      00186700
       SR      2,2                                                      00186800
       CH      3,=H'10'                                                 00186900
       BNE     *+6                                                      00187000
       SR      3,3                                                      00187100
       AR      3,2               *                                      00187200
       AR      3,1               *                                      00187300
       LA      1,0               *                                      00187400
       CH      3,=H'9'           *                                      00187500
       BNH     MAL4C             *                                      00187600
       SH      3,=H'10'          *                                      00187700
       LA      1,1               *                                      00187800
MAL4C  STC     3,MAL4D+1         *                                      00187900
         NI    2(12),X'70'       SAVE B FLD INDEX AND WORD MARK BITS    00188000
       TM      MAL4D+1,X'0F'                                            00188100
       BC      5,MAL4D                                                  00188200
       OI      MAL4D+1,X'0A'                                            00188300
MAL4D  OI      2(12),0           *                                      00188400
       IC      2,1(11)           HUNDREDS                               00188500
       IC      3,1(12)           *                                      00188600
       NR      2,0               *                                      00188700
       NR      3,0               *                                      00188800
       CH      2,=H'10'                                                 00188900
       BNE     *+6                                                      00189000
       SR      2,2                                                      00189100
       CH      3,=H'10'                                                 00189200
       BNE     *+6                                                      00189300
       SR      3,3                                                      00189400
       AR      3,2               *                                      00189500
       AR      3,1               *                                      00189600
       LA      1,0               *                                      00189700
       CH      3,=H'9'           *                                      00189800
       BNH     MAL4E             *                                      00189900
       SH      3,=H'10'          *                                      00190000
       LA      1,16              *                                      00190100
MAL4E  STC     3,MAL4F+1         *                                      00190200
       NI      1(12),X'70'       *                                      00190300
       TM      MAL4F+1,X'0F'                                            00190400
       BC      5,MAL4F                                                  00190500
       OI      MAL4F+1,X'0A'                                            00190600
MAL4F  OI      1(12),0           *                                      00190700
       LA      0,48              THOUSANDS                              00190800
       IC      2,1(11)           *                                      00190900
       IC      3,1(12)           *                                      00191000
       NR      2,0               *                                      00191100
       NR      3,0               *                                      00191200
       AR      3,2               *                                      00191300
       AR      3,1               *                                      00191400
       LA      1,0               *                                      00191500
       CH      3,=H'48'          *                                      00191600
       BNH     MAL4G             *                                      00191700
       SH      3,=H'64'          *                                      00191800
       LA      1,16              *                                      00191900
MAL4G  STC     3,MAL4H+1         *                                      00192000
       NI      1(12),X'4F'       *                                      00192100
MAL4H  OI      1(12),0           *                                      00192200
       IC      2,3(11)           FOUR THOUSANDS                         00192300
       IC      3,3(12)           *                                      00192400
       NR      2,0               *                                      00192500
       NR      3,0               *                                      00192600
       AR      3,2               *                                      00192700
       AR      3,1               *                                      00192800
       CH      3,=H'48'          *                                      00192900
       BNH     MAL4I             *                                      00193000
       SH      3,=H'64'          *                                      00193100
MAL4I  STC     3,MAL4J+1         *                                      00193200
       NI      3(12),X'4F'       *                                      00193300
MAL4J  OI      3(12),0           *                                      00193400
       B       NXTOP                                                    00193500
       TITLE  'STORE A-ADDRESS REGISTER'                                00193600
       USING   SAR,13                                                   00193700
SAR    CH      9,=H'4'                                                  00193800
       BNE     ILEGLN                                                   00193900
       LR      12,11                                                    00194000
       LA      6,1(10)                                                  00194100
       BAL     8,CVAD43                                                 00194200
       LR      11,5                                                     00194300
       ST      12,ADR360                                                00194400
       BAL     8,CVAD34                                                 00194500
       SH      11,=H'3'                                                 00194600
       NC      1(3,11),=X'404040'                                       00194700
       OC      1(3,11),ADR140                                           00194800
       B       NXTOP                                                    00194900
       TITLE  'STORE B-ADDRESS REGISTER'                                00195000
       USING   SBR,13                                                   00195100
SBR    CH      9,=H'4'                                                  00195200
       BE      SBRL4                                                    00195300
         CH    9,=H'1'                                                  00195400
         BE    SBRL1                                                    00195500
       CH      9,=H'7'                                                  00195600
       BNE     ILEGLN                                                   00195700
       LA      6,4(10)                                                  00195800
       BAL     8,CVAD43                                                 00195900
       LR      12,5                                                     00196000
SBRL4  LA      6,1(10)                                                  00196100
       BAL     8,CVAD43                                                 00196200
       LR      11,5                                                     00196300
       ST      12,ADR360                                                00196400
       BAL     8,CVAD34                                                 00196500
SBRL1    SH    11,=H'3'                                                 00196600
       NC      1(3,11),=X'404040'                                       00196700
       OC      1(3,11),ADR140                                           00196800
       B       NXTOP                                                    00196900
       TITLE  'MOVE CHARACTERS TO RCD MARK OR GROUP MARK - WORD MARK'   00197000
       USING   MCM,13                                                   00197100
MCM    CH      9,=H'1'                                                  00197200
       BE      MCML1                                                    00197300
       CH      9,=H'7'                                                  00197400
       BNE     ILEGLN                                                   00197500
       LA      6,1(10)                                                  00197600
       BAL     8,CVAD43                                                 00197700
       LR      11,5                                                     00197800
       LA      6,4(10)                                                  00197900
       BAL     8,CVAD43                                                 00198000
       LR      12,5                                                     00198100
MCML1    NI    MCMSW+1,X'0F'                                            00198200
         LR    6,11                A-FIELD PTR                          00198300
MCMSCAN  TRT   0(256,6),TRTGMWRM   SCAN FOR GMWM - RM - RMWM            00198400
         BNZ   MCMHIT                                                   00198500
         LA    6,256(6)                                                 00198600
         B     MCMSCAN                                                  00198700
MCMHIT   SR    1,11                COMPUTE RECORD LENGTH                00198800
         LA    1,1(1)              BUMP FOR TERM CHAR                   00198900
         CH    1,=H'256'           TOTAL LENGTH GT 256                  00199000
         BNH   MCMDECR             NO                                   00199100
         OI    MCMSW+1,X'F0'       YES - SET SW FOR MULTIPLE MOVES      00199200
         LR    3,1                                                      00199300
MCM256   LA    1,256                                                    00199400
MCMDECR  BCTR  1,0                 DECREMENT FOR EX INSTRUCTIONS        00199500
         EX    1,MCMCHMOV          MOVE RECORD TO WORK AREA             00199600
         EX    1,MCMCHCLR          CLEAR RECEIVING AREA EXCEPT WM       00199700
         EX    1,MCMWMCLR          ELIMINATE WORD MARKS IN WORK AREA    00199800
         EX    1,MCMCHORC          OR DATA BITS (BA8421) INTO REC AREA  00199900
         LA    1,1(1)                                                   00200000
         AR    11,1                                                     00200100
         AR    12,1                                                     00200200
MCMSW    NOP   MCMBUMP             SW SET IF RECORD GT 256 BYTES        00200300
         B     NXTOP               TO NEXT 1401 INSTRUCTION             00200400
MCMBUMP  SR    3,1                 COMPUTE BYTES REMAINING              00200500
         CH    3,=H'256'           Q / BYTES REMAINING GT 256           00200600
         BH    MCM256              YES                                  00200700
         LR    1,3                                                      00200800
         NI    MCMSW+1,X'0F'       TURN OFF SWITCH                      00200900
         B     MCMDECR             MOVE REMAINING BYTES                 00201000
*                                                                       00201100
MCMCHCLR NC    0(0,12),WM256                                            00201200
MCMCHMOV MVC   WORK256(0),0(11)                                         00201300
MCMWMCLR NC    WORK256(0),STRIPWM                                       00201400
MCMCHORC OC    0(0,12),WORK256                                          00201500
*                                                                       00201600
WORK256  DC    CL256' '                                                 00201700
TRTGMWRM DC    26X'00'             MCM SCAN TABLE                       00201800
         DC    X'1A'               RECORD MARK - A8 2                   00201900
         DC    63X'00'                                                  00202000
         DC    X'5A'               RECORD MARK WORD MARK - M A8 2       00202100
         DC    36X'00'                                    W             00202200
         DC    X'7F'               GROUP MARK WORD MARK - MBA8421       00202300
         DC    128X'00'                                                 00202400
STRIPWM  DC    256X'3F'                                                 00202500
       TITLE  'BRANCH IF BIT EQUAL'                                     00202600
       USING   BBE,13                                                   00202700
BBE    CH      9,=H'1'                                                  00202800
       BE      BBEL1                                                    00202900
       CH      9,=H'8'                                                  00203000
       BNE     ILEGLN                                                   00203100
         LA    6,1(10)                                                  00203200
         BAL   8,CVAD43                                                 00203300
         LR    11,5                                                     00203400
       LA      6,4(10)                                                  00203500
       BAL     8,CVAD43                                                 00203600
       LR      12,5                                                     00203700
       MVC     DCHAR(1),7(10)                                           00203800
       NI      DCHAR,X'BF'                                              00203900
BBEL1  SH      12,=H'1'                                                 00204000
       MVC     TEMP1,DCHAR                                              00204100
       NC      TEMP1(1),1(12)                                           00204200
       BZ      NXTOP                                                    00204300
         LR    10,11                                                    00204400
       LA      9,0                                                      00204500
       B       NXTOP                                                    00204600
         TITLE ' '                                                      00204700
BEGIN    SAVE  (14,12)          SAVE CONTROL PROGRAMS REGISTERS         00204800
         ENTRY BEGIN                   ENTRY POINT               TAB    00204810
         BALR  15,0              LOAD BASE REGISTERS                    00204900
SETBS1   L     14,BASE2          *                                      00205000
         ST    13,SAVEAREA+4     SAVE CONTROL PROGRAMS REGISTER 13      00205100
         STM   14,15,MACREGSV                                           00205200
         LA    13,SAVEAREA                                              00205300
         LA    6,MACREGSV                                               00205400
         L     1,0(1)                  GET PARM FIELD POINTER    TAB    00205410
         LH    2,0(1)                  GET PARM FIELD LENGTH     TAB    00205420
         LTR   2,2                     IS PARM FIELD LENGTH ZERO?  TAB  00205430
         BZ    GETCORE                 YES, NO PARM FIELD PRESENT  TAB  00205440
         BCTR  2,0                     PARM LENGTH LESS ONE      TAB    00205450
         L     3,=A(EOJIAR+5)          LOW ORDER OF EOJIAR       TAB    00205455
         SR    3,2                     NO, EOJIAR - PARM LENGTH - 1 TAB 00205460
         EX    2,MOVEPARM              MOVE PARM TO EOJIAR       TAB    00205470
         B     GETCORE                 TO CONTINUE INIT          TAB    00205475
MOVEPARM MVC   0(0,3),2(1)             MOVE PARM TO EOJIAR       TAB    00205480
GETCORE  EQU   *                       CONTINUE WITH INIT        TAB    00205485
         GETMAIN  R,LV=16020     GET CORE FOR 1401 SIMULATED CORE       00205500
         LM    14,15,0(6)                                               00205600
         LR    7,1               *                                      00205700
         A     1,=F'15999'       STORE UPPER LIMIT OF 1401 CORE         00205800
         ST    1,SIMLIMIT        *                                      00205900
         GETMAIN  R,LV=18000    GET CORE FOR TAPE I/O BUFFER            00206000
         LM    14,15,0(6)                                               00206100
         ST    1,TAPEAREA       *                                       00206200
         MVC   WTCCW2+1(3),TAPEAREA+1                                   00206300
         MVC   RTCCW1+1(3),TAPEAREA+1                                   00206400
         MVC   LDTCCW1+1(3),TAPEAREA+1                                  00206500
         XC    SIMCOR+201,SIMCOR                                        00206600
       B       BGN1                                                     00206700
BASE2  DC      A(SETBS1+4096)                                           00206800
*                                                                       00206900
WTORTN   XC    RQSTIN,RQSTIN                                            00207000
         MVC   OKWTOR+27(8),JOBNAME    MOVE JOBNAME TO WTOR             00207100
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00207200
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00207300
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00207400
OKWTOR   TM    MOD,X'FF'                                                00207500
         BO    CHGRDCD                                                  00207600
         WTOR  'OK         ',RQSTIN,50,WTECB                            00207700
CHGA     LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00207800
TESTA    STM   13,15,MACREGSV  SAVE MACRO REG                           00207900
         TM    MOD,X'FF'                                                00208000
         BO    TRANS                                                    00208100
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00208200
         LA    13,SAVEAREA                                              00208300
TESTAA   WAIT  1,ECB=WTECB     WAIT FOR RESPONSE                        00208400
         LM    13,15,0(6)      RESTORE MACRO REG                        00208500
         XC    WTECB,WTECB     CLEAR ECB                                00208600
         B     TRANS                                                    00208700
CHGRDCD  CLC   TMPARA(4),=C'CCTL'                                       00208800
         BNE   DGCC7                                                    00208900
         CLI   TMPARA+9,X'7D'                                           00209000
         BNE   DGCC2                                                    00209100
         XC    CARTAB(66),CARTAB                                        00209200
         L     3,=A(CARTAB)                                             00209300
DGCC10   L     1,=A(TMPARA+10)                                          00209400
       LA      2,70                                                     00209500
DGCC5    CLI   0(1),X'7D'                                               00209600
         BE    DGCC3                                                    00209700
         CLI   0(1),C' '                                                00209800
         BE    DGCC4                                                    00209900
         MVC   0(1,3),0(1)                                              00210000
DGCC4    LA    1,1(1)                                                   00210100
         LA    3,1(3)                                                   00210200
         BCT   2,DGCC5                                                  00210300
         B     DGCC2                                                    00210400
DGCC3    STM 13,15,MACREGSV                                             00210500
         ST    3,DGEND                                                  00210600
         MVC   DGCUR,=A(CARTAB)                                         00210700
         LA    6,MACREGSV                                               00210800
         LA    13,SAVEAREA                                              00210900
         GET   CARD,TMPARA                                              00211000
         LM    13,15,0(6)                                               00211100
         CLC   TMPARA(5),=C'CCTL2'                                      00211200
         BNE   DGCC7                                                    00211300
       L       3,DGEND                                                  00211400
         B     DGCC10                                                   00211500
DGCC2    STM   13,15,MACREGSV                                           00211600
         LA    6,MACREGSV                                               00211700
         LA    13,SAVEAREA                                              00211800
         WTO   'INVALID CCTL CARD,RESUBMIT'                             00211900
         LM    13,15,0(6)                                               00212000
DGCC7    EQU   *                                                        00212100
         MVC   RQSTIN,TMPARA                                            00212200
         GET   CARD,TMPARA                                              00212300
         LM    13,15,0(6)                                               00212400
         B     CHGA                                                     00212500
MOD      DC    X'FF'                                                    00212600
WTECB    DC    F'0'                                                     00212700
BGN1     STM   13,15,MACREGSV  SAVE MACRO REGS                          00212800
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00212900
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00213000
         OPEN  (CARD)                  OPEN CARD INPUT DCB ONLY  TAB    00213100
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00213200
         MVI   PRNTBUFF,X'8B'      RESTORE PRINT FORM IMMEDIATELY       00213300
         BAL   8,WRITE              *                                   00213400
         LA    8,BGN2                                                   00213500
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00213600
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00213700
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00213800
         GET   CARD,TMPARA     READ CARD                                00213900
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00214000
         LA    1,76            CLEAR                                    00214100
         L     2,TAPEAREA        TAPE                                   00214200
CLEAR    XC    0(100,2),0(2)   AREA                                     00214300
         LA    2,100(2)                                                 00214400
         BCT   1,CLEAR                                                  00214500
BGN2   L       1,=F'16010'                                              00214600
         AR    1,7                                                      00214700
       MVI     0(1),X'7F'                                               00214800
         LA    8,16                    ADDR OF CVT PTR IN 8      TAB    00214900
         L     8,0(8)                  ADDR OF CVT IN 8          TAB    00214910
         L     8,0(8)                  ADDR OF TCB WORDS IN 8    TAB    00215000
         L     8,4(8)                  ADDR OF TCB IN 8          TAB    00215100
       L       8,12(8)        ADDR OF TIOT IN 8                         00215200
       MVC     JOBNAME,0(8)                                             00215300
   B WTORTN                                                             00215400
TRANS    TR    RQSTIN,TYPTBL                                            00215500
       CLC     RQSTIN(3),=C'SSS'                                        00215600
       BE      SSIN                                                     00215700
       CLC     RQSTIN(3),=C'LDC'                                        00215800
       BE      CDLOAD                                                   00215900
       CLC     RQSTIN(3),=C'LDT'                                        00216000
       BE      TPLOAD                                                   00216100
       CLC     RQSTIN(3),=C'SRS'                                        00216200
       BE      STRST                                                    00216300
       CLC     RQSTIN(3),=C'STT'                                        00216400
       BE      START                                                    00216500
       CLC     RQSTIN(3),=C'CLR'                                        00216600
       BE      CLR                                                      00216700
       CLC     RQSTIN(3),=C'DIS'                                        00216800
       BE      DIS                                                      00216900
       CLC     RQSTIN(3),=C'ALT'                                        00217000
       BE      ALT                                                      00217100
       CLC     RQSTIN(3),=C'WTM'                                        00217200
       BE      WTMCMD                                                   00217300
       CLC     RQSTIN(3),=C'RWD'                                        00217400
       BE      RWDCMD                                                   00217500
         CLC   RQSTIN(3),=C'TRM'                                        00217600
         BE    TERMINAT                                                 00217700
         CLC   RQSTIN(3),=C'DMP'                                        00217710
         BE    DUMPCORE                                                 00217720
SNDILG   XC    RQSTIN,RQSTIN   ZERO OUT REPLY AREA                      00217800
         STM   13,15,MACREGSV  SAVE MACRO REG                           00217900
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00218000
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00218100
         WTOR  'ILLEGAL ENTRY',RQSTIN,50,WTECB                          00218200
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00218300
         B     TESTAA                                                   00218400
RQSTIN DS      CL50                                                     00218500
*                                                                       00218600
*      THIS SECTION WILL SIMULATE THE 1402 CARD LOAD PUSHBUTTON.        00218700
*                                                                       00218800
CDLOAD XC      SIMCOR+1(80),SIMCOR+1                                    00218900
         MVI   MOD,X'00'                                                00219000
       BAL     8,READ                                                   00219100
       OI      SIMCOR+1,X'40'                                           00219200
       LA      10,SIMCOR+1                                              00219300
       LA      9,0                                                      00219400
       MVI     OKSTT,1                                                  00219500
       B       NXTOP                                                    00219600
*                                                                       00219700
*      THIS SECTION WILL SIMULATE THE START PUSHBUTTON.  IF THE         00219800
*      OPERATOR COMMAND STT IS FOLLOWED BY AN ADDRESS, THE 1401 PROGRAM 00219900
*      WILL RESUME FROM THAT ADDRESS.  HOWEVER, IF STT IS NOT FOLLOWED  00220000
*      BY ANYTHING, THE 1401 PROGRAM WILL RESUME FROM WHERE IT STOPPED. 00220100
*                                                                       00220200
START  CLI     OKSTT,1                                                  00220300
       BNE     START4                                                   00220400
       LA      5,RQSTIN+3        Q/ IS THERE A START ADDRESS            00220500
       CLI     0(5),0            *                                      00220600
       BNE     START1            YES, START FROM THERE                  00220700
         L     8,RETURN                                                 00220800
         BR    8                                                        00220900
START1 CLI     0(5),0            Q/ END OF MESSAGE                      00221000
       BE      START2            YES                                    00221100
       CLI     0(5),C'0'         NO, IS IT NUMERIC                      00221200
       BL      SNDILG            NO, ERROR                              00221300
       LA      5,1(5)            YES, TRY NEXT BYTE                     00221400
       B       START1            *                                      00221500
START2 S       5,=A(RQSTIN+4)    GET LENGTH - 1                         00221600
       CH      5,=H'4'           Q/ LENGTH GT 5 DIGITS                  00221700
       BH      SNDILG            YES, ERROR                             00221800
       STC     5,TEMP1           CONVERT TO BINARY                      00221900
       MVN     START3+1(1),TEMP1  *                                     00222000
START3 PACK    PAKT,RQSTIN+3(0)   *                                     00222100
       CVB     4,PAKT            *                                      00222200
       CH      4,=H'15999'       Q/ ADDRESS GT 15999                    00222300
       BH      SNDILG            YES, ERROR                             00222400
         AR    4,7               NO, GO THERE                           00222500
       LR      10,4              *                                      00222600
       LA      9,0               *                                      00222700
       B       NXTOP             *                                      00222800
START4   XC    RQSTIN,RQSTIN   ZERO OUT REPLY AREA                      00222900
         STM   13,15,MACREGSV  SAVE MACRO REG                           00223000
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00223100
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00223200
         WTOR  'CANNOT START,NO PGM LOADED',RQSTIN,50,WTECB             00223300
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00223400
         B     TESTAA                                                   00223500
OKSTT  DC      X'00'                                                    00223600
*                                                                       00223700
*      THIS SECTION WILL SIMULATE THE START-RESET PUSHBUTTON.           00223800
*                                                                       00223900
STRST  LR      6,10                                                     00224000
       AR      6,9                                                      00224100
       ST      6,ADR360                                                 00224200
       MVI     TPERR,0                                                  00224300
       MVI     TPEOF,0                                                  00224400
       MVI     OVRFLO,0                                                 00224500
       MVI     CPR,0                                                    00224600
         L     8,RETURN                LOAD ADDRESS FOR START    TAB    00224700
         BR    8                       GO DO START               TAB    00224710
*                                                                       00224800
*      THIS SECTION SIMULATES THE LOAD TAPE PUSHBUTTON.                 00224900
*                                                                       00225000
TPLOAD LA      10,=X'00000001'                                          00225100
         MVI   MOD,X'00'                                                00225200
       BAL     8,FNDRIV                                                 00225300
         ST    3,TMDCB                                                  00225400
         BAL   8,TSTOPEN                                                00225500
         BAL   8,LOADMD                                                 00225600
         MVC   TPCCW,=A(LDTCCW)                                         00225700
         MVI   TMIOB,X'44'                                              00225800
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00225900
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00226000
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00226100
         EXCP  TMIOB                                                    00226200
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00226300
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00226400
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00226500
         LH    1,TMIOB+14          LOAD BYTE COUNT FROM CSW             00226600
         LH    2,=H'18000'                                              00226700
       SR      2,1                                                      00226800
       LA      3,SIMCOR+1                                               00226900
         L     1,TAPEAREA                                               00227000
TPLD1  CLI     0(1),X'6D'                                               00227100
       BNE     TPLD2                                                    00227200
       LA      1,1(1)                                                   00227300
       MVC     0(1,3),0(1)                                              00227400
       TR      0(1,3),TREI                                              00227500
       OI      0(3),X'40'                                               00227600
       SH      2,=H'1'                                                  00227700
       B       TPLD3                                                    00227800
TPLD2  MVC     0(1,3),0(1)                                              00227900
       TR      0(1,3),TREI                                              00228000
TPLD3  LA      1,1(1)                                                   00228100
       LA      3,1(3)                                                   00228200
       BCT     2,TPLD1                                                  00228300
       NI      0(3),X'40'                                               00228400
       OI      0(3),X'3F'                                               00228500
       LA      12,1(3)                                                  00228600
       LA      10,SIMCOR+1                                              00228700
       LA      9,0                                                      00228800
       MVI     OKSTT,1                                                  00228900
       B       NXTOP                                                    00229000
*                                                                       00229100
*      THIS SECTION SIMULATES THE SETTING OF SENSE SWITCHES BY SETTING  00229200
*      INDICATORS IN CORE BASED UPON THE SSS INPUT COMMAND.  THE        00229300
*      ROUTINES THAT SIMULATE THE BSS INSTRUCTIONS WILL TEST THESE      00229400
*      INDICATORS.                                                      00229500
*                                                                       00229600
SSIN   LA      6,RQSTIN+3        REFERENCE FIRST SENSE SWITCH           00229700
       XC      TSSA(7),TSSA      CLEAR TEMPORARY SENSE SWITCHES         00229800
       LA      5,8               SET TO SCAN 8 SETTINGS MAX             00229900
SSIN1  CLI     0(6),0            Q/ DONE                                00230000
       BE      SSEND             YES, MOVE THEM                         00230100
       CLI     0(6),C'A'         Q/ IS THIS SENSE SWITCH LEGAL          00230200
       BL      SNDILG            NO                                     00230300
       CLI     0(6),C'G'                                                00230400
       BH      SNDILG            NO                                     00230500
       IC      4,0(6)            YES                                    00230600
       N       4,=F'7'           SET TEMPORARY SENSE SWITCH             00230700
       LA      2,TSSA-1          *                                      00230800
       AR      2,4               *                                      00230900
       MVI     0(2),1            *                                      00231000
       LA      6,1(6)            REFERENCE NEXT INPUT CHARACTER         00231100
       BCT     5,SSIN1           Q/ ARE THERE TOO MANY INPUT CHARACTERS 00231200
       B       SNDILG            YES                                    00231300
SSEND  MVC     SENSEA(7),TSSA                                           00231400
         B     WTORTN                                                   00231500
TSSA   DS      7C                TEMPORARY SENSE SWITCHES               00231600
*                                                                       00231700
*     THIS SECTION WILL SET THE TAPE TABLE WITH THE 1401-360 EQUIVALANT 00231800
*      TAPE DRIVE NUMBERS BASED UPON THE TAS OPERATOR COMMAND.  THE     00231900
*      TAPE INSTRUCTION ROUTINES WILL SEARCH THIS TABLE TO DETERMINE    00232000
*      WHICH DRIVE TO USE.                                              00232100
         DS    0F                                                       00232200
TPTBL  DC      XL4'E3D7F00A'                                            00232300
       DC      XL4'E3D7F101'                                            00232400
         DC    XL4'E3D7F202'                                            00232500
         DC    XL4'E3D7F303'                                            00232600
         DC    XL4'E3D7F404'                                            00232700
         DC    XL4'E3D7F505'                                            00232800
         DC    XL4'E3D7F606'                                            00232900
       DC      XL4'E3D7F707'                                            00233000
       DC      XL4'E3D7F808'                                            00233100
       DC      XL4'E3D7F909'                                            00233200
SAVE5    DC    F'0'                                                     00233300
SAVEDD   DC    F'0'                                                     00233400
DDLEN    DC    H'0'                                                     00233500
*      THIS SECTION WILL CLEAR ALL 1401 CORE UPON ENTRY OF THE OPERATOR 00233600
*      COMMAND CLR.                                                     00233700
*                                                                       00233800
CLR      LA    2,SIMCOR                                                 00233900
         LA    3,64                                                     00234000
CLR1     XC    0(250,2),0(2)                                            00234100
         LA    2,250(2)                                                 00234200
       BCT     3,CLR1                                                   00234300
       MVI     OKSTT,0                                                  00234400
         B     WTORTN                                                   00234500
*                                                                       00234600
*      THIS SECTION WILL DISPLAY ON THE PRINTER THE HUNDREDS GROUP      00234700
*      OF 1401 CORE REFERENCED IN THE OPERATOR COMMAND DIS .            00234800
*                                                                       00234900
DIS    LA      5,RQSTIN+3                                               00235000
DIS1   CLI     0(5),X'00'                                               00235100
       BE      DIS2                                                     00235200
       CLI     0(5),C'0'                                                00235300
       BL      SNDILG                                                   00235400
       LA      5,1(5)                                                   00235500
       B       DIS1                                                     00235600
DIS2   LR      2,5                                                      00235700
       SH      2,=H'2'                                                  00235800
       CLC     0(2,2),=C'00'                                            00235900
       BNE     SNDILG                                                   00236000
       S       5,=A(RQSTIN+4)                                           00236100
       CH      5,=H'4'                                                  00236200
       BH      SNDILG                                                   00236300
       STC     5,DIS3+1                                                 00236400
         MVC   DSMRKR+1(20),WM256                                       00236500
DIS3     MVC   DSMRKR+9(0),RQSTIN+3  MOVE ADDR FOR PRINTING             00236600
       STC     5,TEMP1                                                  00236700
       MVN     DIS4+1(1),TEMP1                                          00236800
DIS4   PACK    PAKT,RQSTIN+3(0)                                         00236900
       CVB     4,PAKT                                                   00237000
       CH      4,=H'15900'                                              00237100
       BH      SNDILG                                                   00237200
         AR    4,7                      ADD IN ADDRESS OF 1401 SIMCORE  00237300
         MVI   PRNTBUFF,X'09'    SET CARRIAGE CONTROL                   00237400
         MVC   PRNTBUFF+1(20),WM256                                     00237500
         MVC   PRNTBUFF+21(100),0(4)                                    00237600
         TR    PRNTBUFF+21(100),TRIE    CHANGE PRINT AREA TO EBCDIC     00237700
         MVC   PRNTBUFF+121(11),WM256   *                               00237800
         STM   13,15,MACREGSV           SAVE BASE REGISTERS             00237900
         LA    6,MACREGSV               *                               00238000
         LA    13,SAVEAREA              LOAD SAVE AREA                  00238100
         MVC   DISPMSG+4(100),DSMRKR+21                                 00238200
         LA    1,DISPMSG                                                00238300
         SVC   35                                                       00238400
         LM    14,15,4(6)                                               00238500
         MVC   DISPMSG+4(100),PRNTBUFF+21                               00238600
         LA    1,DISPMSG                                                00238700
         SVC   35                                                       00238800
         LM    14,15,4(6)                                               00238900
         MVC   PRNTBUFF+21(100),0(4)    CHANGE WORD MARKS TO EBCDIC IS  00239000
         TR    PRNTBUFF+21(100),TRWDMK  *                               00239100
         MVC   DISPMSG+4(100),PRNTBUFF+21                               00239200
         LA    1,DISPMSG                                                00239300
         SVC   35                                                       00239400
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00239500
         B     WTORTN                                                   00239600
         CNOP  0,4                                                      00239700
DISPMSG  DC    AL2(ENDISP-*)     MESSAGE LENGTH                         00239800
         DC    X'0000'                                                  00239900
         DC    100X'40'                                                 00240000
ENDISP   EQU   *                                                        00240100
DSMRKR   DC    X'09',20X'40'                                            00240200
       DC      C'0.......09........19........29........39........49.'   00240300
       DC      C'.......59........69........79........89........99'     00240400
       DC      C'            '                                          00240500
*                                                                       00240600
*      THIS SECTION WILL MODIFY THE 1401 CORE LOCATION REFERENCED IN    00240700
*      THE OPERATOR COMMAND ALT .                                       00240800
*                                                                       00240900
ALT    LA      6,RQSTIN+3                                               00241000
ALT1   CLI     0(6),C','                                                00241100
       BE      ALT2                                                     00241200
       CLI     0(6),C'0'                                                00241300
       BL      SNDILG                                                   00241400
       LA      6,1(6)                                                   00241500
       B       ALT1                                                     00241600
ALT2   LR      5,6                                                      00241700
       S       5,=A(RQSTIN+4)                                           00241800
       CH      5,=H'4'                                                  00241900
       BH      SNDILG                                                   00242000
       STC     5,TEMP1                                                  00242100
       MVN     ALT3+1(1),TEMP1                                          00242200
ALT3   PACK    PAKT,RQSTIN+3(0)                                         00242300
       CVB     4,PAKT                                                   00242400
       CH      4,=H'15999'                                              00242500
       BH      SNDILG                                                   00242600
         AR    4,7                                                      00242700
         LA    6,1(6)                  BUMP TO DATA              TAB    00242710
ALT4     CLI   0(6),0                  END OF DATA?              TAB    00242800
         BE    WTORTN                  YES, GO GET NEXT COMMAND  TAB    00242900
         MVI   0(4),0                  CLEAR 1401 CHAR           TAB    00242910
         CLI   0(6),C'_'               NO, CHECK FOR WORK MARK   TAB    00243000
         BNE   ALT4A                   NO WM, GO MOVE CHAR       TAB    00243100
         MVI   0(4),X'40'              WM REQ, SET WM IN 1401 CORE TAB  00243200
         LA    6,1(6)                  BUMP TO CHAR TO BE MOVED  TAB    00243300
ALT4A    TR    0(1,6),TREI             TRANS CHAR TO 1401 CODE   TAB    00243310
         OC    0(1,4),0(6)             INSERT CHAR IN 1401 CORE  TAB    00243320
         LA    6,1(6)                  BUMP TO NEXT CHAR IN REQ  TAB    00243330
         LA    4,1(4)                  BUMP TO NEXT CHAR IN 1401 TAB    00243340
         B     ALT4                    LOOP TO CHECK NEXT CHAR   TAB    00243350
*                                                                       00243400
*      THIS SECTION WILL WRITE A TAPE MARK ON THE TAPE DRIVE            00243500
*      SELECTED BY THE WTM COMMAND.                                     00243600
*                                                                       00243700
WTMCMD NI      RQSTIN+3,X'0F'    GET DEVICE ADDRESS                     00243800
       LA      10,RQSTIN         *                                      00243900
       BAL     8,FNDRIV          *                                      00244000
         ST    3,TMDCB                                                  00244100
         BAL   8,TSTOPEN                                                00244200
         BAL   8,LOADMD                                                 00244300
         MVC   TPCCW,=A(WTMCCW)                                         00244400
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00244500
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00244600
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00244700
         EXCP  TMIOB                                                    00244800
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00244900
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00245000
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00245100
         B     WTORTN                                                   00245200
*                                                                       00245300
*      THIS ROUTINE WILL TERMINATE THE SIMULATOR UPON THE OPERATOR      00245400
*      ENTRY 'TRM'.                                                     00245500
*                                                                       00245600
TERMINAT STM   13,15,MACREGSV                                           00245700
         LA    13,SAVEAREA                                              00245800
         LA    6,MACREGSV                                               00245900
         CLOSE PRNTDCB                                                  00246000
         LM    14,15,4(6)                                               00246100
         TM    PUNCHR+48,X'10'         IS PUNCH OPEN?            TAB    00246110
         BZ    EXIT                    NO, GO EXIT               TAB    00246120
         PUT   PUNCHR,PCHARAA                                           00246200
EXIT     L     13,4(13)                POINT TO SAVE AREA        TAB    00246300
         RETURN  (14,12),RC=0          RETURN WITH RC=0          TAB    00246400
DUMPCORE L     13,=A(W)
         USING W,13
         OI    DUMPSW+1,X'F0'          SET DUMP SWITCH
         BAL   2,DUMP                  DUMP 201 - 332
         MVC   201(100,7),0(7)
         MVC   SIMCOR+310(6),=X'0A002000909'    0 - 99
         BAL   2,DUMP                  DUMP 000 - 099
         MVC   201(101,7),100(7)
         MVC   SIMCOR+310(9),=X'010A0A002000020A01'  100 - 201
         BAL   2,DUMP                  DUMP 100 - 201
         MVC   DUMPADR,=H'300'         SET UP LOCATION COUNTER
DUMPLOOP LH    1,DUMPADR               GET DUMP ADDRESS
         CH    1,=H'16000'             ARE WE AT END OF 1401 CORE?
         BNL   DUMPDONE                YES, EXIT
         AR    1,7                     ADD 1401 BASE ADDRESS
         MVC   201(100,7),0(1)         MOVE 100 POS TO PRINT AREA
         SR    1,7                     SUBTRACT 1401 BASE ADDRESS
         CVD   1,DUMPSAV2              ADDR TO DECIMAL
         UNPK  SIMCOR+310(5),DUMPSAV2
         OI    SIMCOR+314,X'F0'        REMOVE SIGN
         TR    SIMCOR+310(5),TREI      TRANS TO 1401 CODE
         LA    1,100(1)                BUMP TO NEXT BAND
         STH   1,DUMPADR               STORE ADDR OF NEXT BAND
         BAL   2,DUMP
         B     DUMPLOOP
DUMPDONE NI    DUMPSW+1,X'0F'          TURN OFF DUMP SWITCH
         STM   13,15,MACREGSV          SAVE REGS
         LA    6,MACREGSV              SAVE ADDR OF SAVE AREA
         LA    13,SAVEAREA             GET ADDRESS OF SAVE AREA
         TRUNC PRNTDCB                 FORCE WRITE FOR LAST BLOCK
         LM    13,15,0(6)              RESTORE REGS
         B     WTORTN                  EXIT
DUMP     LA    9,1                     LENGTH OF 1401 INST
         LA    10,DUMPOP               ADDR OF 1401 OP CODE
         LA    4,DUMPCLR               LOAD RETURN ADDRESS
         ST    2,DUMPSAV2              SAVE LINK REG 2
         B     W                       BRANCH TO WRITE ROUTINE
DUMPCLR  XC    201(132,7),210(7)       CLEAR 1401 PRINT AREA
         L     2,DUMPSAV2              LOAD RETURN REG 2
         BR    2
DUMPOP   DC    X'42423C7B'             _2_2<_.  1401 INSTS FOR DUMP
DUMPADR  DC    H'0'
DUMPSAV2 DS    D
         DROP  13
TMIOB    DS    0D                                                       00247500
         DC    X'42'                                                    00247600
         DC    4X'00'                                                   00247700
         DC    AL3(TMECB)                                               00247800
         DC    X'00'                                                    00247900
TPCSW    DC    7X'00'                                                   00248000
TPCCW    DC    XL4'00'             ADDRESS OF CCW FOR TAPE OPERATION    00248100
TMDCB    DC    XL4'00'             DCB ADDRESS FOR TAPE DRIVE SELECTED  00248200
         DC    4X'00'                                                   00248300
         DC    2X'00'                                                   00248400
         DC    2X'00'                                                   00248500
TMECB    DS    0F                                                       00248600
         DC    4X'00'                                                   00248700
*                                                                       00248800
*      THIS SECTION WILL REWIND THE TAPE SELECTED BY THE RWD COMMAND    00248900
*                                                                       00249000
RWDCMD NI      RQSTIN+3,X'0F'    GET DEVICE ADDRESS                     00249100
       LA      10,RQSTIN         *                                      00249200
       BAL     8,FNDRIV          *                                      00249300
         ST    3,TMDCB                                                  00249400
         BAL   8,TSTOPEN                                                00249500
         BAL   8,LOADMD                                                 00249600
         MVC   TPCCW,=A(RWDCCW)                                         00249700
         MVI   TMIOB,X'04'                                              00249800
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00249900
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00250000
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00250100
         EXCP  TMIOB                                                    00250200
         LM    14,15,4(6)      RESTORE REG 14 AND 15                    00250300
         WAIT  1,ECB=TMECB     WAIT FOR I/O                             00250400
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00250500
         B     WTORTN                                                   00250600
*      BEFORE BRANCHING, SET THE B ADDRESS REGISTER TO THE ADDRESS OF   00250700
*      THE INSTRUCTION AFTER THE BRANCH, THEN SET THE INSTRUCTION       00250800
*      COUNTER TO THE BRANCH ADDRESS, AND BRANCH.                       00250900
*                                                                       00251000
SETBCH BAL     8,CVAD43          CONVERT BRANCH ADDRESS                 00251100
       LR      12,10             LOAD B ADDRESS                         00251200
       AR      12,9              *                                      00251300
       ST      10,LSTBCH         SAVE LAST BRANCHED FROM LOCATION       00251400
       LR      10,5              LOAD BRANCH ADDRESS                    00251500
       LA      9,0               *                                      00251600
       B       NXTOP             GO TO BRANCH ADDRESS FOR NXT INSTR.    00251700
ILEGTAPE STM   13,15,MACREGSV    SAVE REGISTERS                         00251800
         LA    6,MACREGSV        *                                      00251900
         LA    13,SAVEAREA       *                                      00252000
         WTO   'UNDEFINED TAPE'                                         00252100
         LM    13,15,0(6)                                               00252200
         B     PANEL                                                    00252300
ILEGOP   STM   13,15,MACREGSV    SAVE MACRO REGS                        00252400
         LA    6,MACREGSV        SAVE ADDRESS TO XR                     00252500
         LA    13,SAVEAREA       GIVE OS OUR SAVE AREA                  00252600
         WTO   'ILLEGAL OP CODE'                                        00252700
         LM    13,15,0(6)        RESTORE MACRO REGISTERS                00252800
       B       PANEL                                                    00252900
ILEGLN   STM   13,15,MACREGSV    SAVE MACRO REGS                        00253000
         LA    6,MACREGSV        SAVE ADDRESS TO XR                     00253100
         LA    13,SAVEAREA       GIVE OS OUR SAVE AREA                  00253200
         WTO   'ILLEGAL LENGTH'                                         00253300
         LM    13,15,0(6)        RESTORE MACRO REGISTERS                00253400
PANEL  LR      1,10                                                     00253500
         SR    1,7                                                      00253600
       CVD     1,PAKT                                                   00253700
         UNPK  PNLWTOR+19(6),PAKT+5(3)                                  00253800
         MVZ   PNLWTOR+24(1),=C'0'                                      00253900
         MVC   PNLWTOR+34(1),0(10)                                      00254000
         NI    PNLWTOR+34,X'BF'                                         00254100
         TR    PNLWTOR+34(1),TRIE                                       00254200
       CVD     9,PAKT                                                   00254300
         UNPK  PNLWTOR+48(6),PAKT+5(3)                                  00254400
         MVZ   PNLWTOR+53(1),=C'0'                                      00254500
         MVI   PNLWTOR+65,X'80'                                         00254600
         MVC   PNLWTOR+66(1),PNLWTOR+65                                 00254700
       CH      9,=H'8'                                                  00254800
         BH    WTORPNL                                                  00254900
       LTR     3,9                                                      00255000
         BZ    WTORPNL                                                  00255100
       SH      3,=H'1'                                                  00255200
       STC     3,PANEL1+1                                               00255300
PANEL1   MVC   PNLWTOR+65(0),0(10)                                      00255400
         TR    PNLWTOR+65(8),TRIE                                       00255500
WTORPNL  XC    RQSTIN,RQSTIN   ZERO OUT REPLY AREA                      00255600
         STM   13,15,MACREGSV  SAVE MACRO REG                           00255700
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00255800
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00255900
PNLWTOR  WTOR  ' I             OP        LENGTH            INST        X00256000
                 ',RQSTIN,50,WTECB                                      00256100
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00256200
         B     TESTAA                                                   00256300
       TITLE   'ADDRESS CONVERSION SUBROUTINES'                         00256400
*                                                                       00256500
*      SUBROUTINE TO CONVERT A 1401 ADDRESS TO A 360 ADDRESS            00256600
*                                                                       00256700
CVAD43 MVI     IXTMP,0                                                  00256800
       LR      5,7               LOAD SIMULATED CORE BASE INTO 5        00256900
CVAD4A IC      3,0(6)            1000'S + 100'S                         00257000
       N       3,=F'63'          *                                      00257100
       SLL     3,1               *                                      00257200
         AH    5,TBHNTH(3)       *                                      00257300
       IC      3,2(6)            4000'S + 1'S                           00257400
       N       3,=F'63'          *                                      00257500
       SLL     3,1               *                                      00257600
         AH    5,TBT4UN(3)       *                                      00257700
       IC      3,1(6)            10'S                                   00257800
       N       3,=F'15'          *                                      00257900
       SLL     3,1               *                                      00258000
         AH    5,TBTENS(3)       *                                      00258100
       TM      1(6),X'30'        Q/ INDEXING                            00258200
       BE      CVAD4D            NO, DONE                               00258300
         CLI   IXTMP,1           Q/ SECOND TIME THROUGH                 00258400
         BE    CVAD4D            YES, DONE                              00258500
       MVI     IXTMP,1           SET SECOND TIME INDICATOR              00258600
         TM    1(6),X'30'        Q/ IX3                                 00258700
       BO      CVAD4C            IX3                                    00258800
       TM      1(6),X'20'                                               00258900
       BO      CVAD4B            IX2                                    00259000
       LA      6,SIMCOR+87       IX1                                    00259100
       B       CVAD4A                                                   00259200
CVAD4B LA      6,SIMCOR+92                                              00259300
       B       CVAD4A                                                   00259400
CVAD4C LA      6,SIMCOR+97                                              00259500
       B       CVAD4A                                                   00259600
CVAD4D   C     5,SIMLIMIT        Q/ IS ADDRESS GREATER THAN 15999       00259700
       BCR     12,8              NO, DONE                               00259800
       SH      5,=H'16000'       YES, SUBTRACT 16000                    00259900
       BR      8                                                        00260000
ADR360 DS      F                                                        00260100
ADR140 DS      CL3                                                      00260200
IXTMP  DS      C                                                        00260300
TEMP1  DS      C                                                        00260400
TEMP2    DS    C                                                        00260500
*                                                                       00260600
*      SUBROUTINE TO CONVERT A 360 ADDRESS TO A 1401 ADDRESS            00260700
*                                                                       00260800
CVAD34 L       5,ADR360                                                 00260900
       SR      5,7               SUBTRACT SIMULATED CORE BASE           00261000
       LA      4,0               4000'S ZONE                            00261100
       D       4,=F'4000'        *                                      00261200
       SLL     5,4               *                                      00261300
       LR      1,5               *                                      00261400
       LR      5,4               1000'S ZONE                            00261500
       LA      4,0               *                                      00261600
       D       4,=F'1000'        *                                      00261700
       SLL     5,4               *                                      00261800
       LR      2,5               *                                      00261900
       LR      5,4               100'S NUMERIC                          00262000
       LA      4,0               *                                      00262100
       D       4,=F'100'         *                                      00262200
       OR      5,2               *                                      00262300
       STC     5,ADR140          *                                      00262400
       LR      5,4               10'S NUMERIC                           00262500
       LA      4,0               *                                      00262600
       D       4,=F'10'          *                                      00262700
       STC     5,ADR140+1        *                                      00262800
       OR      4,1               *                                      00262900
       STC     4,ADR140+2        *                                      00263000
       TM      ADR140,X'0F'      Q/ IS HUNDREDS ZERO                    00263100
       BC      5,CVAD3A          NO                                     00263200
       OI      ADR140,X'0A'      YES, ADD 8-2 BITS                      00263300
CVAD3A TM      ADR140+1,X'0F'    Q/ IS TENS ZERO                        00263400
       BC      5,CVAD3B          NO                                     00263500
       OI      ADR140+1,X'0A'    YES, ADD 8-2 BITS                      00263600
CVAD3B TM      ADR140+2,X'0F'    Q/ IS UNITS ZERO                       00263700
       BCR     5,8               NO, RETURN                             00263800
       OI      ADR140+2,X'0A'    YES, ADD 8-2 BITS                      00263900
       BR      8                 RETURN                                 00264000
       TITLE  'ROUTINES TO HELP UNIT RECORD OPERATIONS'                 00264100
HALTWTO  STM   13,15,MACREGSV       SAVE BASE REGISTERS                 00264200
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00264300
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00264400
HLTWTO   WTO   'HALT  I      , A      , B      '                        00264500
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00264600
         BR    8                     RETURN TO HALT ROUTINE             00264700
READ     CLI   CRDEOF,X'01'    HAVE WE READ LAST CARD                   00264800
         BNE   READ2           BRANCH IF NO                             00264900
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00265000
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00265100
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00265200
         WTO   'READ TRIED AFTER LAST CARD'                             00265300
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00265400
         B     PANEL                                       TAB 71110    00265500
READ2  TR      TMPARA(80),TREI     CHANGE EBCDIC TO INTERNAL 1401 CODE  00265600
       NC      SIMCOR+1(80),WM256   REMOVE CARD AREA INFO, KEEP WD MKS  00265700
       OC      SIMCOR+1(80),TMPARA                                      00265800
       LA      12,SIMCOR+81                                             00265900
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00266000
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00266100
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00266200
         GET   CARD,TMPARA     READ CARD                                00266300
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00266400
         NI    SIMCOR,X'40'      SET BA BITS IN LOC 0 AFTER READ        00266500
         OI    SIMCOR,X'30'      *                                      00266600
       BR      8                                                        00266700
EOC      LM    13,15,0(6)        RESTORE SIMULATOR REGISTERS            00266800
         MVI   CRDEOF,X'01'      SET CARD EOF INDICATOR                 00266900
         BR    8                                                        00267000
WRITE    LA    3,PRNTDCB               GET ADDR OF PRINT DCB     TAB    00267100
         BAL   5,UROPEN                GO CHECK FOR OPEN         TAB    00267110
         MVC   PRNTBUFF+1(132),SIMCOR+201                        TAB    00267120
         TR    PRNTBUFF+1(132),TRIE                                     00267200
         STM   13,15,MACREGSV  SAVE MACRO REG                           00267300
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00267400
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00267500
         PUT   PRNTDCB,PRNTBUFF                                         00267600
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00267700
COUNTER  STM   7,8,DGSAVE                                               00267800
         TM    PRNTBUFF,X'80'     IS IT SKIP TO CHAN                    00267900
         BO    DG1                 YES                                  00268000
         CLI   PRNTBUFF,X'01'     IS IT NO AUTO SPACE                   00268100
         BE    DGEXIT                                                   00268200
         XR    7,7                                                      00268300
         IC    7,PRNTBUFF                                               00268400
         SRL   7,3                                                      00268500
         L     8,DGCUR                                                  00268600
DG11     BCTR  7,0                 SUBT ON FROM NO. OF SPACES           00268700
         LTR   7,7                 IS NO. OF SPACES LESS THAN ZERO      00268800
         BM    DG4                 YES-CLEAN UP AND EXIT                00268900
         LA    8,1(8)                                                   00269000
         C     8,DGEND             IS NO. OF SPACES LESS THAN ZERO      00269100
         BH    DG10                YES-CLEAN UP AND EXIT                00269200
DG14     CLI   0(8),X'00'          NO PUNCH IN CARRIAGE TAPE            00269300
         BE    DG11                BLANK-KEEP TRYING                    00269400
         MVI   PRTP9,X'00'         NOW BLANK                            00269500
         MVI   PRTP12,X'00'                                             00269600
         CLI   0(8),X'F9'          IS IT NINE PUNCH                     00269700
         BE    DG12                YES                                  00269800
         CLI   0(8),X'50'          IS IT 12 PUNCH                       00269900
         BE    DG13                YES                                  00270000
         B     DG11                NO-LOOK FOR NEXT PUNCH               00270100
DG10     L     8,=A(CARTAB)        POINT TO FRONT OF TABLE              00270200
         B     DG14                                                     00270300
DG12     MVI   PRTP9,X'01'         SET CHAN 9 SWITCH                    00270400
         B     DG11                                                     00270500
DG13     MVI   PRTP12,X'01'        SET CHAN 12 SWITCH                   00270600
         B     DG11                                                     00270700
DG1      CLI   PRNTBUFF,X'C9'     CHAN 9 AFTER                          00270800
         BE    DG2                                                      00270900
         CLI   PRNTBUFF,X'CB'     CHAN 9 IMMED                          00271000
         BE    DG2                                                      00271100
         CLI   PRNTBUFF,X'E1'      CHAN 12 AFTER                        00271200
         BE    DG3                                                      00271300
         CLI   PRNTBUFF,X'E3'      CHAN 12 IMMED                        00271400
         BE    DG3                                                      00271500
         MVI   PRTP12,X'00'        CLEAR 12                             00271600
         MVI   PRTP9,X'00'         CLEAR 9                              00271700
         XR    7,7                                                      00271800
         IC    7,PRNTBUFF                                               00271900
         SRL   7,3                                                      00272000
         N     7,=F'15'                                                 00272100
         O     7,=F'240'                                                00272200
         C     7,=F'249'                                                00272300
         BH    DG8                                                      00272400
         STC   7,DG6+1                                                  00272500
         B     DG7                                                      00272600
DG8      C     7,=F'250'                                                00272700
         BE    DG9                                                      00272800
         MVI   DG6+1,X'60'                                              00272900
         B     DG7                                                      00273000
DG9      MVI   DG6+1,X'F0'                                              00273100
         B     DG7                                                      00273200
DG2      MVI   PRTP9,X'01'         CHAN 9 SIGNAL                        00273300
         MVI   PRTP12,X'00'                                             00273400
         MVI   DG6+1,X'F9'                                              00273500
DG7      XR    7,7                                                      00273600
         L     8,DGCUR                                                  00273700
DG6      CLI   0(8),X'FF'          LOOK FOR CHAN                        00273800
         BE    DG4                                                      00273900
         LA    7,1(7)                                                   00274000
       C       7,=F'138'                                                00274100
         BH    DGERROR                                                  00274200
         LA    8,1(8)                                                   00274300
         C     8,DGEND                                                  00274400
         BH    DG5                                                      00274500
         B     DG6                                                      00274600
DG5      L     8,=A(CARTAB)                                             00274700
         B     DG6                                                      00274800
DG3      MVI   PRTP12,X'01'                                             00274900
         MVI   PRTP9,X'00'                                              00275000
         MVI   DG6+1,X'50'                                              00275100
         B     DG7                                                      00275200
DG4      ST    8,DGCUR                                                  00275300
DGEXIT   LM    7,8,DGSAVE                                               00275400
         MVI   PRNTBUFF,X'09'     RESTORE SINGLE SPACE AFTER PRINT      00275500
         XC    PRNTBUFF+1,PRNTBUFF+1                                    00275600
         LA    12,SIMCOR+333       SET B ADDRESS REGISTER               00275700
       BR      8                                                        00275800
DGEND    DC    A(CARTAB+65)                                             00275900
DGCUR    DC    A(CARTAB)                                                00276000
CARTAB   DC    XL6'0000000000F1'                                        00276100
         DC    53X'00'                                                  00276200
         DC    X'50'                                                    00276300
       DC      78X'00'                                                  00276400
ENDCRTP  EQU   *                                                        00276500
DGSAVE   DC    2F'0'                                                    00276600
DGERROR  STM   13,15,MACREGSV                                           00276700
         LA    6,MACREGSV                                               00276800
         LA    13,SAVEAREA                                              00276900
         MVC   DG20+45(1),DG6+1                                         00277000
DG20     WTO   'UNABLE TO FIND CARRIAGE CONTROL CHAR  '                 00277100
         LM    13,15,0(6)                                               00277200
         B     TERMINAT                                                 00277300
PUNCH    LA    3,PUNCHR                GET ADDR OF PUNCH DCB     TAB    00277400
         BAL   5,UROPEN                GO CHECK FOR OPEN         TAB    00277410
         MVC   PCHARA,SIMCOR+101  CONVRT 1401 PCH AREA FOR OUTPUT  TAB  00277420
         TR    PCHARA,TRIE          *                                   00277500
         STM   13,15,MACREGSV  SAVE MACRO REGS                          00277600
         LA    6,MACREGSV      SAVE ADDRESS TO XR                       00277700
         LA    13,SAVEAREA     GIVE OS OUR SAVE AREA                    00277800
         PUT   PUNCHR,PCHARA                                            00277900
         LM    13,15,0(6)      RESTORE MACRO REGISTERS                  00278000
         LA    12,SIMCOR+181                                            00278100
         MVC   PCHAR1,PCHARA                                            00278200
         MVI   PCHARAA,X'01'                                            00278300
         NI    SIMCOR+100,X'40'    SET 82 BITS IN LOC 100 AFTER PUNCH   00278400
         OI    SIMCOR+100,X'0A'    *                                    00278500
       BR      8                                                        00278600
BINMOD   EQU   *                                                        00278700
LOADMD   BR    8                                                        00278800
TSTOPEN  EQU *                                                          00278900
         TM    48(3),X'10'                                              00279000
         BO    0(8)                                                     00279100
         STM   13,15,MACREGSV                                           00279200
         LA    6,MACREGSV                                               00279300
         LA    13,SAVEAREA                                              00279400
         OPEN  ((3),INPUT)                                              00279500
         LM    13,15,0(6)                                               00279600
         TM    48(3),X'10'             IS DCB OPEN NOW?          TAB    00279700
         BCR   1,8                     YES, RETURN TO CALLER     TAB    00279705
         ABEND 1000,DUMP               NO, DCB DID NOT OPEN, ABEND TAB  00279710
*                                                                       00279715
UROPEN   TM    48(3),X'10'             IS THE DCB OPEN?          TAB    00279720
         BCR   1,5                     YES, EXIT                 TAB    00279725
         STM   13,15,MACREGSV          SAVE REGS FOR MACRO       TAB    00279730
         LA    6,MACREGSV              SAVE ADDR OF SAVE AREA    TAB    00279735
         LA    13,SAVEAREA             GET ADDR OF NEW SAVE AREA TAB    00279740
         OPEN  ((3),OUTPUT)            OPEN U/R OUTPUT           TAB    00279745
         LM    13,15,0(6)              RELOAD BASE REGS          TAB    00279750
         TM    48(3),X'10'             DID THE DCB OPEN OK?      TAB    00279755
         BCR   1,5                     YES, EXIT                 TAB    00279760
         ABEND 1001,DUMP               NO, DCB NOT OPEN, ABEND   TAB    00279765
       TITLE  'ROUTINES TO HELP TAPE OPERATIONS'                        00279800
FNDLNG LR      6,12                                                     00279900
FNDLGA TRT     0(256,6),TRGPWM   SCAN FOR GP MK - WD MK                 00280000
       BC      6,FNDLGB          FOUND                                  00280100
       LA      6,256(6)                                                 00280200
       B       FNDLGA                                                   00280300
FNDLGB LR      6,1               CALCULATE LENGTH                       00280400
       SR      6,12              *                                      00280500
       BR      8                                                        00280600
FNDRIV LA      3,10                                                     00280700
       LA      4,TPTBL                                                  00280800
FNDRV1   CLC   3(1,10),3(4)                                             00280900
       BE      FNDRV2                                                   00281000
         LA    4,4(4)                                                   00281100
       BCT     3,FNDRV1                                                 00281200
         B     ILEGTAPE          TAPE NOT DEFINED, SEND ERROR MESSAGE   00281300
FNDRV2   LA    6,10                                                     00281400
         SR    6,3                                                      00281500
         LR    3,6                                                      00281600
         SLL   3,3             MULTIPLY LOGICAL DRIVE NUMBER BY 8       00281700
       A       3,=A(TAPADR)      ADD BASE OF TAPE ADDRESS TABLE         00281800
         L     3,4(3)          GET ACTUAL TAPE ADDRESS FROM TABLE       00281900
         MVI   TMECB,0         CLEAR ECB BEFORE EXCP                    00282000
         NI    0(3),X'3F'        CLEAR DCB EXCEPTION BITS               00282100
         MVI   TMIOB,X'42'       SET IOB CMD CHAIN + UNRELATED BITS     00282200
       BR      8                                                        00282300
TPTEST   MVC   SAVCSW+1(7),TPCSW      SAVE CSW AFTER TAPE OPERATION     00282400
         CLI   TMECB,X'7F'                                              00282500
         BE    TPTIO3                                                   00282600
         MVI   TPERR,1                                                  00282700
TPTIO3   EQU   *                                                        00282800
       TM      SAVCSW+4,1        Q/ EOF                                 00282900
       BZ      TPTIO1                                                   00283000
         LH    4,=H'17999'                                              00283100
       STH     4,SAVCSW+6                                               00283200
         L     4,TAPEAREA        PUT TAPE MARK CHARACTER IN TAPE AREA   00283300
         MVI   0(4),X'0F'        *                                      00283400
       MVI     TPEOF,1                                                  00283500
TPTIO1 MVI     TPERR,0                                                  00283600
       TM      SAVCSW+4,2        Q/ TAPE ERROR                          00283700
       BCR     8,8                                                      00283800
TPTIO2   MVI   TPERR,1                                                  00283900
       BR      8                                                        00284000
RWUCLOSE STM   14,15,MACREGSV    SAVE BASE REGISTERS                    00284100
         LA    6,MACREGSV        *                                      00284200
         LA    13,SAVEAREA       *                                      00284300
         CLOSE ((4))             CLOSE THE DCB                          00284400
         LM    14,15,0(6)                                               00284500
         B     NXTOP                                                    00284600
       TITLE  'ROUTINE TO BRANCH TO NEXT OPCODE PROCESSING ROUTINE'     00284700
*                                                                       00284800
*     THIS SECTION EXAMINES THE NEXT OPERATION CODE AND, BASED UPON IT, 00284900
*      BRANCHES TO THE PROPER ROUTINE TO PROCESS THE INSTRUCTION.       00285000
*                                                                       00285100
NXTOP  AR      10,9              GET NEW OP CODE LOCATION               00285200
         TM    0(10),X'40'        Q/ IS THERE A WORD MARK               00285300
       BZ      ILEGOP            NO                                     00285400
       LA      1,250(10)                                                00285500
       TRT     1(250,10),TRTB                                           00285600
       LR      9,1                                                      00285700
       SR      9,10                                                     00285800
       IC      2,0(10)           GET OP CODE                            00285900
       N       2,=F'63'          ELIMINATE WORD MARK                    00286000
       SLL     2,2               MULTIPLY BY 4                          00286100
         L     13,BCHTBL(2)      LOAD BASE OF PROCESSING ROUTINE        00286200
       BR      13                BRANCH TO OPCODE PROCESSING ROUTINE    00286300
BCHTBL DC      A(ILEGOP)         0                                      00286400
       DC      A(R)              1   1                                  00286500
       DC      A(W)              2   2                                  00286600
       DC      A(WR)             3   3                                  00286700
       DC      A(P)              4   4                                  00286800
       DC      A(RP)             5   5                                  00286900
       DC      A(WP)             6   6                                  00287000
       DC      A(WRP)            7   7                                  00287100
       DC      A(NXTOP)          10  8                                  00287200
       DC      A(NXTOP)          11  9                                  00287300
       DC      A(ILEGOP)         12  0                                  00287400
       DC      A(MA)             13  =                                  00287500
       DC      A(M)              14  @                                  00287600
       DC      A(ILEGOP)         15                                     00287700
       DC      A(ILEGOP)         16                                     00287800
       DC      A(ILEGOP)         17  TP MK                              00287900
       DC      A(ILEGOP)         20  A BIT                              00288000
       DC      A(CS)             21  /                                  00288100
       DC      A(A)              22  S                                  00288200
       DC      A(ILEGOP)         23  T                                  00288300
       DC      A(CU)             24  U                                  00288400
       DC      A(BWZ)            25  V                                  00288500
       DC      A(BBE)            26  W                                  00288600
       DC      A(NXTOP)          27  X                                  00288700
       DC      A(MZ)             30  Y                                  00288800
       DC      A(MCS)            31  Z                                  00288900
       DC      A(ILEGOP)         32                                     00289000
       DC      A(SW)             33  ,                                  00289100
       DC      A(D)              34  %                                  00289200
       DC      A(ILEGOP)         35  WD SEP                             00289300
       DC      A(ILEGOP)         36                                     00289400
       DC      A(ILEGOP)         37                                     00289500
       DC      A(ILEGOP)         40 -                                   00289600
       DC      A(ILEGOP)         41  J                                  00289700
       DC      A(SS)             42 K                                   00289800
       DC      A(LCA)            43  L                                  00289900
       DC      A(MCW)            44  M                                  00290000
       DC      A(NXTOP)          45  N                                  00290100
       DC      A(ILEGOP)         46  O                                  00290200
       DC      A(MCM)            47  P                                  00290300
       DC      A(SAR)            50  Q                                  00290400
       DC      A(ILEGOP)         51  R                                  00290500
       DC      A(ZS)             52 -0                                  00290600
       DC      A(ILEGOP)         53  $                                  00290700
       DC      A(ILEGOP)         54  *                                  00290800
       DC      A(ILEGOP)         55                                     00290900
       DC      A(ILEGOP)         56                                     00291000
       DC      A(ILEGOP)         57                                     00291100
       DC      A(ILEGOP)         60 +                                   00291200
       DC      A(A)              61  A                                  00291300
       DC      A(B)              62  B                                  00291400
       DC      A(C)              63  C                                  00291500
       DC      A(MN)             64  D                                  00291600
       DC      A(MCE)            65  E                                  00291700
       DC      A(CC)             66 F                                   00291800
       DC      A(ILEGOP)         67  G                                  00291900
       DC      A(SBR)            70  H                                  00292000
       DC      A(ILEGOP)         71  I                                  00292100
       DC      A(ZA)             72 +0                                  00292200
       DC      A(H)              73  .                                  00292300
       DC      A(CW)             74                                     00292400
       DC      A(ILEGOP)         75                                     00292500
       DC      A(ILEGOP)         76                                     00292600
       DC      A(ILEGOP)         77                                     00292700
       TITLE  ' '                                                       00292800
SENSEA DC      X'0'                                                     00292900
SENSEB DC      X'0'                                                     00293000
SENSEC DC      X'0'                                                     00293100
SENSED DC      X'0'                                                     00293200
SENSEE DC      X'0'                                                     00293300
SENSEF DC      X'0'                                                     00293400
SENSEG DC      X'0'                                                     00293500
PRTP9    DC    X'0'                                                     00293600
PRTP12 DC      X'0'                                                     00293700
TPERR  DC      X'0'                                                     00293800
TPEOF  DC      X'0'              RESET WHEN TESTED                      00293900
OVRFLO DC      X'0'              RESET WHEN TESTED                      00294000
CPR    DC      X'00'                                                    00294100
DCHAR  DS      C                                                        00294200
LSTBCH DS      F                 TO HOLD ADDRESS OF LAST BRANCH         00294300
CRDEOF DC      X'00'             CARD END-OF-FILE INDICATOR             00294400
POS1   DC      X'0'                                                     00294500
AEND   DC      X'0'                                                     00294600
SAVB   DS      F                                                        00294700
SIMLIMIT DC    F'0'              UPPER LIMIT OF SIMULATED CORE          00294800
TAPEAREA DC    F'0'              ADDRESS OF TAPE I/O BUFFER             00294900
SUPRES DC      X'00'             ZERO SUPPRESSION INDICATOR             00295000
RQSTND DC      X'00'             INDICATOR FOR OPERATOR REQUESTS        00295100
BCDTAP DS      C                 INDICATOR FOR BCD TAPE MODE            00295200
PCHERR DC      X'00'             PUNCH ERROR INDICATOR                  00295300
RDRERR DC      X'00'             CARD READ ERROR INDICATOR              00295400
PRTERR DC      X'00'             PRINTER ERROR INDICATOR                00295500
TMPARA   DS    CL80                                                     00295600
PCHARAA  DC    X'01'              NORMAL STACKER SELECT                 00295700
PCHAR1   DC    CL80' '                                                  00295800
PCHARA   DS    CL80              PUNCH OUTPUT AREA                      00295900
JOBNAME  DC    CL8' '                                                   00296000
WM256  DC      256X'40'                                                 00296100
PAKT   DS      D                                                        00296200
         DS    0F                                                       00296300
PRNTBUFF DC    X'09'                                                    00296400
         DC    CL132' '                                                 00296500
SAVEAREA DS 18F                                                         00296600
MACREGSV DS    18F                                                      00296700
SAVCSW DS      D                                                        00296800
       TITLE  'CHANNEL COMMAND WORDS'                                   00296900
WTCCW1 CCW     X'03',1,X'60',1                                          00297000
WTCCW2   CCW   1,0,X'20',0                                              00297100
LDTCCW   CCW   X'03',0,X'60',1                                          00297200
LDTCCW1  CCW   2,0,X'20',18000                                          00297300
RTCCW    CCW   X'03',0,X'60',1   READ TAPE                              00297400
RTCCW1   CCW   2,0,X'20',18000                                          00297500
WTMCCW   CCW   X'1F',0,X'20',1   WRITE TAPE MARK                        00297600
RWDCCW   CCW   X'07',0,X'20',1   REWIND                                 00297700
RETURN   DS    F                                                        00297800
CUIOB    DS    0D                                                       00297900
         DC    X'02'                                                    00298000
         DC    4X'00'                                                   00298100
         DC    AL3(CUECB)                                               00298200
         DC    8X'00'                                                   00298300
         DC    AL4(CUCCW)              ADDR OF CCW CHAIN FOR CU OPS TAB 00298400
CUDCB    DC    F'0'                                                     00298500
         DC    8X'00'                                                   00298600
CUECB    DC    F'0'                                                     00298700
*                                                                       00298800
*      THIS TABLE EQUATES A 360 TAPE DRIVE TO A 1401 TAPE DRIVE AS A    00298900
*      RESULT OF A TAS ENTRY.                                           00299000
*                                                                       00299100
TAPADR   DC    A(0,TAPEDCB0)                                            00299200
         DC    A(0,TAPEDCB1)                                            00299300
         DC    A(0,TAPEDCB2)                                            00299400
         DC    A(0,TAPEDCB3)                                            00299500
         DC    A(0,TAPEDCB4)                                            00299600
         DC    A(0,TAPEDCB5)                                            00299700
       DC      A(0,TAPEDCB6)                                            00299800
       DC      A(0,TAPEDCB7)                                            00299900
       DC      A(0,TAPEDCB8)                                            00300000
       DC      A(0,TAPEDCB9)                                            00300100
         ORG   TAPADR+1                                                 00300200
       DC      CL3'TP0'                                                 00300300
         ORG   TAPADR+9                                                 00300400
       DC      CL3'TP1'                                                 00300500
         ORG   TAPADR+17                                                00300600
       DC      CL3'TP2'                                                 00300700
         ORG   TAPADR+25                                                00300800
       DC      CL3'TP3'                                                 00300900
         ORG   TAPADR+33                                                00301000
       DC      CL3'TP4'                                                 00301100
         ORG   TAPADR+41                                                00301200
       DC      CL3'TP5'                                                 00301300
       ORG     TAPADR+49                                                00301400
       DC      CL3'TP6'                                                 00301500
       ORG     TAPADR+57                                                00301600
       DC      CL3'TP7'                                                 00301700
       ORG     TAPADR+65                                                00301800
       DC      CL3'TP8'                                                 00301900
       ORG     TAPADR+73                                                00302000
       DC      CL3'TP9'                                                 00302100
       ORG     TAPADR+82                                                00302200
PRNTDCB  DCB   MACRF=PM,DSORG=PS,DDNAME=WRITE,LRECL=133,RECFM=FBM,     C00302300
               EXLST=MURLIST                                            00302305
*        DCB EXIT ROUTINE ADDED BY T BROWN WRO 71061                    00302310
MURLIST  DC    X'85'                   END OF LIST, DCB EXIT ENTRY      00302315
         DC    AL3(MUREXIT)            ADDR OF DCB EXIT ROUTINE         00302320
         USING *,15                    R 15 IS BASE FOR THIS ROUTINE    00302325
MUREXIT  LH    3,62(1)                 GET DCB BLKSIZE                  00302330
         LH    4,82(1)                 GET DCB LRECL                    00302335
         LTR   3,3                     IS BLKSIZE ZERO?                 00302340
         BZ    SETBLK                  YES, GO SET BLKSIZE = LRECL      00302345
         SR    2,2                     NO, ZERO R2 FOR REMAINDER        00302350
         DR    2,4                     DIVIDE BLKSIZE BY LRECL          00302355
         LTR   2,2                     ZERO REMAINDER?                  00302360
         BCR   8,14                    YES,EXIT TO OPEN                 00302365
         BCR   8,14                    YES, EXIT TO OPEN                00302370
SETBLK   STH   4,62(1)                 NO, SET BLKSIZE = LRECL          00302375
         BR    14                      EXIT TO OPEN                     00302380
         USING SETBS1,15                                                00302385
PUNCHR   DCB   MACRF=PM,DSORG=PS,RECFM=FBM,LRECL=81,DDNAME=CARDOUT,    X00302400
               EXLST=MURLIST                                     TAB    00302500
CARD     DCB   MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,                    X00302600
               DDNAME=CARDIN,EODAD=EOC                                  00302700
       TITLE  'DATA CONVERSION TRANSLATE TABLES'                        00302800
TBHNTH  DC     H'0,100,200,300,400,500,600,700,800,900'                 00302900
       DC      6H'0'                                                    00303000
       DC      H'0,1100,1200,1300,1400,1500,1600,1700,1800,1900,1000'   00303100
       DC      5H'0'                                                    00303200
       DC      H'0,2100,2200,2300,2400,2500,2600,2700,2800,2900,2000'   00303300
       DC      5H'0'                                                    00303400
       DC      H'0,3100,3200,3300,3400,3500,3600,3700,3800,3900,3000'   00303500
       DC      5H'0'                                                    00303600
TBT4UN DC      H'0,1,2,3,4,5,6,7,8,9'                                   00303700
       DC      6H'0'                                                    00303800
       DC      H'0,4001,4002,4003,4004,4005,4006,4007,4008,4009,4000'   00303900
       DC      5H'0'                                                    00304000
       DC      H'0,8001,8002,8003,8004,8005,8006,8007,8008,8009,8000'   00304100
       DC      5H'0'                                                    00304200
       DC      H'0,12001,12002,12003,12004,12005,12006,12007,12008'     00304300
       DC      H'12009,12000,0,0,0,0,0'                                 00304400
TBTENS DC      H'0,10,20,30,40,50,60,70,80,90'                          00304500
       DC      6H'0'                                                    00304600
TRTB   DC      64X'00',64X'F1',64X'00',64X'F1'                          00304700
TREI   DC      64X'00'                                                  00304800
       DC      X'00000000000000000000003B3C3D3E3F'                      00304900
       DC      X'30000000000000000000002B2C2D2E2F'                      00305000
       DC      X'20110000000000000000001B1C1D1E1F'                      00305100
       DC      X'201100000000000000000A0B0C0D0E0F'                      00305200
       DC      64X'00'                                                  00305300
       DC      X'3A313233343536373839000000000000'                      00305400
       DC      X'2A212223242526272829000000000000'                      00305500
       DC      X'1A001213141516171819000000000000'                      00305600
       DC      X'0A010203040506070809000000000000'                      00305700
TRIE   DC      X'40F1F2F3F4F5F6F7F8F9F07B7C7D7E7F'                      00305800
       DC      X'F061E2E3E4E5E6E7E8E9E06B6C6D6E6F'                      00305900
       DC      X'60D1D2D3D4D5D6D7D8D9D05B5C5D5E5F'                      00306000
       DC      X'50C1C2C3C4C5C6C7C8C9C04B4C4D4E4F'                      00306100
       DC      X'40F1F2F3F4F5F6F7F8F9F07B7C7D7E7F'                      00306200
       DC      X'F061E2E3E4E5E6E7E8E9E06B6C6D6E6F'                      00306300
       DC      X'60D1D2D3D4D5D6D7D8D9D05B5C5D5E5F'                      00306400
       DC      X'50C1C2C3C4C5C6C7C8C9C04B4C4D4E4F'                      00306500
TR4IBC DC      16AL1(*-TR4IBC)                                          00306600
       DC      X'00'                                                    00306700
       DC      47AL1(*-TR4IBC)                                          00306800
TRI4BC DC      X'10'                                                    00306900
       DC      63AL1(*-TRI4BC)                                          00307000
       DC      X'10'                                                    00307100
       DC      63AL1(*-64-TRI4BC)                                       00307200
TRGPWM DC      127X'00',X'7F',128X'00'                                  00307300
TRWDMK DC      64X'40'                                                  00307400
       DC      64C'1'                                                   00307500
TYPTBL DC      129AL1(*-TYPTBL)                                         00307600
       DC      C'ABCDEFGHI'                                             00307700
       DC      XL7'00'                                                  00307800
       DC      C'JKLMNOPQR'                                             00307900
       DC      XL8'00'                                                  00308000
       DC      C'STUVWXYZ'                                              00308100
       DC      86AL1(*-TYPTBL)                                          00308200
         ORG   TYPTBL+63                                                00308300
         DC    XL3'00'                                                  00308400
         ORG   TYPTBL+256                                               00308500
       TITLE  'LITERALS'                                                00308600
         LTORG                                                          00308700
       TITLE  'TAPE DCBS'
TAPEDCB0 DCB   MACRF=(E),DSORG=PS,DDNAME=TP0                            00246500
TAPEDCB1 DCB   MACRF=(E),DSORG=PS,DDNAME=TP1                            00246600
TAPEDCB2 DCB   MACRF=(E),DSORG=PS,DDNAME=TP2                            00246700
TAPEDCB3 DCB   MACRF=(E),DSORG=PS,DDNAME=TP3                            00246800
TAPEDCB4 DCB   MACRF=(E),DSORG=PS,DDNAME=TP4                            00246900
TAPEDCB5 DCB   MACRF=(E),DSORG=PS,DDNAME=TP5                            00247000
TAPEDCB6 DCB   MACRF=(E),DSORG=PS,DDNAME=TP6                            00247100
TAPEDCB7 DCB   MACRF=(E),DSORG=PS,DDNAME=TP7                            00247200
TAPEDCB8 DCB   MACRF=(E),DSORG=PS,DDNAME=TP8                            00247300
TAPEDCB9 DCB   MACRF=(E),DSORG=PS,DDNAME=TP9                            00247400
SIMCOR   DSECT                                                          00308800
         DS    CL16020                                                  00308900
       END     BEGIN                                                    00309000
/*
//GO.WRITE DD SYSOUT=*
//GO.CARDOUT DD DUMMY
//GO.CARDIN  DD *,DCB=BLKSIZE=80
----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
,008015,022029,033033N      1001                 HELLO WORLD PROGRAM   HELLO 005
L039406,400404,405406N      1001/322/22                                HELLO 010
L046420,407414,415419,4204201001M5762532/25322                         HELLO 015
L044432,421428,429429N      1001M5882802/280                           HELLO 020
L047447,433440,441445,4464471001M6002802/2802.                         HELLO 025
M058576,550550N      N      1001H E L L O    W O R L D                 HELLO 030
M044588,577577N      N      1001RAFA PEREIRA                           HELLO 035
M044600,589589N      N      B400AUGUST, 2007                           HELLO 040
/*
//