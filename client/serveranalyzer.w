&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: serveranalyzer.w

  Description: Call into an appserver and see what's running.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: S.E. Southwell - Progress

  Created: 

  Notes:
      
      Prod connection
      -URL http://tdc3bapdapsrv01:8810/oms/apsv -sessionModel session-free
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
DEFINE VARIABLE servhdl AS HANDLE NO-UNDO.
DEFINE VARIABLE ret AS LOGICAL NO-UNDO.
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE myOutput AS LONGCHAR NO-UNDO.


/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiTree fiObject tglDelOrphans tglDelSupers ~
connectinfo btnGetStatus fiAgent fiSessionID edtResults 
&Scoped-Define DISPLAYED-OBJECTS fiTree fiObject tglDelOrphans tglDelSupers ~
connectinfo fiAgent fiSessionID edtResults 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgentSession C-Win 
FUNCTION getAgentSession RETURNS HANDLE
  (INPUT myAgentID AS INTEGER,
  INPUT mySessionID AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGetStatus 
     LABEL "Get Status" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE edtResults AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 198 BY 34.76
     FONT 0 NO-UNDO.

DEFINE VARIABLE connectinfo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 177 BY 1.19 NO-UNDO.

DEFINE VARIABLE fiAgent AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "AgentID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .95 TOOLTIP "Leave 0 to get whatever is available" NO-UNDO.

DEFINE VARIABLE fiObject AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Delete this object" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 TOOLTIP "Enter an ID of a procedure to delete" NO-UNDO.

DEFINE VARIABLE fiSessionID AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "SessionID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Leave 0 to get first available session" NO-UNDO.

DEFINE VARIABLE fiTree AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Show tree for" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE tglDelOrphans AS LOGICAL INITIAL no 
     LABEL "Delete orphan persistent procs" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81 NO-UNDO.

DEFINE VARIABLE tglDelSupers AS LOGICAL INITIAL no 
     LABEL "Delete redundant session supers" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiTree AT ROW 1.71 COL 65.2 COLON-ALIGNED WIDGET-ID 16
     fiObject AT ROW 1.71 COL 118.8 RIGHT-ALIGNED WIDGET-ID 14
     tglDelOrphans AT ROW 1.71 COL 122.4 WIDGET-ID 12
     tglDelSupers AT ROW 1.71 COL 162 WIDGET-ID 10
     connectinfo AT ROW 2.91 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     btnGetStatus AT ROW 2.91 COL 183 WIDGET-ID 2
     fiAgent AT ROW 4.33 COL 136 COLON-ALIGNED WIDGET-ID 18
     fiSessionID AT ROW 4.33 COL 174 COLON-ALIGNED WIDGET-ID 20
     edtResults AT ROW 5.76 COL 3 NO-LABEL WIDGET-ID 4
     "Appserver Connection Info" VIEW-AS TEXT
          SIZE 35 BY .95 AT ROW 1.71 COL 4 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 205 BY 40 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Appserver Analyzer"
         HEIGHT             = 40
         WIDTH              = 205
         MAX-HEIGHT         = 40
         MAX-WIDTH          = 205
         VIRTUAL-HEIGHT     = 40
         VIRTUAL-WIDTH      = 205
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 0
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiObject IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Appserver Analyzer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Appserver Analyzer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGetStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGetStatus C-Win
ON CHOOSE OF btnGetStatus IN FRAME DEFAULT-FRAME /* Get Status */
DO:
    servhdl = getAgentSession(INTEGER(fiAgent:SCREEN-VALUE),INTEGER(fiSessionid:SCREEN-VALUE)).
    IF VALID-HANDLE(servhdl) THEN ret = true.
/*    CREATE SERVER servhdl.                                 */
/*    ASSIGN ret = servhdl:CONNECT(connectinfo:SCREEN-VALUE).*/
    IF ret THEN DO:
        edtResults:SCREEN-VALUE = "".
        RUN proctree.p ON SERVER servhdl (INPUT fiTree:SCREEN-VALUE, INPUT fiObject:SCREEN-VALUE, INPUT tglDelSupers:SCREEN-VALUE, INPUT tglDelOrphans:SCREEN-VALUE, OUTPUT myOutput).  
        servhdl:DISCONNECT().
        DELETE OBJECT servhdl NO-ERROR.
        ASSIGN
            edtResults:SCREEN-VALUE = myOutput
            fiObject:SCREEN-VALUE = "0"
            tglDelSupers:CHECKED = FALSE
            tglDelOrphans:CHECKED = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  ConnectInfo:SCREEN-VALUE = "-URL http://10.250.4.172:8820/FakeAuth/apsv".
  
  
  
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiTree fiObject tglDelOrphans tglDelSupers connectinfo fiAgent 
          fiSessionID edtResults 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiTree fiObject tglDelOrphans tglDelSupers connectinfo btnGetStatus 
         fiAgent fiSessionID edtResults 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgentSession C-Win 
FUNCTION getAgentSession RETURNS HANDLE
  (INPUT myAgentID AS INTEGER,
  INPUT mySessionID AS INTEGER): 
/*------------------------------------------------------------------------------
 Purpose:  Use trial and error to get a handle to the appserver connection 
           that gets us the agentID and sessionID that we're looking for.
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE result AS HANDLE EXTENT 16 NO-UNDO.
        DEFINE VARIABLE pAgentID AS INTEGER NO-UNDO.
        DEFINE VARIABLE pSessionID AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        DO WHILE (i = 0 
            OR NOT VALID-HANDLE(result[i])
            OR (myAgentID > 0 AND pAgentID NE myAgentID)
            OR (mySessionID > 0 AND pSessionID NE mySessionID)):
            i = i + 1.
            if i > 16 THEN RETURN ?.
            CREATE SERVER result[i].
            result[i]:CONNECT(ConnectInfo:SCREEN-VALUE IN FRAME DEFAULT-FRAME).
            RUN whoami.p ON result[i] (OUTPUT pAgentID, OUTPUT pSessionID).
            /* Momentarily tie up this session so our next try will get another one */
            IF (myAgentID > 0 AND pAgentID NE myAgentID)
                OR (mySessionID > 0 AND pSessionID NE mySessionID)
                THEN RUN delay.p ON result[i] ASYNCHRONOUS.
        END.    
        RETURN result[i].

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

