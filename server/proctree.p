
/*------------------------------------------------------------------------
    File        : proctree.p
    Purpose     : 

    Syntax      :

    Description : Walk the stack and figure out what instantiated what.

    Author(s)   : S.E. Southwell
    Created     : Tue May 05 23:51:33 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.Object FROM PROPATH.

DEFINE INPUT PARAMETER treeObject       AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER deleteObject     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER deleteDupSession AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER deleteOrphanProc AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER myOutput AS LONGCHAR NO-UNDO.

DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
DEFINE VARIABLE hInst AS HANDLE NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE hObj AS Object NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEFINE VARIABLE cTempFile AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttServer NO-UNDO
    FIELD type AS CHARACTER
    FIELD subtype AS CHARACTER
    FIELD name AS CHARACTER
    FIELD isconnected AS LOGICAL.

DEFINE TEMP-TABLE ttProc NO-UNDO
    FIELD hdl AS HANDLE
    FIELD type AS CHARACTER
    FIELD ispersist AS CHARACTER
    FIELD issuper AS CHARACTER
    FIELD procname AS CHARACTER
    FIELD objid AS CHARACTER
    FIELD instobjid AS CHARACTER
    FIELD insthdl AS HANDLE
    FIELD supers AS CHARACTER
    FIELD apsv AS CHARACTER
    FIELD runstatus AS CHARACTER
    .
DEFINE BUFFER btt FOR ttProc.    

DEFINE TEMP-TABLE ttObj NO-UNDO
    FIELD classname AS CHARACTER
    FIELD objcount AS INTEGER.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION DeleteProcTree RETURNS LOGICAL 
    (input myobjid as character) FORWARD.

FUNCTION GetProcTree RETURNS CHARACTER 
    (INPUT myObjId AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */


FUNCTION DeleteProcTree RETURNS LOGICAL 
    (input myobjid as character):
/*------------------------------------------------------------------------------
 Purpose: Delete the given objectid and all of its children and their children.
 Notes: This is recursive.
------------------------------------------------------------------------------*/    
    DEFINE BUFFER ttProc FOR ttProc.
    FOR EACH ttProc
        WHERE ttProc.instobjid = myobjid:
        DeleteProcTree(ttProc.objid).
    END.
    FOR FIRST ttProc
        WHERE ttProc.objid = myobjid:
        DELETE PROCEDURE ttProc.hdl NO-ERROR.
        ttProc.runstatus = "Deleted".
    END.
    RETURN TRUE.    
END FUNCTION.

FUNCTION GetProcTree RETURNS CHARACTER 
    (INPUT myObjId AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE result AS CHARACTER NO-UNDO.
    DEFINE BUFFER ttProc FOR ttProc.
    FOR EACH ttProc
        WHERE ttProc.instobjid = myobjid:
        result = result + GetProcTree(ttProc.objid).
    END.
    IF result > "" THEN result = "~t" + REPLACE(result,"~n","~n~t").
    
    FOR FIRST ttProc WHERE ttProc.objid = myObjId:
        result = ttProc.objid + " " + ttProc.procname + "~n" + result.
    END.
    RETURN RESULT.    
END FUNCTION.

cTempFile = SESSION:TEMP-DIRECTORY + "/proctree.txt".

hServer = SESSION:FIRST-SERVER.
DO WHILE VALID-HANDLE(hServer):
    CREATE ttServer.
    ASSIGN
        ttServer.name = hServer:NAME
        ttServer.type = hServer:TYPE
        ttServer.subtype = hServer:SUBTYPE
        ttServer.isconnected = hServer:CONNECTED ().
    hServer = hServer:NEXT-SIBLING.
END.

hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE (hProc):
    CREATE ttProc.
    ASSIGN
        ttProc.hdl = hProc
        ttProc.procname = hProc:FILE-NAME
        ttProc.type = hProc:TYPE
        ttProc.objid = STRING(hProc)
        ttProc.ispersist = "PERSISTENT" WHEN hProc:PERSISTENT
        ttProc.ispersist = "SINGLE-RUN" WHEN hProc:SINGLE-RUN
        ttProc.ispersist = "SINGLETON" WHEN hProc:SINGLETON
        ttProc.supers = hProc:SUPER-PROCEDURES
        ttProc.runstatus = "Running"
        hInst = hProc:INSTANTIATING-PROCEDURE.
    
    IF VALID-HANDLE (hInst) 
        AND NOT hInst eq THIS-PROCEDURE 
        THEN DO:
        ASSIGN
            ttProc.instobjid = STRING(hInst)
            ttProc.insthdl = hInst. 
    END.   
    ELSE ttProc.instobjid ="Orphan".    
    hProc = hProc:NEXT-SIBLING.
END.

/* Figure out if we have some callers that aren't persistent, but somehow still around */
FOR EACH ttProc WHERE instobjid NE "Orphan":
    IF VALID-HANDLE(ttProc.insthdl) 
        AND NOT CAN-FIND (FIRST btt WHERE btt.objid = ttProc.instobjid) THEN DO:
        CREATE btt.
        ASSIGN
            btt.hdl = ttProc.insthdl
            btt.procname = ttProc.insthdl:FILE-NAME
            btt.type = ttProc.insthdl:TYPE
            btt.objid = STRING(ttProc.instobjid)
            btt.ispersist = "PERSISTENT" WHEN ttProc.insthdl:PERSISTENT
            btt.ispersist = "SINGLE-RUN" WHEN ttProc.insthdl:SINGLE-RUN
            btt.ispersist = "SINGLETON" WHEN ttProc.insthdl:SINGLETON
            btt.supers = ttProc.insthdl:SUPER-PROCEDURES
            btt.runstatus = "Running"
            hInst = btt.hdl:INSTANTIATING-PROCEDURE.
            
        IF VALID-HANDLE (hInst) THEN 
            ASSIGN
                btt.instobjid = STRING(hInst)
                btt.insthdl = hInst.       
        ELSE btt.instobjid = "Orphan". 
    END.
END.

/*/* What's in private-data? */                                        */
/*FOR EACH ttProc:                                                     */
/*    MESSAGE ttProc.objid ttProc.hdl:PRIVATE-DATA ttProc.hdl:ADM-DATA.*/
/*END.                                                                 */

CREATE ttProc.
ASSIGN
    ttProc.hdl = SESSION:HANDLE
    ttProc.procname = "SESSION"
    ttProc.type = "SESSION"
    ttProc.objid = STRING(SESSION:HANDLE)
    ttProc.supers = SESSION:SUPER-PROCEDURES
    ttProc.runstatus = "Running".

/* Now go back and mark those that are supers */
FOR EACH ttProc WHERE ttProc.supers > "":
    DO iCount = 1 TO NUM-ENTRIES(ttProc.supers):
        FIND FIRST btt WHERE btt.objid = ENTRY(iCount,ttProc.supers)
            NO-ERROR.
        IF AVAILABLE btt THEN DO:
            IF btt.issuper > "" THEN btt.issuper = btt.issuper + ",".
            IF ttProc.type = "SESSION" THEN ASSIGN btt.issuper = btt.issuper + "SESSION SUPER".
            ELSE ASSIGN btt.issuper = btt.issuper + "SUPER IN " + ttProc.objid.
        END.
    END.
END.

/* Who is on an appserver? */
FOR EACH ttproc WHERE ttproc.type = "PROCEDURE":
    hProc = ttProc.hdl:SERVER.
    IF VALID-HANDLE(hProc) THEN
        ttProc.apsv = hProc:TYPE + " " + hProc:NAME.
END.

/* Now delete duplicate session-supered procs */
IF deleteDupSession THEN DO:
    FOR EACH ttProc WHERE ttProc.issuper = "SESSION SUPER"
        AND ttProc.runstatus = "Running":
        FOR EACH btt
            WHERE btt.objid ne ttProc.objid
            AND btt.procname = ttProc.Procname
            AND btt.runstatus = "Running"
            AND btt.issuper = "SESSION SUPER"
            :
            ASSIGN btt.runstatus = "Deleted".
            DELETE PROCEDURE btt.hdl NO-ERROR.   
        END.
    END.
END.
IF deleteOrphanProc THEN DO:
    FOR EACH ttProc 
        WHERE ttProc.runStatus = "Running"
        AND ttProc.instobjid = "Orphan"
        AND ttProc.isSuper = "":
        ASSIGN ttProc.runstatus = "Deleted".
        DELETE PROCEDURE ttProc.hdl NO-ERROR.    
    END.
END.

IF deleteObject > 0 THEN DO:
    DeleteProcTree(STRING(deleteObject)).
/*    FOR FIRST ttProc                              */
/*        WHERE ttProc.objid = STRING(deleteObject):*/
/*/*        DELETE PROCEDURE ttProc.hdl NO-ERROR.*/ */
/*/*        ASSIGN ttProc.runstatus = "Deleted". */ */
/*    END.                                          */
END.

/* Throw in the objects */
hObj = SESSION:FIRST-OBJECT.
DO WHILE VALID-OBJECT(hObj):
    FIND FIRST ttObj 
        WHERE ttObj.classname = hObj:GetClass():TypeName
        NO-ERROR.
    IF NOT AVAILABLE ttObj THEN DO:
        CREATE ttObj.
        ASSIGN
            ttObj.classname = hObj:GetClass():TypeName.
    END.
    ttObj.objcount = ttObj.objcount + 1.
    hObj = hObj:Next-Sibling.
END.

OUTPUT TO VALUE(cTempFile).
IF SESSION:CLIENT-TYPE = "MULTI-SESSION-AGENT" THEN DO:
    PUT UNFORMATTED 
        "Agent ID: " SESSION:CURRENT-REQUEST-INFO:AgentID SKIP
        "Session ID: " SESSION:CURRENT-REQUEST-INFO:SessionID SKIP
        "RequestID: " SESSION:CURRENT-REQUEST-INFO:RequestId SKIP(2).
END.

PUT UNFORMATTED "Appservers:" skip
    "type,subtype,name,isConnected" skip.
FOR EACH ttServer:
    EXPORT DELIMITER "," ttServer.
END.

PUT UNFORMATTED skip(2) "Procedures:" skip '"ID","Parent","Type","Name","LoadType","Is Super?","My supers","apsv","Status"' SKIP.
FOR EACH ttProc:
    EXPORT DELIMITER "," 
        ttProc.objId 
        ttProc.instobjid 
        ttProc.type 
        ttProc.procname 
        ttProc.ispersist 
        ttProc.isSuper 
        ttProc.supers
        ttProc.apsv
        ttProc.runstatus.
END.
IF treeObject > 0 THEN DO:
    PUT UNFORMATTED SKIP(2)
        "Requested tree for " string(treeObject)  ":" SKIP
        GetProcTree(STRING(treeObject)).
END.

PUT UNFORMATTED SKIP(2) "Objects:" skip '"Classname","Count"' SKIP.
FOR EACH ttObj:
    EXPORT DELIMITER "," ttObj.
END.
OUTPUT CLOSE.

COPY-LOB FROM FILE cTempfile TO myOutput.

FINALLY:
    /* Don't delete objects, since it could kill the underlying reference.
       Just let them go out of scope. */
/*    DELETE OBJECT hProc NO-ERROR.  */
/*    DELETE OBJECT hServer NO-ERROR.*/
/*    DELETE OBJECT hObj NO-ERROR.   */
END FINALLY.