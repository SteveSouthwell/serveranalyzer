
/*------------------------------------------------------------------------
    File        : whoami.p
    Purpose     : 

    Syntax      :

    Description : Return the agent and session id

    Author(s)   : S.E. Southwell
    Created     : Thu May 07 15:01:20 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
DEFINE OUTPUT PARAMETER pAgentID AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER pSessionID AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN
    pAgentID = SESSION:CURRENT-REQUEST-INFO:AgentID
    pSessionID = SESSION:CURRENT-REQUEST-INFO:SessionID.
