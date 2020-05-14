Server Analyzer
by S.E. Southwell - Progress Software
This is not an officially supported tool of Progress Software.  It is meant to supplement, not replace the tools in OEManager, oejmx, and the rest interface.

It was designed to help you view and manage persistent procedures that may be supered or orphaned on your server, using up memory.

To install:
Take the contents of the server directory, and place all 3 files somewhere in your propath on your PASOE ABLApp.  
You can put them in WEB-INF/openedge or any other place that makes sense.

Run the .w from a Progress session such as PDSOE or AppBuilder.

If you leave agent and sessionID blank, it will grab the first available session, which is probably the one serving most of your calls.  Otherwise, if you're looking at an agent and session in OE Manager, and you see one that you want to check out, you can enter its agentID and sessionID in the GUI.

Agent and SessionID are done using trial and error, hitting the server several times until it lands on the agent and session you want.  It can tie up several sessions simultaneously while it searches for the one you ask for, so doing this during times when your server is totally slammed with requests, and the sessions are all busy is a bad idea.

Controls:
Appserver Connection Info:  Put an appserver connection string in here.  For example:
    -URL http://myhostname:8830/myapp/apsv -sessionModel session-free
    
Show tree for:
Given a procedure ID, it shows that procedure's tree.  0 means don't show.

Delete this object:
Given a procedure ID, it deletes the procedure handle and all of its children, freeing up the memory.

Delete orphan persistent procs:
This cleans up persistent procedures where the original caller can no longer be found.  For example, Procedure A loads up Procedure B persistently, then Procedure A ends, but B is still in memory.  B would be listed with "Orphan" in the second column.  This option deletes all such Orphans, as long as they are not supered to the session itself.

Delete redundant session supers:
If the same procedure is loaded multiple times, and is supered to the session, it removes all duplicate session supers, leaving only the first one super to the session.

AgentID
Try to only show info for the given agent ID

SessionID
Try to only show info for the given session on the given agent.

Get Status button makes the appserver call given the connection info you provided.

Fields:
Appservers shows a comma-delimited list of valid appserver handles in memory.

Procedures show a CSV of all current valid procedures in memory.
ID is the numeric ID of the handle
Parent is the numeric ID of the procedure that originally loaded this procedure.  If no valid instantiating procedure can be found, it displays "Orphan".
Type is the type of handle.  It will most likely always be "PROCEDURE"
Name is the filename of the procedure as it was called.  It won't necessarily be a fully-qualified path.
LoadType will be one of: PERSISTENT, SINGLE-RUN, SINGLETON
Is Super will either be blank if the procedure is not supered, or will say something like SUPER IN 9999, which means it is super to the procedure which has that ID.  It may also say SESSION SUPER"

Objects:
This section, if you have any class instances active, will show the class name and how many instances of that class are active.
