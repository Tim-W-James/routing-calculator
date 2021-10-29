--
--  Framework: Uwe R. Zimmer, Australia, 2015
--

with Generic_Message_Structures;
with Generic_Router_Links;
with Id_Dispenser;

generic

   with package Message_Structures is new Generic_Message_Structures (<>);

package Generic_Router is
   use Message_Structures;
   use Routers_Configuration;

   package Router_Id_Generator is new Id_Dispenser (Element => Router_Range);
   use Router_Id_Generator;

   type Router_Task;
   type Router_Task_P is access all Router_Task;

   package Router_Link is new Generic_Router_Links (Router_Range, Router_Task_P, null);
   use Router_Link;

   use Id_Set;
   use Routing_Table;
   use Path_Vector;

   task type Router_Task (Task_Id  : Router_Range := Draw_Id) is

      entry Configure (Links : Ids_To_Links);

      entry Send_Message    (Message :     Messages_Client);
      entry Receive_Message (Message : out Messages_Mailbox);

      entry Shutdown;

      -- messages for sending messages across the network
      entry Forward_Message       (Frame : Data_Link_Frame);
      -- messages for sharing a routing table
      entry Routing_Table_Message (Table : Map);

   end Router_Task;

end Generic_Router;
