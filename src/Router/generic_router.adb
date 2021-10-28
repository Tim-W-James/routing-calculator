--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions;  use Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;

      Is_Debug_Print : constant Boolean := False;

      procedure Debug_Print (To_Print : String) is
      begin
         if Is_Debug_Print then
            Put_Line (Router_Range'Image (Task_Id) & ": " & To_Print);
         end if;
      end;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);

         Default_Mailbox_Message : constant Messages_Mailbox :=
           (Sender      => Task_Id,
            The_Message => Message_Strings.To_Bounded_String ("Error"),
            Hop_Counter => 0);
         Current_Local_Message : Messages_Mailbox := Default_Mailbox_Message;

         function Determine_Next_Node (Destination : Router_Range) return Router_Ports is
            Routers_Count : constant Router_Range := 20;
         begin
            -- solution for a ring topology only
            if (Routers_Count + (Destination - Task_Id)) mod Routers_Count <
              (Routers_Count + (Task_Id - Destination)) mod Routers_Count
            then
               if (Task_Id + 1) mod Routers_Count = Port_List (Port_List'First).Id then
                  return Port_List (Port_List'First);
               else
                  return Port_List (Port_List'Last);
               end if;
            else
               if (Task_Id + 1) mod Routers_Count = Port_List (Port_List'First).Id then
                  return Port_List (Port_List'Last);
               else
                  return Port_List (Port_List'First);
               end if;
            end if;
         end Determine_Next_Node;

         procedure Forward_Next_Message (Frame : in Data_Link_Frame) is
            Next_Node : constant Router_Ports    := Determine_Next_Node (Frame.Destination);
            New_Frame :          Data_Link_Frame := Frame;
         begin
            Debug_Print (" forwarding message to " &
                           Router_Range'Image (Next_Node.Id) & " (destination: " &
                           Router_Range'Image (Frame.Destination) & " )"
                        );

            New_Frame.Payload.Hop_Counter := New_Frame.Payload.Hop_Counter + 1;
            Next_Node.Link.Forward_Message (New_Frame);
         end Forward_Next_Message;
      begin

         --  Replace the following dummy code with the code of your router.
         --  None of the following code structures make necessarily any sense,
         --  so feel free to delete in full and rewrite from scratch.
         --  You will still need to handle all defined entries and will need to
         --  use the exisitng ports in your own code.

         loop
            select

               accept Send_Message (Message : in Messages_Client) do
                  declare
                     Message_Content : constant Messages_Mailbox := (Sender      => Task_Id,
                                                                     The_Message => Message.The_Message,
                                                                     Hop_Counter => 0
                                                                    );
                     Frame           : constant Data_Link_Frame  := (Destination => Message.Destination,
                                                                     Payload   => Message_Content
                                                                    );
                  begin
                     Forward_Next_Message (Frame);
                  end;
               end Send_Message;

            or
               accept Receive_Message (Message : out Messages_Mailbox) do
                  declare
                  begin
                     Debug_Print (" received message " & Message_Strings.To_String (Current_Local_Message.The_Message));
                     Message := Current_Local_Message;
                  end;
               end Receive_Message;
            or
               accept Forward_Message (Frame : in Data_Link_Frame) do
                  -- receive message
                  if Frame.Destination = Task_Id then
                     Debug_Print (" got message from " & Router_Range'Image (Frame.Payload.Sender));
                     Current_Local_Message := Frame.Payload;

                     -- forward message
                  else
                     Forward_Next_Message (Frame);
                  end if;

               end Forward_Message;

            or
               accept Shutdown;
               exit;
            end select;
         end loop;

         for Port of Port_List loop
            null;
         end loop;

      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
