--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions; use Exceptions;

package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);
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
                     Swallow_Message : Messages_Client := Message; pragma Unreferenced (Swallow_Message);
                  begin
                     null;
                  end;
               end Send_Message;

            or
               accept Receive_Message (Message : out Messages_Mailbox) do
                  declare
                     Made_Up_Mailbox_Message : constant Messages_Mailbox :=
                       (Sender      => Task_Id,
                        The_Message => Message_Strings.To_Bounded_String ("I just see things"),
                        Hop_Counter => 0);
                  begin
                     Message := Made_Up_Mailbox_Message;
                  end;
               end Receive_Message;

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
