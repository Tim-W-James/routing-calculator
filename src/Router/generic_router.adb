--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions;  use Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;
      Routers_Count     : constant Router_Range := Router_Range'Last;
      Current_Routing_Table : Map := Empty_Map;
      Current_Routing_Table_Size : Count_Type := 0;

      Default_Mailbox_Message : constant Messages_Mailbox :=
        (Sender      => Task_Id,
         The_Message => Message_Strings.To_Bounded_String ("Default"),
         Hop_Counter => 0);
      Current_Local_Message : Messages_Mailbox := Default_Mailbox_Message;

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

         procedure Generate_Routing_Table is
            Neighbors : Set := Empty_Set;
         begin
            for I in Port_List'Range loop
               Neighbors.Insert (Port_List (I).Id);
            end loop;
            Current_Routing_Table.Include (Task_Id, Neighbors);
            Current_Routing_Table_Size := 1;
         end Generate_Routing_Table;

         procedure Update_Routing_Table (M : Map; Was_Changed : out Boolean) is
            Start_Size : constant Count_Type := Current_Routing_Table.Length;
            End_Size   : Count_Type;
         begin
            for I in M.Iterate loop
               Current_Routing_Table.Include (Key (I), Element (I));
            end loop;

            End_Size    := Current_Routing_Table.Length;
            Was_Changed := not (Start_Size = End_Size);
            Current_Routing_Table_Size := End_Size;
         end Update_Routing_Table;

         function Determine_Path (Destination : Router_Range) return Vector is
            Shortest_Path : Vector := Empty_Vector;

            procedure Recursive_Path (Destination     : Router_Range;
                                      Current_Path    : Vector;
                                      Current_Node    : Router_Range) is
               Neighbors : constant Set := Current_Routing_Table (Current_Node);
            begin
               if not (Shortest_Path = Empty_Vector) and then Current_Path.Length + 1 > Shortest_Path.Length then
                  return;
               end if;
               for Node of Neighbors loop
                  if Node = Destination then -- found target
                     if Shortest_Path = Empty_Vector or else Current_Path.Length + 1 < Shortest_Path.Length then
                        declare
                           New_Path : Vector := Current_Path;
                        begin
                           New_Path.Append (Node);
                           Shortest_Path := New_Path;
                           return;
                        end;
                     end if;
                  elsif not Current_Path.Contains (Node) then -- avoid backtracking
                     declare
                        New_Path : Vector := Current_Path;
                     begin
                        New_Path.Append (Node);
                        Recursive_Path (Destination, New_Path, Node);
                     end;
                  end if;
               end loop;
            end Recursive_Path;
         begin
            Recursive_Path (Destination, Empty_Vector, Task_Id);
            return Shortest_Path;
         end Determine_Path;

         -- solution for a bi-directional ring topology
         --  function Determine_Next_Node_Ring (Destination : Router_Range) return Router_Ports is
         --  begin
         --     if (Routers_Count + (Destination - Task_Id)) mod Routers_Count <
         --       (Routers_Count + (Task_Id - Destination)) mod Routers_Count
         --     then
         --        if (Task_Id + 1) mod Routers_Count = Port_List (Port_List'First).Id then
         --           return Port_List (Port_List'First);
         --        else
         --           return Port_List (Port_List'Last);
         --        end if;
         --     else
         --        if (Task_Id + 1) mod Routers_Count = Port_List (Port_List'First).Id then
         --           return Port_List (Port_List'Last);
         --        else
         --           return Port_List (Port_List'First);
         --        end if;
         --     end if;
         --  end Determine_Next_Node_Ring;

         --  task type Forward_Message_Task is
         --     entry Send_Content (Frame : Data_Link_Frame);
         --  end Forward_Message_Task;
         --  type Forward_Message_Task_Pointer is access Forward_Message_Task;
         --  task body Forward_Message_Task is
         --     New_Frame : Data_Link_Frame;
         --     Next_Node : Router_Ports;
         --  begin
         --     -- workaround for error
         --     accept Send_Content (Frame : Data_Link_Frame) do
         --        New_Frame := Frame;
         --     end Send_Content;
         --
         --     New_Frame.Payload.Hop_Counter := New_Frame.Payload.Hop_Counter + 1;
         --
         --     -- ensure hops does not exceed possible max: prevent infinite loops
         --     if Routers_Count > Router_Range (New_Frame.Payload.Hop_Counter) then
         --
         --        --  if Task_Id = 1 then
         --        --     Put_Line ("Path to " & Router_Range'Image (New_Frame.Destination) & " : ");
         --        --     for Node of New_Frame.Path loop
         --        --        Put_Line ("-" & Router_Range'Image (Node));
         --        --     end loop;
         --        --  end if;
         --
         --        for I in Port_List'Range loop
         --           if Port_List (I).Id = New_Frame.Path.First_Element then
         --              Next_Node := Port_List (I);
         --           end if;
         --        end loop;
         --
         --        New_Frame.Path.Delete_First;
         --
         --        Debug_Print (" forwarding message to " &
         --                    Router_Range'Image (Next_Node.Id) & " (destination: " &
         --                    Router_Range'Image (New_Frame.Destination) & " )"
         --                 );
         --
         --        Next_Node.Link.Forward_Message (New_Frame);
         --     end if;
         --  end Forward_Message_Task;

         procedure Forward_Next_Message (Frame : in Data_Link_Frame) is
            --     Forward_Message_Task_Instance : constant Forward_Message_Task_Pointer := new Forward_Message_Task;
            --
            --  begin
            --     Forward_Message_Task_Instance.Send_Content (Frame);
            New_Frame : Data_Link_Frame := Frame;
            Next_Node : Router_Ports;
         begin
            New_Frame.Payload.Hop_Counter := New_Frame.Payload.Hop_Counter + 1;

            -- ensure hops does not exceed possible max: prevent infinite loops
            if Routers_Count > Router_Range (New_Frame.Payload.Hop_Counter) then

               --  if Task_Id = 1 then
               --     Put_Line ("Path to " & Router_Range'Image (New_Frame.Destination) & " : ");
               --     for Node of New_Frame.Path loop
               --        Put_Line ("-" & Router_Range'Image (Node));
               --     end loop;
               --  end if;

               for I in Port_List'Range loop
                  if Port_List (I).Id = New_Frame.Path.First_Element then
                     Next_Node := Port_List (I);
                  end if;
               end loop;

               New_Frame.Path.Delete_First;

               Debug_Print (" forwarding message to " &
                              Router_Range'Image (Next_Node.Id) & " (destination: " &
                              Router_Range'Image (New_Frame.Destination) & " )"
                           );

               Next_Node.Link.Forward_Message (New_Frame);
            end if;
         end Forward_Next_Message;

         task type Routing_Table_Task is
            entry Send_Content (Node : Router_Ports; Table : Map);
         end Routing_Table_Task;
         type Routing_Table_Task_Pointer is access Routing_Table_Task;
         task body Routing_Table_Task is
            Next_Node     : Router_Ports;
            Routing_Table : Map;
         begin
            -- workaround for error
            accept Send_Content (Node : in Router_Ports; Table : in Map) do
               Next_Node     := Node;
               Routing_Table := Table;
            end Send_Content;

            Next_Node.Link.Routing_Table_Message (Routing_Table);
         end Routing_Table_Task;

         -- creates a new instance of a task to ensure the main task is non-blocking
         procedure Propagate_Routing_Table (Node : in Router_Ports) is
            Routing_Table_Task_Instance : constant Routing_Table_Task_Pointer := new Routing_Table_Task;
         begin
            Routing_Table_Task_Instance.Send_Content (Node, Current_Routing_Table);
         end Propagate_Routing_Table;

      begin

         --  Replace the following dummy code with the code of your router.
         --  None of the following code structures make necessarily any sense,
         --  so feel free to delete in full and rewrite from scratch.
         --  You will still need to handle all defined entries and will need to
         --  use the exisitng ports in your own code.

         -- initial
         Generate_Routing_Table;

         if Task_Id = 3 then
            for I in Port_List'Range loop
               Propagate_Routing_Table (Port_List (I));
            end loop;
         end if;

         while Current_Routing_Table_Size < Count_Type (Routers_Count) loop
            accept Routing_Table_Message (Table : in Map) do
               declare
                  Was_Map_Changed : Boolean;
               begin
                  Update_Routing_Table (Table, Was_Map_Changed);
                  if Was_Map_Changed then
                     for I in Port_List'Range loop
                        Propagate_Routing_Table (Port_List (I));
                        --  if not Port_List (I).Id = Sender then
                        --  end if;
                     end loop;
                  end if;
               end;
            end Routing_Table_Message;
         end loop;

         --  if Task_Id = 1 then
         --     for I in Current_Routing_Table.Iterate loop
         --        Put_Line (Router_Range'Image (Key (I)) & ":");
         --        for S of Element (I) loop
         --           Put_Line ("-" & Router_Range'Image (S));
         --        end loop;
         --     end loop;
         --  end if;

         loop
            select

               accept Send_Message (Message : in Messages_Client) do
                  declare
                     Message_Content : constant Messages_Mailbox :=
                       (Sender      => Task_Id,
                        The_Message => Message.The_Message,
                        Hop_Counter => 0
                       );
                     Frame           : constant Data_Link_Frame  :=
                       (Destination   => Message.Destination,
                        Path          => Determine_Path (Message.Destination),
                        Payload       => Message_Content
                       );
                  begin
                     --  if Task_Id = 1 then
                     --     Put_Line ("Path to " & Router_Range'Image (Frame.Destination) & " : ");
                     --     for Node of Frame.Path loop
                     --        Put_Line ("-" & Router_Range'Image (Node));
                     --     end loop;
                     --  end if;

                     Forward_Next_Message (Frame);
                  end;
               end Send_Message;

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

               accept Receive_Message (Message : out Messages_Mailbox) do
                  Debug_Print (" received message " & Message_Strings.To_String (Current_Local_Message.The_Message));
                  Message := Current_Local_Message;
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
