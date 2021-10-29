--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

-- === NOTES FOR MARKERS ===
-- My program uses a recursive brute force method to calculate paths of minimal hops.
-- As such, running the full 100 iterations of the test will take some time
-- (around 3 mins 30 seconds on my machine). Consider running with less iterations.
-- All messages should arrive at their destinations with minimal hops,
-- though I recognise that there is room for time complexity optimization.

-- sample command: ./test_routers -t Ring -r 10 -s 20

with Exceptions;     use Exceptions;
with Ada.Containers; use Ada.Containers;

package body Generic_Router is

   task body Router_Task is

      Connected_Routers          : Ids_To_Links;
      Routers_Count              : constant Router_Range := Router_Range'Last;
      -- the routing table stores information about the network topology,
      -- used to find paths across the network and established during setup.
      -- Format -
      --  Keys   : node/router Ids
      --  Values : immediate neighbours of that node
      Current_Routing_Table      : Map                   := Empty_Map;
      Current_Routing_Table_Size : Count_Type            := 0;

      Default_Mailbox_Message : constant Messages_Mailbox :=
        (Sender      => Task_Id,
         The_Message => Message_Strings.To_Bounded_String ("Default"),
         Hop_Counter => 0);
      Current_Local_Message : Messages_Mailbox := Default_Mailbox_Message;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);

         -- generate the initial routing table from neighbours
         procedure Generate_Routing_Table is
            Neighbours : Set := Empty_Set;
         begin
            for I in Port_List'Range loop
               Neighbours.Insert (Port_List (I).Id);
            end loop;
            Current_Routing_Table.Include (Task_Id, Neighbours);
            Current_Routing_Table_Size := 1;
         end Generate_Routing_Table;

         -- update the local routing table with new information.
         -- use Was_Changed to determine if changes were made
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

         -- calculate optimal path in terms of distance (hop count) using the complete routing table
         -- completed by the initial sender task, path is then contained within message frame
         -- assumes that all nodes/routers are present on the routing table (i.e. there are no isolated nodes)
         function Determine_Path (Destination : Router_Range) return Vector is
            Shortest_Path : Vector := Empty_Vector;

            procedure Recursive_Path (Destination     : Router_Range;
                                      Current_Path    : Vector;
                                      Current_Node    : Router_Range) is
               Neighbours : constant Set := Current_Routing_Table (Current_Node);
            begin
               -- don't explore paths that exceed the current best distance
               if not (Shortest_Path = Empty_Vector) and then Current_Path.Length + 1 > Shortest_Path.Length then
                  return;
               end if;
               for Node of Neighbours loop
                  if Node = Destination then -- found target
                     -- update result if minimal
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
                     -- recursive call to attempt various paths
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

         -- unused solution for bi-directional ring topology.
         -- not compatable with other topologies but does not require routing table
         --
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

         -- forward a message towards its destination based on provided path
         procedure Forward_Next_Message (Frame : in Data_Link_Frame) is
            New_Frame : Data_Link_Frame := Frame;
            Next_Node : Router_Ports;
         begin
            New_Frame.Payload.Hop_Counter := New_Frame.Payload.Hop_Counter + 1;

            -- ensure hops does not exceed possible limit: prevent infinite cycles
            if Routers_Count > Router_Range (New_Frame.Payload.Hop_Counter) then

               -- get link for next node/router in the path.
               -- should be a neighbour else path is incorrect
               for I in Port_List'Range loop
                  if Port_List (I).Id = New_Frame.Path.First_Element then
                     Next_Node := Port_List (I);
                  end if;
               end loop;

               -- pop node from the path
               New_Frame.Path.Delete_First;

               -- blocking entry call on neighbour router
               Next_Node.Link.Forward_Message (New_Frame);
            end if;
         end Forward_Next_Message;

         -- propagate routing tables across network
         task type Routing_Table_Task is
            entry Send_Content (Node : Router_Ports; Table : Map);
         end Routing_Table_Task;
         type Routing_Table_Task_Pointer is access Routing_Table_Task;
         task body Routing_Table_Task is
            Next_Node     : Router_Ports;
            Routing_Table : Map;
         begin
            -- need to use an entry as a workaround for type error
            accept Send_Content (Node : in Router_Ports; Table : in Map) do
               Next_Node     := Node;
               Routing_Table := Table;
            end Send_Content;

            -- blocking entry call here, as this is a dynamic task the parent task is not blocked
            Next_Node.Link.Routing_Table_Message (Routing_Table);
         end Routing_Table_Task;

         procedure Propagate_Routing_Table (Node : in Router_Ports) is
            -- creates a new task instance to ensure the main router task is non-blocking
            Routing_Table_Task_Instance : constant Routing_Table_Task_Pointer := new Routing_Table_Task;
         begin
            Routing_Table_Task_Instance.Send_Content (Node, Current_Routing_Table);
         end Propagate_Routing_Table;

      begin

         -- === INITIAL SETUP ===

         -- add immediate neighbours to table
         Generate_Routing_Table;

         -- task 1 nominates itself to initiate routing table sharing.
         -- information propagates across the network and routers converge on a table.
         if Task_Id = 1 then
            for I in Port_List'Range loop
               Propagate_Routing_Table (Port_List (I));
            end loop;
         end if;

         while Current_Routing_Table_Size < Count_Type (Routers_Count) loop

            -- continue to receive new routing tables until this router is aware of all routers in the network.
            -- note: assumes there are no isolated nodes
            accept Routing_Table_Message (Table : in Map) do
               declare
                  Was_Map_Changed : Boolean;
               begin
                  Update_Routing_Table (Table, Was_Map_Changed);
                  -- propagate updated table to neighbours only if information has changed
                  if Was_Map_Changed then
                     for I in Port_List'Range loop
                        Propagate_Routing_Table (Port_List (I));
                     end loop;
                  end if;
               end;
            end Routing_Table_Message;
         end loop;

         -- === MAIN LOOP ===

         loop
            select

               accept Send_Message (Message : in Messages_Client) do
                  declare
                     -- create message frame and calculate path
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
                     Forward_Next_Message (Frame);
                  end;
               end Send_Message;

            or

               accept Forward_Message (Frame : in Data_Link_Frame) do
                  -- receive message
                  if Frame.Destination = Task_Id then
                     Current_Local_Message := Frame.Payload;

                  -- forward message
                  else
                     Forward_Next_Message (Frame);
                  end if;
               end Forward_Message;

            or
               accept Routing_Table_Message (Table : in Map) do
                  declare
                     Swallow_Table : Map := Table; pragma Unreferenced (Swallow_Table);
                  begin
                     -- update local routing table with new information
                     -- HD-level requirement not attempted, but would fit here
                     null;
                  end;
               end Routing_Table_Message;

            or

               accept Receive_Message (Message : out Messages_Mailbox) do
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
