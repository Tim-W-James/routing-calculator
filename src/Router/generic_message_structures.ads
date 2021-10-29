--
--  Framework: Uwe R. Zimmer, Australia, 2015
--

with Ada.Strings.Bounded;           use Ada.Strings.Bounded;
with Generic_Routers_Configuration;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

generic
   with package Routers_Configuration is new Generic_Routers_Configuration (<>);

package Generic_Message_Structures is

   use Routers_Configuration;

   package Message_Strings is new Generic_Bounded_Length (Max => 80);
   use Message_Strings;

   -- containers
   package Id_Set is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Router_Range);
   use Id_Set;
   package Routing_Table is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Router_Range,
        Element_Type => Set);
   package Path_Vector is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Router_Range);
   use Path_Vector;

   subtype The_Core_Message is Bounded_String;

   type Messages_Client is record
      Destination : Router_Range;
      The_Message : The_Core_Message;
   end record;

   type Messages_Mailbox is record
      Sender      : Router_Range     := Router_Range'Invalid_Value;
      The_Message : The_Core_Message := Message_Strings.To_Bounded_String ("");
      Hop_Counter : Natural          := 0;
   end record;

   -- use OSI data link layer as an analogy for the synchronous "wire" connections between routers
   -- modelled from an ethernet frame
   type Data_Link_Frame is record
      Destination   : Router_Range := Router_Range'Invalid_Value;
      -- path across the network to destination
      Path          : Vector       := Empty_Vector;
      Payload       : Messages_Mailbox;
   end record;

end Generic_Message_Structures;
