with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package master is

    num_rooms       : constant Integer := 3;
    num_tables      : constant Integer := 3;

    type type_array is array (1 .. 3) of Integer;

    protected type ClientMonitor is 

        procedure init; -- Initializes the monitor's private variables

        function search_room(r_type : in Integer) return Integer; -- Returns an available room, depending on the type
        function is_available(r_type : in Integer) return Boolean; -- Check if there's a room available, depending on the type

        entry smoke_request(name : in Unbounded_String; room : out Integer); -- Assign a table to a smoking client
        entry nonsmoke_request(name : in Unbounded_String; room : out Integer); -- Assign a table to a non smoking client
        procedure smoke_end(name : in Unbounded_String; room : in Integer); -- Frees up a table for a smoking client
        procedure nonsmoke_end(name : in Unbounded_String; room : in Integer); -- Frees up a table for a non smoking client

    private

        room_type       : type_array; -- Free = -1,  Smokers = 1, Non smokers = 0
        room_capacity   : type_array; -- Number of clients in each room

    end ClientMonitor;

end master;