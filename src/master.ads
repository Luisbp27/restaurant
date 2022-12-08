with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package master is

    num_rooms       : constant Integer := 3;
    num_tables      : constant Integer := 3;

    type room_type_array        is array (-1 .. 1) of integer;
    type room_capacity_array    is array (0 .. 3) of integer;
    type client_names_array     is array (1 .. (num_rooms * num_tables)) of Unbounded_String;

    protected type ClientMonitor is 
        procedure init;
        function get_room(name : in Unbounded_String) return Integer;
        function search_room(r_type : in Integer) return Integer;
        procedure get_table(name : in Unbounded_String; room_num : in Integer);
        function is_available(r_type : in Integer) return Boolean;
        entry smoke_request(name : in Unbounded_String);
        entry nonsmoke_request(name : in Unbounded_String);
        procedure smoke_end(name : in Unbounded_String);
        procedure nonsmoke_end(name : in Unbounded_String);

    private

        room_type       : room_type_array; -- Free = -1,  Smokers = 1, Non smokers = 0
        room_capacity   : room_capacity_array; -- Number of clients in each room
        client_names    : client_names_array; -- Name of client in each room [R1, R1, R1, R2, R2, R2, R3, R3, R3]

    end ClientMonitor;

end master;