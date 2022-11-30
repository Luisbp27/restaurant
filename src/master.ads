with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package master is

    type room_type_array        is array (-1 .. 1) of integer;
    type room_capacity_array    is array (1 .. 3) of integer;
    type client_names_array     is array (1 .. 9) of Unbounded_String;

    protected type ClientMonitor is 
        procedure init;
        function get_room(name : Unbounded_String) return Integer;
        entry smoke_request(name : Unbounded_String);
        entry nonsmoke_request(name : Unbounded_String);
        procedure smoke_end(name : Unbounded_String);
        procedure nonsmoke_end(name : Unbounded_String);

    private
        -- This three variables added together must always give 3 
        free_rooms      : Integer := 3;
        smokers         : Integer := 0;
        non_smokers     : Integer := 0;

        num_rooms       : Integer := 3;
        num_tables      : Integer := 3;

        room_type       : room_type_array; -- Free = -1,  Smokers = 1, Non smokers = 0
        room_capacity   : room_capacity_array; -- Number of clients in each room

        client_names    : client_names_array; -- Name of client in each room [R1, R1, R1, R2, R2, R2, R3, R3, R3]

    end ClientMonitor;

end master;