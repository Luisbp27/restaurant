with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package master is

    num_rooms       : constant Integer := 3;
    num_tables      : constant Integer := 3;

    type room_type_array        is array (-1 .. 1) of integer;
    type room_capacity_array    is array (0 .. num_tables) of integer;

    protected type ClientMonitor is 

        procedure init;

        function search_room(r_type : in Integer) return Integer;
        function is_available(r_type : in Integer) return Boolean;

        entry smoke_request(name : in Unbounded_String; room : out Integer);
        entry nonsmoke_request(name : in Unbounded_String; room : out Integer);
        procedure smoke_end(name : in Unbounded_String; room : in Integer);
        procedure nonsmoke_end(name : in Unbounded_String; room : in Integer);

    private

        room_type       : room_type_array; -- Free = -1,  Smokers = 1, Non smokers = 0
        room_capacity   : room_capacity_array; -- Number of clients in each room

    end ClientMonitor;

end master;