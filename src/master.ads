with Ada.Text_Io; use Ada.Text_Io;
package master is

    type room_type_array      is array (-1 .. 1) of integer;
    type room_capacity_array  is array (1 .. 3) of integer;

    protected type ClientMonitor is 
        procedure init;
        entry smoke_request;
        entry nonsmoke_request;
        procedure smoke_end;
        procedure nonsmoke_end;

    private
        -- This three variables added together must always give 3 
        free_rooms   : integer := 3;
        smokers  : integer := 0;
        non_smokers : integer := 0;

        room_type   : room_type_array; -- Free = -1,  Smokers = 1, Non smokers = 0
        room_capacity  : room_capacity_array; -- Mumber of clients in each room

    end ClientMonitor;

end master;