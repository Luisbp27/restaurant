with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;

package body master is

    protected body ClientMonitor is

        procedure init is
        begin
            -- Array of room capacity initialization to 3 all their positions
            for i in room_capacity'range loop
                room_capacity(i) := 3;
            end loop;

            -- Array of room type initialization to -1 all their positions
            for i in room_type'range loop
                room_type(i) := -1;
            end loop;

            -- Array of room capacity initialization to "free" string all their positions
            for i in client_names'range loop
                client_names(i) := To_Unbounded_String("free");
            end loop;

        end init;

        -- Return the room of the name passed by parameter
        function get_room(name : Unbounded_String) return Integer is
        begin
            for i in client_names'range loop
                if client_names(i) = name then
                    return i;
                end if;
            end loop;

            return 0;

        end get_room; 

        -- Enter when there are free rooms or smoking rooms left
        entry smoke_request(name : Unbounded_String) when (free_rooms > 0) or (smokers > 0) is
            found_room  : Boolean := False;
            found_table : Boolean := False;
            room        : Integer := 1;
            table       : Integer;
            capacity    : Integer;
            initial_pos : constant Integer := ((room - 1) * 3) + 1;
        begin
            -- Search if there is a smooking room available
            loop
                -- If the room is free or smoking and there is capacity
                if (room_type(room) = -1 or room_type(room) = 1) and room_capacity(room) > 0 then
                    found_room := True;

                    -- Found the table and took it with a smoker
                    table := initial_pos;
                    loop
                        if client_names(table) = To_Unbounded_String("free") then
                            found_table := True;
                            client_names(table) := name;

                            room_capacity(room) := room_capacity(room) - 1;
                            capacity := room_capacity(room);
                            Put_Line("----------" & name & " has a table in the smooker room " & room'img & ". Disponibility: " & capacity'img);
                        else
                            table := table + 1;
                        end if;

                        exit when found_table = True or table = initial_pos + 3;

                    end loop;
                else
                    room := room + 1;
                end if;

                exit when found_room = True or room = 4;

            end loop;

            room_type(room) := 1;
            free_rooms := free_rooms - 1;
            smokers := smokers + 1;

        end smoke_request;

        entry nonsmoke_request(name : Unbounded_String) when (free_rooms > 0) and (non_smokers > 0) is
            found_room  : Boolean := False;
            found_table : Boolean := False;
            room        : Integer := 1;
            table       : Integer;
            capacity    : Integer;
            initial_pos : constant Integer := ((room - 1) * 3) + 1;
        begin
            -- Search if there is a non smooking room available
            loop
                -- If the room is free or non smoking and there is capacity
                if (room_type(room) = -1 or room_type(room) = 0) and room_capacity(room) > 0 then
                    found_room := True;

                    -- Found the table and took it with a non smoker
                    table := initial_pos;
                    loop
                        if client_names(table) = To_Unbounded_String("free") then
                            found_table := True;
                            client_names(table) := name;

                            room_capacity(room) := room_capacity(room) - 1;
                            capacity := room_capacity(room);
                            Put_Line("----------" & name & " has a table in the non smooker room " & room'img & ". Disponibility: " & capacity'img);
                        else
                            table := table + 1;
                        end if;

                        exit when found_table = True or table = initial_pos + 3;

                    end loop;
                else
                    room := room + 1;
                end if;

                exit when found_room = True or room = 4;

            end loop;

            room_type(room) := 0;
            free_rooms := free_rooms - 1;
            non_smokers := non_smokers + 1;

        end nonsmoke_request;

        procedure smoke_end(name : Unbounded_String) is
        begin
            smokers := smokers - 1;
        end smoke_end;

        procedure nonsmoke_end(name : Unbounded_String) is
        begin
            non_smokers := non_smokers - 1;
        end nonsmoke_end;

    end ClientMonitor;
    
end master;