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

            -- First outputs
            Put_Line("++++++++++ The master is ready");
            Put_Line("++++++++++ There is " & num_rooms'img & " with a capacity of " & num_tables'img & " clients each");

        end init;

        -- Return the room of the name passed by parameter
        function get_room(name : Unbounded_String) return Integer is
            room : Integer;
        begin

            for i in client_names'range loop
                if client_names(i) = name then
                    if i mod 3 = 0 then
                        room := i / 3;
                    else
                        room := (i / 3) + 1;
                    end if;
                end if;
            end loop;

            return room;

        end get_room; 

        function get_type(r_type : Integer) return Unbounded_String is
            type_string : Unbounded_String;
        begin
            
            if r_type = -1 then
                type_string := To_Unbounded_String("FREE");
            elsif r_type = 0 then
                type_string := To_Unbounded_String("NON SMOKER");
            else
                type_string := To_Unbounded_String("SMOKER");
            end if;

            return type_string;
        
        end get_type;

        entry smoke_request(name : in Unbounded_String) when (free_rooms > 0) or (smokers > 0) is
            found_room  : Boolean := False;
            found_table : Boolean := False;
            room        : Integer := 1;
            table       : Integer;
            capacity    : Integer;
            initial_pos : constant Integer := ((room - 1) * 3) + 1;

            t : Integer;
        begin
            Put_Line("Entra fumador " & name);
            -- Search if there is a smooking room available
            while (found_room = False) and (room /= num_rooms + 1) loop
                t := room_type(room);
                Put_Line("Salon: " & room'img & " Tipo: " & t'img);
                
                -- If the room is free or smoking and there is capacity
                if (room_type(room) = -1 or room_type(room) = 1) and room_capacity(room) > 0 then
                    found_room := True;
                else
                    room := room + 1;
                end if;

                Put_Line("Salon trobat: " & found_room'img & " Salon: " & room'img);
            end loop;

            -- If founded a room, now we gonna find a table
            if found_room = True then 
                -- Found the table and took it with a smoker
                table := initial_pos;
                while found_table = False and table /= initial_pos + num_tables loop
                    if client_names(table) = "free" then
                        found_table := True;
                        client_names(table) := name;

                        room_capacity(room) := room_capacity(room) - 1;
                        capacity := room_capacity(room);
                        Put_Line("---------- " & name & " has a table in the smooker room " & room'img & ". Disponibility: " & capacity'img);

                        -- We decrement the number of smoker rooms because the room is full, and we can't enter again
                        if room_capacity(room) = 0 then
                            smokers := smokers - 1;
                        end if;
                    
                    else
                        table := table + 1;
                    end if;
                end loop;
            end if;

            -- If is the first smoker, the room is for smokers
            if room_capacity(room) = 2 then
                free_rooms := free_rooms - 1;
                smokers := smokers + 1;
                room_type(room) := 1;
            end if;

        end smoke_request;

        entry nonsmoke_request(name : in Unbounded_String) when (free_rooms > 0) or (non_smokers > 0) is
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
                            Put_Line("********** " & name & " has a table in the non smooker room " & room'img & ". Disponibility: " & capacity'img);
                        
                            -- We decrement the number of non smoker rooms because the room is full, and we can't enter again
                            if room_capacity(room) = 0 then
                                non_smokers := non_smokers - 1;
                            end if;
                        
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

            -- If is the first non smoker, the room is for non smokers
            if room_capacity(room) = 2 then
                room_type(room) := 0;
                free_rooms := free_rooms - 1;
                non_smokers := non_smokers + 1;
            end if;

        end nonsmoke_request;

        procedure smoke_end(name : in Unbounded_String) is
            room        : Integer;
            capacity    : Integer;
            r_type      : Integer;
        begin

            room := get_room(name);

            room_capacity(room) := room_capacity(room) + 1;

            -- Reset the name of the table
            for i in client_names'range loop
                if client_names(i) = name then
                    client_names(i) := To_Unbounded_String("free");
                end if;
            end loop;

            -- If you go out the first
            if room_capacity(room) = 2 then
                smokers := smokers + 1;
            end if;

            -- If yo go out the last
            if room_capacity(room) = 3 then
                free_rooms := free_rooms + 1;
                smokers := smokers - 1;
                room_type(room) := -1;
            end if;

            capacity := room_capacity(room);
            r_type := room_type(room);
            Put_Line("---------- " & name & " frees a table in room " & room'img & ". Disponibility: " & capacity'img & " Type: " & get_type(r_type));

        end smoke_end;

        procedure nonsmoke_end(name : in Unbounded_String) is
            room        : Integer;
            capacity    : Integer;
            r_type      : Integer;
        begin

            room := get_room(name);

            room_capacity(room) := room_capacity(room) + 1;
            
            -- Reset the name of the table
            for i in client_names'range loop
                if client_names(i) = name then
                    client_names(i) := To_Unbounded_String("free");
                end if;
            end loop;

            -- If you go out the first
            if room_capacity(room) = 2 then
                non_smokers := non_smokers + 1;
            end if;

            -- If yo go out the last
            if room_capacity(room) = 3 then
                free_rooms := free_rooms + 1;
                non_smokers := non_smokers - 1;
                room_type(room) := -1;
            end if;

            capacity := room_capacity(room);
            r_type := room_type(room);
            Put_Line("---------- " & name & " frees a table in room " & room'img & ". Disponibility: " & capacity'img & " Type: " & get_type(r_type));

        end nonsmoke_end;

    end ClientMonitor;
    
end master;