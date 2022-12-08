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

            -- Array of client names initialization to "free" string all their positions
            for i in client_names'range loop
                client_names(i) := To_Unbounded_String("free");
            end loop;

            -- First outputs, the master is ready
            Put_Line("++++++++++ The master is ready");
            Put_Line("++++++++++ There is " & num_rooms'img & " with a capacity of " & num_tables'img & " clients each");
            Put_Line(" ");
        end init;

        -- Return the room of the name passed by parameter
        function get_room(name : Unbounded_String) return Integer is
            room : Integer;
            found : Boolean := False;
            i : Integer := 1;
        begin

            while found /= True loop

                if client_names(i) = name then
                    found := True;

                    if i mod num_rooms = 0 then
                        room:= i / num_rooms;
                    else
                        room:= (i / num_rooms) + 1;
                    end if;
                end if;

                i := i + 1;

            end loop;
            
            return room;
            
        end get_room; 

        function get_type(r_type : in Integer) return Unbounded_String is
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

        function search_room(r_type : in Integer) return Integer is
            room : Integer := 0;
        begin

            for i in 1 .. num_rooms loop
                if (room_type(i) = r_type and room_capacity(i) > 0) or (room_type(i) = -1) then
                    room := i;

                    return room;
                end if;

            end loop;

            return room;

        end search_room;

        procedure get_table(name : in Unbounded_String; room_num : in Integer) is
            table : Integer;
            initial_pos : constant Integer := ((room_num - 1) * 3) + 1;
        begin
            -- NO PILLA LA MESA BIEN
            for i in initial_pos .. initial_pos + (num_tables - 1) loop
                if client_names(i) = "free" then
                    table := i;
                    client_names(i) := name;
                end if;
            end loop;
            Put_Line("Mesa: " & table'img);
        end get_table;

        function is_available(r_type : in Integer) return Boolean is
            room : Boolean := False;
        begin
            Put_Line("Entrado");
            for i in 1 .. num_rooms loop

                if (room_type(i) = r_type and room_capacity(i) > 0) or (room_type(i) = -1) then
                    room := True;

                    return room;
                end if;

            end loop;

            return room;

        end is_available;

        entry smoke_request(name : in Unbounded_String) when (is_available(1) = True) is
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
                            Put_Line("---------- " & name & " has a table in the smooker room " & room'img & ". Disponibility: " & capacity'img);
                        
                        else
                            table := table + 1;
                        end if;

                        exit when found_table = True or table = initial_pos + num_tables;

                    end loop;
                else
                    room := room + 1;
                end if;

                exit when found_room = True or room = num_rooms + 1;

            end loop;

            -- If is the first smoker, the room is for smokers
            if room_capacity(room) = 2 then
                room_type(room) := 1;
            end if;

        end smoke_request;

        entry nonsmoke_request(name : in Unbounded_String) when (is_available(0) = True) is
            room        : Integer;
        begin
            Put_Line("Entra en el request");
            -- Search if there is a non smooking room available
            room := search_room(0);

            Put_Line(room'img);

            if room > 0 then
                
                -- Modify the dynamic variables
                room_capacity(room) := room_capacity(room) - 1;

                -- Take a table for a non smoker
                get_table(name, room);

            end if;

            -- If is the first non smoker, the room is for non smokers
            if room_capacity(room) = 2 then
                room_type(room) := 0;
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

            -- If yo go out the last
            if room_capacity(room) = num_tables then
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

            -- If yo go out the last
            if room_capacity(room) = num_tables then
                room_type(room) := -1;
            end if;

            capacity := room_capacity(room);
            r_type := room_type(room);
            Put_Line("********** " & name & " frees a table in room " & room'img & ". Disponibility: " & capacity'img & " Type: " & get_type(r_type));

        end nonsmoke_end;

    end ClientMonitor;
    
end master;