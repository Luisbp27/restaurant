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

            -- First outputs, the master is ready
            Put_Line("++++++++++ The master is ready");
            Put_Line("++++++++++ There is " & num_rooms'img & " with a capacity of " & num_tables'img & " clients each");
            Put_Line(" ");
        end init;

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
            room    : Boolean := False;
            n       : Integer;
        begin

            for i in room_capacity'range loop
                
                if (room_capacity(i) > 0) and (room_type(i) = -1 or room_type(i) = r_type) then
                    room := True;
                    n := i;
                end if;

                exit when room = True;

            end loop;

            return n;

        end search_room;

        function is_available(r_type : in Integer) return Boolean is
            room : Boolean := False;
        begin

            for i in room_capacity'range loop

                if (room_capacity(i) > 0) and (room_type(i) = -1 or room_type(i) = r_type) then
                    room := True;
                end if;

                exit when room = True;

            end loop;                

            return room;

        end is_available;

        entry smoke_request(name : in Unbounded_String; room : out Integer) when (is_available(1) = True) is
            capacity    : Integer;
        begin
            -- Search if there is a smooking room available
            room := search_room(1);

            if room > 0 then
                -- Update room type
                room_type(room) := 1;

                -- Update the dynamic variables
                room_capacity(room) := room_capacity(room) - 1;

                -- Print
                capacity := room_capacity(room);
                Put_Line("---------- " & name & " has a table at the room " & room'img & " of smokers. Disponibility: " & capacity'img);

            end if;

            -- If is the first smoker, the room is for smokers
            if room_capacity(room) = 2 then
                room_type(room) := 1;
            end if;

        end smoke_request;

        entry nonsmoke_request(name : in Unbounded_String; room : out Integer) when (is_available(0) = True) is
            capacity    : Integer;
        begin
            -- Search if there is a non smooking room available
            room := search_room(0);
            
            if room > 0 then
                -- Update room type
                room_type(room) := 0;

                -- Update the dynamic variables
                room_capacity(room) := room_capacity(room) - 1;

                -- Print
                capacity := room_capacity(room);
                Put_Line("********** " & name & " has a table at the room " & room'img & " of non smokers. Disponibility: " & capacity'img);

            end if;

            -- If is the first non smoker, the room is for non smokers
            if room_capacity(room) = 2 then
                room_type(room) := 0;
            end if;

        end nonsmoke_request;

        procedure smoke_end(name : in Unbounded_String; room : in Integer) is
            capacity    : Integer;
            r_type      : Integer;
        begin

            -- Update the capacity of the room
            room_capacity(room) := room_capacity(room) + 1;

            -- If yo go out the last
            if room_capacity(room) = num_tables then
                room_type(room) := -1;
            end if;

            -- Print
            capacity := room_capacity(room);
            r_type := room_type(room);
            Put_Line("---------- " & name & " frees a table in room " & room'img & ". Disponibility: " & capacity'img & " Type: " & get_type(r_type));

        end smoke_end;

        procedure nonsmoke_end(name : in Unbounded_String; room : in Integer) is
            capacity    : Integer;
            r_type      : Integer;
        begin

            -- Update the capacity of the room
            room_capacity(room) := room_capacity(room) + 1;

            -- If yo go out the last
            if room_capacity(room) = num_tables then
                room_type(room) := -1;
            end if;

            -- Print
            capacity := room_capacity(room);
            r_type := room_type(room);
            Put_Line("********** " & name & " frees a table in room " & room'img & ". Disponibility: " & capacity'img & " Type: " & get_type(r_type));

        end nonsmoke_end;

    end ClientMonitor;
    
end master;