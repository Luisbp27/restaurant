package body master is

    protected body ClientMonitor is

        procedure init is
            i : Integer := 1;
        begin
            
            for i in room_capacity'range loop
                room_capacity(i) := 3;
            end loop;

        end init;

        -- Enter when there are free rooms or smoking rooms left
        entry smoke_request when (free_rooms < 3) and (smokers > 0) is
            found : Boolean := False;
            i : Integer := 1;
        begin
            
            -- Search if there is a smooking room available
            loop
                -- If the room is free or smoking and there is capacity
                if (room_type(i) = -1 or room_type(i) = 1) and room_capacity(i) > 0 then
                    found := True;
                else
                    i := i + 1;
                end if;
                exit when found = True or i = 4;

            end loop;

            room_capacity(i) := room_capacity(i) - 1;
            room_type(i) := 1;
            free_rooms := free_rooms - 1;
            smokers := smokers + 1;

        end smoke_request;

        entry nonsmoke_request when (free_rooms < 3) and (non_smokers > 0) is
            found : Boolean := False;
            i : Integer := 1;
        begin
            
            -- Search if there is a smooking room available
            loop
                -- If the room is free or smoking and there is capacity
                if (room_type(i) = -1 or room_type(i) = 0) and room_capacity(i) > 0 then
                    found := True;
                else
                    i := i + 1;
                end if;
                exit when found = True or i = 4;

            end loop;

            room_capacity(i) := room_capacity(i) - 1;
            room_type(i) := 1;
            free_rooms := free_rooms - 1;
            non_smokers := non_smokers + 1;

        end nonsmoke_request;

        procedure smoke_end is
        begin
            smokers := smokers - 1;
        end smoke_end;

        procedure nonsmoke_end is
        begin
            non_smokers := non_smokers - 1;
        end nonsmoke_end;

    end ClientMonitor;
    
end master;