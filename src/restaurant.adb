with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Task_Identification;   use Ada.Task_Identification;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;    

with master;                    use master;

procedure restaurant is

    monitor     : ClientMonitor; -- Clients monitor

    -- Sleep type
    type sleepTime is range 1 .. 3;
    package time is new Ada.Numerics.Discrete_Random (sleepTime);
    Generator : time.Generator;

    -- Randomly delay for tasks
    procedure sleep is
    begin
        delay Duration (time.Random (Generator));
    end sleep;

    -- Tasks types
    task type client is
        entry Start (Name_Client : in Unbounded_String; t : in Integer);
    end client;

    -- Smoker task
    task body client is
        name    : Unbounded_String;
        room    : Integer;
        typ     : Integer;
    begin
        accept Start (Name_Client : in Unbounded_String; t : in Integer) do 
            name    := Name_Client;
            typ     := t;
        end Start;

            if typ = 0 then

                Put_Line("  GOOD MORNING, I am " & name & " and I don't smoke");

                monitor.nonsmoke_request(name, room);
                Put_Line("  " & name & " says: I wanna take the menu day. I am in the room " & room'img);
                sleep;

                monitor.nonsmoke_end(name, room);
                Put_Line("  " & name & " says: I've already eaten, the bill please");
                Put_Line("  " & name & " GOES OUT");

            else 

                Put_Line("GOOD MORNING, I am " & name & " and I smoke");

                monitor.smoke_request(name, room);
                Put_Line(name & " says: I wanna take the menu day. I am in the room " & room'img);
                sleep;

                monitor.smoke_end(name, room);
                Put_Line(name & " says: I've already eaten, the bill please");
                Put_Line(name & " GOES OUT");

            end if;
            

    end client;

    -- Task array
    type clients_array is array (1 .. 14) of client;
    clients : clients_array;

    -- Names array
    type name_arrays is array (1 .. 14) of Ada.Strings.Unbounded.Unbounded_String;
    names   : name_arrays;

    -- File
    file     : File_Type;

begin
    -- Read the file of names
    Open(file, In_File, "names.txt");
    for i in names'range loop
        names(i) := To_Unbounded_String(Get_Line(file));
    end loop;
    Close(file);
    
    monitor.init; -- Monitor initialization

    -- Tasks initialization
    for i in names'range loop
        if i mod 2 = 0 then
            clients(i).Start(names(i), 0);
        else
            clients(i).Start(names(i), 1);
        end if;
    end loop;

end restaurant;