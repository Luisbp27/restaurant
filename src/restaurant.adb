with Ada.Text_Io;               use Ada.Text_Io;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Task_Identification;   use Ada.Task_Identification;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;    

with master;                    use master;

procedure restaurant is

    ROOMS       : constant integer := 3; -- Number of salons
    TABLES      : constant integer := 3; -- Number of tables per room

    monitor     : ClientMonitor; -- Clients monitor
    names_file  : constant String := "names.txt";

    -- Sleep type
    type sleepTime is range 1 .. 3;
    package time is new Ada.Numerics.Discrete_Random (sleepTime);
    Generator : time.Generator;

    -- Tasks types
    task type smokers is
        entry Start (Name_Client : in Unbounded_String);
    end smokers;

    task type non_smokers is
        entry Start (Name_Client : in Unbounded_String);
    end non_smokers;

    -- Randomly delay for tasks
    procedure sleep is
    begin
        delay Duration (time.Random (Generator));
    end sleep;

    -- Smoker task
    task body smokers is
        name : Unbounded_String;
    begin
        accept Start (Name_Client : in Unbounded_String) do 
            name := Name_Client;
        end Start;
            Put_Line("Good morning, I am " & name & " and I smoke");

            monitor.smoke_request(name);
            Put_Line(name & " says: I wanna take the menu day. I am in the room " & monitor.get_room(name)'img);
            sleep;

            monitor.smoke_end(name);
            Put_Line(name & " says: I've already eaten, the bill please");
            sleep;

            Put_Line(name & " GOES OUT");

    end smokers;

    -- Non-Smoker task
    task body non_smokers is
        name : Unbounded_String;
    begin
        accept Start (Name_Client : in Unbounded_String) do
            name := Name_Client;
        end Start;
            Put_Line("Good morning, I am " & name & " and I don't smoke");

            monitor.nonsmoke_request(name);
            Put_Line(name & " says: I wanna take the menu day. I am in the room " & monitor.get_room(name)'img);
            sleep;

            monitor.nonsmoke_end(name);
            Put_Line(name & " says: I've already eaten, the bill please");
            sleep;

            Put_Line(name & " GOES OUT");

    end non_smokers;

    -- Tasks arrays
    type smokers_array      is array (1 .. 7) of smokers;
    type non_smokers_array  is array (1 .. 7) of non_smokers;
    s       : smokers_array;
    ns      : non_smokers_array;

    -- Names array
    type name_arrays        is array (1 .. 14) of Ada.Strings.Unbounded.Unbounded_String;
    names   : name_arrays;

    -- File
    file     : File_Type;

begin
    -- Read the file of names
    Open(file, In_File, names_file);
    for i in names'range loop
        names(i) := To_Unbounded_String(Get_Line(file));
    end loop;
    Close(file);

    -- First outputs
    Put_Line("++++++++++ The master is ready");
    Put_Line("++++++++++ There is " & ROOMS'img & " with a capacity of " & TABLES'img & " clients each");
    
    monitor.init; -- Monitor initialization

    -- Tasks initialization
    for i in names'range loop
        if i mod 2 = 0 then
            s(i).Start(names(i));
        else
            ns(i).Start(names(i));
        end if;
    end loop;

end restaurant;