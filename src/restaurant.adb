with Ada.Text_Io;               use Ada.Text_Io;
with master;                    use master;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Task_Identification;   use Ada.Task_Identification;
with Ada.Numerics.Discrete_Random;    

procedure restaurant is

    ROOMS       : constant integer := 3; -- Number of salons
    TABLES      : constant integer := 3; -- Number of tables per room

    monitor     : ClientMonitor; -- Clients monitor
    names_file       : constant String := "names.txt";

    type sleepTime is range 1 .. 3;
    package time is new Ada.Numerics.Discrete_Random (sleepTime);
    Generator : time.Generator;

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

    -- Smoker tasks
    task body smokers is
        name : Unbounded_String;
    begin
        accept Start (Name_Client : in Unbounded_String) do 
            name := Name_Client;
        end Start;
            Put_Line("Hi, smoker number: " & name);

            monitor.smoke_request; 
            sleep;
            monitor.smoke_end;
            sleep;

    end smokers;

    -- Non-Smoker tasks
    task body non_smokers is
        name : Unbounded_String;
    begin
        accept Start (Name_Client : in Unbounded_String) do
            name := Name_Client;
        end Start;
            Put_Line("Hi, non smoker number: " & name);

            monitor.nonsmoke_request;
            sleep;
            monitor.nonsmoke_end;
            sleep;

    end non_smokers;

    -- Tasks arrays
    type smokers_array      is array (1 .. 7) of smokers;
    type non_smokers_array  is array (1 .. 7) of non_smokers;
    s           : smokers_array;
    ns          : non_smokers_array;

    -- Names array
    subtype name_index      is Positive range Positive'first .. 14;
    type name_arrays        is array (name_index) of Unbounded_String;
    names : name_arrays;

    -- File
    file        : File_Type;

begin
    -- Read the file of names
    Open(file, In_File, names_file);
    for i in names'range loop
        names(i) := To_Unbounded_String(Get_Line(file));
    end loop;
    Close(file);

    Put_Line("++++++++++ The master is ready");
    Put_Line("++++++++++ There is " & ROOMS'img & " with a capacity of " & TABLES'img & " clients each");
    monitor.init;
    -- Tasks initialization
    for i in names'range loop
        s(i).Start(names(name_arrays'first + i));
        ns(i + 1).Start(names(name_arrays'first + i + 1));
    end loop;

end restaurant;