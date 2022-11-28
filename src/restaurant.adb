with Ada.Text_Io;   use Ada.Text_Io;
with master;        use master;
with Ada.Numerics.Discrete_Random;    

procedure restaurant is

    
    ROOMS       : constant integer := 3; -- Number of salons
    TABLES      : constant integer := 3; -- Number of tables per room

    monitor     : ClientMonitor; -- Clients monitor

    type sleepTime is range 1 .. 3;
    package time is new Ada.Numerics.Discrete_Random (sleepTime);
    Generator : time.Generator;

    task type smokers is
        entry Start (Idx : in integer);
    end smokers;

    task type non_smokers is
        entry Start (Idx : in integer);
    end non_smokers;

    -- Randomly delay for tasks
    procedure sleep is
    begin
        delay Duration (time.Random (Generator));
    end sleep;

    -- Smoker tasks
    task body smokers is
        My_Idx : integer;
    begin
        accept Start (Idx : in integer) do 
            My_Idx := Idx;
        end Start;

            Put_Line("Hi, smoker number: " & My_Idx'img);

    end smokers;

    -- Non-Smoker tasks
    task body non_smokers is
        My_Idx : integer;
    begin
        accept Start (Idx: in integer) do
            My_Idx := Idx;
        end Start;

            Put_Line("Hi, non smoker number: " & My_Idx'img);
    end non_smokers;

    -- Tasks arrays
    type smokers_array      is array (1 .. 7) of smokers;
    type non_smokers_array  is array (1 .. 7) of non_smokers;
    s   : smokers_array;
    ns  : non_smokers_array;

begin
    -- Tasks initialization
    for Idx in 1 .. 7 loop
        s(Idx).Start(Idx);
        ns(Idx).Start(Idx);
    end loop;

end restaurant;