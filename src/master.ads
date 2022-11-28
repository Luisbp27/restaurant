with Ada.Text_Io; use Ada.Text_Io;
package master is

    protected type ClientMonitor is 
        function table_request(client : Integer) return Integer;

    private
        rooms   : integer := 0;
        tables  : integer := 0;
    end ClientMonitor;

end master;