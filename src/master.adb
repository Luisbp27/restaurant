package body master is

    protected body ClientMonitor is
        -- This method checks if there is availability in a room and returns 1 
        -- if there is availability for smokers or 0 if there is availability for non-smokers.
        function table_request(client : Integer) return Integer is 
        begin
            return 1;
        end table_request;

    end ClientMonitor;
    
end master;