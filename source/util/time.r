library(R6)

#
# Timer
#
Timer = R6Class("Timer",
    
    private = list
    (
        start_tick = 0,
        diff_tick = 0,
        started = 0,
        stoped = 0,
        paused = 0,
        state = "stopped",
        time_active = 0,
        time_paused = 0,
        time_expired = 0
    ),
    
    public = list
    (
        initialize = function()
        {
            self$start()
        },
        
        getTicks = function()
        {
            return(as.numeric(Sys.time()) * 1000 * 1000 * 1000)
        },
        
        start = function()
        {
            private$started = self$getTicks()
            private$start_tick = private$started
            private$diff_tick = private$started
            if(private$state == "stopped")
            {
                private$state = "started"
            }
            return (private$started)
        },
        
        pause = function()
        {
            private$paused = self$getTicks()
            if(private$state == "started")
            {
                private$state = "paused"
                last_interval = private$paused - private$started
                private$time_active = private$time_active + last_interval
                private$time_expired = private$time_expired + last_interval
            }
            return (private$paused)
        },
        
        resume = function()
        {
            private$started = self$getTicks()
            if(private$state == "paused")
            {
                private$state = "started"
                last_interval = private$started - private$paused
                private$time_paused = private$time_paused + last_interval
                private$time_expired = private$time_expired + last_interval
            }
        },
        
        stop = function()
        {
            private$stoped = self$getTicks()
            if(private$state != "stopped")
            {
                last_interval = private$stoped - private$started
                private$time_expired = private$time_expired + last_interval
                if(private$state == "active")
                {
                    private$time_active = private$time_active + last_interval
                }
                else if(private$state == "paused")
                {
                    private$time_active = private$time_paused + last_interval
                }
                private$state = "stopped"
            }
        },
        
        getDiff = function()
        {
            res = self$getTicks() - private$diff_tick
            private$diff_tick = self$getTicks()
            return (res)  
        },
        
        getTime = function()
        {
            self$pause()
            self$resume()
            
            return (self$getTicks() - private$start_tick)
        },
        
        getFormattedTime = function(time = NULL)
        {
            if(is.null(time))
            {
                time = self$getTime()
            }

            result = ""
            seconds = as.integer(time / 1000 / 1000 / 1000)
            hours = as.integer(seconds / 60 / 60) %% 24
            minutes = as.integer(seconds / 60) %% 60
            
            if (hours > 0)
            {
                result = paste0(result, toString(hours), "h")
            }
            if (minutes > 0)
            {
                result = paste0(result, toString(minutes), "m")
            }
            if (seconds > 0)
            {
                result = paste0(result, toString(seconds %% 60), "s")
            }
            if (seconds < 1)
            {
                result = paste0(result, toString(seconds * 1000 %% 60), "ms")
            }
            
            return (result)
        },
        
        printTime = function()
        {
            cat(paste0(self$getFormattedTime(), "\n"))
        }
    )
)