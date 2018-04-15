library(R6)
library(ggplot2)

source("source/chart/themes.r", encoding = "UTF-8")

#
# 
#
Chart = R6Class("Chart",
    
    private = list
    (
        type = "",
        chart = 0,
        data = 0,
        maps = list(
                    world = list(xlim = c(-180, 180),
                                 ylim = c(-90, 90),
                                 ratio = 1.3),
                    europe = list(xlim = c(-20, 59),
                                  ylim = c(35, 71),
                                  ratio = 1.3)
                   )
    ),
    
    public = list
    (
        
        #
        # Draws a ggplot diagram with the desired specs
        # @return Chart
        #
        initialize = function(data, aes, additional = NA)
        {
            private$data = data
            private$chart = ggplot(data, aes) + additional
            return (self)
        },
        
        #
        # Draws a ggplot diagram with the desired specs
        # @return Chart
        #
        draw = function(data, aes, additional = NA)
        {
            private$data = data
            private$chart = ggplot(data, aes) + additional
            return (self)
        },
        
        #
        # Use this function only if the functionalities in the initialize/draw are not enough.
        #
        drawBarChart = function()
        {
            
            return (self)  
        },
        
        #
        # Use this function only if the functionalities in the initialize/draw are not enough.
        #
        drawEcdfChart = function()
        {
            return (self)
        },
        
        #
        # Use this function only if the functionalities in the initialize/draw are not enough.
        #
        drawHeatmap = function()
        {
            return (self)   
        },
        
        #
        # Use this function only if the functionalities in the initialize/draw are not enough.
        #
        drawMap = function(location = "world")
        {
            loc = private$maps[[location]]
            map = map_data("world")
            private$chart = ggplot(map, aes(x = long, y = lat, group = group)) +
                            geom_polygon(fill = world$group %% 5 + 1, color = "grey30", size = 0.3) +
                            coord_fixed(xlim = loc$xlim, ylim = loc$ylim, ratio = loc$ratio) +
                            theme_void()
            return (self)
        },
        
        #
        # Returns the instance of the ggplot chart
        # @return ggplot instance
        #
        getChart = function()
        {
            return (private$chart)  
        },
        
        #
        # Sets the theme of the chart
        # @param theme : theme object from ggplot
        # @see source/chart/themes.r
        # @return Chart
        #
        setTheme = function(theme)
        {
            private$chart = private$chart + theme
            return (self)
        },
        
        #
        # Adds text to the diagram
        # @param x : list(double)
        # @param y : list(double)
        # @param label : list(string)
        # @return Chart
        #
        addText = function(x = c(0), y = c(0), label = c(""))
        {
            private$chart = private$chart + annotate("text", x = x, y = y, label = label)
            return (self)
        },
        
        #
        # Sets the x-axis title
        # @param title : string
        # @return Chart
        #
        setXAxisTitle = function(title)
        {
            private$chart = private$chart + xlab(title)
            return (self)
        },
        
        #
        # Sets the y-axis title
        # @param title : string
        # @return Chart
        #
        setYAxisTitle = function(title)
        {
            private$chart = private$chart + ylab(title)
            return (self)
        },
        
        #
        # Sets the legend title
        # @param title : string
        # @return Chart
        #
        setLegendTitle = function(title)
        {
            private$chart = private$chart + labs(color = title)
            return(self)  
        },
        
        #
        # Sets the chart title
        # @param title : string
        # @return Chart
        #
        setChartTitle = function(title)
        {
            private$chart = private$chart + labs(title = title)
            return (self)
        },
        
        #
        # Sets the chart subtitle
        # @param subtitle : string
        # @return Chart
        #
        setChartSubTitle = function(subtitle)
        {
            private$chart = private$chart + labs(subtitle = subtitle)  
            return (self)
        },
        
        #
        # Sets titles for the graph.
        # Possible values: x, y, color, fill, title, subtitle, caption
        # 
        # @param titles : list(string)
        # @return Chart
        #
        setTitles = function(titles)
        {
            private$chart = private$chart + labs(titles)
            return (self)
        },
        
        #
        # Saves the chart to the desired location
        # @param filename : string
        # @return Chart
        #
        save = function(filename, options = NULL)
        {
            ggsave(filename)
            return (self)
        }
    )
)