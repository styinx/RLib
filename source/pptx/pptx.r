library(R6)
library(ReporteRs)

FlexTable = R6Class(
    "FlexTable",
    private = list
    (table = NULL),
    
    public = list
    (
        initialize = function(data, headers = FALSE)
        {
            private$table = FlexTable(data = data, headers)
        },
        
        setZebraStyle = function(odd = "#EEEEEE", even = "#BBBBBB")
        {
            private$table = setZebraStyle(private$table, odd = odd, even = even)
            return (self)
        },
        
        setBorderStyle = function(iv = borderProperties(color = "#000000", style = "solid"),
                                  ih = borderProperties(color = "#000000", style = "solid"),
                                  ov = borderProperties(color = "#000000", style = "solid"),
                                  oh = borderProperties(color = "#000000", style = "solid"))
        {
            private$table = setFlexTableBorders(
                private$table,
                inner.vertical = iv,
                inner.horizontal = ih,
                outer.vertical = ov,
                outer.horizontal = oh
            )
            return (self)
        },
        
        setTextProperties = function(type, properties = textProperties(color = "#000000", size = "12"))
        {
            if (type == "all" | type == "a")
            {
                
            }
            else if (type == "header" | type == "h")
            {
                
            }
            else if (type == "body" | type == "b")
            {
                
            }
            return (self)
        },
        
        setColumnWidth = function(widths)
        {
            private$table = setFlexTableWidths(private$table, widths)
            return (self)
        },
        
        setColSpan = function(x, from = 1, to = 1)
        {
            private$table = spanFlexTableColumns(private$table,
                                                 i = x,
                                                 from = from,
                                                 to = to)
            return (self)
        },
        
        setRowSpan = function(y, from = 1, to = 1)
        {
            private$table = spanFlexTableRows(y, from = from, to = to)
            return (self)
        }
    )
)

DOC = R6Class("DOC",
              
    private = list
    (
        handle = NULL,
        filename = ""
    ),
    
    public = list
    (
        initialize = function(filename)
        {
            private$filename = filename
        },
        
        write = function()
        {
            writeDoc(private$handle, file = private$filename)
        }
    )
)

PPT = R6Class("PPT",
    
    private = list
    (
        handle = 0,
        filename = "",
        layouts = list(),
        slides = list(),
        slide_num = 0
    ),
    
    public = list
    (
        initialize = function(filename, template = "")
        {
            if (template != "")
            {
                private$handle = pptx(template = template)
            }
            else
            {
                private$handle = pptx()
            }
            private$filename = filename
            private$layouts = slide.layouts(private$handle)
            return (self)
        },
        
        getHandle = function()
        {
            return (private$handle)
        },
        
        write = function()
        {
            writeDoc(private$handle, private$filename)
        },
        
        addSlide = function(layout)
        {
            private$handle = addSlide(private$handle, layout)
            private$slide_num = private$slide_num + 1
            return (self)
        },
        
        # Function does not appear to exist anymore ???
        addTitle = function(title)
        {
            private$handle = addTitle(private$handle, title)
            return (self)
        },
        
        addSubTitle = function(subtitle)
        {
            private$handle = addSubtitle(private$handle, subtitle)
            return (self)
        },
        
        addParagraph = function(content)
        {
            private$handle = addParagraph(private$handle, content)
            return (self)
        },
        
        addDate = function()
        {
            private$handle = addDate(private$handle)
            return (self)
        },
        
        addPageNumber = function()
        {
            private$handle = addPageNumber(private$handle)
            return (self)
        },
        
        # Creates a text instead of the page number
        addPageNumberText = function(text = "")
        {
            private$handle = addPageNumber(private$handle, text)
            return (self)
        },
        
        addFooter = function(text)
        {
            private$handle = addFooter(private$handle, text)
            return (self)
        },
        
        addFlexTable = function(flex_table, x, y, w, h)
        {
            private$handle = addFlexTable(private$handle, flex_table, x, y, w, h)
            return (self)
        },
        
        addPlot = function(chart, editable = TRUE)
        {
            private$handle = addPlot(private$handle, fun = function() print(chart), vector.graphic = editable)
            return (self)
        }
    )
)