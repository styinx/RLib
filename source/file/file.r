library(R6)
library(xlsx)

#
# File
#
File = R6Class("File",
               
    private = list
    (
       filename = ""
    ),
    
    public = list
    (
        #
        # Sets the current file
        # @param filename : string
        # @return File
        #
        initialize = function(filename)
        {
            private$filename = filename
            return (self)
        },
       
        #
        # Returns the type of the file (file extension)
        # @return string
        #
        getType = function()
        {
            filename = private$filename
            filename_len = nchar(filename)
            indexes = unlist(gregexpr('\\.', filename))
            type = substr(filename, tail(indexes, 1), filename_len)
            return (type)
        }
    )
)

#
# DataFileReader
#
DataFileReader = R6Class("DataFileReader",
                         
    # private members
    private = list
    (
        handle = "",
        columns = ""
    ),
    
    # public members
    public = list
    (
        #
        # Reads a data file and stores its contents
        #
        # @param filename   : string      name of the file to read
        # @param sheet      : int/string  number or name of the sheet to read
        # @param sep        : character   character that defines the column separator
        # @param header     : boolean     defines if the first row contains headers or not
        # @return DataFileReader
        #
        initialize = function(filename, sheet = 1, sep = ";", header = TRUE, cols = c())
        {
            file = File$new(filename)
            type = file$getType()
         
            if (type == ".csv")
            {
                private$handle = read.csv2(file = filename, sep = sep, header = header)
            }
            else if (type == ".xlsx")
            {
                if (!is.numeric(sheet))
                {
                    private$handle = read.xlsx(file = filename, sheetIndex = sheet, header = header, 
                                               stringsAsFactors = FALSE, colIndex = cols)
                }
                else
                {
                    private$handle = read.xlsx(file = filename,sheetName = sheet, header = header,
                                               stringsAsFactors = FALSE, colIndex = cols)
                }
            }
            
            private$columns = names(private$handle)
            return (self)
        },
        
        #
        # Returns the the data file handle
        # @return File
        #
        getHandle = function()
        {
            return (private$handle)
        },
        
        #
        # Returns the selected contents from the data file
        # @param row : string | double | vector(string) | vector(double)
        # @param col : string | double | vector(string) | vector(double)
        # @return data.frame
        #
        get = function(row = NULL, col = NULL)
        {
            return(private$handle[row, col])  
        },
        
        #
        # Returns the selected contents from the data file with a condition.
        # This function aims to simulate a sql query: 'select what where column = value'
        # @param cols  : string | double | vector(string) | vector(double)
        # @param col   : string | double | vector(string) | vector(double)
        # @param eq    : string                                            the condition of the targeted value
        # @param value : string | double                                   the value of the targeted rows
        # @return data.frame
        #
        select = function(cols, col, eq, val)
        {
            if(eq == "==")
            {
                return (private$handle[private$handle[,col] == val, cols])
            }
            else if(eq == "<=")
            {
                return (private$handle[private$handle[,col] <= val, cols])
            }
            else if(eq == ">=")
            {
                return (private$handle[private$handle[,col] >= val, cols])
            }
            else if(eq == "<")
            {
                return (private$handle[private$handle[,col] < val, cols])
            }
            else if(eq == ">")
            {
                return (private$handle[private$handle[,col] > val, cols])
            }
            else if(eq == "!=")
            {
                return (private$handle[private$handle[,col] != val, cols])
            }
        },
        
        #
        # Returns the column names of the data file
        # @return list(string)
        #
        getHeaders = function()
        {
            return (private$columns)
        },
        
        #
        # Returns the number of columns the data file has
        # @return double
        #
        getColumnCount = function()
        {
            return (length(private$columns))  
        },
        
        #
        # Returns the number of rows without first/header row
        # @return double
        #
        getRowCount = function()
        {
            return (nrow(private$handle))
        },
        
        #
        # Returns multiple columns from from the data file
        # @param col_name : string | double | vector(string) | vector(double)
        # @return data.frame
        #
        getColumns = function(col_name)
        {
            return (private$handle[,col_name])   
        }
    )
)