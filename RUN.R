# Run
# This script downloads the requested quarter and year of sec company .txt file
# sorts the downloaded data into R data frame and then use that as a directory to fish for all other files in that same folder

# main (company full name as it appears on edgar, year of publish, and published in X quarter)



getBaseRange <- function ()
{
  baseRange = 5
  return (baseRange)
}

main <- function()
{
  currentYear = 2018
  currentQuarter = 2
  inputName = readCompanyname()
  startYear = readSInteger()
  endYear = readEInteger()
   
  if (endYear < startYear || (endYear - startYear) > 50 || startYear <1990 || endYear > currentYear)
  {
    stop("Invalid search range!")
  }
  
  
  print(paste("loading Quarter filings for " , inputName  , "starting from ",startYear , "to " , endYear)) 
  start.time <- Sys.time()
  for (i in startYear:endYear)
  {
    for (q in 2:4)
    run(inputName, i, q)
  }
  SearchAndRescue()
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
}


readSInteger <- function()
{
  n <- readline(prompt="Enter the starting year :  ")
  return(as.integer(n))
}

readEInteger <- function()
{
  n <- readline(prompt="Enter the ending year :  ")
  return(as.integer(n))
}

readCompanyname <- function()
{
  n <- readline(prompt="Enter Full or Partial company name with right Caps :  ")
  return(paste(n))
}


getHighestDensityCal <- function(matrixData){
  
  input = matrixData[complete.cases(matrixData),]
  
  location = 0
  
  for (i in 2:nrow(input))
  {
    if ((input[i,1]-input[i-1,1])<5)
      location = input[i,1]
  }
  
  return(location)
  
  #baseRange = getBaseRange()
  
  ##for (i in 2:length(input))
  ##{
    
    
    
   # if (!is.na(input[i-1,]) && isTRUE((input[i,1]-input[i-1,1]) < baseRange)){
    #  if (input[i,2] >= 1){
   #     endAt = input[i,1]
    #  }
      
      
   # }
   # if (isTRUE(i <= (length(input)-1)))
   #  if (!is.na(input[i,]) && isTRUE((input[i,1]-input[i-1,1]) < baseRange)){
    #    if (input[i,2]>=1){
      #   startAt = input[i,1]
    #    }
        
        
    # }
    
 # }
  
  #print("the two indexes is" + endAt-startAt + "apart")
  
 # return ((endAt-startAt)/2+startAt)
}


searchWord <- function(inputString,fileDirectory){
   
   require(XML)
   require(stringi)
   
  # fileD <- file.path(fileDirectory)
    fileD <- file.path(fileDirectory)
    #  x <- fileDirectory
   page <- readLines(fileD)
    # print (page)
    
    p <- stri_locate_all(pattern = inputString, page , fixed = TRUE)
    
    return (p)
}





moveQs <- function (directory,companyName) {
  
  toLocation = paste0("./Test_Folder/10q/", companyName, ".htm")
  #toLocation = paste0("C:/Users/wsheung/Documents/Wsheung/GeneralFiles/Research/Rstudio_Scripts/SEC_extraction/Test_Folder/10q/", companyName)  # this will get the txt file instead of the htm one above
  
  setwd(directory)
  setwd(companyName)
  
  fileVectors <- list.files()
  
  for (i in 1:length(fileVectors))
  {
    if (grepl(fileVectors[i],pattern = "10q")) {
    
      x <- paste(getwd(),fileVectors[i],sep="/")
      file.rename(x,toLocation)
      
    }
    
    if (grepl(fileVectors[i],pattern = "10-q")) {
      
      x <- paste(getwd(),fileVectors[i],sep="/")
      file.rename(x,toLocation)
      
    }
  }
  
  
}

#my.file.rename <- function(from, to) {
 # todir <- dirname(to)
  #if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  #file.rename(from = from,  to = to)
#}


run <- function(inputCompanyName, inputYear, inputQuarter) {

	# downloads and gets the SEC index file, assigns it to a dataframe object


	indexFile_df <- getSECIndexFile(inputYear, inputQuarter)
	
	# get more info about indexFile_df

	# str(indexFile_df)
  
	rowLocation <- findCompanyLocation(inputCompanyName,indexFile_df)
	
	#Find the newRowLocation after comparing first search research of company name with target document code
	newRowLocation <- numeric()
	
	#if(!is.null(rowLocation))
	for(location in rowLocation){
	  #print(indexFile_df[location,1])
	  #  print(indexFile_df[location,2])
	  
	   if(indexFile_df[location,2] == "10-Q"){
	     newRowLocation <- c(newRowLocation,location)
	   }
	}
	
  #This newRowLocation vector stores the row number of target document code
	newRowLocation
	
	for(companyIndex in newRowLocation){
	 print(indexFile_df[companyIndex,1])
	 print(indexFile_df[companyIndex,2])
	# check the extension of the web address
 # webAddress <- substring((indexFile_df[newRowLocation,5]),1)
	  comName <- indexFile_df[companyIndex,1]
	  webAddress <- substring((indexFile_df[companyIndex,5]),1)
	  webAddress
	
	
	# Download .txt file
	saveAddress <- paste("./Downloads/", inputYear,"_Q", inputQuarter,"_", comName, ".txt",sep="")
	
	# Start extraction of the .txt file , following function credits to Ian Grow's blog (see above link)
	# next steps are to read the txt html
	result <- try(download.file(url=paste("http://www.sec.gov/Archives/",webAddress,sep=""),
	                            destfile=saveAddress,method="auto",quiet=TRUE)) #use quiet to suppress progress bar

	  #print(newRowLocation)
	  place <- extract.filings(paste(inputYear,"_Q", inputQuarter,"_", comName,".txt",sep=""))
    moveQs(place,paste(inputYear,"_Q", inputQuarter,"_", comName,".txt",sep=""))
	  setwd("./Test_Folder/10q")
	}
}
	# Now that our docs are together in a folder, lets try to (finally :D) extract that data

SearchAndRescue <- function()
{
  setwd("./10q")
	tenQVector <- list.files()
	#print(tenQVector)
	
	numberingTable = 1
	
	for (i in 1:length(tenQVector))
	{
	  # every file needs to be searched
	  location <- paste0("./10q/",tenQVector[i])
	  
	  listA <- searchWord("backlog",location)
	  
	  newList <- data.frame(nrow = 100, ncol = 2)
	  
	  indexrow <- 0
	  
	# for (a in 1:(length(listA)-1))
	   # {
	   # if (length(listA[[a]][,2]) > 0) ## && (length(listA[[a]][,2]) > 0)
	   # {
	      #print(a)
	    #  indexrow = indexrow + 1
	    #  newList[indexrow,1] = a
	      
	     # newList[indexrow,2] = length(listA[[a]][,2])
	      
	      
	   # }
	   
	 # }
	 # print(newList)
	  
	 rawHTML <- paste(readLines(location),collapse = "\n")
	 
	 parsed <- htmlParse(rawHTML)
	 
	 allTable <- readHTMLTable(rawHTML)
	 
	 foundInfo = FALSE
	 
	 resultTables <- c()
	 
	 for (t in 1:length(allTable))
	 {
	   tempTable = allTable[[t]]
	   #tempTable = tempTable[complete.cases(tempTable),]
	   for (r in 1:nrow(tempTable)) {
	     for (c in 1:ncol(tempTable)) {
	       tempTable[r,c] = gsub("Â","",tempTable[r,c])
	       
	       if (grepl("Backlog",tempTable[r,c])) {
	         foundInfo = TRUE
	       }
	    }
	   }
	   
	   if (foundInfo) {
	   #write.csv(tempTable,paste0("testing",numberingTable,".csv"),append = TRUE)
	   #numberingTable = numberingTable + 1
	   ## write.table(tempTable,paste0("testing",i,".csv"),append = TRUE,na="")
	   #foundInfo = FALSE
	     resultTables <- c(resultTables,tempTable)
	     
	     foundInfo = FALSE
	   }
	 }
	 
	  # After getting the location and count of each matches, now push the resulting matrix into an density calculation function to return the highest density area
	  #id = getHighestDensityCal(newList)
	  #print(id)
	 ### Scraped  ## cropTable(id,location)
	  
	  
	  
	  
	  
	}
	write.csv(tempTable,paste0("testing",numberingTable,".csv"),append = TRUE)
	#numberingTable = numberingTable + 1
	## write.table(tempTable,paste0("testing",i,".csv"),append = TRUE,na="")
	#foundInfo = FALSE
	#setwd("C:/Users/wsheung/Documents/Wsheung/GeneralFiles/Research/Rstudio_Scripts/SEC_extraction/Test_Folder/10q")
	afterRbind <- rbind(resultTables)
	afterRbind
	#write.csv(afterRbind,"Result.csv")
}

tryAsInteger = function(node) {
  val = xmlValue(node)
  ans = as.integer(gsub(",", "", val))
  if(is.na(ans))
    val
  else
    ans
}

cropTable <- function(id,inputFile) {
  require(XML)
  require(stringi)
  
  fileD <- file.path(fileDirectory)
  #  x <- fileDirectory
  page <- readLines(fileD)
  # print (page)
  
  p <- stri_locate_all(pattern = inputString, page , fixed = TRUE)
  
  return(p)
  
}


#-----------------------functions-----------------------------------



vector.isnt.empty <- function(x) return(length(x) > 0 )


findCompanyLocation <- function(companyName, inputData_df) {
  
 # companyNameDot <- paste(companyName,".")
 # vector_data <- inputData_df[,1]
 # vector_data2 <- which(apply(inputData_df,1, function(x) any(grepl(companyNameDot,x))))
  
 ### if(vector.isnt.empty(vector_data)){
  #  return(vector_data)
  #} else if(vector.isnt.empty(vector_data2)){                      /** messy stuff, but finally got the string search to work hahahaha :D
  #  return(vector_data2)                                               Still need to make sure it returns something if search failed tho
  #} else {
  #  return(return)
 # }
 #
 # return(location <- which(apply(inputData_df,2, function(x) any(grepl(companyName,x)))))
return(which(apply(inputData_df,1, function(x) any(grepl(companyName,x)))))
}

# Small function to remove leading and trailing spaces
trim <- function (string) {
  string <- enc2native(string)
  gsub("^\\s*(.*?)\\s*$","\\1", string, perl=TRUE)
}

getSECIndexFile <- function(year, quarter) {
     
    # Download file
    tf <- tempfile()
    result <- try(download.file(
        url=paste("http://www.sec.gov/Archives/edgar/full-index/",
                  year,"/QTR", quarter, "/company.zip",sep=""),
        destfile=tf,method="auto"))
     
    # no error? good to go
    if (!inherits(result, "try-error")) {
         
        # Small function to remove leading and trailing spaces
        trim <- function (string) {
            string <- enc2native(string)
            gsub("^\\s*(.*?)\\s*$","\\1", string, perl=TRUE)
        }
         
        # Read the downloaded file
        raw.data <- readLines(con=(zz<- unz(description=tf,
                                            filename="company.idx")))
        close(zz)
        raw.data <- raw.data[11:length(raw.data)] # Remove the first 10 rows.
         
        # Parse the downloaded file and return the extracted data as a data frame
        company_name <- trim(substr(raw.data,1,62))
        form_type <- trim(substr(raw.data,63,74))
        cik <- trim(substr(raw.data,75,86))
        date_filed <- as.Date(substr(raw.data,87,98))
        file_name <- trim(substr(raw.data,99,150))
        rm(raw.data)
        return(data.frame(company_name, form_type, cik, date_filed, file_name))
    } else { return(NULL)}
}


extract.filings <- function(file) {
  ## A function to extract filings from complete submission text files submitted
  ## to the SEC into the component files contained within them.
  require(XML)
  
  new_location <- file.path("./Test_Folder/", file)
  file <- file.path("./Downloads", file)
  QLocation <- file.path("./Test_Folder/10q/", file)
  # Parse the file as an XML file containing multiple documents
  webpage <- readLines(file)
  
  file.name <- gsub("<FILENAME>","", grep("<FILENAME>.*$", webpage,  perl=TRUE, value=TRUE))
  
  print(file.name)
  # If there are no file names, then the full text submission is simply a text file.
  # Rather than copying this to the new location, I just symlink it (this saves space).
  if (length(file.name)==0) { 
    if (!file.exists(new_location)) {
      dir.create(dirname(new_location), showWarnings=FALSE,
                 recursive = TRUE)
      try(file.remove(new_location))
      file.symlink(from=file, to=new_location)
    }
    return(new_location)
  } else {
      # return(dirname(new_location))
  }
  
  # If got here, we have a full-text submission that isn't simply a text file
  # We need to make the parent directory for the component files that are 
  # embedded in the submission
  file.dir <- gsub("-(\\d{2})-(\\d{6})\\.txt$", "\\1\\2", new_location, perl=TRUE)
  print(file.dir)
  dir.create(file.dir, showWarnings=FALSE, recursive=TRUE)
  
  # Get a list of file names, and their start and end locations within the
  # text file. (I use unique file names, as sometimes--albeit rarely--the
  # filename is repeated).
  file.name <- unique(file.path(file.dir, file.name))
  start.line <- grep("<DOCUMENT>.*$", webpage,  perl=TRUE) 
  end.line <- grep("</DOCUMENT>.*$", webpage,  perl=TRUE)     
  print(file.name)
  
  
  
  for (i in 1:length(file.name)) {
    # Skip the file if it already exists and the extracted file was extracted 
    # recently.
    if(file.exists(file.name[i]) && 
       as.Date(file.info(file.name[i])$ctime) > "2017-07-11") {
      next
    }
    
   
    # Get the extension of the file to be extracted
    file.ext <- gsub(".*\\.(.*?)$", "\\1", file.name[i])
    
    # Extract binary files
    if (file.ext %in% c("zip", "xls", "jpg", "gif", "xlsx")) {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^begin", temp,  perl=TRUE)
      pdf.end <- grep("^end", temp,  perl=TRUE)  
      t <- tempfile()
      writeLines(temp[pdf.start:pdf.end], con=t)
      print(paste("uudecode -o", file.name[i], t))
      system(paste("uudecode -o", file.name[i], t))
      unlink(t)
    }
    
    # Extract simple text files
    if (file.ext=="txt") {
      temp <- webpage[start.line[i]:end.line[i]]
      writeLines(temp, con=file.name[i])
    }
    
    # Extract text-based formatted file types
    if (file.ext %in% c("htm", "js", "css", "paper", "xsd")) {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^<TEXT>", temp,  perl=TRUE) +1
      pdf.end <- grep("^</TEXT>", temp,  perl=TRUE) -1  
      t <- tempfile()
      writeLines(temp[pdf.start:pdf.end], con=file.name[i])
      unlink(t)
    }
    
    # Extract PDFs
    if (file.ext=="pdf") {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^<PDF>", temp,  perl=TRUE) +1
      pdf.end <- grep("^</PDF>", temp,  perl=TRUE) -1  
      t <- tempfile()
      writeLines(temp[pdf.start:pdf.end], con=t)
      print(paste("uudecode -o", file.name[i], t))
      system(paste("uudecode -o", file.name[i], t))
      unlink(t)
    }
    
 if(FALSE){
    # Extract XBRLS :D:D:D:D:D   (**************************************************WIP right now)
    if(file.ext=="xml") {
      temp <- webpage[start.line[i]:end.line[i]]
      pdf.start <- grep("^<XBRL>", temp, perl=TRUE) +1
      pdf.end <- grep("</XBRL>", temp, perl=TRUE) -1
      t <- tempfile()
      try(writeLines(temp[pdf.start:pdf.end], con=file.name[i]))
      unlink(t)
      
    }
    }
    
  }
  return(dirname(new_location))
}