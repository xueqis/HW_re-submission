#MATH 510
#HW 8
#C&D

#For your information, the HW 8 part starts from line 400

#1.
#A frequency table for every categorical and logical variable

freq_table <- function(data){
  #define a function that accepts a dataframe as the parameter
  tb1 <- lapply(data.frame(data[,sapply(data,is.logical)]),table)
  #apply the lapply() function here to list the frequencies of 
  #each logical variable in a table
  tb2 <- lapply(data.frame(data[,sapply(data,is.factor)]),table)
  #apply the lapply() function here to list the frequencies of 
  #each categorical variable in a table
  return(list(tb1,tb2))
  #return the frequency table
}

data(diamonds)
#call the dataframe diamonds for future use

#check
#freq_table(diamonds)

#2. 
#for numerical variables
#(a)
summary_stat <- function(data){
  #define a function that accepts a dataframe as the parameter
  num <- Filter(is.numeric,data)
  #filter the numeric variables
  return(summary(num))
  #apply the summary() to get the summary statistics table 
  #for each numerical variables
  #return the summary table
}

#check
#summary_stat(diamonds)

#(b)

num_rsquare <- function(num) {
  #define a function that accepts the numeric variables 
  #as the parameter
  if (ncol(num) <= 1){
    OP ='We cannot compute pairwise R spuare value because the numeric variables are not enough for computing'
  }else{
    colna <- colnames(num)
    #take out the column names
    com_num <- combn(colna, 2) 
    #combine the names pairwise
    VP <- c()
    R2 <- c()
    for(i in 1:ncol(com_num)){
      temp1 <- paste(com_num[1,i],com_num[2,i],sep = '-')
      VP <- c(VP,temp1)                                   
      #write the pairwise names and add them in vector
      a<- num[,com_num[1,i]]
      b<- num[,com_num[2,i]]
      model <- lm(a~1+b)
      #use linear regression model to calculate the r-squared value
      temp2 <- summary(model)["r.squared"]                
      #take out corresponding r-square
      R2 <- c(R2,as.numeric(temp2))
    }
    OP <- data.frame(VP,R2)                       
    #create a dataframe and change its name
    colnames(OP) <- c("Variable Pairs","R-square")
  }
  return(OP)
  #return the table
}





#(c)

num_coe <- function(num, threshold = 0) {
  #define a function that accepts the numeric variables and 
  #a coefficient threshold as parameters.
  #here, we set the default threshold as 0
  if (ncol(num) <= 1){
    COP ='We cannot compute the pairwise correlation coefficient because the numeric variables are not enough for computing'
  }else{
    colna <- colnames(num) 
    #take out all the variables' names
    com_num <- combn(colna, 2) 
    #combine the names pairwise
    VP2 <- c()
    Cor <- c()
    #create two vectors for future use
    for(i in 1:ncol(com_num)){
      temp1 <- paste(com_num[1,i],com_num[2,i],sep = '-')  
      #write pairwise names
      corr <- cor(num[,com_num[1,i]],num[,com_num[2,i]],method = "pearson") 
      #get the corresponding Pearson correlation coefficient
      if(corr >= threshold){
        VP2 <- c(VP2,temp1)
        Cor <- c(Cor,corr)  
        #if the correlation coefficient is greater than the given
        #threshold then add them to vectors
      }
    }
    COP <- data.frame(VP2,Cor) 
    #create a dataframe for pairwise names and corresponding
    #correlation coefficients
    colnames(COP) <- c("Variable Pairs","Pearson Exceeds Threshold")
    #rename the dataframe
  }
  return(COP)
  
}






#3
#plot histograms

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #set the default layout as NULL
  plots <- c(list(...), plotlist)
  #make a list from the ... arguments and the plotlist
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    #if layout is NULL, then use 'cols' to determine layout
    #create a panel
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #where the ncol is the number of columns of plots
    #where the nrow is the number of rows needed, calculated from 
    #the numbers of cols
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    #set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    #make each plot, in the correct location
    for (i in 1:numPlots) {
      #get the i,j matrix positions of the regions that contain 
      #this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


plot_density_count <- function(num,switch="on",vector=NULL){
  #define a function that accepts the numeric variables, switch
  #options, and vectors as parameters, where the default switch 
  #option is set to be "on"
  require(ggplot2)
  require(grid)
  if(ncol(num)==0){
    return ("There is no numeric variables")
  }else{
    if(switch == "on"){
      if(!is.null(vector)){ 
        for(j in 1:length(vector)){
          for(i in 1:ncol(num)){
            
            mean <- mean(num[,i]) 
            #compute the mean of each numeric variable
            p1 <- ggplot(num,aes(x=num[i]),color = "blue")+
              geom_histogram(fill="blue",bins=vector[j])+
              ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
              xlab(colnames(num[i]))+
              geom_vline(xintercept = mean,col="red")  
            #use ggplot to plot histograms of count and add a 
            #vetical red line on the graph
            
            p2 <- ggplot(num,aes(x=num[i],..density..))+
              geom_histogram(fill="blue",bins=vector[j])+
              ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
              xlab(colnames(num[i]))+
              geom_vline(xintercept = mean,col="red") 
            #use ggplot to plot histograms of density and add a 
            #vetical red line on the graph
            
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
            title <- paste(colnames(num[i]),vector[j],sep=" bin=")
            grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
            print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
            print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) 
            #use grid package to separate the picture and plot 
            #two histograms for each numeric variables
            
          }
        }
      }else{ 
        for(i in 1:ncol(num)){
          
          mean <- mean(num[,i]) 
          #compute the mean of each numeric variable
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+
            geom_histogram(fill="blue")+
            ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red")  
          #use ggplot to plot histograms of count and add a 
          #vetical red line on the graph
          p2 <- ggplot(num,aes(x=num[i],..density..))+
            geom_histogram(fill="blue")+
            ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          #use ggplot to plot histograms of density and add a 
          #vetical red line on the graph
          
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),"default bins",sep=" bins=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) 
          #use grid package to separate the picture and plot 
          #two histograms of each numeric variables
          
        }
        
      }
      
    }else{
      if(switch == "grid"){
        if(!is.null(vector)){
          for(j in 1:length(vector)){
            grid.newpage()
            his_count <-list()
            his_density <- list()  
            #create two empty list for storing pictures later
            for(i in 1:ncol(num)){
              mean <- mean(num[,i])
              his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") +
                geom_histogram(fill="blue", bins = vector[j])+
                labs(title= paste(vector[j], "bins"))+ 
                
                geom_vline(xintercept = mean,col="red")
              #use ggplot to plot histograms of count and add a 
              #vetical red line on the graph
              #store the plot in the list
            }
            
            multiplot(plotlist = his_count, cols = 2)  
            #draw all histograms of count with same bins in 
            #one picture
            
            for(i in 1:ncol(num)){
              mean <- mean(num[,i])
              his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") +
                geom_histogram(aes(y= ..density..), fill="blue", bins = vector[j])+
                labs(title= paste(vector[j], "bins"))+ #draw histograms of density and store them in list
                geom_vline(xintercept = mean,col="red")
              #use ggplot to plot histograms of density and add a 
              #vetical red line on the graph
              #store the plot in the list
            }
            multiplot(plotlist = his_density, cols = 2)  
            #draw all histograms of density with same bins in 
            #one picture
          }
        }else{
          grid.newpage()
          his_count <-list()
          his_density <- list()  
          #create two empty list for storing pictures later
          for(i in 1:ncol(num)){
            mean <- mean(num[,i])
            his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") +
              geom_histogram(fill="blue")+
              labs(title= 'default bins')+ 
              geom_vline(xintercept = mean,col="red")
            #use ggplot to plot histograms of count and add a 
            #vetical red line on the graph
            #store the plot in the list 
          }
          multiplot(plotlist = his_count, cols = 2)  
          #draw all histogram of count with same bins in one picture
          
          for(i in 1:ncol(num)){
            mean <- mean(num[,i])
            his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") +
              geom_histogram(aes(y= ..density..), fill="blue")+
              labs(title= 'default bins')+ 
              #draw histograms of density and store them in list
              geom_vline(xintercept = mean,col="red")
            #use ggplot to plot histograms of density and add a 
            #vetical red line on the graph
            #store the plot in the list 
          }
          multiplot(plotlist = his_density, cols = 2)
        }
        
      }
    }
  }
}


#4
#graphs for every categorical and binary variables

is.binary <- function(v) {
  #define a function that accepts vectors as parameter
  #check if the vector is binary 
  #return True if the vector is binary, otherwise return false
  x <- unique(v)                    
  #remove duplicate elements
  length(x) - sum(is.na(x)) == 2L         
  #check if x only contains 2 distinct values
}

plot_categ_binary <- function(data,switch){
  #define a function that accepts a dataframe and switch options
  #as parameter
  data1 <- data[,sapply(data,is.factor)]
  data2 <- data[,sapply(data,is.logical)]
  data3 <- data[,sapply(data,is.binary)]
  data <- data.frame(data1,data2,data3)
  #constitute a new data frame that only accepts factor, logical,
  #and binary variables
  if(ncol(data)==0){
    return("There is no factor or logical or binary variables")
  }else{
    if(switch=="on"|| switch=="grid"){
      for(i in 1:ncol(data)){
        p <- ggplot(data,aes(x=data[,i]))+
          geom_bar(fill='gray')+
          xlab(colnames(data)[i])
        #use ggplot to plot binary and categorical variables
        print(p)
      }
    }
  }
}


#finally
explore <- function(data,switch = "on",threshold = 0,vector = NULL){
  #define a function that accepts four variables as parameters, 
  #which are (1). a dataframe, (2). a plot switch	that can accept	
  #three values: off,	on,	or grid, (3). a threshold cut-off value 
  #between 0 and 1 for correlations, and (4). an optional 
  #vector	that contains	one	or more	integers that	represent	
  #the numbers of bins to use for a histogram. If the vector	
  #is not	provided,	then let ggplot use its default
  
  num <- Filter(is.numeric,data) 
  #filter the dataframe and take out all numeric variables
  output <- list(freq_table(data),
                 #create a list that calls function freq_table(data), which returns
                 #a frequency table for each logical and categorical variables
                 
                 summary_stat(data),
                 #calls function summary_stat(data), which returns a
                 #summary statistics table for each numerical variable
                 
                 num_rsquare(num), 
                 #calls function num_rsquare(num) that returns a dataframe
                 #that contains pairwise names of variables and their 
                 #r-square values
                 
                 num_coe(num,threshold), 
                 #calls function num_coe(num, threshold), which returns a
                 #dataframe that contains pairwise names of variables and
                 #their pearson correlation coefficient
                 plot_density_count(num,switch,vector),
                 plot_categ_binary(data,switch))
  
  
  return(output)
  #return the output
}


#check
#explore(diamonds, "on", 0.9, 30)




#HW8
explore2 <- function(data,switch = 'on', threshold = 0, vector = NULL){
  #this function is the updated version of the function explore()
  #this function accepts four variables as parameters, 
  #which are (1). a dataframe, (2). a plot switch	that can accept	
  #three values: off,	on,	or grid, (3). a threshold cut-off value 
  #between 0 and 1 for correlations, and (4). an optional 
  #vector	that contains	one	or more	integers that	represent	
  #the numbers of bins to use for a histogram. If the vector	
  #is not	provided,	then let ggplot use its default
  
  data_frame <- na.omit(data) 
  #omit the whole row if there are any nas
  
  if(!is.data.frame(data_frame)){
    #check if the first parameter is a dataframe
    #if it is not, transform it to be a dataframe
    data_frame <- as.data.frame(data_frame)  
  }
  
  
  while(switch != "off" && switch != "on" && switch != "grid"){        #check if the input switch option is valid, 
  #if not, let the user know and ask to re-enter the input
    print("Invalid input for switch")
    switch <- readline(prompt="Enter your option(off / on / grid): ")     
  }
  
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){     #check if threshold is a valid input,
  #if not, let the user know and ask to re-enter the input
    print("Correlation threshold must be numeric and in range [0,1]")
    threshold <- as.numeric(readline(prompt="Enter your correlation threshold: "))   
  }
  
  if(!is.null(vector)){
    if(!is.numeric(vector)||(is.numeric(vector) && (TRUE %in% (vector <= 0)))){ 
      #check if bin vector is all numeric positive, 
      #if the input is not valid, ask the user to re-enter the input
      print("the bins vector must be numeric and positive vector, please enter new bins one by one and press 'enter' keep running")
      vector <- c()
      bin <- 1
      if(bin != ""){  
      #input "enter" to keep running the loop
        vector <- readline(prompt="Enter the number of bins: ")->vector
        vector <- as.numeric(vector)
        vector <- c(vector, bin1)
      }
      vector <- na.omit(vector) 
      #omit the NAs
    }
    
    #if the inputed bin vector is not an integer, round it
    if (!is.integer(vector)) {            
      
      vector <- round(vector)
    }
  }
  return(explore(data,switch,threshold,vector))
}

#check
#explore2(diamonds,1,2,-20)


