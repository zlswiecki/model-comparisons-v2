#function for randomizing a given percentage of discourse data
#data is typically split into conversations prior to using this function
#shuffle-based implementation

ena.randomize.data = function(dataset,
                                  speakerCol,
                                  codeCols,
                                  percent,
                                  lineID){
  
  if(percent == 0){
    return(dataset)
  }
  else{
    #pull out lines for randomization
    line.num = round((percent/100) * nrow(dataset))
    line.ids = sample(x = seq(1:nrow(dataset)),size = line.num)
    rand.data = dataset[-line.ids,]
    lines = dataset[line.ids,]
    lines = lines[order(lines[,lineID]),]
    
    #shuffle speakers
    lines[,speakerCol] = sample(lines[,speakerCol],length(lines[,speakerCol]))
    
    #shuffle codes by speaker
    temp = lines[,c(speakerCol,codeCols,lineID)]
    temp = split(x = temp, f = temp[,speakerCol])
    
    for(i in 1:length(temp)){
      new.codes = temp[[i]][,codeCols]
      new.codes = sapply(codeCols,function(i) sample(x = new.codes[[i]],size = nrow(new.codes)))
      temp[[i]][,codeCols] = new.codes
    }

    #assign shuffled codes to shuffled speakers
    new.lines = do.call("rbind",temp)
    new.lines = new.lines[order(new.lines[,lineID]),]
    lines[,codeCols] = new.lines[,codeCols]
    
    #put randomized data back in the dataset
    rand.data = rbind(rand.data,lines)
    
    #sort by line id
    rand.data = rand.data[order(rand.data[,lineID]),]
    
    return(rand.data)

  }
  
}

