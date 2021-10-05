#function for returning normalized line weights from randomized ena datasets

ena.random.lws = function(dataset,
                          speakerCol,
                          codeCols,
                          percent,
                          unitCols,
                          convoCols,
                          metaCols,
                          model,
                          window){
  
  #set line id
  dataset$line.id = seq(1:nrow(dataset))
  
  #split data by convo
  dataset$split = do.call(paste, c(dataset[convoCols], sep="-"))
  split.dat = split(dataset,dataset[,"split"])
  
  #randomize lines and codes
  rand.by.convo = lapply(split.dat, ena.randomize.data,speakerCol = speakerCol,codeCols = codeCols,percent = percent, lineID = "line.id")
  rand.data = do.call("rbind",rand.by.convo)
  rand.data = rand.data[order(rand.data$line.id),]
  #print("randomization for this run complete")
  
  #get adjacency vecs
  rand.accum = ena.accumulate.data(units = rand.data[,unitCols],
                        conversation = rand.data[,convoCols],
                        codes = rand.data[,codeCols],
                        metadata = rand.data[,metaCols],
                        model = model,
                        window.size.back = window)
  
  rand.accum = rand.accum$connection.counts
  rand.accum = rand.accum %>%
    as.matrix() %>% 
    rENA::fun_sphere_norm()
}
  
#replicate function above reps times
rep.random.lws = function( dataset,
                           speakerCol,
                           codeCols,
                           percent,
                           unitCols,
                           convoCols,
                           metaCols,
                           model,
                           window,
                           reps){
  replicate(n = reps,exp = ena.random.lws(dataset = dataset,
                                          speakerCol = speakerCol,
                                          codeCols = codeCols,
                                          percent = percent,
                                          unitCols = unitCols,
                                          convoCols = convoCols,
                                          metaCols = metaCols,
                                          model = model,
                                          window = window),
  simplify = FALSE)
}
  