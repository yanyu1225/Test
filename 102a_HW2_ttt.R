#Sherry(Yan) Yu
#104802411


# The `state` of the game can be stored as a character vector of length 9. 
# I used NA for spots that were unplayed, and entered "x" and "o" as the game progressed.

state <- rep(NA,9)

# When I created the game, I created the following functions:

# display(state)  
# displays the current state of the board.

display <- function(state){
  for(i in 1:9){
    if(is.na(state[i] == TRUE))
    {
      state[i] <- i
    }
  }
  cat("",state[1],"|",state[2],"|",state[3],"\n")
  cat("---+---+---","\n")
  cat("",state[4],"|",state[5],"|",state[6],"\n")
  cat("---+---+---","\n")
  cat("",state[7],"|",state[8],"|",state[9],"\n")
}

# update(state, who, pos)  
# updates the state of the board by putting an x or o (who) 
# in the designated position (pos) 

update <- function(state, who, pos){
  if(!is.na(match(state[pos],c("x","o")))){
    cat("This spot has been taken. Please choose another position")
  }
  else{
    state <- replace(state, pos, who)
  }
  return(state)
}




# computer_turn(state)  
# has the computer take a turn. The input is the state.
# The function returns the position where the computer will play.

computer_turn <- function(state)
  {
  signal <- NA
  if((sum(is.na(match(state,c("x","o"))))%%2) == 0){
    signal<-"o"
  }
  if((sum(is.na(match(state,c("x","o"))))%%2) == 1){
    signal<-"x"
  }
  
  for(i in 1:9){
    if(is.na(state[i])){
      state[i]<-i
    }
  }
  position <- rep(NA,9)
  for(a in 1:9){
    if(state[a]!="x"&&state[a]!="o"){
      position[a] <-a
    }
  }
  
  
  win <- list(c(1,2),c(1,3),c(2,3),
              c(4,5),c(4,6),c(5,6),
              c(7,8),c(7,9),c(8,9),
              c(1,4),c(1,7),c(4,7),
              c(2,5),c(2,8),c(5,8),
              c(3,6),c(3,9),c(6,9),
              c(1,5),c(1,9),c(5,9),
              c(3,5),c(3,7),c(5,7))
  cor <- c(3,2,1,6,5,4,9,8,7,7,4,1,8,5,2,9,6,3,9,5,1,7,5,3)
  
  if (signal == "x"){
    for (p in 1:24){
      if (state[win[[p]][1]] == "x" 
          && state[win[[p]][2]] == "x" 
          && state[cor[p]] != "x" 
          && state[cor[p]] != "o")
      {
        state[cor[p]] <- "x"
        return(state)
        stop()
      }
      p <- p+1
    }
    for (p in 1:24){
      if (state[win[[p]][1]] == "x" 
          && state[win[[p]][2]] == "x" 
          && state[cor[p]] != "o" 
          && state[cor[p]] != "x"){
        state[cor[p]] <- "x"
        return(state)
        stop()
      }
      p <- p+1
    }
    
    
    for (q in 1:24){
      if (state[win[[q]][1]] == "o" 
          && state[win[[q]][2]] == "o" 
          && state[cor[q]] != "x" 
          && state[cor[q]] != "o"){
        state[cor[q]] <- "x"
        return(state)
        stop()
      }
      q <- q+1
    }
    
    for (q in 1:24){
      if (state[win[[q]][1]] == "o" 
          && state[win[[q]][2]] == "o" 
          && state[cor[q]] != "o" 
          && state[cor[q]] != "x"){
        state[cor[q]] <- "x"
        return(state)
        stop()
      }
      q <- q+1
    }
    
    position <- position[! position %in% c(NA)]
    state[as.numeric(sample(position, size = 1))] <- "x"
    return(state)
    stop()
  }
  
  else if (signal == "o"){
    for (i in 1:24){
      if (state[win[[i]][1]] == "o" 
          && state[win[[i]][2]] == "o" 
          && state[cor[i]] != "x" 
          && state[cor[i]] != "o")
      {
        state[cor[i]] <- "o"
        return(state)
        stop()
      }
      i <- i + 1
    }
    for (i in 1:24){
      if (state[win[[i]][1]] == "o" 
          && state[win[[i]][2]] == "o" 
          && state[cor[i]] != "o" 
          && state[cor[i]] != "x"){
        state[cor[i]] <- "o"
        return(state)
        stop()
      }
      i <- i + 1
    }
    
    for (j in 1:24){
      if (state[win[[j]][1]] == "x" 
          && state[win[[j]][2]] == "x" 
          && state[cor[j]] != "x" 
          && state[cor[j]] != "o"){
        state[cor[j]] <- "o"
        return(state)
        stop()
      }
      j <- j + 1
    }
    
    for (j in 1:24){
      if (state[win[[j]][1]] == "x" 
          && state[win[[j]][2]] == "x" 
          && state[cor[j]] != "o" 
          && state[cor[j]] != "x"){
        state[cor[j]] <- "o"
        return(state)
        stop()
      }
      j <- j + 1
    }
    
    position <- position[! position %in% c(NA)]
    state[as.numeric(sample(position, size = 1))] <- "o"
    return(state)
    stop()
  }
}


# check_winner(state)   
# checks if there is a winner.

check_winner<- function(state){
  for(i in 1:9){
    if(is.na(state[i]) == TRUE){
      state[i] <- i
    }
  }
  triples <- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7)
  )
  if ((state[triples[[1]][1]] == "x" && state[triples[[1]][2]] == "x" && state[triples[[1]][3]] == "x")
      ||(state[triples[[2]][1]] == "x" && state[triples[[2]][2]] == "x" && state[triples[[2]][3]] == "x")
      ||(state[triples[[3]][1]] == "x" && state[triples[[3]][2]] == "x" && state[triples[[3]][3]] == "x")
      ||(state[triples[[4]][1]] == "x" && state[triples[[4]][2]] == "x" && state[triples[[4]][3]] == "x")
      ||(state[triples[[5]][1]] == "x" && state[triples[[5]][2]] == "x" && state[triples[[5]][3]] == "x")
      ||(state[triples[[6]][1]] == "x" && state[triples[[6]][2]] == "x" && state[triples[[6]][3]] == "x")
      ||(state[triples[[7]][1]] == "x" && state[triples[[7]][2]] == "x" && state[triples[[7]][3]] == "x")
      ||(state[triples[[8]][1]] == "x" && state[triples[[8]][2]] == "x" && state[triples[[8]][3]] == "x"))
  {
    return(TRUE)
    cat("Player x wins! ")
  } 
  else if ((state[triples[[1]][1]] == "o" && state[triples[[1]][2]] == "o" && state[triples[[1]][3]] == "o")
           ||(state[triples[[2]][1]] == "o" && state[triples[[2]][2]] == "o" && state[triples[[2]][3]] == "o")
           ||(state[triples[[3]][1]] == "o" && state[triples[[3]][2]] == "o" && state[triples[[3]][3]] == "o")
           ||(state[triples[[4]][1]] == "o" && state[triples[[4]][2]] == "o" && state[triples[[4]][3]] == "o")
           ||(state[triples[[5]][1]] == "o" && state[triples[[5]][2]] == "o" && state[triples[[5]][3]] == "o")
           ||(state[triples[[6]][1]] == "o" && state[triples[[6]][2]] == "o" && state[triples[[6]][3]] == "o")
           ||(state[triples[[7]][1]] == "o" && state[triples[[7]][2]] == "o" && state[triples[[7]][3]] == "o")
           ||(state[triples[[8]][1]] == "o" && state[triples[[8]][2]] == "o" && state[triples[[8]][3]] == "o"))
  {
    return(TRUE)
    cat("Player o wins! ")
    
  }
  else
  {
    return(FALSE)
  }
}


check_draw <- function(state){
  if (sum(is.na(match(state, c("x","o")))) == 0 && check_winner(state) == FALSE)
  {
    cat("The game ends in a draw. ")
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}


# play() # the 'wrapping' function that lets you play a game by combining the above functions.

play <- function(){
  state <- rep(NA,9)
  num <- readline("How many human players? ")
  if(num == "1"){
    order <- readline("Do you want to play first or second? (type 1 or 2): ")
    if(order == "1"){
      display(state)
      
      while(!check_winner(state)){
        put_x <- readline("Where should x play: ")
        if(is.na(match(put_x,1:9))){
          cat("Not a valid number. ")
          put_x <- readline("Where should x play: ")
        }
        while(identical(state,update(state,"x",as.integer(put_x))) == TRUE){
          put_x <- readline("Where should x play: ")
        }
        state <- update(state, "x", as.integer(put_x))
        display(state)
        
        if(check_winner(state)){
          break
        }
        if(check_draw(state)){
          break
        }
        
        if(!check_winner(state)){
          state <- computer_turn(state)
          display(state)
          
          if(check_winner(state)){
            break
          }
          if(check_draw(state)){
            break
          }
        }				
        
      }
    }
    
    
    else if (order == "2"){
      display(state)
      
      while(!check_winner(state)){
        state <- computer_turn(state)
        display(state)
        if(check_winner(state)){
          break
        }
        if(check_draw(state)){
          break
        }
        if (!check_winner(state)){
          put_o <- readline("Where should o play: ")
          if (is.na(match(put_o,1:9))){
            cat("Not a valid number. ")
            put_o <- readline("Where should o play: ")
          }
          while(identical(state,update(state,"o", as.integer(put_o))) == TRUE){
            put_o <- readline("Where should o play: ")
          }
          state <- update(state,"o", as.integer(put_o))
          display(state)
          
          if(check_winner(state)){
            break
          }
          if(check_draw(state)){
            break
          }
        }
      }
    }
    
    
    else
    {
      break
    }
  }
  
  
  
  else if (num == "2"){
    display(state)
    
    while(!check_winner(state)){
      bi_x <- readline("Where should x play: ")
      if (is.na(match(bi_x,1:9))){
        cat("Not a valid number. ")
        bi_x <- readline("Where should x play: ")
      }
      while(identical(state,update(state,"x", as.integer(bi_x))) == TRUE){
        bi_x <- readline("Where should x play: ")
      }
      state <- update(state, "x", as.integer(bi_x))
      display(state)
      
      if(check_winner(state))
      {
        break
      }
      if(check_draw(state))
      {
        break
      }
      
      
      if (!check_winner(state))
      {
        bi_o <- readline("Where should o play: ")
        if (is.na(match(bi_o,1:9))){
          cat("Not a valid number. ")
          bi_o <- readline("Where should o play: ")
        }
        while(identical(state,update(state,"o", as.integer(bi_o))) == TRUE){
          bi_o <- readline("Where should o play: ")
        }
        state <- update(state,"o", as.integer(bi_o))
        display(state)
        
        if(check_winner(state))
        {
          break
        }
        if(check_draw(state))
        {
          break
        }
      }
    }
  }
  
  else{
    cat("Not a valid number of human players. ")
    break
  }
  
}

play()

