#Script for fairy, genie, monster, and moving to the next floor events

#Function for fairy event
fairy_event = function(castle_data,player){
  #Add space between previously printed screen and new screen
  cat(rep("\n", 50))
  #cat with \n can be used here; however, unlike print,
  #using makes the printed screen look aesthetically unpleasing
  #numbers that are printed along with the board are moved aside to allow for the text
  #that does not happen with print
  print('You encountered a fairy!',quote = F)
  #Fairies increase health
  print('Your HP increased by 10 points.',quote = F)
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
  print(castle_data$castle[,,player$floor], quote = F)
  player$hp = player$hp + 10
  print(paste('New HP:',player$hp), quote = F)
  #Add pause to allow player to read information
  Sys.sleep(1)
  #Adding back door and a zero in dataframe
  castle_data$castle[player$movement_coordinate] = '\U1F6AA'
  castle_data$dataframe[player$castle_dataframe_row,5] = 0
  #Return information
  fairy_event_output = c(castle_data,player)
  return(fairy_event_output)
}
#Function for genie event
genie_event = function(castle_data,player){
  cat(rep("\n", 50))
  print('You encountered a genie!',quote = F)
  print('Your attack range increased by 2 points.',quote = F)
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
  print(castle_data$castle[,,player$floor], quote = F)
  player$attack_range = player$attack_range + 2
  #Genies inclease attack range
  print(paste('New attack range: ', paste0(min(player$attack_range),':',max(player$attack_range))), quote = F)
  Sys.sleep(1)
  #Adding back door and a zero in dataframe
  castle_data$castle[player$movement_coordinate] = '\U1F6AA'
  castle_data$dataframe[player$castle_dataframe_row,5] = 0
  #Return information
  genie_event_output = c(castle_data,player)
  return(genie_event_output)
}
#Function for monster event
monster_event = function(castle_data,player){
  cat(rep("\n", 50))
  #Go to dataframe and extract 
  castle_data$castle[player$movement_coordinate] = player$encountered_object
  print('You encountered a monster!', quote = F)
  print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
  print(castle_data$castle[,,player$floor], quote = F)
  #Display player information and monster hp that was extracted from the dataframe
  print(paste('Your HP: ',player$hp), quote = F)
  monster_hp = castle_data$dataframe[player$castle_dataframe_row,'hp']
  print(paste('Monster HP: ', monster_hp),quote = F)
  
  #An argument = length 0 issue occurs if this variable is left empty 
  player_action = 'a'
  #While loop so that player can engage with monster unless they decide to run, they faint, or they win
  while(!(player_action %in% c('r','run') | monster_hp == 0 | player$hp == 0)){
    print('Would you like to attack(a) or run(r)? ', quote = F)
    player_action = tolower(keypress(block = T))
    while(!(player_action %in% c('r','run','attack','a'))){
      player_action = tolower(keypress(block = T))
    }
    if(player_action == 'attack' | player_action == 'a'){
      event_output = monster_combat_event(castle_data,player,monster_hp)
      castle_data = event_output[1:3]
      player = event_output[[4]]
      monster_hp = castle_data$dataframe[player$castle_dataframe_row,'hp']
    }
  }
  #if player decides to run
  if(isTRUE(player_action %in% c('r','run')) == T){
    print('You decided to run.')
    castle_data$castle[player$movement_coordinate] = '\U1F6AA'
  }
  monster_event_output = c(castle_data,player)
  return(monster_event_output)
  
}
#Function for monster combat
monster_combat_event = function(castle_data,player,monster_hp){
  cat(rep("\n", 50))
  print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
  print(castle_data$castle[,,player$floor], quote = F)
  print('You decided to attack',quote = F)
  player$attack_power = sample( player$attack_range,1)
  print(paste('You dealt',player$attack_power, 'points of damage'),quote = F)
  #Monster hp set to zero to exit loop if attack > than monster hp
  if(monster_hp - player$attack_power <= 0){
    castle_data$dataframe[player$castle_dataframe_row,'hp'] = monster_hp = 0
    print('The monster fainted. You won!',quote = F)
    Sys.sleep(1)
    #Adding back door and a zero in dataframe
    castle_data$castle[player$movement_coordinate] = '\U1F6AA'
    castle_data$dataframe[player$castle_dataframe_row,5] = 0
    if(player$monster_threshold > 0){
      player$monster_threshold = player$monster_threshold - 1
    }
  }
  else{
    #If monster lives it gets to attack 
    #attack power is random
    castle_data$dataframe[player$castle_dataframe_row,'hp'] = monster_hp =  monster_hp - player$attack_power
    monster_attack = sample(1:5,1)
    player$hp = player$hp - monster_attack
    print(paste('Monster dealt', monster_attack, 'points of damage'),quote = F)
    
    if(player$hp <= 0){
      #player health set to zero if monster attack > than player hp
      player$hp = 0
      print('You died.', quote = F)
    }
  }
  #Print health information
  print('', quote = F)
  print(paste('Monster HP: ', monster_hp), quote = F)
  print(paste('Your HP: ', player$hp), quote = F)
  
  monster_combat_event_output = c(castle_data,player)
  return(monster_combat_event_output)
}


