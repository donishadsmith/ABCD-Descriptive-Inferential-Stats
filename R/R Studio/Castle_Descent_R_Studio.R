#Script to play the actual game
#Objective is to get to the bottom of the castle and find the exit while avoiding the zombie
source('Castle_Create.R')
source('Events.R')
source('API.R')
source('Classes.R')
#The dot hides variable from global environment
#Variable used to determine if the player is greeted with the 
#welcome screen or not
.iteration <<- 0

castle_descent = function(){
  
  #Using castle-create to generate a new random game board
  castle_data = castle_create()
  #creating a class for the player
  player = player_class(current_coordinate = which(castle_data$castle=='\U1F93A', arr.ind = T),
                        hp = 100,
                        attack_range=5:10,
                        #wasd controls will be added to the players current 3d coordinate to 
                        #obtain a new coordinate corresponding to the direction
                        movement_dict = list('w' = c(-1,0,0), 'a' = c(0,-1,0), 's' = c(1,0,0),  'd' = c(0,1,0)),
                        floor = 1, 
                        total_floors = length(castle_data$castle[1,1,]),
                        encountered_object = ' ',
                        max_velocity = 0,
                        acceleration = 0,
                        previous_velocity = 0,
                        previous_game_update_time = 0,
                        monster_threshold = round(length(which(castle_data$dataframe['z'] == 1 & castle_data$dataframe['object']=='\U1F479'))*0.60,0))
  
  zombie = zombie_class(current_coordinate = which(castle_data$castle=='\U1F9DF', arr.ind = T),
                        movement_dict = list('w' = c(-1,0,0), 'a' = c(0,-1,0), 's' = c(1,0,0),  'd' = c(0,1,0),
                                             'diag_up_left' = c(-1,-1,0), 'diag_up_right' = c(-1,1,0),
                                             'diag_down_left' = c(1,-1,0), 'diag_down_right' = c(1,1,0)),
                        floor = 1
  )
  zombie$distance_to_player = zombie$chebyshev_distance(zombie$current_coordinate,player$current_coordinate)
  #######################################Setup Complete########################################
  #if the player decides to play a new game, 
  #they do not need to be greeted with the welcome screen and objective agin
  
  if(.iteration == 0){
    print('Welcome to Castle Descent!', quote = F)
    Sys.sleep(1)
    print('The objective of the game is to descend the bottom of the castle while avoiding the zombie.', quote = F)
    Sys.sleep(1)
    print('You will be starting at the top of the castle!', quote = F)
    Sys.sleep(1)
  }
  else{
    cat(rep("\n", 50))
    print('New game.', quote = F)
    Sys.sleep(1)
  }
  
  while(!(player$encountered_object=='\U2395' & player$floor == player$total_floors | player$hp <= 0 | zombie$distance_to_player == 0)){
    
    cat(rep("\n", 50))
    print(paste('Floor', player$floor, 'of',player$total_floors), quote = F)
    #If player kills a certain number of monsters, the stairs or exit is revealed
    object = ''
    if(player$monster_threshold == 0){
      #Add multiple spaces so that only a single print out of the game board is on the screen 
      #Obtain coordinates for downstairs or the exit depending on player's current floor
      get_coord_object = castle_data$dataframe[which(castle_data$dataframe$z == player$floor & castle_data$datafram$object %in% c('DS','\U2395')),1:4]
      coord = get_coord_object[1:3]
      object = get_coord_object[4]
      castle_data$castle[coord[[1]],coord[[2]],coord[[3]]] = '\U2395'
    }
    
    print(castle_data$castle[,,player$floor], quote = F)
    switch (object[[1]],
            'DS' = {print('The location of the stairs has been revealed!', quote = F)},
            '\U2395' = {print('The location of the exit has been revealed!', quote = F)},
            print(paste(player$monster_threshold, 'required to defeat to reveal the door.'), quote = F)
    )
    
    #Get time before the game updates
    player$stimulus_time = as.numeric(Sys.time())
    read_console_player_movement_action()
    #Get the movement coordinate which is the sum of the player coordinate and the vector in the movement dictionary
    #corresponding to the valid player action
    player$movement_coordinate = player$current_coordinate + player$movement_dict[[player_action]]
    #Allow player to appear on the opposite end of the grid if the coordinate is out of bounds
    if(length(dimension <- which(player$movement_coordinate[1:2] %in% c(0,castle_data$castle_length + 1))) > 0){
      min = 0
      max = castle_data$castle_length + 1
      bound = which(c(min,max) == player$movement_coordinate[dimension])
      if(length(bound) > 0){
        if(bound == 1){
          player$movement_coordinate[dimension] = max - 1
        }
        else{
          player$movement_coordinate[dimension] = max - (max -1)
        }
      }
    }
    #Get changed dimension
    player$coordinate_difference = (player$movement_coordinate - player$current_coordinate)[1:2]
    player$changed_dimension = which(player$coordinate_difference != 0)
    player$coordinate_difference  = player$coordinate_difference[player$changed_dimension]
    #Get the encountered object, depending on whether the object is an empty space, or zombie, or needs a dataframe search
    player$encountered_object = castle_data$castle[player$movement_coordinate]
    if(player$encountered_object %in% c('\U1F6AA','\U2395')){
      player$castle_dataframe_row = which(castle_data$dataframe$x == player$movement_coordinate[1] & castle_data$dataframe$y == player$movement_coordinate[2] & castle_data$dataframe$z == player$movement_coordinate[3])
      number = castle_data$dataframe[player$castle_dataframe_row,'hp']
      if(number > 0){
        player$encountered_object = castle_data$dataframe[player$castle_dataframe_row,'object']
      }
    }
    #Update player and zombie location if encountered object is an empty space or zombie
    if(player$encountered_object %in% c(' ','\U1F9DF','\U1F6AA')){
      #Make previous player coordinate empty
      if(player$encountered_object %in% c(' ','\U1F6AA')){
        #Clear current coordinate
        if(castle_data$castle[player$current_coordinate] == '\U1F93A'){
          castle_data$castle[player$current_coordinate] = ' '
        }
        #Add emoji
        if(player$encountered_object == ' '){
          castle_data$castle[player$movement_coordinate] = '\U1F93A'
        }
        #Update coordinate
        player$current_coordinate = player$movement_coordinate
        #Get epoch time after object movement and get velocity
        player$current_game_update_time = as.numeric(Sys.time())
        player$calculate_player_velocity()
        #Zombie event
        event_output = zombie$pathfinding(castle_data = castle_data, player = player)
        castle_data = event_output[1:3]
        player = event_output[[4]]
      }
      else if(player$encountered_object == '\U1F9DF'){
        # Add emoji to new coordinate is the object is empty
        castle_data$castle[player$current_coordinate] = ' '
        #Make previous player coordinate empty
        player$current_coordinate = player$movement_coordinate
        zombie$distance_to_player = 0
      }
    }
    else{
      #Look into dataframe to see what encountered object is supposed to be
      #Switch statement for events
      switch(player$encountered_object,
             '\U1F9DA'= {event_output = fairy_event(castle_data = castle_data, player = player)},
             '\U1F9DE'= {event_output = genie_event(castle_data = castle_data, player = player)},
             '\U1F479'= {event_output = monster_event(castle_data = castle_data, player = player)},
             'DS' = {
               castle_data = player$move_to_new_floor_event(castle_data = castle_data)
               event_output = zombie$move_to_new_floor_event(castle_data = castle_data, player = player)},
             '\U2395' = {
               if (player$floor < player$total_floors){
                 castle_data = player$move_to_new_floor_event(castle_data = castle_data)
                 event_output = zombie$move_to_new_floor_event(castle_data = castle_data, player = player)
               }},
             'AS' = {
               cat(rep("\n", 50))
               print('You already came from upstairs.', quote = F)
               print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
               print(castle_data$castle[,,player$floor], quote = F)
               Sys.sleep(1)
             }
      )
      #Collect the outputs from each function
      castle_data = event_output[1:3]
      player = event_output[[4]]  
    }
  }
  #if/else statement to continue or quit when they die
  if(zombie$distance_to_player == 0){
    cat(rep("\n", 50))
    print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
    print(castle_data$castle[,,player$floor], quote = F)
    print('You were eaten by the zombie' , quote = F)
  }
  else if(player$encountered_object=='\U2395' & player$floor == player$total_floors){
    cat(rep("\n", 50))
    print('You found the exit!', quote = F)
    castle_data$castle[player$movement_coordinate] = player$encountered_object
    print(paste('Floor',player$floor, 'of',player$total_floors), quote = F)
    print(castle_data$castle[,,player$floor], quote = F)
  }
  #Retry 
  .iteration <<- .iteration + 1
  read_console_try_again_action()
}
castle_descent()
