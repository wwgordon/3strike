##MAKE THIS SHINY
##To fix: randomly selects opponent from all opponents available, not based on opponents strikes

winRate = 0.8 #"batting average" of single hypothetical player
iterations = 1000 #how many times to iterate sim
entrants = 30 #how many players in hypothetical tournament
indivSkill = runif(entrants-1, min = -1, max = 1) #set distribution of individual skill levels across tournament here 
performanceFunction = function(x){return(rnorm(1, mean = x))} #set distirubtion of an individual's game performances here
randomSelect = TRUE #(to be implemented) are oppponents chosen at random or based on records?

simulatedRecord = data.frame(iteration = rep(NA, iterations), wins = rep(NA, iterations), losses = rep(NA, iterations), wonTourney = rep(FALSE, iterations))

for(i in 1:iterations){ #which tourney (iteration) is this?
    sim = data.frame(entrantID = 1, #entrantID = 1 is simulated player of interest
                     playerQuality = winRate,
                     wins = 0,
                     losses = 0)
    record = data.frame(entrantID = 2:entrants, #other hypothetical entrants
                        playerQuality = indivSkill,
                        wins = rep(0, entrants-1), 
                        losses = rep(0, entrants-1)) #df to track records for individuals (iterations)
    record = rbind(sim, record) #combine dataframes
    
    repeat{ #iterating through rounds
        availablePlayers = record[record$losses < 3,1] #who is still in?
        if(record$losses[1] == 3){break} #is simulated player out?
        if(length(availablePlayers) == 1){ #did simulated player win?
            simulatedRecord$wonTourney[i] = TRUE
            break
            }
        repeat{ #iterating through matches
            if(length(availablePlayers) == 1){ #all other players drawn, give bye
            byePlayer = availablePlayers[1]
            record$wins[byePlayer] = record$wins[byePlayer] + 1
            break
            }
            thisMatch = sample(availablePlayers, size = 2, replace = FALSE) #still at least 2 players to draw
            availablePlayers = availablePlayers[!availablePlayers %in% thisMatch] #remove this pair from list of available players
            player1score = performanceFunction(record$playerQuality[thisMatch[1]])
            player2score = performanceFunction(record$playerQuality[thisMatch[2]])
            record$wins[thisMatch[1]] = record$wins[thisMatch[1]] + sum(player1score > player2score) #record win
            record$wins[thisMatch[2]] = record$wins[thisMatch[2]] + sum(player2score > player1score)
            record$losses[thisMatch[1]] = record$losses[thisMatch[1]] + sum(player1score < player2score) #record loss
            record$losses[thisMatch[2]] = record$losses[thisMatch[2]] + sum(player2score < player1score)
            if(length(availablePlayers) == 0){break} #round is over
        }
    }
    simulatedRecord$wins[i] = record$wins[1] #update overall simulation df with wins/losses
    simulatedRecord$losses[i] = record$losses[1]
    simulatedRecord$iteration[i] = i
}


simulatedRecord$winRate = simulatedRecord$wins / (simulatedRecord$wins + simulatedRecord$losses)
simulatedRecord$gamesPlayed = simulatedRecord$wins + simulatedRecord$losses
