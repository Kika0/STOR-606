Tandem <- function(Starting.seed, c, Tend){
  # Discrete-event simulation of a tandem queue where some customers
  # must repeat the first queue
  # Generates customer waiting times in queue.
  
  # c = number of parallel servers
  # Tend = ending time of the simulation
  
  # Define arrival and service distributions here  
  ArrivalDistribution <- function() return(rexp(1, rate = 1))
  ServiceDistribution1 <- function() return(rexp(1, rate = 1/0.7))
  ServiceDistribution2 <- function() return(rexp(1, rate = 1/0.9))
  
  # Event calendar management functions
  Schedule <- function(Event, Time){
    # inserts events into the event calendar
    CalIndex <- which.max(EventTime)  
    EventTime[CalIndex] <<- Clock + Time
    EventType[CalIndex] <<- Event
  }
  
  TimerQ <- function(){
    # find next event, update Clock, and return event type
    CalIndex <- which.min(EventTime)
    Clock <<- EventTime[CalIndex]
    EventTime[CalIndex] <<- Inf
    return(EventType[CalIndex])
  }
  
  # Events
  Arrival1 <- function(Again){
    # models the arrival of a customer to queue 1
    if (Again == FALSE){
      Schedule("Arrival1", ArrivalDistribution()) # schedule next NEW arrival
    }
    if (Busy1 < c[1]){
      Busy1 <<- Busy1 + 1
      Waits1 <<- c(Waits1, 0)
      Schedule("Departure1", ServiceDistribution1())
    }
    else{
      Queue1 <<- c(Queue1, Clock)
    }
  }
  
  Departure1 <- function(){
    # models departure of a customer from queue 1
    if (runif(1) < 0.7){
     # Arrival1(TRUE) # return to queue 1
      Schedule("Arrival2", 0) # schedule arrival 2
    }
    
    NumQ <- length(Queue1)
    if (NumQ > 0){
      Waits1 <<- c(Waits1, Clock - Queue1[1])
      Schedule("Departure1", ServiceDistribution1())
      if (NumQ == 1){
        Queue1 <<- NULL
      }
      else {
        Queue1 <<- Queue1[2:NumQ]
      }
    }
    else{
      Busy1 <<- Busy1 - 1
    }    
  }
  
  Arrival2 <- function(){
    # models the arrival of a customer to queue 2
    if (Busy2 < c[2]){
      Busy2 <<- Busy2 + 1
      Waits2 <<- c(Waits2, 0)
      Schedule("Departure2", ServiceDistribution2())
    }
    else{
      Queue2 <<- c(Queue2, Clock)
    }
  }
  
  Departure2 <- function(){
    # models departure of a customer from queue 2
    NumQ <- length(Queue2)
    if (NumQ > 0){
      Waits2 <<- c(Waits2, Clock - Queue2[1])
      Schedule("Departure2", ServiceDistribution2())
      if (NumQ == 1){
        Queue2 <<- NULL
      }
      else {
        Queue2 <<- Queue2[2:NumQ]
      }
    }
    else{
      Busy2 <<- Busy2 - 1
    }    
  }
  
  # Main simulation loop; execution starts here
  set.seed(Starting.seed)
  Queue1 <<- NULL
  Queue2 <<- NULL
  Busy1 <<- 0
  Busy2 <<- 0
  Clock <<- 0
  Waits1 <<-  NULL
  Waits2 <<- NULL
  # create event calendar and schedule initial events
  EventTime <<- rep(Inf, c[1]+c[2]+3)
  EventType <<- rep("", c[1]+c[2]+3)
  Schedule("Arrival1", ArrivalDistribution())
  Schedule("EndSimulation", Tend)
  
  NextEvent <- ""
  
  while(NextEvent != "EndSimulation"){
    NextEvent <- TimerQ()
#    print(c(Clock, NextEvent)) # uncomment for basic event trace
    switch(NextEvent,
           Arrival1 = Arrival1(FALSE),
           Arrival2 = Arrival2(),           
           Departure1 = Departure1(),
           Departure2 = Departure2()
    )
  } 
#  list(Waits1 = Waits1, Waits2 = Waits2, EventTime = EventTime, EventType = EventType, Clock=Clock)
  list(Waits1 = Waits1, Waits2 = Waits2)
}