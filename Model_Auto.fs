///Model
type Details =
  { Name: string
    Description: string }

type Item = 
  { Details: Details }

type RoomId =
  | RoomId of string

type Exit =
  | PassableExit of string * destination: RoomId
  | LockedExit of string * key: Item * next: Exit
  | NoExit of string option

type Exits = 
  { North: Exit
    South: Exit
    East: Exit
    West: Exit }

type Room =
  { Id: RoomId
    Details: Details
    Items: Item list
    Exits: Exits }

type Player = 
  { Details: Details
    Location: RoomId
    Inventory: Item list }

type World = 
  { Rooms: Map<RoomId, Room>
    Player: Player }

// Initial World

let key: Item = 
  { Details =
    { Name = "A shiny key"
      Description = "This key looks like it could open a nearby door."} }

let allRooms = [
    
  { Id = RoomId "center"
    Details = 
      { Name = "A central room"
        Description = "You are standing in a central room with exits in all directions.  A single brazier lights the room."}
    Items = []
    Exits =
      { North = PassableExit ("You see a darkened passageway to the north.", RoomId "north1")
        South = PassableExit ("You see door to the south.  A waft of cold air hits your face.", RoomId "south1")
        East = LockedExit ("You see a locked door to the east.", key, PassableExit ("You see an open door to the east.", RoomId "east1"))
        West = PassableExit ("You see an interesting room to the west.", RoomId "west1") }}

  { Id = RoomId "north1"
    Details = 
      { Name = "A dark room"
        Description = "You are standing in a very dark room.  You hear the faint sound of rats scurrying along the floor."}
    Items = []
    Exits =
      { North = NoExit None
        South = PassableExit ("You see an dimly lit room to the south.", RoomId "center")
        East = NoExit None
        West = NoExit None }}

  { Id = RoomId "south1"
    Details = 
      { Name = "A cold room"
        Description = "You are standing in a room that feels very cold.  Your breath instantly turns into a white puff."}
    Items = []
    Exits =
      { North = PassableExit ("You see an exit to the north.  That room looks much warmer.", RoomId "center")
        South = NoExit None
        East = NoExit None
        West = NoExit None }}

  { Id = RoomId "west1"
    Details = 
      { Name = "A cozy room"
        Description = "This room seems very cozy, as if someone had made a home here.  Various personal belongings are strewn about."}
    Items = [ key ]
    Exits =
      { North = NoExit None
        South = NoExit None
        East = PassableExit ("You see a doorway back to the lit room.", RoomId "center")
        West = NoExit None }}

  { Id = RoomId "east1"
    Details = 
      { Name = "An open meadow"
        Description = "You are in an open meadow.  The sun is bright and it takes some time for your eyes to adjust."}
    Items = []
    Exits =
      { North = NoExit None
        South = NoExit None
        East = NoExit None
        West = PassableExit ("You see stone doorway to the west.  Why would you want to go back there?", RoomId "center") }}
  ]

let player = 
  { Details = { Name = "Bob"; Description = "Average Joe named Bob."}
    Inventory = []
    Location = RoomId "center" }

let gameWorld =
  { Rooms =
      allRooms
      |> Seq.map (fun room -> (room.Id, room))
      |> Map.ofSeq
    Player = player }

// Logic

type Result<'TSuccess, 'TFailure> = //Start of Railway
  | Success of 'TSuccess
  | Failure of 'TFailure

let bind processFunc lastResult =
  match lastResult with
  | Success s -> processFunc s 
  | Failure f -> Failure f

let (>>=) x f = // equal to |> bind (fun val) 
  bind f x

let switch processFunc input =
  Success (processFunc input)

let getRoom world roomId =
  match world.Rooms.TryFind roomId with
  | Some room -> Success room
  | None -> Failure "Room does not exist!"

let describeDetails details =
  sprintf "\n\n%s\n\n%s\n\n" details.Name details.Description

let extractDetailsFromRoom (room: Room) =
  room.Details

let describeCurrentRoom world = 
  world.Player.Location
  |> getRoom world
  |> (bind (switch extractDetailsFromRoom) >> bind (switch describeDetails))

// let north ({ North = northExit }: Exits) = northExit //more complicated way showing destructuring
let north exits = exits.North
let south exits = exits.South
let east exits = exits.East
let west exits = exits.West

let getCurrentRoom world =
  world.Player.Location
  |> getRoom world

let setCurrentRoom world room =
  { world with //change world
      Player = { world.Player with Location = room.Id} } //updates player inside world using the current roomId for the location

let getExit direction exits =
  match (direction exits) with 
  | PassableExit (_, roomId) -> Success roomId 
  | LockedExit (_) -> Failure "There is a locked door in that direction." //(_, _, _ was replaced with _)
  | NoExit (_) -> Failure "There is no room in that direction"

let move direction world = //Main Pipeline
  world
  |> getCurrentRoom
  >>= switch(fun room -> room.Exits)
  >>= getExit direction
  >>= getRoom world
  >>= switch (setCurrentRoom world)

let displayResult result =
  match result with
  | Success s -> printf "%s" s
  | Failure f -> printf "%s" f

//Game State and Its Modifiers
type GameEvent = //descriminated union
  | UpdateState of (World -> Result<World, string>)
  | ResetState of World
  | EndGameLoop

let applyUpdate updateFunc worldState =
  match updateFunc worldState with 
  | Success newState -> 
    describeCurrentRoom newState |> displayResult //describe and show new state
    newState //change to new state
  | Failure message -> 
    printfn "\n\n%s\n\n" message //send message
    worldState //keep current world state

//Game Engine
type GameEngine(initialState: World) = //Class
  let gameLoop = 
    MailboxProcessor.Start(fun inbox -> //Mailbox processor
      let rec innerLoop worldState = //recursive function
        async { //computation expression
          let! eventMsg = inbox.Receive() //can add timeout later
          match eventMsg with
          | UpdateState updateFunc -> return! innerLoop (applyUpdate updateFunc worldState) //update the state for recursive function
          | ResetState newState -> return! innerLoop newState //recall recursive function with new state
          | EndGameLoop -> return () //return value of type unit to end recursive function
        }
      innerLoop initialState) //invoke recursive function for the first time and give initial game world state

  member this.ApplyUpdate(updateFunc) = //member method for sending messages to mailbox proc
    gameLoop.Post(UpdateState updateFunc)

  member this.ResetState(newState) =
    gameLoop.Post(ResetState newState)  //member for reseting the state

  member this.Stop() = //member method for ending gameLoop
    gameLoop.Post(EndGameLoop)

//Call the Game Engine
let gameEngine = GameEngine(gameWorld) //passes in initial state
gameEngine.ApplyUpdate(move south) //passes in update message

//Random value
let rand = System.Random();

//Player Controller
let playerController =
  MailboxProcessor.Start(fun inbox ->
    let rec innerLoop state =
      async {
        try
          let! eventMsg = inbox.Receive(2000) //2 second time out
          if eventMsg = "Stop" then return ()
        with 
        | :? System.TimeoutException ->       // :? is type comparison operator
          [ "north", north //list of 4 tupples with 2 parts
            "south", south 
            "east", east 
            "west", west] //then gets random from list
          |> List.item (rand.Next 4)
          |> fun (dir, dirFunc) -> printfn "Wandering %s..." dir; dirFunc //desctructure item from list and pipes it
          |> move
          |> gameEngine.ApplyUpdate
          
          do! innerLoop state //call recursive function again to reset after timeout
      }
      
    innerLoop 0)//init innerloop with 0 state

//Game Controls
gameEngine.ResetState(gameWorld) //Reset world state
playerController.Post("Stop") //stop the player

// gameWorld
// |> move south
// >>= move north
// >>= move west
// >>= describeCurrentRoom
// |> displayResult
// |> ignore //throw away result


//Fira Code for better F# aesthetic