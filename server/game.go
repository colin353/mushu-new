package main

import (
	"log"
	"time"
)

type CommodityType string

const (
	Tomato    CommodityType = "tomato"
	Blueberry CommodityType = "blueberry"
	Corn      CommodityType = "corn"
	Purple    CommodityType = "purple"
)

var AllCommodities []CommodityType = []CommodityType{Tomato, Blueberry, Corn, Purple}

// User represents a single connection to a player, e.g. a websocket.
type User interface {
	Message(message Message) error
	Name() string
	SetName(name string)
}

// GameConnection holds a list of all the active players, and can be
// used to broadcast messages to all players.
type GameConnection interface {
	Broadcast(message Message) error
}

// Game represents the state of an individual game instance.
type Game struct {
	name        string
	connection  GameConnection
	state       StateController
	nextTimeout time.Duration
	tick        time.Duration
	MinPlayers  int
	Yield       map[CommodityType]float64
}

// NewGame constructs a game.
func NewGame(name string, connection GameConnection) *Game {
	game := Game{
		name:       name,
		connection: connection,
		state:      nil,
		Yield:      make(map[CommodityType]float64),
		MinPlayers: MinPlayers,
	}
	game.state = NewStateController(&game, WaitingState)
	game.state.Begin()

	for _, c := range AllCommodities {
		game.Yield[c] = 1.00
	}

	return &game
}

// SetTimeout sets a time, after which the callback (state.Timer())
// on the currently active state will be invoked. Only one timer can
// be active at a time, and the callback will only occur in increments
// of the tick interval.
func (g *Game) SetTimeout(duration time.Duration) {
	g.nextTimeout = g.tick + duration
}

// GetTime returns the current time since the game began.
func (g *Game) GetTime() time.Duration {
	return g.tick
}

// Tick is called each time that the tick interval elapses.
func (g *Game) Tick(time time.Duration) {
	g.tick = time

	// If a timer is currently set, notify the state controller.
	if g.nextTimeout != 0 && time > g.nextTimeout {
		g.nextTimeout = 0
		g.state.Timer(time)
	}
}

func (g *Game) ActivateEffects(msg ActivateEffectMessage, user User) {
	// Inform the consumers that the effects are activated.
	g.connection.Broadcast(NewEffectMessage(msg.Id, user.Name()))
}

// RecieveMessage is called when a user sends a message to the server.
func (g *Game) RecieveMessage(user User, message Message) {
	switch msg := message.(type) {
	case JoinMessage:
		user.Message(NewWelcomeMessage(g.name, string(g.state.Name())))
		// TODO: store effects and broadcast to new players
	case SetNameMessage:
		user.SetName(msg.Name)
	case ActivateEffectMessage:
		g.ActivateEffects(msg, user)
	}
	g.state.RecieveMessage(user, message)
}

// ChangeState can be called by the state to transition to a new state.
func (g *Game) ChangeState(newState GameState) {
	g.state.End()

	log.Printf("State changed from %q to %q", g.state.Name(), newState)

	// Clean up any timers that are currently running
	g.nextTimeout = 0

	g.connection.Broadcast(NewGameStateChangedMessage(newState))
	g.state = NewStateController(g, newState)
	g.state.Begin()
}
