package main

type MessageAction string

const (
	GameStateChangedAction MessageAction = "game_state_changed"
	AuctionSeedAction      MessageAction = "auction_seed"
)

type Message interface{}

type MessageInterface struct {
	message interface{}
}

func NewMessage(message interface{}) Message {
	return MessageInterface{message}
}

type GameStateChangedMessage struct {
	Action   string `json:"action"`
	NewState string `json:"new_state"`
}

func NewGameStateChangedMessage(newState GameState) Message {
	return GameStateChangedMessage{
		Action:   string(GameStateChangedAction),
		NewState: string(newState),
	}
}

type AuctionSeedMessage struct {
	Action string `json:"action"`
	Seed   int    `json:"seed"`
}

func NewAuctionSeedMessage(seed int) Message {
	return AuctionSeedMessage{
		Action: string(AuctionSeedAction),
		Seed:   seed,
	}
}
