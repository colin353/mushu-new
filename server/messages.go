package main

import (
	"encoding/json"
	"fmt"
	"time"
)

// MessageAction is the string used in the `action` field
// within the JSON of the message. Each message has a unique
// action string.
type MessageAction string

const (
	// Server broadcast messages
	GameStateChangedAction MessageAction = "game_state_changed"
	AuctionSeedAction      MessageAction = "auction_seed"
	WelcomeAction          MessageAction = "welcome"
	BidUpdatedAction       MessageAction = "bid_updated"
	SetClockAction         MessageAction = "set_clock"
	EffectAction           MessageAction = "effect_activated"
	PlayerInfoUpdateAction MessageAction = "player_info_updated"

	// Server-to-client messages
	AuctionWonAction     MessageAction = "auction_won"
	TradeCompletedAction MessageAction = "trade_completed"

	// Client messages
	BidAction            MessageAction = "bid"
	ReadyAction          MessageAction = "ready"
	JoinAction           MessageAction = "join"
	LeaveAction          MessageAction = "leave"
	TradeAction          MessageAction = "trade"
	SetNameAction        MessageAction = "set_name"
	ActivateEffectAction MessageAction = "activate_effect"

	// Special debug-only actions
	TickAction MessageAction = "tick"
)

// A Message is an object which must contain an Action string, serializable
// to the MessageAction, and may also contain other JSON serializable fields.
type Message interface{}

// BasicMessage is a dummy message. All JSON messages sent or recieved by the
// server should be deserializable into this message type. This allows us to
// read the Action string without knowing the internal structure of the
// message.
type BasicMessage struct {
	Action string `json:"action"`
}

// TickMessage is sent to increment the current game clock. Users shouldn't send
// this message, it is only generated internally.
type TickMessage struct {
	Action string  `json:"action"`
	Tick   float64 `json:"tick_ms"`
}

func NewTickMessage(tick time.Duration) TickMessage {
	return TickMessage{
		Action: string(TickAction),
		Tick:   float64(tick / time.Millisecond),
	}
}

// Messages broadcast by the server.

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

type BidUpdatedMessage struct {
	Action string `json:"action"`
	Bid    int    `json:"bid"`
	Winner string `json:"winner"`
}

func NewBidUpdatedMessage(bid int, winner string) Message {
	return BidUpdatedMessage{
		Action: string(BidUpdatedAction),
		Bid:    bid,
		Winner: winner,
	}
}

type EffectMessage struct {
	Action string `json:"action"`
	Id     int    `json:"id"`
	Author string `json:"author"`
}

func NewEffectMessage(id int, author string) Message {
	return EffectMessage{
		Action: string(EffectAction),
		Id:     id,
		Author: author,
	}
}

type SetClockMessage struct {
	Action string `json:"action"`
	Time   int    `json:"time"`
}

func NewSetClockMessage(t time.Duration) Message {
	return SetClockMessage{
		Action: string(SetClockAction),
		Time:   int(t / time.Millisecond),
	}
}

type PlayerInfo struct {
	Name  string `json:"name"`
	Ready bool   `json:"ready"`
}

type PlayerInfoUpdateMessage struct {
	Action string       `json:"action"`
	Info   []PlayerInfo `json:"info"`
}

func NewPlayerInfoUpdateMessage(info []PlayerInfo) Message {
	return PlayerInfoUpdateMessage{
		Action: string(PlayerInfoUpdateAction),
		Info:   info,
	}
}

// Server-to-client messages:

type TradeCompletedMessage struct {
	Action    string `json:"action"`
	Materials string `json:"materials"`
}

func NewTradeCompletedMessage(materials string) Message {
	return TradeCompletedMessage{string(TradeCompletedAction), materials}
}

type WelcomeMessage struct {
	Action string `json:"action"`
	Game   string `json:"game"`
	State  string `json:"state"`
}

func NewWelcomeMessage(game, state string) Message {
	return WelcomeMessage{
		Action: string(WelcomeAction),
		Game:   game,
		State:  state,
	}
}

// Client messages

type BidMessage struct {
	Action string `json:"action"`
	Amount int    `json:"amount"`
}

func NewBidMessage(amount int) Message {
	return BidMessage{
		Action: string(BidAction),
		Amount: amount,
	}
}

type AuctionWonMessage struct {
	Action string `json:"action"`
}

func NewAuctionWonMessage() Message {
	return AuctionWonMessage{string(AuctionWonAction)}
}

type ReadyMessage struct {
	Action string `json:"action"`
	Ready  bool   `json:"ready"`
}

func NewReadyMessage(ready bool) Message {
	return ReadyMessage{
		Action: string(ReadyAction),
		Ready:  ready,
	}
}

type JoinMessage struct {
	Action string `json:"action"`
}

func NewJoinMessage() Message {
	return JoinMessage{string(JoinAction)}
}

type LeaveMessage struct {
	Action string `json:"action"`
}

func NewLeaveMessage() Message {
	return LeaveMessage{string(LeaveAction)}
}

type TradeMessage struct {
	Action    string `json:"action"`
	Materials string `json:"materials"`
}

func NewTradeMessage(materials string) Message {
	return TradeMessage{
		Action:    string(TradeAction),
		Materials: materials,
	}
}

type SellMessage struct {
	Action   string `json:"action"`
	Quantity int64  `json:"quantity"`
	Type     string `json:"type"`
}

type SetNameMessage struct {
	Action string `json:"action"`
	Name   string `json:"name"`
}

func NewSetNameMessage(name string) SetNameMessage {
	return SetNameMessage{
		Action: string(SetNameAction),
		Name:   name,
	}
}

type ActivateEffectMessage struct {
	Action  string `json:"action"`
	Id      int    `json:"id"`
	Timeout int64
}

func NewActivateEffectMessage(id int, timeout int64) Message {
	return ActivateEffectMessage{
		Action:  string(ActivateEffectAction),
		Id:      id,
		Timeout: timeout,
	}
}

// DecodeMessage takes data in bytes, determines which message it corresponds
// to, and decodes it to the appropriate type.
func DecodeMessage(data []byte) (Message, error) {
	msg := BasicMessage{}
	if err := json.Unmarshal(data, &msg); err != nil {
		return nil, fmt.Errorf("Unable to decode message: %q", data)
	}

	// Now that we know the type of the message (based on the action) we
	// can decode it properly.
	var message Message
	var err error
	switch msg.Action {
	case string(GameStateChangedAction):
		m := GameStateChangedMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(AuctionSeedAction):
		m := AuctionSeedMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(WelcomeAction):
		m := WelcomeMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(AuctionWonAction):
		m := AuctionWonMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(TradeCompletedAction):
		m := TradeCompletedMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(BidAction):
		m := BidMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(ReadyAction):
		m := ReadyMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(JoinAction):
		m := JoinMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(LeaveAction):
		m := LeaveMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(TradeAction):
		m := TradeMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(TickAction):
		m := TickMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(SetNameAction):
		m := SetNameMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	case string(ActivateEffectAction):
		m := ActivateEffectMessage{}
		err = json.Unmarshal(data, &m)
		message = m
	default:
		err = fmt.Errorf("Unknown action: %v", msg.Action)
	}

	return message, err
}
