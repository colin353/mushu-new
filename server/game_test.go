package main

import (
	"encoding/json"
	"math/rand"

	"github.com/google/go-cmp/cmp"

	"testing"
)

type TestConnection struct {
	broadcastLog []string
}

func (c *TestConnection) Broadcast(message Message) error {
	result, err := json.Marshal(message)
	if err != nil {
		panic(err)
	}
	c.broadcastLog = append(c.broadcastLog, string(result))
	return nil
}

type TestUser struct {
	name       string
	messageLog []string
}

func (u *TestUser) Name() string {
	return u.name
}

func (u *TestUser) SetName(name string) {
	u.name = name
}

func (u *TestUser) Message(message Message) error {
	result, err := json.Marshal(message)
	if err != nil {
		panic(err)
	}
	u.messageLog = append(u.messageLog, string(result))
	return nil
}

func CompareBroadcastLog(got, want TestConnection) string {
	return cmp.Diff(got.broadcastLog, want.broadcastLog)
}

func CompareMessageLog(got, want *TestUser) string {
	return cmp.Diff(got.messageLog, want.messageLog)
}

func TestChangeState(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.ChangeState(TradeState)

	expected := TestConnection{}
	expected.Broadcast(NewGameStateChangedMessage(TradeState))
	expected.Broadcast(NewPriceUpdatedMessage(game.Market))
	expected.Broadcast(NewSetClockMessage(TradingStageTime))

	if diff := CompareBroadcastLog(connection, expected); diff != "" {
		t.Errorf("ChangeState(WaitingState): %v", diff)
	}

	if game.state.Name() != TradeState {
		t.Errorf("game.state = %v, want %v", game.state, TradeState)
	}
}

func TestAuctionStart(t *testing.T) {
	// Need to set the random seed to force deterministic behavior.
	rand.Seed(1)
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.ChangeState(AuctionState)

	rand.Seed(1)
	expected := TestConnection{}
	expected.Broadcast(NewGameStateChangedMessage(AuctionState))
	expected.Broadcast(NewAuctionSeedMessage(rand.Int()))
	expected.Broadcast(NewSetClockMessage(AuctionBidTime))

	if diff := CompareBroadcastLog(connection, expected); diff != "" {
		t.Errorf("ChangeState(WaitingState): %v", diff)
	}

	if game.state.Name() != AuctionState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), AuctionState)
	}
}

func TestReadyMechanism(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.MinPlayers = 2

	userA := &TestUser{}
	userB := &TestUser{}
	game.RecieveMessage(userA, NewReadyMessage())

	if game.state.Name() != WaitingState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), WaitingState)
	}

	// If the user posts ready again, it shouldn't work, since
	// we need two different players.
	game.RecieveMessage(userA, NewReadyMessage())
	if game.state.Name() != WaitingState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), WaitingState)
	}

	// Now the game should start.
	game.RecieveMessage(userB, NewReadyMessage())
	if game.state.Name() != ProductionState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), ProductionState)
	}

}

func TestPlayerInfoMessage(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.MinPlayers = 2

	userA := &TestUser{name: "George"}
	userB := &TestUser{name: "Paul"}
	game.RecieveMessage(userB, NewJoinMessage())
	game.RecieveMessage(userA, NewReadyMessage())

	expected := TestConnection{}

	info := []PlayerInfo{
		{Name: "Paul", Ready: false},
	}
	expected.Broadcast(NewPlayerInfoUpdateMessage(info))

	info = []PlayerInfo{
		{Name: "Paul", Ready: false},
		{Name: "George", Ready: true},
	}
	expected.Broadcast(NewPlayerInfoUpdateMessage(info))

	if diff := CompareBroadcastLog(connection, expected); diff != "" {
		t.Errorf("PlayerInfoMessage: %v", diff)
	}

}
func TestReadyMechanismWithMorePlayers(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.MinPlayers = 2

	userA := &TestUser{}
	userB := &TestUser{}
	userC := &TestUser{}
	game.RecieveMessage(userA, NewReadyMessage())
	game.RecieveMessage(userB, NewJoinMessage())
	game.RecieveMessage(userC, NewReadyMessage())

	// Since user B has joined but is not ready, game shouldn't start.
	if game.state.Name() != WaitingState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), WaitingState)
	}

	game.RecieveMessage(userB, NewReadyMessage())
	if game.state.Name() != ProductionState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), ProductionState)
	}
}

func TestReadyMechanismWithLeaver(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.MinPlayers = 2

	userA := &TestUser{}
	userB := &TestUser{}
	userC := &TestUser{}
	game.RecieveMessage(userA, NewReadyMessage())
	game.RecieveMessage(userB, NewJoinMessage())
	game.RecieveMessage(userC, NewReadyMessage())

	// Since user B has joined but is not ready, game shouldn't start.
	if game.state.Name() != WaitingState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), WaitingState)
	}

	// Now the user has left, and the rest are ready, so begin.
	game.RecieveMessage(userB, NewLeaveMessage())
	if game.state.Name() != ProductionState {
		t.Errorf("game.state.Name() = %v, want %v", game.state.Name(), ProductionState)
	}
}

func TestAuctionPhases(t *testing.T) {
	// Need to set the random seed to force deterministic behavior.
	rand.Seed(1)
	connection := TestConnection{}
	game := NewGame("g", &connection)
	game.ChangeState(AuctionState)

	// Bid on a card.
	user := &TestUser{name: "tester"}
	game.RecieveMessage(user, NewBidMessage(10))

	// Wait until the player wins.
	game.Tick(AuctionBidTime + 1)

	// Wait until the second auction expires with no bids.
	game.Tick(2*AuctionBidTime + 2)

	// Wait until the third auction expires with no bids.
	game.Tick(3*AuctionBidTime + 3)

	rand.Seed(1)
	expected := TestConnection{}
	expected.Broadcast(NewGameStateChangedMessage(AuctionState))
	expected.Broadcast(NewAuctionSeedMessage(rand.Int()))
	expected.Broadcast(NewSetClockMessage(AuctionBidTime))

	expected.Broadcast(NewBidUpdatedMessage(10, user.Name()))
	expected.Broadcast(NewSetClockMessage(AuctionBidTime))
	expected.Broadcast(NewAuctionSeedMessage(rand.Int()))
	expected.Broadcast(NewSetClockMessage(AuctionBidTime))

	expected.Broadcast(NewAuctionSeedMessage(rand.Int()))
	expected.Broadcast(NewSetClockMessage(AuctionBidTime))

	expected.Broadcast(NewGameStateChangedMessage(TradeState))
	expected.Broadcast(NewPriceUpdatedMessage(game.Market))
	expected.Broadcast(NewSetClockMessage(TradingStageTime))

	if diff := CompareBroadcastLog(connection, expected); diff != "" {
		t.Errorf("Auction bidding: ", diff)
	}
}
