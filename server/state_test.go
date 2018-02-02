package main

import "testing"

func TestAuctionBidding(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	ctrl := NewAuctionController(game)

	u1 := &TestUser{}
	u2 := &TestUser{}
	ctrl.RecieveMessage(u1, NewBidMessage(10))
	ctrl.RecieveMessage(u2, NewBidMessage(5))

	if ctrl.bid != 10 {
		t.Errorf("Expected ctrl.bid = 10, got %v", ctrl.bid)
	}
	if ctrl.winner != u1 {
		t.Errorf("Expected ctrl.winner = u, got %v", ctrl.winner)
	}

	// Can't win by bidding the same amount.
	ctrl.RecieveMessage(u2, NewBidMessage(10))
	if ctrl.winner != u1 {
		t.Errorf("Expected ctrl.winner = u, got %v", ctrl.winner)
	}

	// Outbidding will switch winner.
	ctrl.RecieveMessage(u2, NewBidMessage(12))
	if ctrl.winner != u2 {
		t.Errorf("Expected ctrl.winner = u, got %v", ctrl.winner)
	}
}

func TestProductionTimeout(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	ctrl := NewProductionController(game)
	ctrl.Begin()

	game.state = ctrl

	// Wait for the auction to end.
	game.Tick(ProductionTimeout + 1)

	// Expect the winner to get a winning message.
	want := TestConnection{}
	want.Broadcast(NewGameStateChangedMessage(AuctionState))

	if len(want.broadcastLog) == 0 || connection.broadcastLog[0] != want.broadcastLog[0] {
		t.Errorf("Production timeout: got %q, want %q",
			connection.broadcastLog, want.broadcastLog)
	}
}

func TestAuctionTimeout(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	ctrl := NewAuctionController(game)
	game.state = ctrl

	user := &TestUser{}
	loser := &TestUser{}
	ctrl.RecieveMessage(user, NewBidMessage(10))
	ctrl.RecieveMessage(loser, NewBidMessage(5))

	// Wait for the auction to end.
	game.Tick(2 * AuctionBidTime)

	// Expect the winner to get a winning message.
	want := &TestUser{}
	want.Message(NewAuctionWonMessage())

	if diff := CompareMessageLog(user, want); diff != "" {
		t.Errorf("AuctionWonMessage: %q, %q, diff: %v",
			user.messageLog, want.messageLog, diff)
	}
}

func TestSelling(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	ctrl := NewTradeController(game)
	game.state = ctrl
	game.Market.Commodities[Tomato].Value = 100
	game.Market.Commodities[Tomato].Supply = 100
	game.Market.Commodities[Tomato].Demand = 100

	user := &TestUser{}
	ctrl.RecieveMessage(user, NewSellMessage(Tomato, 1))

	// Expect the winner to get a winning message.
	want := &TestUser{}
	want.Message(NewSaleCompletedMessage(NewSellMessage(Tomato, 1), 50))

	if diff := CompareMessageLog(user, want); diff != "" {
		t.Errorf("SaleCompletedMessage: %q, %q, diff: %v",
			user.messageLog, want.messageLog, diff)
	}
}

func TestTradeMechanism(t *testing.T) {
	connection := TestConnection{}
	game := NewGame("g", &connection)
	ctrl := NewTradeController(game)
	game.state = ctrl

	userA := &TestUser{}
	userB := &TestUser{}
	ctrl.RecieveMessage(userA, NewTradeMessage("a gold bar"))
	ctrl.RecieveMessage(userB, NewTradeMessage("a ham sandwich"))

	// Expect the users to exchange messages.
	wantA := &TestUser{}
	wantA.Message(NewTradeCompletedMessage("a ham sandwich"))
	wantB := &TestUser{}
	wantB.Message(NewTradeCompletedMessage("a gold bar"))

	if diff := CompareMessageLog(userA, wantA); diff != "" {
		t.Errorf("TradeMessage: %q, %q, diff: %v",
			userA.messageLog, wantA.messageLog, diff)
	}

	if diff := CompareMessageLog(userB, wantB); diff != "" {
		t.Errorf("TradeMessage: %q, %q, diff: %v",
			userB.messageLog, wantB.messageLog, diff)
	}

	// Subsequent trade is too slow and fails to complete.
	userC := &TestUser{}
	userD := &TestUser{}
	ctrl.RecieveMessage(userC, NewTradeMessage("nothing"))

	game.Tick(TradeTimeout * 2)

	ctrl.RecieveMessage(userD, NewTradeMessage("something"))
	wantC := &TestUser{}
	wantD := &TestUser{}

	if diff := CompareMessageLog(userC, wantC); diff != "" {
		t.Errorf("TradeMessage: %q, %q, diff: %v",
			userC.messageLog, wantC.messageLog, diff)
	}
	if diff := CompareMessageLog(userD, wantD); diff != "" {
		t.Errorf("TradeMessage: %q, %q, diff: %v",
			userD.messageLog, wantD.messageLog, diff)
	}

	game.Tick(TradeTimeout * 4)

	userE := &TestUser{}
	userF := &TestUser{}
	ctrl.RecieveMessage(userE, NewTradeMessage("widget"))

	// Short delay.
	game.Tick(TradeTimeout*4 + 5)

	ctrl.RecieveMessage(userF, NewTradeMessage("wodget"))

	// Expect the users to exchange messages.
	wantE := &TestUser{}
	wantE.Message(NewTradeCompletedMessage("wodget"))
	wantF := &TestUser{}
	wantF.Message(NewTradeCompletedMessage("widget"))

	if diff := CompareMessageLog(userE, wantE); diff != "" {
		t.Errorf("TradeMessage: %q, %q, diff: %v",
			userE.messageLog, wantE.messageLog, diff)
	}
	if diff := CompareMessageLog(userF, wantF); diff != "" {
		t.Errorf("TradeMessage: %q, %q, diff: %v",
			userF.messageLog, wantF.messageLog, diff)
	}
}
