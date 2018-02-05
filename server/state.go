package main

import (
	"log"
	"math/rand"
	"time"
)

type GameState string

const (
	WaitingState    GameState = "waiting"
	ProductionState GameState = "production"
	AuctionState    GameState = "auction"
	TradeState      GameState = "trade"
)

const (
	// AuctionBidTime specifies the amount of time after the last bid is
	// placed that the auction will expire.
	AuctionBidTime time.Duration = 5 * time.Second
	// ProductionTimeout is how long the production phase will last before
	// the next phase begins.
	ProductionTimeout time.Duration = 10 * time.Second
	// TradeTimeout specifies how long a trade can hang without a
	// counterpart before it is cancelled.
	TradeTimeout time.Duration = 100 * time.Millisecond
	// TradingStageTime is how long the production phase will last before
	// the next phase begins.
	TradingStageTime time.Duration = 10 * time.Second

	// MinPlayers sets the minimum number of players required before the game
	// will proceed past the Waiting stage.
	MinPlayers int = 1
)

type StateController interface {
	Name() GameState
	Begin()
	End()
	Timer(tick time.Duration)
	RecieveMessage(User, Message)
}

type WaitingController struct {
	game  *Game
	name  GameState
	ready map[User]bool
}

func NewWaitingController(game *Game) *WaitingController {
	return &WaitingController{
		game:  game,
		name:  WaitingState,
		ready: map[User]bool{},
	}
}

// Name returns the name of the current state.
func (s *WaitingController) Name() GameState { return s.name }

// Begin is called when the state becomes active.
func (s *WaitingController) Begin() {}

// End is called when the state is no longer active.
func (s *WaitingController) End() {}

// Timer is called when a timeout occurs.
func (s *WaitingController) Timer(tick time.Duration) {}

// RecieveMessage is called when a user sends a message to the server.
func (s *WaitingController) RecieveMessage(u User, m Message) {
	log.Printf("Ready state: %v", s.ready)
	switch m.(type) {
	case ReadyMessage:
		s.ready[u] = true
		s.proceedIfReady()
	case JoinMessage:
		s.ready[u] = false
	case LeaveMessage:
		delete(s.ready, u)
		s.proceedIfReady()
	}
}

func (s *WaitingController) proceedIfReady() {
	count := 0
	for _, v := range s.ready {
		if !v {
			return
		}
		count++
	}

	if count >= s.game.MinPlayers {
		s.game.ChangeState(ProductionState)
	}
}

type ProductionController struct {
	game *Game
	name GameState
}

func NewProductionController(game *Game) *ProductionController {
	return &ProductionController{
		game: game,
		name: ProductionState,
	}
}

// Name returns the name of the current state.
func (s *ProductionController) Name() GameState { return s.name }

// End is called when the state is no longer active.
func (s *ProductionController) End()                             {}
func (s *ProductionController) RecieveMessage(u User, m Message) {}

// Begin is called when the state becomes active.
func (s *ProductionController) Begin() {
	// The production stage is timed, so we should move to the next stage
	// after the time interval.
	s.game.connection.Broadcast(NewSetClockMessage(ProductionTimeout))
	s.game.SetTimeout(ProductionTimeout)
}

// Timer is called when the state ends, so just transition to the next state.
func (s *ProductionController) Timer(tick time.Duration) {
	s.game.ChangeState(AuctionState)
}

type AuctionController struct {
	name   GameState
	game   *Game
	bid    int
	step   int
	steps  int
	winner User
}

func NewAuctionController(game *Game) *AuctionController {
	return &AuctionController{
		name:  AuctionState,
		game:  game,
		steps: 3,
	}
}

// Name returns the name of the current state.
func (s *AuctionController) Name() GameState { return s.name }

// Begin is called when the state becomes active.
func (s *AuctionController) Begin() {
	s.issueCard()
}

func (s *AuctionController) issueCard() {
	// When the auction begins, we need to choose a random number and broadcast
	// it to the participants.
	seed := rand.Int()
	s.game.connection.Broadcast(
		NewAuctionSeedMessage(seed),
	)

	// Set a timeout, and update player clocks.
	s.game.SetTimeout(AuctionBidTime)
	s.game.connection.Broadcast(NewSetClockMessage(AuctionBidTime))
}

// End is called when the state is no longer active.
func (s *AuctionController) End() {}

// Timer is only used to determine when the auction is over. So when we get
// this call, the current auction is over.
func (s *AuctionController) Timer(tick time.Duration) {
	if s.winner != nil {
		s.winner.Message(NewAuctionWonMessage())
	}

	// Reset the bid and winner.
	s.bid = 0
	s.winner = nil

	s.step++
	if s.step == s.steps {
		// We've reached the end of the auction process. So let's change
		// phases. The TradeState is next.
		s.game.ChangeState(TradeState)
	} else {
		// Issue the next card.
		s.issueCard()
	}
}

// RecieveMessage is called when a new message is sent by a user.
func (s *AuctionController) RecieveMessage(u User, m Message) {
	switch msg := m.(type) {
	case BidMessage:
		if msg.Amount > s.bid {
			s.bid = msg.Amount
			s.winner = u
			s.game.SetTimeout(AuctionBidTime)

			// Update everyone on the new bid and winner.
			s.game.connection.Broadcast(NewBidUpdatedMessage(s.bid, u.Name()))
		}
	}
}

// TradeController manages the state of the game during trading.
type TradeController struct {
	name            GameState
	game            *Game
	stagedMaterials string
	stagedUser      User
	stagingTime     time.Duration
}

// NewTradeController creates a TradeController instance.
func NewTradeController(game *Game) *TradeController {
	return &TradeController{
		name: TradeState,
		game: game,
	}
}

// Name returns the name of the current state.
func (s *TradeController) Name() GameState { return s.name }

// Begin is called when the state becomes active.
func (s *TradeController) Begin() {
	// Should automatically update the prices at the beginning of the stage.
	s.game.connection.Broadcast(NewPriceUpdatedMessage(s.game.Market))
	// The trading stage ends after a certain time.
	s.game.SetTimeout(TradingStageTime)
	s.game.connection.Broadcast(NewSetClockMessage(TradingStageTime))
}

// Timer is called when the stage is over, so just begin next stage.
func (s *TradeController) Timer(tick time.Duration) {
	s.game.ChangeState(ProductionState)
}

// End is called when the state is no longer active.
func (s *TradeController) End() {}

// RecieveMessage is called when a user sends the server a message.
func (s *TradeController) RecieveMessage(u User, m Message) {
	switch msg := m.(type) {
	case TradeMessage:
		isntSelfTrade := s.stagedUser != u
		withinTimeInterval := s.game.GetTime()-s.stagingTime < TradeTimeout
		if isntSelfTrade && s.stagedUser != nil && withinTimeInterval {
			// Execute the currently proposed trade.
			s.stagedUser.Message(NewTradeCompletedMessage(msg.Materials))
			u.Message(NewTradeCompletedMessage(s.stagedMaterials))

			// Reset the staged materials
			s.stagedUser = nil
			s.stagingTime = 0
			s.stagedMaterials = ""
		} else {
			s.stagedUser = u
			s.stagingTime = s.game.GetTime()
			s.stagedMaterials = msg.Materials
		}
	case SellMessage:
		// First, determine the price that the user would get.
		price, err := s.game.Market.Sell(CommodityType(msg.Type), msg.Quantity)
		if err != nil {
			log.Printf("Got invalid SellMessage: %v", err)
			return
		}
		// Inform the user that their sale is done.
		response := NewSaleCompletedMessage(msg, price)
		u.Message(response)
		// Update all other users that the price has changed.
		s.game.connection.Broadcast(NewPriceUpdatedMessage(s.game.Market))
	}
}

// NewStateController creates a state controller based on the requested state.
func NewStateController(game *Game, state GameState) StateController {
	switch state {
	case WaitingState:
		return NewWaitingController(game)
	case ProductionState:
		return NewProductionController(game)
	case AuctionState:
		return NewAuctionController(game)
	case TradeState:
		return NewTradeController(game)
	default:
		panic("Unknown state!")
	}
}
