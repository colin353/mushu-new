package main

import (
	"fmt"
	"math"
)

type CommodityType string

const (
	Tomato    CommodityType = "tomato"
	Blueberry CommodityType = "blueberry"
	Corn      CommodityType = "corn"
	Purple    CommodityType = "purple"
)

var AllCommodities []CommodityType = []CommodityType{Tomato, Blueberry, Corn, Purple}

type Market struct {
	Commodities map[CommodityType]*Commodity
	Modifier    map[CommodityType]float64
}

func NewMarket() Market {
	m := Market{
		Commodities: make(map[CommodityType]*Commodity),
		Modifier:    make(map[CommodityType]float64),
	}

	for _, c := range AllCommodities {
		m.Modifier[c] = 1.00

		m.Commodities[c] = &Commodity{
			Name:   string(c),
			Supply: 100,
			Value:  100.00,
			Demand: 100,
		}
	}

	return m
}

func (m *Market) ApplyModifier(modifier map[CommodityType]float64) {
	for _, c := range AllCommodities {
		m.Modifier[c] *= modifier[c]
	}
}

func (m *Market) Prices() map[CommodityType]float64 {
	prices := make(map[CommodityType]float64)
	for _, c := range AllCommodities {
		prices[c] = m.Modifier[c] * m.Commodities[c].Price()
	}

	return prices
}

// Sell returns the unit price of the commodity after it has been sold.
func (m *Market) Sell(t CommodityType, quantity int64) (float64, error) {
	c, ok := m.Commodities[t]
	if !ok {
		return 0, fmt.Errorf("Invalid commodity type: %v", t)
	}
	return m.Modifier[t] * c.Sell(quantity), nil
}

type Commodity struct {
	// Name is the name of the commodity.
	Name string

	// Supply is the amount of product that the market is holding,
	// currently trying to sell.
	Supply int64

	// Value is the value of the product if it was infinitely rare, i.e.
	// supply is zero.
	Value float64

	// Demand is the number which, when supply = demand, the price is 1/2
	// of the Value.
	Demand float64
}

// Price returns the current market price for a single unit of the commodity,
// by looking at the amount of supply.
func (c *Commodity) Price() float64 {
	return c.Value * math.Exp2(-float64(c.Supply)/c.Demand)
}

// Sell takes a quantity of items to be sold, and returns the price
// that you get for selling it.
func (c *Commodity) Sell(quantity int64) float64 {
	initialPrice := c.Price()
	c.Supply += quantity - 1
	finalPrice := c.Price()
	c.Supply += 1

	// To simplify it, just average the initial and final price. It gives a
	// slight benefit to users that sell in bulk.
	return (initialPrice + finalPrice) / 2
}
