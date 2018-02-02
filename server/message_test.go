package main

import "testing"

func TestSellingCommodity(t *testing.T) {
	c := Commodity{
		Supply: 100,
		Value:  2.00,
		Demand: 100,
	}

	got := c.Price()
	want := 1.00

	if got != want {
		t.Errorf("c.Price() = %v, want %v", got, want)
	}
}

func TestPriceDropsWhenSelling(t *testing.T) {
	c := Commodity{
		Supply: 100,
		Value:  2.00,
		Demand: 100,
	}

	before := c.Price()
	price := c.Sell(100)
	after := c.Price()

	if before <= price {
		t.Errorf("price before (%v) <= sale price (%v)"+
			", want before > sale price", before, price)
	}

	if price > after {
		t.Errorf("sale price (%v) <= price after (%v)"+
			", want sale price > price after", price, after)
	}
}
