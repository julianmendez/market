package soda.se.umu.cs.soda.prototype.example.market

/*
 * This package contains tests for classes to test a market.
 */

trait Market01
  extends
    Market
{

   lazy val accounts : List [Int] =
     List [Int] (1000, 500, 12000)

   lazy val items : List [Item] =
     List [Item] (
       Item_ (0, 125, false) ,
       Item_ (1, 375, false) ,
       Item_ (1, 10, false)
     )

}

case class Market01_ () extends Market01

object Market01 {
  def mk : Market01 =
    Market01_ ()
}

trait Market02
  extends
    Market
{

   lazy val accounts : List [Int] =
     List [Int] (1000, 500, 12000)

   lazy val items : List [Item] =
     List [Item] (
       Item_ (0, 125, false) ,
       Item_ (1, 375, true) ,
       Item_ (1, 10, false)
     )

}

case class Market02_ () extends Market02

object Market02 {
  def mk : Market02 =
    Market02_ ()
}

trait Market03
  extends
    Market
{

   lazy val accounts : List [Int] =
     List [Int] (1000, 875, 11625)

   lazy val items : List [Item] =
     List [Item] (
       Item_ (0, 125, false) ,
       Item_ (2, 375, false) ,
       Item_ (1, 10, false)
     )

}

case class Market03_ () extends Market03

object Market03 {
  def mk : Market03 =
    Market03_ ()
}


case class MarketSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val module = MarketMod_ (true)

  lazy val market01 : Market = Market01_ ()

  lazy val market02 : Market = Market02_ ()

  lazy val market03 : Market = Market03_ ()

  test ("should advertise an item") (
    check (
      obtained = module .advertise (market01) (1)
    ) (
      expected = module .as_market (market02)
    )
  )

  test ("should sell an item") (
    check (
      obtained =  module .sell (market02) (1) (2)
    ) (
      expected = module .as_market (market03)
    )
  )

}

