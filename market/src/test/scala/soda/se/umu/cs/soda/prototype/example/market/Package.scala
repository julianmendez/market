package soda.se.umu.cs.soda.prototype.example.market

/*
 * This package contains tests for classes to test a market.
 */

trait Package

trait Market01
  extends
    Market
{

   lazy val accounts : Seq [Int] =
     Seq [Int] (1000, 500, 12000)

   lazy val items : Seq [Item] =
     Seq [Item] (
       Item_ (0, 125, false) ,
       Item_ (1, 375, false) ,
       Item_ (1, 10, false)
     )

}

case class Market01_ () extends Market01

trait Market02
  extends
    Market
{

   lazy val accounts : Seq [Int] =
     Seq [Int] (1000, 500, 12000)

   lazy val items : Seq [Item] =
     Seq [Item] (
       Item_ (0, 125, false) ,
       Item_ (1, 375, true) ,
       Item_ (1, 10, false)
     )

}

case class Market02_ () extends Market02

trait Market03
  extends
    Market
{

   lazy val accounts : Seq [Int] =
     Seq [Int] (1000, 875, 11625)

   lazy val items : Seq [Item] =
     Seq [Item] (
       Item_ (0, 125, false) ,
       Item_ (2, 375, false) ,
       Item_ (1, 10, false)
     )

}

case class Market03_ () extends Market03


case class MarketSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val market01 : Market = Market01_ ()

  lazy val market02 : Market = Market02_ ()

  lazy val market03 : Market = Market03_ ()

  test ("should advertise an item") (
    check (
      obtained = market01 .advertise (1)
    ) (
      expected = market02 .as_market ()
    )
  )

  test ("should sell an item") (
    check (
      obtained = market02 .sell (1) (2)
    ) (
      expected = market03 .as_market ()
    )
  )

}

