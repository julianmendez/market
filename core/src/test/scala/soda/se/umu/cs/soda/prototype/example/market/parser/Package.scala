package soda.se.umu.cs.soda.prototype.example.market.parser

import   org.scalatest.funsuite.AnyFunSuite
import   org.scalatest.Assertion
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.se.umu.cs.soda.prototype.example.market.core.Market
import   soda.se.umu.cs.soda.prototype.example.market.core.MarketBuilder
import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OpAssign
import   soda.se.umu.cs.soda.prototype.example.market.core.OpDeposit
import   soda.se.umu.cs.soda.prototype.example.market.core.OpPrice
import   soda.se.umu.cs.soda.prototype.example.market.core.OpSell
import   soda.se.umu.cs.soda.prototype.example.market.core.OpUndefined





trait Example0Instance
{



  lazy val instance = Seq (
    Seq (
      Tuple2 ("operations" ,
        Seq (
          "deposit 0 4500" ,
          "deposit 1 75" ,
          "deposit 2 1800" ,
          "deposit 3 2000" ,
          "deposit 4 5700" ,
          "deposit 5 2500" ,
          "deposit 6 1900" ,
          "deposit 7 2200" ,
          "deposit 8 6600" ,
          "deposit 9 2400" ,
          "deposit 10 1750" ,
          "deposit 11 2150" ,
          "deposit 12 1850" ,
          "deposit 13 2350" ,
          "deposit 14 1950" ,
          "deposit 15 2100" ,
          "deposit 16 1650" ,
          "deposit 17 2450" ,
          "deposit 18 7950" ,
          "deposit 19 2250" ,
          "deposit 20 1700" ,
          "deposit 21 2300" ,
          "deposit 22 1800" ,
          "deposit 23 2400" ,
          "deposit 24 1750" ,
          "deposit 25 2350" ,
          "assign 0 12" ,
          "price 0 40" ,
          "assign 1 7" ,
          "price 1 50" ,
          "assign 2 20" ,
          "price 2 25" ,
          "assign 3 15" ,
          "price 3 70" ,
          "assign 4 8" ,
          "price 4 100" ,
          "assign 5 23" ,
          "price 5 60" ,
          "assign 6 14" ,
          "price 6 30" ,
          "assign 7 2" ,
          "price 7 40" ,
          "assign 8 19" ,
          "price 8 25" ,
          "assign 9 5" ,
          "price 9 20" ,
          "assign 10 22" ,
          "price 10 90" ,
          "assign 11 13" ,
          "price 11 50" ,
          "assign 12 4" ,
          "price 12 100" ,
          "assign 13 18" ,
          "price 13 150" ,
          "assign 14 9" ,
          "price 14 30" ,
          "assign 15 24" ,
          "price 15 30" ,
          "assign 16 11" ,
          "price 16 25" ,
          "assign 17 3" ,
          "price 17 200" ,
          "assign 18 17" ,
          "price 18 20" ,
          "assign 19 10" ,
          "price 19 30" ,
          "assign 20 21" ,
          "price 20 20" ,
          "assign 21 16" ,
          "price 21 100" ,
          "assign 22 1" ,
          "price 22 100" ,
          "assign 23 25" ,
          "price 23 20" ,
          "assign 24 6" ,
          "price 24 30" ,
          "assign 25 0" ,
          "price 25 20" ,
          "assign 26 15" ,
          "price 26 25" ,
          "assign 27 10" ,
          "price 27 40" ,
          "assign 28 20" ,
          "price 28 80" ,
          "assign 29 5" ,
          "price 29 100" ,
          "sell 10 25"
        )
      )
    )
  )

}

case class Example0Instance_ () extends Example0Instance

object Example0Instance {
  def mk : Example0Instance =
    Example0Instance_ ()
}


trait Example0OperationList
{



  lazy val basic_instance : List [Operation] =
    List [Operation] (
      OpDeposit .mk (0) (4500) ,
      OpDeposit .mk (1) (75) ,
      OpDeposit .mk (2) (1800) ,
      OpDeposit .mk (3) (2000) ,
      OpDeposit .mk (4) (5700) ,
      OpDeposit .mk (5) (2500) ,
      OpDeposit .mk (6) (1900) ,
      OpDeposit .mk (7) (2200) ,
      OpDeposit .mk (8) (6600) ,
      OpDeposit .mk (9) (2400) ,
      OpDeposit .mk (10) (1750) ,
      OpDeposit .mk (11) (2150) ,
      OpDeposit .mk (12) (1850) ,
      OpDeposit .mk (13) (2350) ,
      OpDeposit .mk (14) (1950) ,
      OpDeposit .mk (15) (2100) ,
      OpDeposit .mk (16) (1650) ,
      OpDeposit .mk (17) (2450) ,
      OpDeposit .mk (18) (7950) ,
      OpDeposit .mk (19) (2250) ,
      OpDeposit .mk (20) (1700) ,
      OpDeposit .mk (21) (2300) ,
      OpDeposit .mk (22) (1800) ,
      OpDeposit .mk (23) (2400) ,
      OpDeposit .mk (24) (1750) ,
      OpDeposit .mk (25) (2350) ,
      OpAssign .mk (0) (12) ,
      OpPrice .mk (0) (40) ,
      OpAssign .mk (1) (7) ,
      OpPrice .mk (1) (50) ,
      OpAssign .mk (2) (20) ,
      OpPrice .mk (2) (25) ,
      OpAssign .mk (3) (15) ,
      OpPrice .mk (3) (70) ,
      OpAssign .mk (4) (8) ,
      OpPrice .mk (4) (100) ,
      OpAssign .mk (5) (23) ,
      OpPrice .mk (5) (60) ,
      OpAssign .mk (6) (14) ,
      OpPrice .mk (6) (30) ,
      OpAssign .mk (7) (2) ,
      OpPrice .mk (7) (40) ,
      OpAssign .mk (8) (19) ,
      OpPrice .mk (8) (25) ,
      OpAssign .mk (9) (5) ,
      OpPrice .mk (9) (20) ,
      OpAssign .mk (10) (22) ,
      OpPrice .mk (10) (90) ,
      OpAssign .mk (11) (13) ,
      OpPrice .mk (11) (50) ,
      OpAssign .mk (12) (4) ,
      OpPrice .mk (12) (100) ,
      OpAssign .mk (13) (18) ,
      OpPrice .mk (13) (150) ,
      OpAssign .mk (14) (9) ,
      OpPrice .mk (14) (30) ,
      OpAssign .mk (15) (24) ,
      OpPrice .mk (15) (30) ,
      OpAssign .mk (16) (11) ,
      OpPrice .mk (16) (25) ,
      OpAssign .mk (17) (3) ,
      OpPrice .mk (17) (200) ,
      OpAssign .mk (18) (17) ,
      OpPrice .mk (18) (20) ,
      OpAssign .mk (19) (10) ,
      OpPrice .mk (19) (30) ,
      OpAssign .mk (20) (21) ,
      OpPrice .mk (20) (20) ,
      OpAssign .mk (21) (16) ,
      OpPrice .mk (21) (100) ,
      OpAssign .mk (22) (1) ,
      OpPrice .mk (22) (100) ,
      OpAssign .mk (23) (25) ,
      OpPrice .mk (23) (20) ,
      OpAssign .mk (24) (6) ,
      OpPrice .mk (24) (30) ,
      OpAssign .mk (25) (0) ,
      OpPrice .mk (25) (20) ,
      OpAssign .mk (26) (15) ,
      OpPrice .mk (26) (25) ,
      OpAssign .mk (27) (10) ,
      OpPrice .mk (27) (40) ,
      OpAssign .mk (28) (20) ,
      OpPrice .mk (28) (80) ,
      OpAssign .mk (29) (5) ,
      OpPrice .mk (29) (100)
    )

  lazy val instance : Seq [Operation] =
    basic_instance .++ (
      List [Operation] ( OpSell .mk (10) (25) )
    )

  lazy val market_builder = MarketBuilder .mk

  lazy val market_basic_instance : Market =
    market_builder
      .build (basic_instance)
      .getOrElse (market_builder .empty_market)

}

case class Example0OperationList_ () extends Example0OperationList

object Example0OperationList {
  def mk : Example0OperationList =
    Example0OperationList_ ()
}


case class OperationParserSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  lazy val yaml_parser = YamlParser .mk

  lazy val operation_parser = OperationParser .mk

  lazy val example0_name = "/example/example0.yaml"

  lazy val example0_contents = read_file (example0_name)

  lazy val example0_instance = Example0OperationList .mk .instance

  test ("read example 0 as operations") (
    check (
      obtained =
        operation_parser .parse (
          yaml_parser .parse ( new StringReader (example0_contents) )
        )
    ) (
      expected = example0_instance
    )
  )

}


case class YamlParserSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  lazy val parser = YamlParser .mk

  lazy val example0_name = "/example/example0.yaml"

  lazy val example0_contents = read_file (example0_name)

  lazy val example0_instance = Example0Instance .mk .instance

  test ("read example 0") (
    check (
      obtained = parser .parse ( new StringReader (example0_contents) )
    ) (
      expected = example0_instance
    )
  )

}

