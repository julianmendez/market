package soda.se.umu.cs.soda.prototype.example.market.core

/*
 * This package contains tests for classes to test a market.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   org.scalatest.Assertion
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.se.umu.cs.soda.prototype.example.market.parser.OperationParser
import   soda.se.umu.cs.soda.prototype.example.market.parser.YamlParser

trait Market01
  extends
    Market
{



   lazy val accounts : List [Int] =
     List [Int] (1000, 500, 12000)

   lazy val items : List [Item] =
     List [Item] (
       Item .mk (0) (125) ,
       Item .mk (1) (0) ,
       Item .mk (1) (10)
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
       Item .mk (0) (125) ,
       Item .mk (1) (375) ,
       Item .mk (1) (10)
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
       Item .mk (0) (125) ,
       Item .mk (2) (0) ,
       Item .mk (1) (10)
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

  test ("should price an item") (
    check (
      obtained = module .price_item (market01) (1) (375)
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


case class MyListSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_list_0 : List [Int] =
    List (0 , 1 , 1 , 2 , 3 , 5 , 8)

  lazy val example_list_1 : List [Int] =
    List (13 , 21 , 34 , 55 , 89 , 144)

  lazy val short_list : List [Nat] =
     instance .range (100)

  lazy val long_list : List [Nat] =
     instance .range (1000000)

  lazy val instance : MyList =
    MyList .mk (true)

  test ("reverse") (
    check (
      obtained = instance .reverse [Int] (example_list_0)
    ) (
      expected = List (8 , 5 , 3 , 2 , 1 , 1 , 0)
    )
  )

  test ("length") (
    check (
      obtained = instance .length [Int] (example_list_0)
    ) (
      expected = 7
    )
  )

  test ("length_def - short list") (
    check (
      obtained = instance .length_def (short_list)
    ) (
      expected = 100
    )
  )

  test ("length_tr - long list") (
    check (
      obtained = instance .length_tr (long_list)
    ) (
      expected = 1000000
    )
  )

  test ("concat") (
    check (
      obtained = instance .concat [Int] (example_list_0) (example_list_1)
    ) (
      expected = List (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
    )
  )

  test ("map") (
    check (
      obtained = instance .map [Int, Int] (example_list_0) ( (x : Int) => x + 1)
    ) (
      expected = List (1, 2, 2, 3, 4, 6, 9)
    )
  )

  test ("get 1") (
    check (
      obtained = instance .get [Int] (example_list_0) (0)
    ) (
      expected = Some (0)
    )
  )

  test ("get 2") (
    check (
      obtained = instance .get [Int] (example_list_0) (3)
    ) (
      expected = Some (2)
    )
  )

  test ("get 3") (
    check (
      obtained = instance .get [Int] (example_list_0) (6)
    ) (
      expected = Some (8)
    )
  )

  test ("get 4") (
    check (
      obtained = instance .get [Int] (example_list_0) (7)
    ) (
      expected = None
    )
  )

  test ("get 5") (
    check (
      obtained = instance .get [Int] (example_list_0) (-1)
    ) (
      expected = None
    )
  )

  test ("set 1") (
    check (
      obtained = instance .set [Int] (example_list_0) (0) (144)
    ) (
      expected = List (144, 1, 1, 2, 3, 5, 8)
    )
  )

  test ("set 2") (
    check (
      obtained = instance .set [Int] (example_list_0) (3) (144)
    ) (
      expected = List (0, 1, 1, 144, 3, 5, 8)
    )
  )

  test ("set 3") (
    check (
      obtained = instance .set [Int] (example_list_0) (6) (144)
    ) (
      expected = List (0, 1, 1, 2, 3, 5, 144)
    )
  )

  test ("set 4") (
    check (
      obtained = instance .set [Int] (example_list_0) (7) (144)
    ) (
      expected = List (0, 1, 1, 2, 3, 5, 8)
    )
  )

  test ("set 5") (
    check (
      obtained = instance .set [Int] (example_list_0) (-1) (144)
    ) (
      expected = List (0, 1, 1, 2, 3, 5, 8)
    )
  )

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

  lazy val empty_market = Market .mk (List [Money] () ) (List [Item] () )

  lazy val yaml_parser = YamlParser .mk

  lazy val operation_parser = OperationParser .mk

  lazy val operation_processor = OperationProcessor .mk

  lazy val example0_name = "/example/example0.yaml"

  lazy val example0_contents = read_file (example0_name)

  lazy val processed_instance =
    operation_processor
      .process (Some (empty_market) ) (
         operation_parser .parse (
           yaml_parser .parse ( new StringReader (example0_contents) )
         )
      )

  test ("apply operations and check accounts") (
    check (
      obtained = processed_instance .get .accounts .length
    ) (
      expected = 26
    )
  )

  test ("apply operations and check items") (
    check (
      obtained = processed_instance .get .items .length
    ) (
      expected = 30
    )
  )

  test ("apply operations and check ownership of 1 item") (
    check (
      obtained = processed_instance .get .items .apply (10) .owner
    ) (
      expected = 25
    )
  )

}

