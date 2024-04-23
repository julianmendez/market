package soda.se.umu.cs.soda.prototype.example.market.parser

import   org.scalatest.funsuite.AnyFunSuite
import   org.scalatest.Assertion
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader





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
          "assign 1 7" ,
          "assign 2 20" ,
          "assign 3 15" ,
          "assign 4 8" ,
          "assign 5 23" ,
          "assign 6 14" ,
          "assign 7 2" ,
          "assign 8 19" ,
          "assign 9 5" ,
          "assign 10 22" ,
          "assign 11 13" ,
          "assign 12 4" ,
          "assign 13 18" ,
          "assign 14 9" ,
          "assign 15 24" ,
          "assign 16 11" ,
          "assign 17 3" ,
          "assign 18 17" ,
          "assign 19 10" ,
          "assign 20 21" ,
          "assign 21 16" ,
          "assign 22 1" ,
          "assign 23 25" ,
          "assign 24 6" ,
          "assign 25 0" ,
          "assign 26 15" ,
          "assign 27 10" ,
          "assign 28 20" ,
          "assign 29 5" ,
          "price 0 40" ,
          "price 1 50" ,
          "price 2 25" ,
          "price 3 70" ,
          "price 4 100" ,
          "price 5 60" ,
          "price 6 30" ,
          "price 7 40" ,
          "price 8 25" ,
          "price 9 20" ,
          "price 10 90" ,
          "price 11 50" ,
          "price 12 100" ,
          "price 13 150" ,
          "price 14 30" ,
          "price 15 30" ,
          "price 16 25" ,
          "price 17 200" ,
          "price 18 20" ,
          "price 19 30" ,
          "price 20 20" ,
          "price 21 100" ,
          "price 22 100" ,
          "price 23 20" ,
          "price 24 30" ,
          "price 25 20" ,
          "price 26 25" ,
          "price 27 40" ,
          "price 28 80" ,
          "price 29 100" ,
          "transfer 10 25"
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

