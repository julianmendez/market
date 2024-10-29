package soda.se.umu.cs.soda.prototype.example.market.serializer

import   org.scalatest.funsuite.AnyFunSuite
import   org.scalatest.Assertion
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.se.umu.cs.soda.prototype.example.market.parser.Example0Instance
import   soda.se.umu.cs.soda.prototype.example.market.parser.Example0OperationList
import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OpAssign
import   soda.se.umu.cs.soda.prototype.example.market.core.OpDeposit
import   soda.se.umu.cs.soda.prototype.example.market.core.OpPrice
import   soda.se.umu.cs.soda.prototype.example.market.core.OpSell
import   soda.se.umu.cs.soda.prototype.example.market.core.OpUndefined
import   soda.se.umu.cs.soda.prototype.example.market.core.Item
import   soda.se.umu.cs.soda.prototype.example.market.core.Market
import   soda.se.umu.cs.soda.prototype.example.market.core.MarketBuilder
import   soda.se.umu.cs.soda.prototype.example.market.core.Money
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationProcessor
import   soda.se.umu.cs.soda.prototype.example.market.parser.OperationParser
import   soda.se.umu.cs.soda.prototype.example.market.parser.YamlParser





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

  lazy val yaml_serializer = YamlSerializer .mk

  lazy val example0_name = "/example/example1.yaml"

  lazy val example0_contents =
    read_file (example0_name)

  lazy val example0_market = Example0OperationList .mk .market_basic_instance

  test ("serializer example 1") (
    check (
      obtained = example0_contents
    ) (
      expected = yaml_serializer .serialize_market (example0_market)
    )
  )

}

