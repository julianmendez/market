package soda.se.umu.cs.soda.prototype.example.market.main

/*
 * This package contains classes to model a market
 *
 */

import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.se.umu.cs.soda.prototype.example.market.core.Item
import   soda.se.umu.cs.soda.prototype.example.market.core.Market
import   soda.se.umu.cs.soda.prototype.example.market.core.Money
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationProcessor
import   soda.se.umu.cs.soda.prototype.example.market.core.MarketBuilder
import   soda.se.umu.cs.soda.prototype.example.market.parser.OperationParser
import   soda.se.umu.cs.soda.prototype.example.market.parser.YamlParser
import   soda.se.umu.cs.soda.prototype.example.market.serializer.YamlSerializer





/**
 * This is the main entry point.
 */

trait Main
{

  lazy val help = "" +
   "This is a multi-agent prototype to show a market modeled in the Soda language." +
   "\n" +
   "\nhttps://julianmendez.github.io/market/" +
   "\n" +
   "\nParameter: FILE_NAME" +
   "\n" +
   "\n  FILE_NAME     YAML file containing the market operations" +
   "\n" +
   "\n"

  def read_file (file_name : String) : String =
    new String (Files .readAllBytes (Paths .get (file_name) ) )

  lazy val market_builder = MarketBuilder .mk

  lazy val empty_market = market_builder .empty_market

  lazy val yaml_parser = YamlParser .mk

  lazy val yaml_serializer = YamlSerializer .mk

  lazy val operation_parser = OperationParser .mk

  lazy val operation_processor = OperationProcessor .mk

  def process_file (file_name : String) : Option [Market] =
    market_builder
      .build (
         operation_parser .parse (
           yaml_parser .parse ( new StringReader (read_file (file_name) ) )
         )
      )

  def serialize_market (maybe_market : Option [Market] ) : String =
    maybe_market match  {
      case Some (market) => yaml_serializer .serialize_market (market)
      case None => "Undefined market"
    }

  def execute (arguments : List [String] ) : Unit =
    if ( arguments .length > 0
    ) println (serialize_market (process_file (arguments .apply (0) ) ) )
    else println (help)

  def main (arguments : Array [String] ) : Unit =
    execute (arguments .toList)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

