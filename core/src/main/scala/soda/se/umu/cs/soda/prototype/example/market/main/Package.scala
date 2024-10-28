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
import   soda.se.umu.cs.soda.prototype.example.market.parser.OperationParser
import   soda.se.umu.cs.soda.prototype.example.market.parser.YamlParser
import   soda.se.umu.cs.soda.prototype.example.market.serializer.YamlSerializer

/**
 * This is the main entry point.
 */

trait Main
{

  lazy val help = "Usage: it has one parameter, a YAML file containing the operations."

  def read_file (file_name : String) : String =
    new String (Files .readAllBytes (Paths .get (file_name) ) )

  lazy val empty_market = Market .mk (List [Money] () ) (List [Item] () )

  lazy val yaml_parser = YamlParser .mk

  lazy val yaml_serializer = YamlSerializer .mk

  lazy val operation_parser = OperationParser .mk

  lazy val operation_processor = OperationProcessor .mk

  def process_file (file_name : String) : Option [Market] =
    operation_processor
      .process (Some (empty_market) ) (
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

