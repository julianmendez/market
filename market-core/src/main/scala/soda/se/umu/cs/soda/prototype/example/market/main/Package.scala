package soda.se.umu.cs.soda.prototype.example.market.main

/*
 * This package contains classes to model a market
 *
 */

/**
 * This is the main entry point.
 */

trait Main
{

  def execute (arguments : Seq [String] ) : Unit =
    print ("Under construction ...\n")

  def main (arguments : Array [String] ) : Unit =
    execute (arguments .toSeq)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

