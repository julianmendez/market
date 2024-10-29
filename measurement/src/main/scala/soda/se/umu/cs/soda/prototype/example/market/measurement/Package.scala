package soda.se.umu.cs.soda.prototype.example.market.measurement

/*
 * This package contains classes to create instances for measurements
 *
 */

import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OpAssign
import   soda.se.umu.cs.soda.prototype.example.market.core.OpDeposit
import   soda.se.umu.cs.soda.prototype.example.market.core.OpPrice
import   soda.se.umu.cs.soda.prototype.example.market.core.OpSell
import   soda.se.umu.cs.soda.prototype.example.market.serializer.YamlSerializer





type Nat = Int
object Succ_ {
  def apply (n : Int) : Int = n + 1
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}

/*
directive lean
notation "Succ_" => Nat.succ
*/

/*
directive coq
Notation "'Succ_'" := S (at level 99) .
*/

trait Range
{



  private def _tailrec_range (non_negative_number : Nat) (sequence : List [Nat] ) : List [Nat] =
    non_negative_number match  {
      case Succ_ (k) =>
        _tailrec_range (k) ( (k) +: (sequence) )
      case _otherwise => sequence
    }

  def apply (length : Int) : List [Nat] =
    _tailrec_range (length) (Nil)

}

case class Range_ () extends Range

object Range {
  def mk : Range =
    Range_ ()
}


/**
 * This is the main entry point.
 */

trait Main
{



  import   scala.util.control.Exception.allCatch

  lazy val help = "" +
    "This creates an instance of synthetically generated transactions." +
    "\nIt outputs a YAML string containing the operations." +
    "\n" +
    "\nParameters: ACCOUNTS ITEMS TRANSACTIONS" +
    "\n" +
    "\n  ACCOUNTS      Number of accounts ('deposit') in the market" +
    "\n  ITEMS         Number of items ('assign' and 'price') in the market" +
    "\n  TRANSACTIONS  Number of transactions ('sell') in the market" +
    "\n" +
    "\n"

  lazy val operation_generator = OperationGenerator .mk

  lazy val yaml_serializer = YamlSerializer .mk

  def generate_operations (accounts : Nat) (items : Nat) (transactions : Nat) : List [Operation] =
    operation_generator
      .generate (accounts) (items) (transactions)

  def to_nat (n : Int) : Nat =
    if ( n < 0
    ) 0
    else n

  def to_int_or_zero (s : String) : Int =
    allCatch
      .opt (s .toInt)
      .getOrElse (0)

  def to_nat_or_zero (s : String) : Nat =
    to_nat (
      to_int_or_zero (s)
    )

  def create_header (accounts : Nat) (items : Nat) (transactions : Nat) : String =
    "---" +
    "\n# "+
    "\n# This is a synthetically generated instance." +
    "\n# " +
    "\n#   accounts: " + accounts .toString +
    "\n#   items: " + items .toString +
    "\n#   transactions: " + transactions .toString +
    "\n#" +
    "\n"

  def create_output (accounts : Nat) (items : Nat) (transactions : Nat) : String =
    (create_header (accounts) (items) (transactions) ) +
      yaml_serializer .serialize_operations (
        generate_operations (accounts) (items) (transactions)
      )

  def execute (arguments : List [String] ) : Unit =
    if ( arguments .length > 2
    ) println (
      create_output (
        to_nat_or_zero (arguments .apply (0) ) ) (
        to_nat_or_zero (arguments .apply (1) ) ) (
        to_nat_or_zero (arguments .apply (2) )
      )
    )
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


trait OperationGenerator
{



  lazy val deposit_factor = 100

  lazy val deposit_offset = 0

  lazy val max_deposit = 1000000

  lazy val owner_factor = 65537

  lazy val owner_offset = 1

  lazy val buyer_factor = 8191

  lazy val buyer_offset = 1

  lazy val merchandise_factor = 31

  lazy val merchandise_offset = 1

  lazy val price_factor = 127

  lazy val price_offset = 0

  lazy val max_price = 100000

  def abs (x : Int) : Nat =
    if ( x < 0
    ) -x
    else x

  def generate (index : Nat) (multiplier : Nat) (increment : Nat) (modulus : Nat) : Nat =
    abs ( (index * multiplier + increment) % modulus)

  def deposit_of_user (user_id : Nat) : Nat =
    generate (user_id) (deposit_factor) (deposit_offset) (max_deposit)

  def owner_of_item (item_id : Nat) (accounts : Nat) : Nat =
    generate (item_id) (owner_factor) (owner_offset) (accounts)

  def price_of_item (item_id : Nat) : Nat =
    generate (item_id) (price_factor) (price_offset) (max_price)

  def buyer_of_item (transaction_id : Nat) (accounts : Nat) : Nat =
    generate (transaction_id) (buyer_factor) (buyer_offset) (accounts)

  def item_being_sold (transaction_id : Nat) (items : Nat) : Nat =
    generate (transaction_id) (merchandise_factor) (merchandise_offset) (items)

  def make_deposits (accounts : Nat) : List [Operation] =
    Range .mk .apply (accounts)
      .map ( user_id =>
        OpDeposit .mk (user_id) (deposit_of_user (user_id) )
      )

  def make_items_with_prices (accounts : Nat) (items : Nat) : List [Operation] =
    Range .mk .apply (items)
      .flatMap ( item_id =>
        List [Operation] (
          OpAssign .mk (item_id) (owner_of_item (item_id) (accounts) ) ,
          OpPrice .mk (item_id) (price_of_item (item_id) )
        )
      )

  private def _make_transaction_pair (item_id : Nat) (buyer_id : Nat) : List [Operation] =
    List [Operation] (
      OpSell .mk (item_id ) (buyer_id) ,
      OpPrice .mk (item_id) (price_of_item (item_id) )
    )

  def make_transactions (accounts : Nat) (items : Nat) (transactions : Nat) : List [Operation] =
    Range .mk .apply (transactions)
      .flatMap ( transaction_id =>
        _make_transaction_pair (
          item_being_sold (transaction_id) (items) ) (
          buyer_of_item (transaction_id) (accounts)
        )
      )

  def generate (accounts : Nat) (items : Nat) (transactions : Nat) : List [Operation] =
    if ( (accounts > 0) && (items > 0) && (transactions > 0)
    )
      make_deposits (accounts) .++ (
        make_items_with_prices (accounts) (items) .++ (
          make_transactions (accounts) (items) (transactions)
        )
      )
    else List [Operation] ()

}

case class OperationGenerator_ () extends OperationGenerator

object OperationGenerator {
  def mk : OperationGenerator =
    OperationGenerator_ ()
}

