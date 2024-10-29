package soda.se.umu.cs.soda.prototype.example.market.measurement

/*
 * This package contains classes to create instances for measurements
 *
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationProcessor
import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OpAssign
import   soda.se.umu.cs.soda.prototype.example.market.core.OpAssign_
import   soda.se.umu.cs.soda.prototype.example.market.core.OpDeposit
import   soda.se.umu.cs.soda.prototype.example.market.core.OpDeposit_
import   soda.se.umu.cs.soda.prototype.example.market.core.OpPrice
import   soda.se.umu.cs.soda.prototype.example.market.core.OpPrice_
import   soda.se.umu.cs.soda.prototype.example.market.core.OpSell
import   soda.se.umu.cs.soda.prototype.example.market.core.OpSell_
import   soda.se.umu.cs.soda.prototype.example.market.core.OpUndefined
import   soda.se.umu.cs.soda.prototype.example.market.core.OpUndefined_
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationEnum





case class OperationGeneratorSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val generator = OperationGenerator .mk

  lazy val accounts = 10000

  lazy val items = 20000

  lazy val transactions = 1000000

  lazy val instance : List [Operation] =
    generator .generate (accounts) (items) (transactions)

  def complies_invariant (op : Operation) : Boolean =
    op match  {
      case OpDeposit_ (user_id , amount) =>
        (0 <= user_id) && (user_id < accounts) && (amount >= 0)
      case OpAssign_ (item_id , user_id) =>
        (0 <= item_id) && (item_id < items) && (0 <= user_id) && (user_id < accounts)
      case OpPrice_ (item_id , price) =>
        (0 <= item_id) && (item_id < items) && (price >= 0)
      case OpSell_ (item_id , user_id) =>
        (0 <= item_id) && (item_id < items) && (0 <= user_id) && (user_id < accounts)
      case otherwise => false
    }

  def find_exceptions_to_invariant (operations : List [Operation] ) : List [Operation] =
    operations
      .filter ( operation => ! complies_invariant (operation) )

  test ("count exceptions to invariant") (
    check (
      obtained = find_exceptions_to_invariant (instance) .length
    ) (
      expected = 0
    )
  )

}

