package soda.se.umu.cs.soda.prototype.example.market.serializer

import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationEnum
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





trait OperationSerializer
{



  lazy val op_enum = OperationEnum .mk

  lazy val empty = ""

  lazy val sp = " "

  def serialize (op : Operation) : String =
    op match  {
      case OpDeposit_ (user_id , amount) =>
        op_enum .deposit .name + sp + user_id .toString + sp + amount .toString
      case OpAssign_ (item_id , user_id) =>
        op_enum .assign .name + sp + item_id  .toString + sp + user_id .toString
      case OpPrice_ (item_id , price) =>
        op_enum .price .name + sp + item_id  .toString + sp + price .toString
      case OpSell_ (item_id , user_id) =>
        op_enum .sell .name + sp + item_id .toString + sp + user_id .toString
      case otherwise => empty
    }

}

case class OperationSerializer_ () extends OperationSerializer

object OperationSerializer {
  def mk : OperationSerializer =
    OperationSerializer_ ()
}

