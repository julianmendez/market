package soda.se.umu.cs.soda.prototype.example.market.serializer

import   soda.se.umu.cs.soda.prototype.example.market.core.Item
import   soda.se.umu.cs.soda.prototype.example.market.core.Market
import   soda.se.umu.cs.soda.prototype.example.market.core.Money
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
import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationEnum
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationProcessor





trait MarketSerializer
{



  def serialize_accounts (accounts : List [Money] ) : List [Operation] =
    accounts
      .zipWithIndex
      .map ( pair => OpDeposit .mk (pair ._2) (pair ._1) )

  def serialize_items (items : List [Item] ) : List [Operation] =
    items
      .zipWithIndex
      .flatMap ( pair =>
        List [Operation] (
          OpAssign .mk (pair ._2) (pair ._1 .owner) ,
          OpPrice .mk (pair ._2) (pair ._1 .price)
        )
      )

  def serialize (m : Market) : List [Operation] =
     serialize_accounts (m .accounts) .++ (
       serialize_items (m .items)
     )

}

case class MarketSerializer_ () extends MarketSerializer

object MarketSerializer {
  def mk : MarketSerializer =
    MarketSerializer_ ()
}


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


trait YamlSerializer
{



  lazy val market_serializer = MarketSerializer .mk

  lazy val operation_serializer = OperationSerializer .mk

  def serialize_operations (operations : List [Operation] ) : String =
    "operations:" +
    "\n- " +
    operations
      .map ( operation => operation_serializer .serialize (operation) )
      .mkString ("\n- ") +
    "\n\n"

  def serialize_market (m : Market) : String =
    "---\n" +
    serialize_operations (
      market_serializer .serialize (m)
    )

}

case class YamlSerializer_ () extends YamlSerializer

object YamlSerializer {
  def mk : YamlSerializer =
    YamlSerializer_ ()
}

