package soda.se.umu.cs.soda.prototype.example.market.parser

import   soda.se.umu.cs.soda.prototype.example.market.core.Operation
import   soda.se.umu.cs.soda.prototype.example.market.core.OperationEnum
import   soda.se.umu.cs.soda.prototype.example.market.core.OpAssign
import   soda.se.umu.cs.soda.prototype.example.market.core.OpDeposit
import   soda.se.umu.cs.soda.prototype.example.market.core.OpPrice
import   soda.se.umu.cs.soda.prototype.example.market.core.OpSell
import   soda.se.umu.cs.soda.prototype.example.market.core.OpUndefined





/**
 * This is a generic YAML parser.
 * This parser converts all the Integer objects into String objects.
 */

trait GenericYamlParser
{

  import   org.snakeyaml.engine.v2.api.Load
  import   org.snakeyaml.engine.v2.api.LoadSettings
  import   java.io.Reader
  import   scala.jdk.CollectionConverters.CollectionHasAsScala
  import   scala.jdk.CollectionConverters.IteratorHasAsScala
  import   scala.jdk.CollectionConverters.MapHasAsScala

  /**
   * Converts Java data structures into immutable Scala data structures .
   * Integer objects are converted into String objects .
   *
   * @param obj object
   * @return the object with immutable Scala data structures
   */

  private def _rec_as_scala (obj : Any) : Option [Any] =
    obj match  {
      case x : String => Some (x)
      case x : Integer => Some (x .toString)
      case x : Tuple2 [Any, Any] =>
        _rec_as_scala (x ._1)
          .flatMap ( a =>
            _rec_as_scala (x ._2)
              .map ( b =>
                Tuple2 (a , b)
              )
          )
      case x : java.util.Map [Any, Any] =>
        Some (x .asScala
          .flatMap ( elem => _rec_as_scala (elem) )
          .toSeq
        )
      case x : java.util.Collection [Any] =>
        Some (x .asScala
          .flatMap ( elem => _rec_as_scala (elem) )
          .toSeq
        )
      case otherwise => None
    }

  /**
   * Parses a YAML document.
   *
   * @param reader reader
   * @return a structure with the YAML document.
   */

  def parse (reader : Reader) : Seq [Any] =
    ( new Load (LoadSettings .builder () .build () ) )
      .loadAllFromReader (reader)
      .iterator ()
      .asScala
      .toSeq
      .flatMap ( x => _rec_as_scala (x) )

}

case class GenericYamlParser_ () extends GenericYamlParser

object GenericYamlParser {
  def mk : GenericYamlParser =
    GenericYamlParser_ ()
}


trait OperationParser
{



  lazy val space = " "

  lazy val op_enum = OperationEnum .mk

  def parse_pieces_0 (op : String) (p0 : Int) (p1 : Int) : Operation =
    if ( (p0 >= 0) && (p1 >= 0)
    )
      if ( (op == op_enum .deposit .name) ) OpDeposit .mk (p0) (p1)
      else if ( op == op_enum .assign .name ) OpAssign .mk (p0) (p1)
      else if ( op == op_enum .price .name ) OpPrice .mk (p0) (p1)
      else if ( op == op_enum .sell .name ) OpSell .mk (p0) (p1)
      else OpUndefined .mk
    else OpUndefined .mk

  def parse_pieces_1 (op : String) (maybe_p0 : Option [Int] ) (p1 : Int) : Operation =
    maybe_p0 match  {
      case Some (p0) => parse_pieces_0 (op) (p0) (p1)
      case None => OpUndefined .mk
    }

  def parse_pieces_2 (op : String) (maybe_p0 : Option [Int] ) (maybe_p1 : Option [Int] )
      : Operation =
    maybe_p1 match  {
      case Some (p1) => parse_pieces_1 (op) (maybe_p0) (p1)
      case None => OpUndefined .mk
    }

  def parse_array (a : Array [String] ) : Operation  =
    if ( (a .length < 3)
    ) OpUndefined .mk
    else parse_pieces_2 (a .apply (0) ) (a .apply (1) .toIntOption) (a .apply (2) .toIntOption)

  def parse_operation_text (text : String) : Operation =
    parse_array (text .split (space) )

  def parse (seq : Seq [Seq [Tuple2 [String, Seq [String] ] ] ] ) :
      Seq [Operation] =
    seq
      .flatMap ( seq1 => seq1
        .flatMap ( tuple => tuple
           ._2
           .map ( operation_text =>
             parse_operation_text (operation_text)
           )
        )
      )

}

case class OperationParser_ () extends OperationParser

object OperationParser {
  def mk : OperationParser =
    OperationParser_ ()
}


/**
 * Parser for YAML format.
 */

trait YamlParser
{

  import   java.io.BufferedReader
  import   java.io.Reader

  def parse_string (part : Any) : Option [String] =
    part match  {
      case p : String => Some (p)
      case otherwise => None
    }

  def parse_seq_of_pairs (part : Any) : Option [Seq [String] ] =
    part match  {
      case p : Seq [Any] =>
        Some (p .flatMap ( e => parse_string (e)) )
      case otherwise => None
    }

  def parse_record (part : Any) : Option [Tuple2 [String, Seq [String] ] ] =
    part match  {
      case p : Tuple2 [Any, Any] =>
        parse_string (p ._1)
          .flatMap ( a =>
            parse_seq_of_pairs (p ._2)
              .map ( b =>
                Tuple2 (a , b)
              )
          )
      case otherwise => None
    }

  def get_as_seq (elem : Any) : Seq [Tuple2 [String, Seq [String] ] ] =
    elem match  {
      case e : Seq [Any] => e .flatMap ( x => parse_record (x) )
      case otherwise => Seq ()
    }

  def parse_record_list (parts : Seq [Any] )
      : Seq [Seq [Tuple2 [String, Seq [String] ] ] ] =
    parts .map ( elem => get_as_seq (elem) )

  def parse (reader : Reader)
      : Seq [Seq [Tuple2 [String, Seq [String] ] ] ] =
    parse_record_list (GenericYamlParser .mk .parse (reader) )

}

case class YamlParser_ () extends YamlParser

object YamlParser {
  def mk : YamlParser =
    YamlParser_ ()
}

