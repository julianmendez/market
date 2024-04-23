package soda.se.umu.cs.soda.prototype.example.market.parser

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

