package soda.se.umu.cs.soda.prototype.example.market.parser

import   org.scalatest.funsuite.AnyFunSuite
import   org.scalatest.Assertion
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader





trait Example0Instance
{



  lazy val instance = Seq (
    Seq (
      Tuple2 ("operations" ,
        Seq (
          Tuple2 ("0" , "deposit 0 4500") ,
          Tuple2 ("1" , "deposit 1 75") ,
          Tuple2 ("2" , "deposit 2 1800") ,
          Tuple2 ("3" , "deposit 3 2000") ,
          Tuple2 ("4" , "deposit 4 5700") ,
          Tuple2 ("5" , "deposit 5 2500") ,
          Tuple2 ("6" , "deposit 6 1900") ,
          Tuple2 ("7" , "deposit 7 2200") ,
          Tuple2 ("8" , "deposit 8 6600") ,
          Tuple2 ("9" , "deposit 9 2400") ,
          Tuple2 ("10" , "deposit 10 1750") ,
          Tuple2 ("11" , "deposit 11 2150") ,
          Tuple2 ("12" , "deposit 12 1850") ,
          Tuple2 ("13" , "deposit 13 2350") ,
          Tuple2 ("14" , "deposit 14 1950") ,
          Tuple2 ("15" , "deposit 15 2100") ,
          Tuple2 ("16" , "deposit 16 1650") ,
          Tuple2 ("17" , "deposit 17 2450") ,
          Tuple2 ("18" , "deposit 18 7950") ,
          Tuple2 ("19" , "deposit 19 2250") ,
          Tuple2 ("20" , "deposit 20 1700") ,
          Tuple2 ("21" , "deposit 21 2300") ,
          Tuple2 ("22" , "deposit 22 1800") ,
          Tuple2 ("23" , "deposit 23 2400") ,
          Tuple2 ("24" , "deposit 24 1750") ,
          Tuple2 ("25" , "deposit 25 2350") ,
          Tuple2 ("26" , "assign 0 12") ,
          Tuple2 ("27" , "assign 1 7") ,
          Tuple2 ("28" , "assign 2 20") ,
          Tuple2 ("29" , "assign 3 15") ,
          Tuple2 ("30" , "assign 4 8") ,
          Tuple2 ("31" , "assign 5 23") ,
          Tuple2 ("32" , "assign 6 14") ,
          Tuple2 ("33" , "assign 7 2") ,
          Tuple2 ("34" , "assign 8 19") ,
          Tuple2 ("35" , "assign 9 5") ,
          Tuple2 ("36" , "assign 10 22") ,
          Tuple2 ("37" , "assign 11 13") ,
          Tuple2 ("38" , "assign 12 4") ,
          Tuple2 ("39" , "assign 13 18") ,
          Tuple2 ("40" , "assign 14 9") ,
          Tuple2 ("41" , "assign 15 24") ,
          Tuple2 ("42" , "assign 16 11") ,
          Tuple2 ("43" , "assign 17 3") ,
          Tuple2 ("44" , "assign 18 17") ,
          Tuple2 ("45" , "assign 19 10") ,
          Tuple2 ("46" , "assign 20 21") ,
          Tuple2 ("47" , "assign 21 16") ,
          Tuple2 ("48" , "assign 22 1") ,
          Tuple2 ("49" , "assign 23 25") ,
          Tuple2 ("50" , "assign 24 6") ,
          Tuple2 ("51" , "assign 25 0") ,
          Tuple2 ("52" , "assign 26 15") ,
          Tuple2 ("53" , "assign 27 10") ,
          Tuple2 ("54" , "assign 28 20") ,
          Tuple2 ("55" , "assign 29 5") ,
          Tuple2 ("56" , "price 0 40") ,
          Tuple2 ("57" , "price 1 50") ,
          Tuple2 ("58" , "price 2 25") ,
          Tuple2 ("59" , "price 3 70") ,
          Tuple2 ("60" , "price 4 100") ,
          Tuple2 ("61" , "price 5 60") ,
          Tuple2 ("62" , "price 6 30") ,
          Tuple2 ("63" , "price 7 40") ,
          Tuple2 ("64" , "price 8 25") ,
          Tuple2 ("65" , "price 9 20") ,
          Tuple2 ("66" , "price 10 90") ,
          Tuple2 ("67" , "price 11 50") ,
          Tuple2 ("68" , "price 12 100") ,
          Tuple2 ("69" , "price 13 150") ,
          Tuple2 ("70" , "price 14 30") ,
          Tuple2 ("71" , "price 15 30") ,
          Tuple2 ("72" , "price 16 25") ,
          Tuple2 ("73" , "price 17 200") ,
          Tuple2 ("74" , "price 18 20") ,
          Tuple2 ("75" , "price 19 30") ,
          Tuple2 ("76" , "price 20 20") ,
          Tuple2 ("77" , "price 21 100") ,
          Tuple2 ("78" , "price 22 100") ,
          Tuple2 ("79" , "price 23 20") ,
          Tuple2 ("80" , "price 24 30") ,
          Tuple2 ("81" , "price 25 20") ,
          Tuple2 ("82" , "price 26 25") ,
          Tuple2 ("83" , "price 27 40") ,
          Tuple2 ("84" , "price 28 80") ,
          Tuple2 ("85" , "price 29 100") ,
          Tuple2 ("86" , "transfer 10 25")
        )
      )
    )
  )

}

case class Example0Instance_ () extends Example0Instance

object Example0Instance {
  def mk : Example0Instance =
    Example0Instance_ ()
}


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

  lazy val parser = YamlParser .mk

  lazy val example0_name = "/example/example0.yaml"

  lazy val example0_contents = read_file (example0_name)

  lazy val example0_instance = Example0Instance .mk .instance

  test ("read example 0") (
    check (
      obtained = parser .parse ( new StringReader (example0_contents) )
    ) (
      expected = example0_instance
    )
  )

}

