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
      Tuple2 ("agents" ,
        Seq (
          Tuple2("0" , "Alice") ,
          Tuple2("1" , "Benjamin") ,
          Tuple2("2" , "Charlotte") ,
          Tuple2("3" , "Daniel") ,
          Tuple2("4" , "Emily") ,
          Tuple2("5" , "Fiona") ,
          Tuple2("6" , "George") ,
          Tuple2("7" , "Hannah") ,
          Tuple2("8" , "Isaac") ,
          Tuple2("9" , "James") ,
          Tuple2("10" , "Kevin") ,
          Tuple2("11" , "Lily") ,
          Tuple2("12" , "Matthew") ,
          Tuple2("13" , "Natalie") ,
          Tuple2("14" , "Olivia") ,
          Tuple2("15" , "Quinn") ,
          Tuple2("16" , "Peter") ,
          Tuple2("17" , "Rachel") ,
          Tuple2("18" , "Sarah") ,
          Tuple2("19" , "Timothy") ,
          Tuple2("20" , "Ursula") ,
          Tuple2("21" , "Victoria") ,
          Tuple2("22" , "William") ,
          Tuple2("23" , "Xavier") ,
          Tuple2("24" , "Yasmine") ,
          Tuple2("25" , "Zachary")
        )
      ) ,
      Tuple2 ("balances" ,
        Seq (
          Tuple2 ("0" , "4500") ,
          Tuple2 ("1" , "75") ,
          Tuple2 ("2" , "1800") ,
          Tuple2 ("3" , "2000") ,
          Tuple2 ("4" , "5700") ,
          Tuple2 ("5" , "2500") ,
          Tuple2 ("6" , "1900") ,
          Tuple2 ("7" , "2200") ,
          Tuple2 ("8" , "6600") ,
          Tuple2 ("9" , "2400") ,
          Tuple2 ("10" , "1750") ,
          Tuple2 ("11" , "2150") ,
          Tuple2 ("12" , "1850") ,
          Tuple2 ("13" , "2350") ,
          Tuple2 ("14" , "1950") ,
          Tuple2 ("15" , "2100") ,
          Tuple2 ("16" , "1650") ,
          Tuple2 ("17" , "2450") ,
          Tuple2 ("18" , "7950") ,
          Tuple2 ("19" , "2250") ,
          Tuple2 ("20" , "1700") ,
          Tuple2 ("21" , "2300") ,
          Tuple2 ("22" , "1800") ,
          Tuple2 ("23" , "2400") ,
          Tuple2 ("24" , "1750") ,
          Tuple2 ("25" , "2350")
        )
      ) ,
      Tuple2 ("resources" ,
        Seq (
          Tuple2 ("0" , "Aromatherapy Essential Oil Diffuser" ) ,
          Tuple2 ("1" , "Bluetooth Speaker" ) ,
          Tuple2 ("2" , "Cookbook from a Famous Chef" ) ,
          Tuple2 ("3" , "Digital Photo Frame" ) ,
          Tuple2 ("4" , "Electric Toothbrush" ) ,
          Tuple2 ("5" , "Fitness Tracker" ) ,
          Tuple2 ("6" , "Gourmet Spice Set" ) ,
          Tuple2 ("7" , "Handheld Massager" ) ,
          Tuple2 ("8" , "Insulated Stainless Steel Water Bottle" ) ,
          Tuple2 ("9" , "Jigsaw Puzzle (1000 pieces)" ) ,
          Tuple2 ("10" , "Kindle E-reader" ) ,
          Tuple2 ("11" , "Leather Wallet" ) ,
          Tuple2 ("12" , "Mini Projector" ) ,
          Tuple2 ("13" , "Noise Cancelling Earbuds" ) ,
          Tuple2 ("14" , "Outdoor Solar Lights (set of 4)" ) ,
          Tuple2 ("15" , "Portable Charger" ) ,
          Tuple2 ("16" , "Quality Wine Aerator" ) ,
          Tuple2 ("17" , "Robot Vacuum Cleaner" ) ,
          Tuple2 ("18" , "Silk Sleep Mask" ) ,
          Tuple2 ("19" , "Tea Sampler Set" ) ,
          Tuple2 ("20" , "Umbrella with Wind Vents" ) ,
          Tuple2 ("21" , "Virtual Reality Headset" ) ,
          Tuple2 ("22" , "Wireless Headphones" ) ,
          Tuple2 ("23" , "X-shaped Wine Rack" ) ,
          Tuple2 ("24" , "Yoga Mat" ) ,
          Tuple2 ("25" , "Zinc Alloy Corkscrew Wine Opener" ) ,
          Tuple2 ("26" , "6-Port USB Charging Station" ) ,
          Tuple2 ("27" , "7-Piece Cocktail Shaker Set" ) ,
          Tuple2 ("28" , "8-Bit Retro Gaming Console" ) ,
          Tuple2 ("29" , "9-Piece Stainless Steel Knife Set" )
        )
      ) ,
      Tuple2 ("prices" ,
        Seq (
          Tuple2 ("0" , "40") ,
          Tuple2 ("1" , "50") ,
          Tuple2 ("2" , "25") ,
          Tuple2 ("3" , "70") ,
          Tuple2 ("4" , "100") ,
          Tuple2 ("5" , "60") ,
          Tuple2 ("6" , "30") ,
          Tuple2 ("7" , "40") ,
          Tuple2 ("8" , "25") ,
          Tuple2 ("9" , "20") ,
          Tuple2 ("10" , "90") ,
          Tuple2 ("11" , "50") ,
          Tuple2 ("12" , "100") ,
          Tuple2 ("13" , "150") ,
          Tuple2 ("14" , "30") ,
          Tuple2 ("15" , "30") ,
          Tuple2 ("16" , "25") ,
          Tuple2 ("17" , "200") ,
          Tuple2 ("18" , "20") ,
          Tuple2 ("19" , "30") ,
          Tuple2 ("20" , "20") ,
          Tuple2 ("21" , "100") ,
          Tuple2 ("22" , "100") ,
          Tuple2 ("23" , "20") ,
          Tuple2 ("24" , "30") ,
          Tuple2 ("25" , "20") ,
          Tuple2 ("26" , "25") ,
          Tuple2 ("27" , "40") ,
          Tuple2 ("28" , "80") ,
          Tuple2 ("29" , "100")
        )
      ) ,
      Tuple2 ("possessions" ,
        Seq (
          Tuple2 ("0" , "12") ,
          Tuple2 ("1" , "7") ,
          Tuple2 ("2" , "20") ,
          Tuple2 ("3" , "15") ,
          Tuple2 ("4" , "8") ,
          Tuple2 ("5" , "23") ,
          Tuple2 ("6" , "14") ,
          Tuple2 ("7" , "2") ,
          Tuple2 ("8" , "19") ,
          Tuple2 ("9" , "5") ,
          Tuple2 ("10" , "22") ,
          Tuple2 ("11" , "13") ,
          Tuple2 ("12" , "4") ,
          Tuple2 ("13" , "18") ,
          Tuple2 ("14" , "9") ,
          Tuple2 ("15" , "24") ,
          Tuple2 ("16" , "11") ,
          Tuple2 ("17" , "3") ,
          Tuple2 ("18" , "17") ,
          Tuple2 ("19" , "10") ,
          Tuple2 ("20" , "21") ,
          Tuple2 ("21" , "16") ,
          Tuple2 ("22" , "1") ,
          Tuple2 ("23" , "25") ,
          Tuple2 ("24" , "6") ,
          Tuple2 ("25" , "0") ,
          Tuple2 ("26" , "15") ,
          Tuple2 ("27" , "10") ,
          Tuple2 ("28" , "20") ,
          Tuple2 ("29" , "5")
        )
      ) ,
      Tuple2 ("operations" ,
        Seq (
          Tuple2 ("0" , "price 10 100") ,
          Tuple2 ("1" , "advertise 10") ,
          Tuple2 ("2" , "transfer 10 25") ,
          Tuple2 ("3" , "advertise 11") ,
          Tuple2 ("4" , "hide 11")
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

