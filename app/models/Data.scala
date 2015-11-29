package models

import java.util.Locale

object CurrencyCode extends Enumeration {
  val EUR, USD = Value
}

case class LocalizedString(values: Map[Locale, String])
object LocalizedString {
  def apply(ls: (Locale, String)*): LocalizedString =
    LocalizedString(ls.toMap)
}

case class Price(
  centAmount: Long,
  currencyCode: CurrencyCode.Value)

case class Variant(
  master: Boolean,
  name: String,
  names: LocalizedString,
  price: Price,
  prices: Map[String, Price])

case class Product(
  id: String,
  name: String,
  names: LocalizedString,
  variants: List[Variant]) {

  def masterVariant: Option[Variant] = variants.find(_.master == true)
}

object Product {
  import Locale._

  import CurrencyCode._

  val products = List(
    Product(
      id = "1",
      name = "running shoes",
      names = LocalizedString(ENGLISH → "running shoes", FRENCH → "godasses pour courir vite"),
      variants = List(
        Variant(master = true,
          "white", LocalizedString(ENGLISH → "white", FRENCH → "blanc"),
          Price(3900, USD),
          Map("us" → Price(3900, USD), "fr" → Price(3400, EUR))),
        Variant(master = false,
          "black", LocalizedString(ENGLISH → "black", FRENCH → "noir"),
          Price(3600, USD),
          Map("us" → Price(3600, USD), "fr" → Price(3200, EUR))),
        Variant(master = false,
          "red", LocalizedString(ENGLISH → "red", FRENCH → "rouge"),
          Price(3500, USD),
          Map("us" → Price(3500, USD), "fr" → Price(3100, EUR))))))
}

class ProductRepo {
  def getProduct(id: String): Option[Product] =
    Product.products.find(_.id == id)

}
