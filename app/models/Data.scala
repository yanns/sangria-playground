package models

import java.util.Locale

object CurrencyCode extends Enumeration {
  val EUR, USD = Value
}

case class LocalizedString(values: Map[Locale, String]) {
  def apply(l: Locale): String = values(l)
}

object LocalizedString {
  def apply(ls: (Locale, String)*): LocalizedString =
    LocalizedString(ls.toMap)

  def i18n(ls: (Locale, String)*): LocalizedString =
    LocalizedString(ls.toMap)

}

case class Price(
  centAmount: Long,
  currencyCode: CurrencyCode.Value)

case class Variant(
  master: Boolean,
  descriptions: LocalizedString,
  prices: Map[String, Price]) {

  def description = descriptions(Locale.ENGLISH)
  def price = prices("us")
}

case class Product(
  id: String,
  names: LocalizedString,
  variants: List[Variant],
  canBeCombinedWith: List[String] = Nil) {

  def name = names(Locale.ENGLISH)

  def masterVariant: Option[Variant] = variants.find(_.master == true)
}

object Product {
  import Locale._
  import CurrencyCode._
  import LocalizedString.i18n
  import scala.collection.mutable

  val products = mutable.ListBuffer(
    Product(
      id = "45",
      names = i18n(ENGLISH → "running shoes", FRENCH → "godasses pour courir vite"),
      variants = List(
        Variant(master = true,
          i18n(ENGLISH → "white", FRENCH → "blanc"),
          Map("us" → Price(3900, USD), "fr" → Price(3400, EUR))),
        Variant(master = false,
          i18n(ENGLISH → "black", FRENCH → "noir"),
          Map("us" → Price(3600, USD), "fr" → Price(3200, EUR))),
        Variant(master = false,
          i18n(ENGLISH → "red", FRENCH → "rouge"),
          Map("us" → Price(3500, USD), "fr" → Price(3100, EUR))))),

    Product(
      id = "46",
      names = i18n(ENGLISH → "basketball shoes", FRENCH → "godasses pour sauter haut"),
      variants = List(
        Variant(master = true,
          i18n(ENGLISH → "white", FRENCH → "blanc"),
          Map("us" → Price(3900, USD), "fr" → Price(3400, EUR))),
        Variant(master = false,
          i18n(ENGLISH → "black", FRENCH → "noir"),
          Map("us" → Price(4600, USD), "fr" → Price(4200, EUR)))),
      canBeCombinedWith = List("54", "58")),

    Product(
      id = "54",
      names = i18n(ENGLISH → "basketball shirt", FRENCH → "shirt de basket"),
      variants = List(
        Variant(master = true,
          i18n(ENGLISH → "blank", FRENCH → "sans inscriptions"),
          Map("us" → Price(3600, USD), "fr" → Price(3200, EUR))),
        Variant(master = false,
          i18n(ENGLISH → "with number 23", FRENCH → "avec le numéro 23"),
          Map("us" → Price(3900, USD), "fr" → Price(3300, EUR)))),
      canBeCombinedWith = List("46", "58")),

    Product(
      id = "58",
      names = i18n(ENGLISH → "basketball T-shirt", FRENCH → "T-shirt de basket"),
      variants = List(
        Variant(master = true,
          i18n(ENGLISH → "blank", FRENCH → "sans inscriptions"),
          Map("us" → Price(3600, USD), "fr" → Price(3200, EUR))),
        Variant(master = false,
          i18n(ENGLISH → "with number 23", FRENCH → "avec le numéro 23"),
          Map("us" → Price(3900, USD), "fr" → Price(3300, EUR)))),
      canBeCombinedWith = List("46", "54"))
  )
}

class ProductRepo {
  import Locale._
  import CurrencyCode._
  import LocalizedString.i18n

  def getProduct(id: String): Option[Product] =
    Product.products.find(_.id == id)

  def getProducts(): List[Product] =
    Product.products.toList

  def addProduct(names: (Locale, String)*): Product = {
    val id = Product.products.map(_.id.toLong).max + 1
    val newProduct =
      Product(
        id = id.toString,
        names = i18n(names: _*),
        variants = Nil,
        canBeCombinedWith = Nil)

    Product.products += newProduct
    newProduct
  }

}

class MyShopContext {
  lazy val productRepo = new ProductRepo
}
