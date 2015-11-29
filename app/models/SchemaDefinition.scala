package models

import java.util.Locale

import com.neovisionaries.i18n.CountryCode
import sangria.ast
import sangria.schema._
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}

/**
 * Defines a GraphQL schema for the current project
 */
object SchemaDefinition {
  val ID = Argument("id", StringType, description = "id of the product")
  val LimitArg = Argument("limit", OptionInputType(IntType))
  val OffsetArg = Argument("offset", OptionInputType(IntType))
  val MasterVariantArg = Argument("master", OptionInputType(BooleanType))

  case object LocaleCoercionViolation extends ValueCoercionViolation("Locale value expected")
  case object CountryCodeViolation extends ValueCoercionViolation("ISO 639-1 country code expected")

  def parseLocale(s: String) = Try(Locale.forLanguageTag(s)) match {
    case Success(l) ⇒ Right(l)
    case Failure(_) ⇒ Left(LocaleCoercionViolation)
  }

  val LocaleType = ScalarType[Locale]("Locale",
    description = Some("Locale is a scalar value represented as a string language tag."),
    coerceOutput = locale ⇒ ast.StringValue(locale.toLanguageTag),
    coerceUserInput = {
      case s: String ⇒ parseLocale(s)
      case _ ⇒ Left(LocaleCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(s, _) ⇒ parseLocale(s)
      case _ ⇒ Left(LocaleCoercionViolation)
    })

  def parseCountry(s: String): Either[CountryCodeViolation.type, String] =
    if (CountryCode.getByCode(s) == CountryCode.UNDEFINED) Left(CountryCodeViolation)
    else Right(s)

  val CountryType = ScalarType[String]("Country",
    description = Some("Country is a scalar value represented as a string language tag."),
    coerceOutput = ast.StringValue(_),
    coerceUserInput = {
      case s: String ⇒ parseCountry(s)
      case _ ⇒ Left(CountryCodeViolation)
    },
    coerceInput = {
      case ast.StringValue(s, _) ⇒ parseCountry(s)
      case _ ⇒ Left(CountryCodeViolation)
    })

  val LocaleArg = Argument("locale", LocaleType, description =
    "String is define for different locales. This argument specifies the desired locale.")

  val CountryArg = Argument("country", CountryType, description =
    "String is define for different countries. This argument specifies the desired country.")

  def localizedStringField[Ctx, Val](name: String, resolve: Val ⇒ LocalizedString): Field[Ctx, Val] =
    Field(name, OptionType(StringType),
      arguments = LocaleArg :: Nil,
      resolve = ctx ⇒ resolve(ctx.value).values get ctx.arg(LocaleArg))


  val CurrencyEnum = EnumType(
    "Currency",
    Some("ISO_4217 currency code"),
    List(
      EnumValue("EUR", Some("euro"), CurrencyCode.EUR),
      EnumValue("USD", Some("US dollar"), CurrencyCode.USD)))

  val Price =
    ObjectType(
      "price",
      fields[Unit, Price](
        Field("centAmount", LongType,
          Some("The amount in cents (the smallest indivisible unit of the currency)."),
          resolve = _.value.centAmount),
        Field("currencyCode", CurrencyEnum, resolve = _.value.currencyCode)))

  val Variant =
    ObjectType(
      "variant",
      fields[Unit, Variant](
        Field("name", StringType, Some("name"), resolve = _.value.name),
        localizedStringField("names", _.names),
        Field("price", Price, resolve = _.value.price),
        Field("master", BooleanType, Some("master variant"), resolve = _.value.master),
        Field("prices", OptionType(Price),
          arguments = CountryArg :: Nil,
          resolve = ctx ⇒ ctx.value.prices.get(ctx.arg(CountryArg)))))

  private def filter[A](l: List[A], limit: Option[Int], offset: Option[Int]): List[A] =
    (limit, offset) match {
      case (Some(li), Some(of)) ⇒ l.slice(of, of + li)
      case (Some(li), None) ⇒ l.take(li)
      case (None, Some(of)) ⇒ l.drop(of)
      case (None, None) ⇒ l
    }

  private def filterVariant(l: List[Variant], master: Option[Boolean], limit: Option[Int], offset: Option[Int]): List[Variant] = {
    val variants = master.fold(l)(m ⇒ l.filter(_.master == m))
    filter(variants, limit = limit, offset = offset)
  }

  val Product =
    ObjectType(
      "product",
      fields[Unit, Product](
        Field("id", StringType, Some("unique identifier"), resolve = _.value.id),
        Field("name", StringType, Some("name"), resolve = _.value.name),
        localizedStringField("names", _.names),
        Field("masterVariant", OptionType(Variant), Some("variant used by default"), resolve = _.value.masterVariant),
        Field("variants", ListType(Variant), Some("other possible variants"),
          arguments = MasterVariantArg :: LimitArg :: OffsetArg :: Nil,
          resolve = ctx ⇒ filterVariant(ctx.value.variants, ctx.argOpt(MasterVariantArg), limit = ctx.argOpt(LimitArg), offset = ctx.argOpt(OffsetArg)))
      ))

  val QueryType = ObjectType[ProductRepo, Unit]("query",
    fields[ProductRepo, Unit](
      Field("product", OptionType(Product),
        arguments = ID :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.getProduct(ctx arg ID))))

  val MyShopSchema = Schema(QueryType)
}
