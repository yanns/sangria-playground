package models

import java.util.Locale

import sangria.ast
import sangria.schema._
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}

/**
 * Defines a GraphQL schema for the current project
 */
object SchemaDefinition {
  val ID = Argument("id", StringType, description = "id of the product")

  case object LocaleCoercionViolation extends ValueCoercionViolation("Locale value expected")

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

  val LocaleArg = Argument("locale", LocaleType, description =
    "String is define for different locales. This argument specifies the desired locale.")

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
        Field("price", Price, resolve = _.value.price)))

  val Product =
    ObjectType(
      "product",
      fields[Unit, Product](
        Field("id", StringType, Some("unique identifier"), resolve = _.value.id),
        Field("name", StringType, Some("name"), resolve = _.value.name),
        localizedStringField("names", _.names),
        Field("masterVariant", Variant, Some("variant used by default"), resolve = _.value.masterVariant)))
//        Field("variants", ListType(Variant), Some("other possible variants"), resolve = _.value.variants)))

  val QueryType = ObjectType[ProductRepo, Unit]("query",
    fields[ProductRepo, Unit](
      Field("product", OptionType(Product),
        arguments = ID :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.getProduct(ctx arg ID))))

  val MyShopSchema = Schema(QueryType)
}
