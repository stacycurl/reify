package reify.internal.util

import java.lang.reflect.Field
import reify.{Reified, ReifiedFields, Reify}

import scala.collection.immutable.ListMap
import scala.util.Try


case class Fields(fields: List[Field]) {
  private def toMap: Map[String, Field] =
    ListMap.apply(fields.map(field => field.getName -> field): _*)

  def --(other: Fields): Fields = {
    val (lhs, rhs) = (toMap, other.toMap)

    val resultMap = lhs.filterNot {
      case (name, _) => rhs.contains(name)
    }

    Fields(resultMap.values.toList)
  }

  override def toString: String =
    s"""Fields(
       |  ${fields.map(_.getName).mkString(",\n  ")}
       |)""".stripMargin

  def reify(owner: Any): ReifiedFields = {
    val reifiedFields: Map[String, Either[String, Reified]] =
      fields.map(field => field.getName -> reifyField(owner, field))(collection.breakOut)

    val result = reifiedFields.flatMap {
      case (name, value) => value.toOption.map(_ -> name)
    }

    ReifiedFields(result)
  }

  private def reifyField(owner: Any, field: Field): Either[String, Reified] = {
    field.setAccessible(true)

    for {
      fieldValue <- Try(field.get(owner)).toEither.left.map(_.getMessage)
      reify      <- Reify.find(field.getType).toRight("Could not find reify instance")
      reified    <- Try(reify.asInstanceOf[Reify[AnyRef]].reify(fieldValue)).toEither.left.map(_.getMessage)
    } yield reified
  }
}

object Fields {
  def create(value: Any): Fields =
    forClass(value.getClass)

  def forClass(klass: Class[_]): Fields =
    Fields(klass.getDeclaredFields.toList)
}