package shop.http.routes.secured

import cats.Monad
import cats.syntax.all._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server._
import shop.domain.order._
import shop.http.auth.users.CommonUser
import shop.http.vars.OrderIdVar
import shop.services._

final case class OrderRoutes[F[_]: Monad](
    orders: Orders[F],
    items: Items[F]
) extends Http4sDsl[F] {

  private[routes] val prefixPath = "/orders"

  object ExpandedQueryParam extends OptionalQueryParamDecoderMatcher[Boolean]("expanded")

  private val httpRoutes: AuthedRoutes[CommonUser, F] = AuthedRoutes.of {

    case GET -> Root as user =>
      Ok(orders.findBy(user.value.id))

    case GET -> Root / OrderIdVar(orderId) :? ExpandedQueryParam(expanded) as user =>
      if (expanded.getOrElse(false))
        Ok {
          for {
            order <- orders.get(user.value.id, orderId)
            items <- order.toList
              .flatTraverse { order =>
                order.items.keySet.toList.traverse { i =>
                  items.findById(i)
                }
              }
              .map(_.flatten)
          } yield (order, items)
        } else
        Ok(orders.get(user.value.id, orderId))

  }

  def routes(authMiddleware: AuthMiddleware[F, CommonUser]): HttpRoutes[F] = Router(
    prefixPath -> authMiddleware(httpRoutes)
  )

}
