package shop.http.routes.secured

import shop.domain.item._
import shop.domain.order._
import shop.http.auth.users.CommonUser
import shop.http.vars.OrderIdVar
import shop.programs.Proofs
import shop.services._

import cats.Monad
import cats.effect.Clock
import cats.syntax.all._
import decrel.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server._

final case class OrderRoutes[F[_]: Monad: Clock](
    orders: Orders[F],
    proofs: Proofs[F]
) extends Http4sDsl[F] {
  import proofs._

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
            items <- order.traverse(
              o => (Order.items <>: (Item.brand & Item.category)).toF(o)
            )
          } yield (order, items)
        } else
        Ok(orders.get(user.value.id, orderId))

  }

  def routes(authMiddleware: AuthMiddleware[F, CommonUser]): HttpRoutes[F] = Router(
    prefixPath -> authMiddleware(httpRoutes)
  )

}
