package shop.programs

import shop.domain.brand.Brand
import shop.domain.item.Item
import shop.domain.order.Order
import shop.effects.Background
import shop.retries.Retry
import shop.services.{ Brands, Items }

import cats.effect.Concurrent
import cats.syntax.all._
import decrel.reify.fetch
import org.typelevel.log4cats.Logger

final case class Proofs[F[_]: Background: Logger: Retry](
    items: Items[F],
    brands: Brands[F]
)(implicit override protected val CF: Concurrent[F])
    extends fetch[F] {

  implicit val orderItemsProof: Proof.Many[Order.items.type, Order, List, Item] =
    implementManyDatasource(Order.items) { (os: List[Order]) =>
      val iids = os.flatMap(_.items.keySet).distinct

      for {
        is <- items.findByIds(iids)
        iMap   = is.map(i => i.uuid -> i).toMap
        result = os.map(o => (o, o.items.keySet.map(iid => iMap.get(iid)).toList.flatten))
      } yield result
    }

  implicit val itemBrandProof: Proof.Single[Item.brand.type, Item, Brand] =
    implementSingleDatasource(Item.brand) { (items: List[Item]) =>
      for {
        b <- brands.findByIds(items.map(_.brand))
        brands = b.map(b => b.uuid -> b).toMap
        result <- items.traverse { i =>
          CF.fromOption(
              brands.get(i.brand),
              new RuntimeException(s"brand with id ${i.brand} not found")
            )
            .map(b => i -> b)
        }
      } yield result

    }

}
