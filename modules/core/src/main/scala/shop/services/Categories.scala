package shop.services

import shop.domain.ID
import shop.domain.category._
import shop.effects.GenUUID
import shop.sql.codecs._

import cats.effect._
import cats.syntax.all._
import skunk._
import skunk.implicits._

trait Categories[F[_]] {
  def findAll: F[List[Category]]
  def findByIds(ids: List[CategoryId]): F[List[Category]]
  def create(name: CategoryName): F[CategoryId]
}

object Categories {
  def make[F[_]: GenUUID: Concurrent](
      postgres: Resource[F, Session[F]]
  ): Categories[F] =
    new Categories[F] {
      import CategorySQL._

      def findAll: F[List[Category]] =
        postgres.use(_.execute(selectAll))

      def findByIds(ids: List[CategoryId]): F[List[Category]] =
        postgres.use { session =>
          session.prepare(selectByIds(ids)).use { q =>
            q.stream(ids, chunkSize = 1024).compile.toList
          }
        }

      def create(name: CategoryName): F[CategoryId] =
        postgres.use { session =>
          session.prepare(insertCategory).use { cmd =>
            ID.make[F, CategoryId].flatMap { id =>
              cmd.execute(Category(id, name)).as(id)
            }
          }
        }
    }
}

private object CategorySQL {

  val codec: Codec[Category] =
    (categoryId ~ categoryName).imap {
      case i ~ n => Category(i, n)
    }(c => c.uuid ~ c.name)

  val selectAll: Query[Void, Category] =
    sql"""
        SELECT * FROM categories
       """.query(codec)

  def selectByIds(ids: List[CategoryId]): Query[List[CategoryId], Category] =
    sql"""
        SELECT * FROM categories
        WHERE id IN (${categoryId.list(ids.size)})
        """.query(codec)

  val insertCategory: Command[Category] =
    sql"""
        INSERT INTO categories
        VALUES ($codec)
        """.command

}
