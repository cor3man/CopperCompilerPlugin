import akka.actor.ActorSystem
import com.sbuslab.sbus.{Context, Sbus}
import com.sbuslab.utils._

import scala.concurrent.{ExecutionContext, Future}

class InvoiceService(sbus: Sbus)(implicit val ec: ExecutionContext, actorSystem: ActorSystem) extends Logging with Memoize with FutureHelpers {

  @Schedule("3 minutes")
  @Subscribe("invoices.calculate-fees")
  def calculateFees(cmd: Any)(implicit context: Context): Future[Unit] = {
    Future.unit
  }

  sbus.request[Unit]("rose.send-transaction")
}