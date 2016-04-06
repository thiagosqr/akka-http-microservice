import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.stream.FlowMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.typesafe.config.Config

import scala.concurrent.{ExecutionContextExecutor, Future, Promise}

/**
  * Created by thiago on 4/5/16.
  */
trait Service extends Protocols {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: FlowMaterializer

  def config: Config
  val logger: LoggingAdapter

  lazy val telizeConnectionFlow: Flow[HttpRequest, HttpResponse, Any] =
    Http().outgoingConnection(config.getString("services.telizeHost"), config.getInt("services.telizePort"))

  def telizeRequest(request: HttpRequest): Future[HttpResponse] = Source.single(request).via(telizeConnectionFlow).runWith(Sink.head)

  def fetchIpInfo(ip: String): Future[Either[String, IpInfo]] = {

    val p = Promise[Either[String, IpInfo]]()
    p.success(Right(IpInfo("192.168.0.1",Some("BR"), Some("Goiânia"), Some(2.2), Some(2.2))))
    p.future

    //    telizeRequest(RequestBuilding.Get(s"/geoip/$ip")).flatMap { response =>
    //      response.status match {
    //        case OK => Unmarshal(response.entity).to[IpInfo].map(Right(_))
    //        case BadRequest => Future.successful(Left(s"$ip: incorrect IP format"))
    //        case _ => Unmarshal(response.entity).to[String].flatMap { entity =>
    //          val error = s"Telize request failed with status code ${response.status} and entity $entity"
    //          logger.error(error)
    //          Future.failed(new IOException(error))
    //        }
    //      }
    //    }
  }

  val routes = {
    logRequestResult("akka-http-microservice") {
      pathPrefix("ip") {
        (get & path(Segment)) { ip =>
          complete {
            fetchIpInfo(ip).map[ToResponseMarshallable] {
              case Right(ipInfo) => ipInfo
              case Left(errorMessage) => BadRequest -> errorMessage
            }
          }
        } ~
          (post & entity(as[IpPairSummaryRequest])) { ipPairSummaryRequest =>
            complete {
              val ip1InfoFuture = fetchIpInfo(ipPairSummaryRequest.ip1)
              val ip2InfoFuture = fetchIpInfo(ipPairSummaryRequest.ip2)
              ip1InfoFuture.zip(ip2InfoFuture).map[ToResponseMarshallable] {
                case (Right(info1), Right(info2)) => IpPairSummary(info1, info2)
                case (Left(errorMessage), _) => BadRequest -> errorMessage
                case (_, Left(errorMessage)) => BadRequest -> errorMessage
              }
            }
          }
      }
    }
  }
}
