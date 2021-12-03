import scala.jdk.StreamConverters._
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

object PuzzleInputDownloader {
  def download(url: String): LazyList[String] = {
    val request = HttpRequest.newBuilder.GET.uri(URI.create(url))
      .header("cookie", System.getenv("puzzle-cookies"))
      .build
    val client = HttpClient.newBuilder.build
    client.send(request, HttpResponse.BodyHandlers.ofLines)
      .body
      .toScala(LazyList)
  }
}
