package hrf.gg

import slick.jdbc.HsqldbProfile.api._
import slick.jdbc.HsqldbProfile.api.DBIO.seq

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.settings.ServerSettings
import akka.stream.ActorMaterializer

import ch.megard.akka.http.cors.scaladsl.CorsDirectives._

object GoodGame {
    case class User(name : String, secret : String, id : String)

    class Users(tag : Tag) extends Table[User](tag, "Users") {
        def name = column[String]("name")
        def secret = column[String]("secret")
        def id = column[String]("id", O.PrimaryKey)
        def * = (name, secret, id).mapTo[User]
    }

    val users = TableQuery[Users]


    case class Journal(name : String, public : Boolean, status : String, message : String, id : String)

    class Journals(tag : Tag) extends Table[Journal](tag, "Journals") {
        def name = column[String]("name")
        def public = column[Boolean]("public")
        def status = column[String]("status")
        def message = column[String]("message")
        def id = column[String]("id", O.PrimaryKey)
        def * = (name, public, status, message, id).mapTo[Journal]
    }

    val journals = TableQuery[Journals]


    case class Entry(journalId : String, index : Int, userId : String, text : String)

    class Entries(tag : Tag) extends Table[Entry](tag, "Entries") {
        def journalId = column[String]("journalId")
        def index = column[Int]("index")
        def userId = column[String]("userId")
        def text = column[String]("text")
        def * = (journalId, index, userId, text).mapTo[Entry]
        def pk = primaryKey("Entries" + "Key", (journalId, index))
        def journal = foreignKey("Entries" + "Journals", journalId, journals)(_.id)
        def user = foreignKey("Entries" + "Users", userId, users)(_.id)
    }

    val entries = TableQuery[Entries]


    case class AccessRight(journalId : String, userId : String, right : String)

    class AccessRights(tag : Tag) extends Table[AccessRight](tag, "AccessRights") {
        def journalId = column[String]("journalId")
        def userId = column[String]("userId")
        def right = column[String]("right")
        def * = (journalId, userId, right).mapTo[AccessRight]
        def pk = primaryKey("AccessRights" + "Key", (journalId, userId, right))
        def journal = foreignKey("AccessRights" + "Journals", journalId, journals)(_.id)
        def user = foreignKey("AccessRights" + "Users", userId, users)(_.id)
    }

    val accessRights = TableQuery[AccessRights]


    case class Play(journalId : String, userId : String, secret : String)

    class Plays(tag : Tag) extends Table[Play](tag, "Plays") {
        def journalId = column[String]("journalId")
        def userId = column[String]("userId")
        def secret = column[String]("secret")
        def * = (journalId, userId, secret).mapTo[Play]
        def journal = foreignKey("Play" + "Journals", journalId, journals)(_.id)
        def user = foreignKey("Play" + "Users", userId, users)(_.id)
    }

    val plays = TableQuery[Plays]


    def main(args : Array[String]) {
        if (args.size != 6) {
            println("gg <create|run> <directory> <database> <url> <cdn> <port>")
            return
        }

        val mode = args(0)
        val database = args(1)
        val directory = args(2)
        val url = args(3)
        val cdn = args(4)
        val port = args(5).toInt

        def readFile(path : String) = {
            import java.nio.charset.StandardCharsets._
            import java.nio.file.{Files, Paths}

            new String(Files.readAllBytes(Paths.get(path)), UTF_8)
        }

        implicit class Ascii(val s : String) {
            def ascii = s.filter(c => c >= 32 && c < 128)
            def asciiplus = s.filter(c => (c >= 32 && c < 128) || (c > 158 && c < 256 && c.isLetter))
            def safe = ascii.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
            def safeplus = asciiplus.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
        }

        def newSecret(n : Int) = {
            val random = new scala.util.Random()

            0.until(n).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(random.nextInt(26))).mkString("")
        }

        val db = Database.forURL("jdbc:hsqldb:file:" + database + ";hsqldb.cache_rows=10000;hsqldb.nio_data_file=false", driver="org.hsqldb.jdbcDriver")

        object execute {
            import scala.concurrent.Await
            import scala.concurrent.duration.Duration

            def apply[E <: Effect](actions : DBIOAction[_, NoStream, E]*) = Await.result(db.run(DBIO.seq(actions : _*).withPinnedSession), Duration.Inf)
            def apply[R](action : DBIOAction[R, NoStream, Effect.Read]) : R = Await.result(db.run(action.withPinnedSession), Duration.Inf)
        }

        if (mode == "create") {
            execute(users.schema.create, journals.schema.create, entries.schema.create, accessRights.schema.create, plays.schema.create)
            println("Created database.")
            return
        }

        if (mode != "run") {
            println("Unknown mode.")
            return
        }

        implicit val system = ActorSystem()
        implicit val executionContext = system.dispatcher

        def hasRight[R, E <: Effect with Effect.Read](userId : String, userSecret : String, journalId : String, right : String)(then : => DBIOAction[R, NoStream, E]) : DBIOAction[R, NoStream, E] = {
            users.filter(_.id === userId).filter(_.secret === userSecret).result.head.flatMap { _ =>
                accessRights.filter(_.journalId === journalId).filter(_.userId === userId).filter(_.right === right).result.head.flatMap { _ =>
                    then
                }
            }
        }

        def index = readFile(directory + "/index.html")

        def html(s : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s))
        def plain(s : String) = complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, s))
        def redir(s : String) = redirect(s, StatusCodes.SeeOther)

        val route = cors() {
            (pathPrefix("hrf")) {
                optionalHeaderValueByName("Referer") { referer =>
                    if (referer.exists(_.startsWith(url)))
                        getFromDirectory(directory)
                    else
                        complete(StatusCodes.NotFound, "")
                }
            } ~
            (get & path("")) {
                redir("/play")
            } ~
            (get & path("play" / "")) {
                redir("/play")
            } ~
            (get & path("play")) {
                html(index
                    .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                    .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                    .replace("data-meta=\"" + "\"", "data-meta=\"" + "" + "\"")
                )
            } ~
            (get & path("play" / Segment / "")) { meta =>
                redir("/play/" + meta)
            } ~
            (get & path("play" / Segment)) { meta =>
                html(index
                    .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                    .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                    .replace("data-meta=\"" + "\"", "data-meta=\"" + meta + "\"")
                )
            } ~
            (get & path("play" / Segment / Segments)) { (meta, secret) =>
                if (secret.length == 1 && secret(0).length == 16) {
                    val (user, play) = execute(plays.filter(_.secret === secret(0)).flatMap { play =>
                        users.filter(_.id === play.userId).map((_, play))
                    }.result.head)

                    html(index
                        .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                        .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                        .replace("data-meta=\"" + "\"", "data-meta=\"" + meta + "\"")
                        .replace("data-user=\"" + "\"", "data-user=\"" + user.id + "\"")
                        .replace("data-secret=\"" + "\"", "data-secret=\"" + user.secret + "\"")
                        .replace("data-lobby=\"" + "\"", "data-lobby=\"" + play.journalId + "\"")
                    )
                }
                else
                    html(index
                        .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                        .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                        .replace("data-meta=\"" + "\"", "data-meta=\"" + meta + "\"")
                    )
            } ~
            (post & path("new-user")) {
                decodeRequest {
                    entity(as[String]) { body =>
                        val name = body.take(32).trim.safeplus
                        val user = User(name, newSecret(16), newSecret(16))
                        execute(users += user)
                        plain(user.id + "\n" + user.secret)
                    }
                }
            } ~
            (post & path("new-journal" / Segment / Segment)) { case (userId, userSecret) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val name = body.take(128).trim.safeplus
                        val id = newSecret(16)
                        execute(users.filter(_.id === userId).filter(_.secret === userSecret).map(_.id).result.head.flatMap { userId =>
                            seq(
                                journals += Journal(name, false, "", "", id),
                                accessRights += AccessRight(id, userId, "full"),
                                accessRights += AccessRight(id, userId, "read"),
                                accessRights += AccessRight(id, userId, "append")
                            )
                        })
                        plain(id)
                    }
                }
            } ~
            (post & path("grant-read" / Segment / Segment / Segment / Segment)) { case (userId, userSecret, journalId, anotherUser) =>
                execute(hasRight(userId, userSecret, journalId, "full") {
                    users.filter(_.id === anotherUser).result.head.flatMap { _ =>
                        accessRights += AccessRight(journalId, anotherUser, "read")
                    }
                })
                plain("")
            } ~
            (post & path("grant-read-append" / Segment / Segment / Segment / Segment)) { case (userId, userSecret, journalId, anotherUser) =>
                execute(hasRight(userId, userSecret, journalId, "full") {
                    users.filter(_.id === anotherUser).result.head.flatMap { _ =>
                        accessRights ++= List(AccessRight(journalId, anotherUser, "read"), AccessRight(journalId, anotherUser, "append"))
                    }
                })
                plain("")
            } ~
            (post & path("new-play" / Segment / Segment / Segment)) { case (userId, userSecret, journalId) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val name = body.take(32).trim.safeplus
                        val secret = newSecret(16)
                        val user = User(name, newSecret(16), newSecret(16))
                        execute(hasRight(userId, userSecret, journalId, "full") {
                            seq(
                                users += user,
                                accessRights += AccessRight(journalId, user.id, "read"),
                                accessRights += AccessRight(journalId, user.id, "append"),
                                plays += Play(journalId, user.id, secret)
                            )
                        })
                        plain(user.id + "\n" + secret)
                    }
                }
            } ~
            (get & path("read" / Segment / Segment / Segment / IntNumber)) { (userId, userSecret, journalId, from) =>
                val log = execute(hasRight(userId, userSecret, journalId, "read") {
                    entries.filter(_.journalId === journalId).filter(_.index >= from).map(_.text).result
                })
                plain(log.mkString("\n"))
            } ~
            (post & path("append" / Segment / Segment / Segment / IntNumber)) { (userId, userSecret, journalId, from) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val ss = body.split('\n').toList.map(_.asciiplus)

                        try {
                            execute(hasRight(userId, userSecret, journalId, "append") {
                                entries ++= 0.until(ss.size).map(n => Entry(journalId, from + n, userId, ss(n)))
                            })
                            complete(StatusCodes.Accepted)
                        }
                        catch {
                            case e : java.sql.SQLIntegrityConstraintViolationException => complete(StatusCodes.Conflict)
                        }
                    }
                }
            }
        }

        val settings = ServerSettings("").withRemoteAddressAttribute(true)

        var server = Http().newServerAt("0.0.0.0", port).withSettings(settings)

        val keyFile = new java.io.File("certificate.pkcs12")

        if (keyFile.exists()) {
            val hcc = Ssl.serverHttpsContext(keyFile, "")

            server = server.enableHttps(hcc)
        }

        val bindingFuture = server.bind(route)

        println("Started server.")

        if (port != 80 && keyFile.exists()) {
            val redirroute = get {
                redirect(url, StatusCodes.MovedPermanently)
            }

            Http().newServerAt("0.0.0.0", 80).bind(redirroute)

            println("Started redirect server.")
        }

        while (true)
            Thread.sleep(1000)

        bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
    }
}
