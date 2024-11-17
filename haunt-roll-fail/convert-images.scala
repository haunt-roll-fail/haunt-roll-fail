package hrf
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

import scala.util.control.NonFatal
import scala.jdk.CollectionConverters._

import javax.imageio.ImageIO
import java.nio.file.Files

object ConvertImages {
    val metas = $(
        root.Meta,
        cthw.Meta,
        dwam.Meta,
        vast.Meta,
        arcs.Meta,
        coup.Meta,
        sehi.Meta,
        suok.Meta,
        yarg.Meta,
    )

    implicit class StringToPath(val s : String) {
        def asPath = java.nio.file.Paths.get(s)
    }

    def main(args : Array[String]) {
        var missing = 0

        var expected : $[String] = $

        metas.foreach { m =>
            m.assets.foreach { l =>
                l.get.foreach { a =>
                    var source = m.path + "/images/" + a.src

                    expected :+= ("webp2/" + m.path + "/images/" + a.copy(ext = "webp").src).asPath.toAbsolutePath.toString

                    if (Files.exists(source.asPath).not) {
                        +++("missing", source)

                        missing += 1

                        if (missing > 20) {
                            +++("...")
                            return
                        }
                    }
                }
            }
        }

        val unexpected = Files.walk("webp2".asPath).iterator.asScala.filter(Files.isRegularFile(_)).map(_.toAbsolutePath.toString).filterNot(expected.has)

        unexpected.take(20).foreach(s => println("unexpected " + s))

        if (unexpected.length > 20)
            +++("...")

        if (missing > 0)
            return

        metas.foreach { m =>
            m.assets.foreach { l =>
                l.get.foreach { a =>
                    var source = m.path + "/images/" + a.src

                    val destination = "webp2/" + m.path + "/images/" + a.copy(ext = "webp").src

                    if (Files.exists(destination.asPath).not || Files.getLastModifiedTime(destination.asPath).toMillis < Files.getLastModifiedTime(source.asPath).toMillis) {
                        var lossless = a.lossless

                        if (a.ext == "png") {
                            try {
                                val picture = ImageIO.read(new java.io.File(source))
                                if (picture.getWidth <= 200 || picture.getHeight <= 200)
                                    lossless = true
                            }
                            catch {
                                case NonFatal(e) => println("asset read failed: " + source)
                            }
                        }

                        Files.createDirectories(destination.reverse.dropWhile(_ != '/').reverse.asPath)

                        val builder = new java.lang.ProcessBuilder()
                        builder.environment().clear()

                        var command = $("magick")
                        command :+= source
                        command :+= "-strip"
                        if (a.scale != 100) {
                            command :+= "-scale"
                            command :+= a.scale + "%"
                        }
                        command :+= "-quality"
                        command :+= "82"
                        command :+= "-define"
                        command :+= "webp:method=6"
                        if (lossless) {
                            command :+= "-define"
                            command :+= "webp:lossless=true"
                        }
                        command :+= destination

                        builder.command(command : _*)

                        val process = builder.start()

                        if (process.waitFor() != 0)
                            println("asset convertion failed: " + source)
                        else
                            println(">>> " + destination + lossless.??(" LOSSLESS"))
                    }
                }
            }
        }
    }
}
