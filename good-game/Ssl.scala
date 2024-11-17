package hrf.gg

import java.io.InputStream
import java.security.{ KeyStore, SecureRandom }

import java.io.File
import javax.net.ssl.{ KeyManagerFactory, SSLContext, TrustManagerFactory }
import akka.http.scaladsl.{ ConnectionContext, Http, HttpsConnectionContext }

object Ssl {
    def serverHttpsContext(pkcs12File : File, pkcs12Password : String) : HttpsConnectionContext = {
        util.Using(new java.io.FileInputStream(pkcs12File)) { contentStream =>
            val ks = KeyStore.getInstance("PKCS12")
            ks.load(contentStream, pkcs12Password.toCharArray)
            ks
        }.map {
            keyStore =>
            val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
            keyManagerFactory.init(keyStore, pkcs12Password.toCharArray)
            val context = SSLContext.getInstance("TLS")
            context.init(keyManagerFactory.getKeyManagers, null, new SecureRandom)
            ConnectionContext.httpsServer(context)
        }.get
    }
}
