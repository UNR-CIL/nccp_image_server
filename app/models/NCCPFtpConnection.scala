package models

import java.net.UnknownHostException
import java.io._
import org.apache.commons.net.ftp._

/**
  * NCCPFtpConnection represents a connection to the NCCP FTP
  * server. This class hides the messy FTP connection and directory
  * string parsing code.
  * 
  * @author Richard Kelley
  */
object NCCPFtpConnection {

  // The IP address of the FTP server
  val server = "134.197.38.160"

  /**
    * constructFtpDirectory - Use the given (region, string) pair
    * to construct the corresponding ftp directory. The resulting
    * URL points to a directory containing directories that contain
    * images. The directories are named with numbers. Increasing
    * numbers correspond to more recent sets of images.
    * 
    * NB - assumes that (region,site) has been validated!!!!!!!!
    * 
    * @param region A string describing a valid region in our
    * current configuration
    * 
    * @param site A string describing a valid site for the given
    * region.
    */
  def constructFtpDirectory(region : String, site : String) = {
    s"Raw Data Files/Nevada Climate Change Project/${region}/Site $site/Camera/Images/"
  }

  def getMostRecentImage(region : String, site : String) : 
      (String, FTPFile) = {
    val baseDirectory = constructFtpDirectory(region, site)
    val ftpClient = new FTPClient()

    ftpClient.connect(server)
    val reply = ftpClient.getReplyCode()
    if (!FTPReply.isPositiveCompletion(reply)) {
      // something went wrong, we have to disconnect...
      try {
        ftpClient.disconnect()
        println("ftp server refused connection...")
      } catch {
        case ioe : IOException => println("Disconnect weirdness...")
      }
    }

    ftpClient.login("anonymous", "")
    var cdSuccess = ftpClient.changeWorkingDirectory(baseDirectory)
    if (!cdSuccess) {
      println("Couldn't cd...")
    }

    val baseDirFiles = ftpClient.listFiles(".")
    val directoryList = for {
      f <- baseDirFiles if f.isDirectory()
    } yield f

    // get the newest directory
    var currentMaxDir = directoryList.head
    for(d <- directoryList.tail) {
      if (d.getTimestamp().compareTo(currentMaxDir.getTimestamp()) > 0) {
        currentMaxDir = d
      }
    }

    // cd to newest directory
    cdSuccess = ftpClient.changeWorkingDirectory(currentMaxDir.getName())
    if (!cdSuccess) {
      println("Couldn't cd to newest directory")
      println(baseDirectory + currentMaxDir.getName())
    }

    // get files in newest directory
    val newestFiles = ftpClient.listFiles(".")
    val fileList = for {
      f <- newestFiles if f.isFile()
    } yield f

    // get newest file
    var currentMaxFile = fileList.head
    for (f <- fileList.tail) {
      if (f.getTimestamp().compareTo(currentMaxFile.getTimestamp()) > 0) {
        currentMaxFile = f
      }
    }
    
    ("ftp://sensor.nevada.edu/" + 
      baseDirectory + 
      currentMaxDir.getName(), currentMaxFile)

  }

  def getMostRecentImages(region : String, site : String, 
    limit : Int) : (String, Array[FTPFile]) = {
    val baseDirectory = constructFtpDirectory(region, site)
    val ftpClient = new FTPClient()

    ftpClient.connect(server)
    val reply = ftpClient.getReplyCode()
    if (!FTPReply.isPositiveCompletion(reply)) {
      // something went wrong, we have to disconnect...
      try {
        ftpClient.disconnect()
        println("ftp server refused connection...")
      } catch {
        case ioe : IOException => println("Disconnect weirdness...")
      }
    }

    ftpClient.login("anonymous", "")
    var cdSuccess = ftpClient.changeWorkingDirectory(baseDirectory)
    if (!cdSuccess) {
      println("Couldn't cd...")
    }

    val baseDirFiles = ftpClient.listFiles(".")
    val directoryList = for {
      f <- baseDirFiles if f.isDirectory()
    } yield f

    // determine the number of folders we need to go into. Start with
    // the most recent folder, and go backwards from there.
    
    // get the newest directory
    var currentMaxDir = directoryList.head
    for(d <- directoryList.tail) {
      if (d.getTimestamp().compareTo(currentMaxDir.getTimestamp()) > 0) {
        currentMaxDir = d
      }
    }
    // cd to newest directory
    cdSuccess = ftpClient.changeWorkingDirectory(currentMaxDir.getName())
    if (!cdSuccess) {
      println("Couldn't cd to newest directory")
      println(baseDirectory + currentMaxDir.getName())
    }    

    // get files in newest directory
    val newestFiles = ftpClient.listFiles(".")
    val fileList = for {
      f <- newestFiles if f.isFile()
    } yield f

    val files = fileList.sortWith((f1 : FTPFile, f2 : FTPFile) => 
      (f1.getTimestamp()).compareTo (f2.getTimestamp()) > 0
    ).take(limit)

    ("ftp://sensor.nevada.edu/" + 
      baseDirectory + 
      currentMaxDir.getName(), files)


  }

}
