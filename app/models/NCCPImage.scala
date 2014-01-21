package models

/**
  NCCPImage is the interface between the REST API implemented in this
  app and the FTP site that hosts the actual images at
  ftp://sensor.nevada.edu.

  @author Richard Kelley
  */

import org.apache.commons.net.ftp.FTPFile
import java.util.Calendar
import play.api.libs.json._

object NCCPImage {

  /**
    * Check that the given region is valid given our current
    * set of regions.
    */
  def validateRegion(region : String) = region match {
    case "sheep" => true
    case "snake" => true
    case "spring" => true
    case _ => false
  }

  /**
    * Check the given (region,site) pair to ensure that 
    * the pair is valid for our current collection of sites.
    */
  def validateSite(region: String, site : String) = {
    region.toLowerCase match {
      case "sheep" => Seq("1", "2", "3", "4") contains site
      case "snake" => Seq("1", "2", "3") contains site
      case "spring" => Seq("0", "1", "2", "3", "4") contains site
      case _ => false
    }
  }

  /**
    * Return a JSON list of regions for which images are available.
    */
  def regions = Json.arr("Snake", "Sheep", "Spring")

  /**
    * Given a string naming a region, return a JSON list of sites for
    * which images are available.
    * 
    * @param region A string describing the region for which to get
    * a list of sites.
    * 
    * @return an option containing either the list of sites for a 
    * correct region, or None for an invalid region. 
    */
  def sites(region : String) : Option[JsArray] = region.toLowerCase match {
    case "sheep" => Some(Json.arr("1", "2", "3", "4"))
    case "snake" => Some(Json.arr("1", "2", "3"))
    case "spring" => Some(Json.arr("0", "1", "2", "3", "4"))
    case _ => None
  }

  def padDate(date : Int, r : Range) : String =
    if (r contains date) {
      "0" + date.toString
    } else {
      date.toString
    }

  def jsonify(url : String, f : FTPFile) : JsObject = {
    val timestamp = f.getTimestamp()
    val year = timestamp.get(Calendar.YEAR)

    // The +1 is because JANUARY == 0
    val month = padDate(timestamp.get(Calendar.MONTH) + 1, 0 to 9)
    val day = padDate(timestamp.get(Calendar.DAY_OF_MONTH), 0 to 9)
    val hour = padDate(timestamp.get(Calendar.HOUR_OF_DAY), 0 to 9)
    val minute = padDate(timestamp.get(Calendar.MINUTE), 0 to 9)
    val second = padDate(timestamp.get(Calendar.SECOND), 0 to 9)

    val dateString = s"$year-$month-$day"
    val timeString = s"$hour:$minute:$second"
    val urlString = url + "/" + f.getName()
    Json.obj(
      "fileName" -> f.getName(),
      "size" -> f.getSize(),
      "date" -> dateString,
      "time" -> timeString,
      "url" -> urlString
    )

  }

  /**
    * latestImage - return a JSON object containing metadata for the
    * latest image at the given (region, site) pair. Before returning an
    * object, we perform validation to ensure that the given 
    * (region, site) pair is valid.
    * 
    * @param region A string describing the region from which to get 
    * the latest image.
    * 
    * @param site A string describing the site from which to get the 
    * latest image.
    * 
    * @return an option containing a JSON object describing the latest 
    * image for a valid (region, site) pair, or None for an invalid 
    * pair
    * 
    */
  def latestImage(region : String, site : String) =
    if (!(validateRegion(region) && validateSite(region, site))) {
      None
    } else {
      // get newest FTPFile object
      val newestFile = NCCPFtpConnection.getMostRecentImage(region, site)
      
      Some(jsonify(newestFile._1, newestFile._2))
    }

  /**
    * 
    * 
    */
  def image(region : String, site : String, 
    limit : Integer, fromDate : Option[String], 
    toDate : Option[String]) : Option[JsArray] = {

    // check that limit is valid
    if (limit < 0) {
      None
    } else if (!(validateRegion(region) && validateSite(region, site))) {
      None
    } else {
      (fromDate, toDate) match {
        case (None, None) => { 
          val (baseURL, files) = NCCPFtpConnection.getMostRecentImages(
            region, site, limit
          )
          Some(JsArray(files.map(f => jsonify(baseURL, f))))
        }
        case (None, Some(to)) => {
          Some(Json.arr(
            Json.obj("to" -> "only")
          ))
        }
        case (Some(from), None) => {
          Some(Json.arr(
            Json.obj("from" -> "only")
          ))
        }
        case (Some(from), Some(to)) => {
          Some(Json.arr(
            Json.obj("to" -> "from")
          ))
        }
      }
    }
  }
}
