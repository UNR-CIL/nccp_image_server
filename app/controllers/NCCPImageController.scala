package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json._

object NCCPImageController extends Controller {

  // done
  def regions = Action {
    Ok(Json.arr("Snake", "Sheep", "Spring"))
  }

  // done
  def sites(region : String) = Action { implicit request =>
    region.toLowerCase match {
      case "sheep" => Ok(Json.arr("1", "2", "3", "4"))
      case "snake" => Ok(Json.arr("1", "2", "3"))
      case "spring" => Ok(Json.arr("0", "1", "2", "3", "4"))
      case _ => NotFound
    }
  }

  def latestImage(region : String, site : String) = Action { implicit request =>
    Ok("You'll get the latest image for the following region and site: " + region + " " + site)
  }

  def image(region : String, site : String, limit : Integer, fromDate : Option[String], toDate : Option[String]) = Action { implicit request =>
    println("fromDate: " + fromDate)
    println("toDate: " + toDate)
    val response = for {
      from <- fromDate;
      to <- toDate
    } yield { s"You'll get $limit images from $from to $to for site $site at region $region" }

    println(response)

    response match {
      case Some(str) => Ok(str)
      case None => NotFound
    }

  }

}
