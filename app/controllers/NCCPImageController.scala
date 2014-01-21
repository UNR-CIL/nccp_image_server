package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json._

import models.NCCPImage

object NCCPImageController extends Controller {

  // done
  def regions = Action {
    Ok(NCCPImage.regions)
  }

  // done
  def sites(region : String) = Action { 
    NCCPImage.sites(region) match {
      case Some(sites) => Ok(sites)
      case None => NotFound
    }
  }

  def latestImage(region : String, site : String) = Action {
    NCCPImage.latestImage(region, site) match {
      case Some(imageObject) => Ok(imageObject)
      case None => NotFound
    }
  }

  def image(region : String, site : String, limit : Integer, fromDate : Option[String], toDate : Option[String]) = Action { 
    NCCPImage.image(region, site, limit, fromDate, toDate) match {
      case Some(imageObject) => Ok(imageObject)
      case None => NotFound
    }

  }

}
