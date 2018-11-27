package teamthree.clp.YaPlaceApi.model

import io.circe._

object ApiModels {
  final case class Phone(numberPhone: String)
  final case class MetaData(id: String, name: String, address: Option[String],
                            url: Option[String], phones: Option[List[Phone]], timeWork: Option[TimeWork])

  final case class AdditionalData(url: Option[String], phones: Option[List[Phone]], timeWork: Option[TimeWork])
  final case class StateCafe(isOpen: Int, text: String)
  final case class TimeWork(time: String, state: Option[StateCafe])
  final case class Position(longitude: Double, latitude: Double)
  final case class Geometry(typeGeom: String, coordinates: (Double, Double))

  final case class Cafe(id: String, name: String, address: Option[String],
                        data: AdditionalData, geometry: Geometry)

  implicit val decodePhone: Decoder[Phone] =
    Decoder.forProduct1("formatted")(Phone.apply)

  implicit val decodeStateCafe: Decoder[StateCafe] =
    Decoder.forProduct2("is_open_now", "text")(StateCafe.apply)

  implicit val decodeTimeWork: Decoder[TimeWork] =
    Decoder.forProduct2("text", "State")(TimeWork.apply)

  implicit val decodePosition: Decoder[Position] =
    Decoder.forProduct2("0", "1")(Position.apply)

  implicit val decodeGeometry: Decoder[Geometry] =
    Decoder.forProduct2("type", "coordinates")(Geometry.apply)

  implicit val decodeMetaData: Decoder[MetaData] =
    Decoder.forProduct6("id", "name", "address", "url", "Phones", "Hours")(MetaData.apply)

  implicit val decodeCafe: Decoder[Cafe] = (c: HCursor) => {
    for {
      data <- c.downField("properties").downField("CompanyMetaData").as[MetaData]
      position <- c.downField("geometry").as[Geometry]
    } yield Cafe(data.id, data.name, data.address,
        AdditionalData(data.url, data.phones, data.timeWork), position)
  }

  implicit val decodeListCafes: Decoder[List[Cafe]] =
    Decoder.forProduct1("features")(List[Cafe])
}